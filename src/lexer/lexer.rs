use crate::{
    error::{LangError, LangErrorData},
    span::Span,
};

use super::Token;

#[cfg(test)]
mod test {
    use crate::lexer::Token;

    use super::Lexer;

    #[test]
    pub fn lexer_keywords() {
        use Token::*;

        let (tokens, errors) =
            Lexer::new("if while break return for fn let or and true false let2 fn2 foo bar")
                .process_all();
        assert_eq!(
            &tokens.iter().map(|t| t.0).collect::<Vec<_>>(),
            &[
                If, While, Break, Return, For, Fn, Let, Or, And, True, False, Ident, Ident, Ident,
                Ident, EOF
            ]
        );
        assert_eq!(
            &tokens.iter().map(|t| t.1).collect::<Vec<_>>(),
            &[
                0..2,
                3..8,
                9..14,
                15..21,
                22..25,
                26..28,
                29..32,
                33..35,
                36..39,
                40..44,
                45..50,
                51..55,
                56..59,
                60..63,
                64..67,
                67..67
            ]
        );
        assert!(errors.is_empty(), "Errors should be empty: {errors:#?}");
    }

    #[test]
    pub fn lexer_numbers() {
        use Token::*;

        let (tokens, errors) = Lexer::new("0 2147483648 100.20").process_all();
        assert_eq!(
            &tokens.iter().map(|t| t.0).collect::<Vec<_>>(),
            &[Integer, Integer, Float, EOF]
        );
        assert_eq!(
            &tokens.iter().map(|t| t.1).collect::<Vec<_>>(),
            &[0..1, 2..12, 13..19, 19..19]
        );
        assert!(errors.is_empty(), "Errors should be empty: {errors:#?}");
    }
}

pub struct Lexer<'src> {
    src: Box<dyn Iterator<Item = char> + 'src>,
    current_char: Option<char>,
    ahead_char: Option<char>,
    current_byte_index: usize,
    errors: Vec<LangError>,
    last_span: Span,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Self {
        Self::new_from_iter(Box::new(src.chars()))
    }

    pub fn span(&self) -> Span {
        self.last_span
    }

    pub fn new_from_iter(mut src: Box<dyn Iterator<Item = char> + 'src>) -> Self {
        let current_char = src.next();
        let ahead_char = src.next();
        Self {
            src,
            current_char,
            ahead_char,
            current_byte_index: 0,
            errors: Vec::new(),
            last_span: Span::from(0..0),
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.current_char
    }

    fn peek_ahead_char(&self) -> Option<char> {
        self.ahead_char
    }

    fn next_char(&mut self) -> Option<char> {
        match self.current_char {
            Some(c) => self.current_byte_index += c.len_utf8(),
            None => (),
        }
        let ret = self.current_char.take();
        self.current_char = self.ahead_char.take();
        self.ahead_char = self.src.next();
        ret
    }

    pub fn next(&mut self) -> Token {
        match lex(self) {
            Ok((token, span)) => {
                self.last_span = span;
                token
            }
            Err(err) => {
                self.last_span = err.span();
                Token::Error
            }
        }
    }

    pub fn process_all(mut self) -> (Vec<(Token, Span)>, Vec<LangError>) {
        let mut tokens = Vec::new();

        loop {
            let token = self.next();
            tokens.push((token, self.span()));
            if token == Token::EOF {
                break;
            }
        }

        (tokens, self.to_errors())
    }

    pub fn to_errors(self) -> Vec<LangError> {
        self.errors
    }
}

macro_rules! skip_while {
    ($lexer:expr, $pattern:pat) => {
        let lexer = $lexer;
        while let $pattern = lexer.peek_char() {
            lexer.next_char();
        }
    };

    ($lexer:expr, $pattern:pat => $expr:expr) => {
        let lexer = $lexer;
        while let $pattern = lexer.peek_char() {
            if !$expr {
                break;
            }
            lexer.next_char();
        }
    };
}

fn lex(lexer: &mut Lexer) -> Result<(Token, Span), LangError> {
    let start = lexer.current_byte_index;
    macro_rules! m {
        ($token:expr) => {{
            lexer.next_char();
            Ok($token)
        }};

        ($token:expr, $pat:pat => $tk:expr) => {{
            lexer.next_char();
            if matches!(lexer.peek_char(), $pat) {
                lexer.next_char();
                Ok($tk)
            } else {
                Ok($token)
            }
        }};
    }
    match lexer.peek_char() {
        None => m!(Token::EOF),
        Some('0'..='9') => {
            lex_num(lexer);
            if matches!(
                (lexer.peek_char(), lexer.peek_ahead_char()),
                (Some('.'), Some('0'..='9'))
            ) {
                lexer.next_char();
                lex_num(lexer);
                Ok(Token::Float)
            } else {
                Ok(Token::Integer)
            }
        }
        Some('a'..='z' | 'A'..='Z' | '_') => Ok(lex_kw(lexer)),
        Some('"') => lex_str(lexer),
        Some('+') => m!(Token::Plus),
        Some('-') => m!(Token::Minus),
        Some('*') => m!(Token::Star),
        Some('/') => m!(Token::Slash),
        Some('=') => m!(Token::Equal, Some('=') => Token::EqualEqual),
        Some('!') => m!(Token::Bang, Some('=') => Token::BangEqual),
        Some('>') => m!(Token::Greater, Some('=') => Token::GreaterEqual),
        Some('<') => m!(Token::Lesser, Some('=') => Token::LesserEqual),
        Some(';') => m!(Token::Semicolon),
        Some('{') => m!(Token::LCurly),
        Some('}') => m!(Token::RCurly),
        Some('(') => m!(Token::LParen),
        Some(')') => m!(Token::RParen),
        Some(',') => m!(Token::Comma),
        Some('.') => m!(Token::Dot),
        Some(c) if c.is_whitespace() => {
            lexer.next_char();
            while let Some(c) = lexer.peek_char() {
                if !c.is_whitespace() {
                    break;
                }
                lexer.next_char();
            }
            return lex(lexer);
        }
        _ => {
            lexer.next_char();
            Err(LangErrorData::BadChar)
        }
    }
    .map(|tk| (tk, Span::from(start..lexer.current_byte_index)))
    .map_err(|err| LangError::new(err, Span::from(start..lexer.current_byte_index)))
}

fn lex_num(lexer: &mut Lexer) {
    skip_while!(lexer, Some('0'..='9'));
}

fn lex_ident(lexer: &mut Lexer) {
    skip_while!(lexer, Some('a'..='z' | 'A'..='Z' | '0'..='9' | '_'));
}

fn lex_str(lexer: &mut Lexer) -> Result<Token, LangErrorData> {
    lexer.next_char();
    loop {
        if let Some('\\') = lexer.peek_char() {
            lexer.next_char();
            lexer.next_char();
        } else if let Some('"') = lexer.peek_char() {
            lexer.next_char();
            break Ok(Token::String);
        } else if let None = lexer.peek_char() {
            break Err(LangErrorData::UnboundedString);
        } else {
            lexer.next_char();
        }
    }
}

macro_rules! lex_branch {
    ($name:ident; default: $default:expr;
        $($char:pat $(in $func:ident)? $(=> $branch:expr)?),+$(,)?) => {
        fn $name(lexer: &mut Lexer) -> Token {
            macro_rules! all {
                ($string:literal: $token:expr) => {{
                    for c in $string.chars() {
                        if lexer.peek_char() != Some(c) {
                            lex_ident(lexer);
                            return $default;
                        }
                        lexer.next_char();
                    }
                    all!($token)
                }};
                ($token:expr) => {
                    if matches!(lexer.peek_char(), Some('a'..='z' | 'A'..='Z' | '0'..='9' | '_')) {
                        lex_ident(lexer);
                        $default
                    } else {
                        $token
                    }
                }
            }
            match lexer.peek_char() {
                $(
                Some($char) => {
                    lexer.next_char();
                    $(return $branch)?
                    $(return $func(lexer))?
                }
                )+
                _ => {
                    lex_ident(lexer);
                    return $default;
                }
            }
        }
    };
}

lex_branch!(lex_kw; default: Token::Ident;
    'i' => all!("f": Token::If),
    'w' => all!("hile": Token::While),
    'b' => all!("reak": Token::Break),
    'r' => all!("eturn": Token::Return),
    'l' => all!("et": Token::Let),
    'a' => all!("nd": Token::And),
    't' => all!("rue": Token::True),
    'N' => all!("one": Token::None),
    'e' in lex_kw_e,
    'o' in lex_kw_o,
    'f' in lex_kw_f,
);

lex_branch!(lex_kw_e; default: Token::Ident;
    'l' => all!("se": Token::Else),
    'x' => all!("tends": Token::Extends),
);

lex_branch!(lex_kw_o; default: Token::Ident;
    'b' => all!("j": Token::Obj),
    'r' => all!(Token::Or),
);

lex_branch!(lex_kw_f; default: Token::Ident;
    'o' => all!("r": Token::For),
    'a' => all!("lse": Token::False),
    'n' => all!(Token::Fn),
);
