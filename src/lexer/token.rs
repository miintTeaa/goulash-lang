use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token {
    // Keywords
    If,
    While,
    Break,
    Return,
    For,
    Fn,
    Let,
    Or,
    And,
    True,
    False,
    Else,
    Obj,
    Extends,
    None,

    // Symbols
    LCurly,
    RCurly,
    LParen,
    RParen,
    LBrack,
    RBrack,
    Plus,
    Minus,
    Star,
    Slash,
    Semicolon,
    Comma,
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Dot,

    // Literals
    Ident,
    Integer,
    Float,
    String,

    // Special
    Error,
    EOF,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Token::*;
        f.write_str(match self {
            If => "'if'",
            While => "'while'",
            Break => "'break'",
            Return => "'return'",
            For => "'for'",
            Fn => "'fn'",
            Let => "'let'",
            Or => "'or'",
            And => "'and'",
            Else => "'else'",
            Obj => "'class'",
            Extends => "'extends'",
            None => "'None'",
            LCurly => "'{'",
            RCurly => "'}'",
            LParen => "'('",
            RParen => "')'",
            LBrack => "'['",
            RBrack => "']'",
            Plus => "'+'",
            Minus => "'-'",
            Star => "'*'",
            Slash => "'/'",
            Equal => "'='",
            EqualEqual => "'=='",
            Bang => "'!'",
            Dot => "'.'",
            BangEqual => "'!='",
            Semicolon => "semicolon",
            Comma => "comma",
            Ident => "identifier",
            True | False => "boolean literal",
            Integer => "integer literal",
            Float => "float literal",
            String => "string literal",
            Error => "bad token",
            EOF => "eof",
        })
    }
}
