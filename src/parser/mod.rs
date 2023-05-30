use crate::{ast::Stmt, error::LangError, lexer::Token, span::Span};

use self::{parser::Parser, stmt::parse_stmt};

macro_rules! recover_to {
    ($parser:expr; $(before { $before:pat })? $(after { $after:pat })?) => {{
        loop {
            match $parser.peek() {
                $($before | crate::lexer::Token::EOF => break,)?
                $($after => {
                    $parser.next();
                    break;
                })?
                _ => {
                    $parser.next();
                }
            }
        }
    }};
}

pub mod expr;
pub mod parser;
pub mod stmt;

pub fn parse(src: &str) -> (Vec<LangError>, Vec<Stmt>) {
    let mut parser = Parser::new(src);

    while !parser.is_empty() {
        parse_stmt(&mut parser);
        _ = parser.try_consume(Token::Semicolon).map_err(|e| {
            parser.report(e);
            recover_to!(&mut parser; before { Token::Let } after { Token::Semicolon });
        });
    }

    parser.finalize()
}

fn parse_delimited<T>(
    parser: &mut Parser,
    lhs: Token,
    rhs: Token,
    sep: Token,
    parse_item: impl Fn(&mut Parser) -> T,
) -> (Vec<T>, bool, Span) {
    let mut span = match parser.try_consume(lhs) {
        Ok(span) => span,
        Err(e) => {
            let span = Span::from(e.span().start as usize..e.span().start as usize);
            parser.report(e);
            return (Vec::new(), false, span);
        }
    };

    if let Ok(rhs_span) = parser.try_consume(rhs) {
        span.set_end(rhs_span.end());
        return (Vec::new(), false, span);
    };

    let mut items = Vec::new();

    macro_rules! recover {
        () => {
            loop {
                match (parser.consume(sep), parser.try_consume(rhs)) {
                    (sep_at_end, Ok(rhs_span)) => {
                        span.set_end(rhs_span.end());
                        return (items, sep_at_end, span);
                    }
                    (true, Err(_)) => {
                        parser.next();
                        break;
                    }
                    _ => {
                        if parser.peek() == Token::EOF {
                            span.set_end(parser.span().end());
                            return (items, false, span);
                        }
                        parser.next();
                    }
                }
            }
        };
    }

    loop {
        let item = parse_item(parser);
        items.push(item);

        match (parser.consume(sep), parser.try_consume(rhs)) {
            (sep_at_end, Ok(rhs_span)) => {
                span.set_end(rhs_span.end());
                return (items, sep_at_end, span);
            }
            (true, Err(_)) => {}
            (false, Err(e)) => {
                parser.report(LangError::new_syntax(
                    format!("expected {rhs} or {sep}, got {}", parser.peek()),
                    e.span(),
                ));
                recover!();
            }
        }
    }
}
