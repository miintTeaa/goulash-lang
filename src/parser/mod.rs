use crate::{ast::Stmt, error::LangError, lexer::Token};

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
