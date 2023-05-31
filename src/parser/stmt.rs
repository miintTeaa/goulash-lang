use super::{expr::parse_or, parser::Parser};
use crate::{
    ast::{ExprData, Stmt},
    error::{LangError, LangErrorData},
    lexer::Token,
    parser::expr::parse_expr,
};

pub fn parse_stmt(parser: &mut Parser) {
    match parser.peek() {
        Token::Let => let_stmt_expect_let(parser),
        _ => {
            let expr = parse_expr(parser);
            let _span = expr.span;
            parser.push_stmt(expr.into())
        }
    }
}

pub fn let_stmt_expect_let(parser: &mut Parser) {
    let mut span = parser.span();
    parser.next();

    // lhs
    parser.disable_reporting();
    let lhs = parse_or(parser);
    parser.enable_reporting();
    let lhs_span = if !matches!(lhs.data(), ExprData::Var) {
        parser.report(LangError::new(
            LangErrorData::ExpectedToken(Token::Ident),
            lhs.span,
        ));
        recover_to!(parser;
            before { Token::Semicolon | Token::RCurly | Token::Equal }
        );
        Err(lhs.span)
    } else {
        Ok(lhs.span)
    };
    drop(lhs);

    // equals
    _ = parser
        .try_consume(Token::Equal)
        .map_err(|e| parser.report(e));

    // rhs
    let rhs = parse_expr(parser);

    span.set_end(parser.span().end());
    parser.push_stmt(Stmt::new_let(lhs_span, rhs, span))
}
