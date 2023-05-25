use crate::{
    ast::{
        ops::{BinaryOp, UnaryOp},
        Expr, ExprData, ExprValue,
    },
    error::LangError,
    lexer::Token,
};

use super::parser::Parser;

pub fn parse_expr(parser: &mut Parser) -> Expr {
    parse_assign(parser)
}

macro_rules! binary_parse {
    ($name:ident < $next:expr; ops { $($token:ident => $op:ident),+$(,)? }) => {
        pub fn $name(parser: &mut Parser) -> Expr {
            let mut expr = $next(parser);

            loop {
                let op = match parser.peek() {
                    $(Token::$token => BinaryOp::$op,)+
                    _ => break,
                };
                parser.next();

                let rhs = $next(parser);

                let span = expr.span.to(rhs.span.end());

                expr = Expr::new(ExprData::Op(op, Box::new(expr), Box::new(rhs)), span);
            }

            expr
        }
    };
}

binary_parse!(parse_assign < parse_sum;
    ops {
        Equal => Assign,
    }
);

binary_parse!(parse_sum < parse_mul;
    ops {
        Plus => Add,
        Minus => Sub,
    }
);

binary_parse!(parse_mul < parse_unary;
    ops {
        Star => Mul,
        Slash => Div,
    }
);

fn parse_unary(parser: &mut Parser) -> Expr {
    use Token::*;
    let mut span = parser.span();
    let op = match parser.peek() {
        Minus => UnaryOp::Neg,
        _ => {
            return parse_primary(parser);
        }
    };
    parser.next();
    let expr = parse_unary(parser);
    span.set_end(expr.span.end());
    Expr::new(ExprData::UnOp(op, Box::new(expr)), span)
}

fn parse_primary(parser: &mut Parser) -> Expr {
    use Token::*;
    let expr = match parser.peek() {
        Integer => Expr::new_lit(ExprValue::Int, parser.span()),
        True => Expr::new_lit(ExprValue::True, parser.span()),
        False => Expr::new_lit(ExprValue::False, parser.span()),
        Ident => Expr::new(ExprData::Var, parser.span()),
        Float => Expr::new_lit(ExprValue::Float, parser.span()),
        String => Expr::new_lit(ExprValue::Str, parser.span()),
        None => Expr::new_lit(ExprValue::None, parser.span()),
        LParen => {
            let mut span = parser.span();
            parser.next();
            let mut expr = parse_expr(parser);
            match parser.try_consume(Token::RParen) {
                Ok(_) => {}
                Err(e) => parser.report(e),
            };
            span.set_end(parser.span().end());
            expr.span = span;
            expr
        }

        tk => {
            parser.report(LangError::new_syntax(
                format!("expected value, got {tk}"),
                parser.span(),
            ));
            Expr::new(ExprData::Error, parser.span())
        }
    };
    parser.next();
    expr
}
