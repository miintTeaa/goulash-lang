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

binary_parse!(parse_assign < parse_or;
    ops {
        Equal => Assign,
    }
);

binary_parse!(parse_or < parse_and;
    ops {
        Or => Or,
    }
);

binary_parse!(parse_and < parse_eq;
    ops {
        And => And,
    }
);

binary_parse!(parse_eq < parse_sum;
    ops {
        EqualEqual => Eq,
        BangEqual => NotEq,
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
    let mut span = parser.span();
    let op = match parser.peek() {
        Token::Minus => UnaryOp::Neg,
        Token::Bang => UnaryOp::Not,
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
    macro_rules! m {
        (lit $value:ident) => {{
            let span = parser.span();
            parser.next();
            Expr::new_lit(ExprValue::$value, span)
        }};
        (data $data:ident) => {{
            let span = parser.span();
            parser.next();
            Expr::new(ExprData::$data, span)
        }};
    }

    let expr = match parser.peek() {
        Integer => m!(lit Int),
        True => m!(lit True),
        False => m!(lit False),
        Ident => m!(data Var),
        Float => m!(lit Float),
        String => m!(lit Str),
        None => m!(lit None),
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
            let span = parser.span();
            parser.next();
            parser.report(LangError::new_syntax(
                format!("expected value, got {tk}"),
                span,
            ));
            Expr::new(ExprData::Error, span)
        }
    };
    expr
}
