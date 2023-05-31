use crate::{
    ast::{
        ops::{BinaryOp, UnaryOp},
        Expr, ExprData, ExprValueType, Stmt, StmtData,
    },
    error::LangError,
    lexer::Token,
    span::Span,
};

use super::{parse_delimited, parser::Parser, stmt::parse_stmt};

pub fn parse_expr(parser: &mut Parser) -> Expr {
    let expr = parse_assign(parser);
    expr
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

binary_parse!(parse_eq < parse_gt;
    ops {
        EqualEqual => Eq,
        BangEqual => NotEq,
    }
);

binary_parse!(parse_gt < parse_sum;
    ops {
        Greater => Greater,
        GreaterEqual => GreaterEq,
        Lesser => Lesser,
        LesserEqual => LesserEq,
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
            return parse_call(parser);
        }
    };
    parser.next();
    let expr = parse_unary(parser);
    span.set_end(expr.span.end());
    Expr::new(ExprData::UnOp(op, Box::new(expr)), span)
}

fn parse_call(parser: &mut Parser) -> Expr {
    let mut expr = parse_primary(parser);
    let mut span = expr.span;
    loop {
        match parser.peek() {
            Token::Dot => {
                parser.next();

                let ident_span = match parser.try_consume(Token::Ident) {
                    Ok(ident_span) => ident_span,
                    Err(e) => {
                        parser.next();
                        parser.report(e);
                        continue;
                    }
                };

                span.set_end(ident_span.end());

                expr = Expr::new(ExprData::Access(Box::new(expr), ident_span), span)
            }
            Token::LParen => {
                parser.next();

                let mut args = Vec::new();

                if let Ok(rparen_span) = parser.try_consume(Token::RParen) {
                    span.set_end(rparen_span.end());
                    expr = Expr::new(ExprData::Call(Box::new(expr), args), span);
                    continue;
                }

                'args: loop {
                    let arg = parse_expr(parser);
                    args.push(arg);

                    match (
                        parser.consume(Token::Comma),
                        parser.try_consume(Token::RParen),
                    ) {
                        (_, Ok(rparen_span)) => {
                            span.set_end(rparen_span.end());
                            expr = Expr::new(ExprData::Call(Box::new(expr), args), span);
                            break;
                        }
                        (true, Err(_)) => {}
                        (false, Err(_)) => loop {
                            match (
                                parser.consume(Token::Comma),
                                parser.try_consume(Token::RParen),
                            ) {
                                (_, Ok(rparen_span)) => {
                                    span.set_end(rparen_span.end());
                                    expr = Expr::new(ExprData::Call(Box::new(expr), args), span);
                                    break 'args;
                                }
                                (true, Err(_)) => {
                                    parser.next();
                                    break;
                                }
                                _ => {
                                    parser.report(LangError::new_syntax(
                                        format!(
                                            "expected comma or closing parenthesis, got {}",
                                            parser.peek()
                                        ),
                                        parser.span(),
                                    ));
                                    if parser.peek() == Token::EOF {
                                        span.set_end(parser.span().end());
                                        expr =
                                            Expr::new(ExprData::Call(Box::new(expr), args), span);
                                        break 'args;
                                    }
                                    parser.next();
                                }
                            }
                        },
                    }
                }
            }
            _ => break,
        }
    }
    expr
}

fn parse_primary(parser: &mut Parser) -> Expr {
    use Token::*;
    macro_rules! m {
        (lit $value:ident) => {{
            let span = parser.span();
            parser.next();
            Expr::new_lit(ExprValueType::$value, span)
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
        LCurly => parse_block(parser),
        Fn => parse_fn(parser),
        Obj => parse_class(parser),

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

fn parse_block_data(parser: &mut Parser) -> (Vec<Stmt>, Option<Box<Expr>>, Span) {
    match parse_delimited(
        parser,
        Token::LCurly,
        Token::RCurly,
        Token::Semicolon,
        |parser| {
            parse_stmt(parser);
            parser.pop_stmt().expect("should have parsed a stmt")
        },
    ) {
        (stmts, true, span) => (stmts, None, span),
        (mut stmts, false, span) => {
            let expr = match stmts.pop() {
                Some(stmt) if matches!(stmt.data, StmtData::Error) => {
                    Some(Box::new(Expr::new(ExprData::Error, stmt.span)))
                }
                Some(stmt) if matches!(stmt.data, StmtData::Expr(_)) => {
                    let StmtData::Expr(expr) = stmt.data else {
                        unreachable!();
                    };
                    Some(Box::new(expr))
                }
                Some(stmt) => {
                    stmts.push(stmt);
                    None
                }
                None => None,
            };
            (stmts, expr, span)
        }
    }
}

fn parse_block(parser: &mut Parser) -> Expr {
    let (stmts, expr, span) = parse_block_data(parser);
    Expr::new(ExprData::Block(stmts, expr), span)
}

fn parse_fn(parser: &mut Parser) -> Expr {
    let mut span = match parser.try_consume(Token::Fn) {
        Ok(span) => span,
        Err(e) => {
            let mut span = e.span();
            parser.report(e);
            recover_to!(parser; after { Token::RCurly });
            span.set_end(parser.span().end());
            return Expr::new(ExprData::Error, span);
        }
    };

    let args = 'args: {
        if parser.peek() == Token::RCurly {
            break 'args Vec::new();
        };

        let mut args = Vec::new();

        loop {
            let item = (|parser| parse_expr(parser).span)(parser);
            args.push(item);

            match (parser.consume(Token::Comma), parser.peek()) {
                (_, Token::LCurly) => {
                    break 'args args;
                }
                (true, _) => {}
                (false, _) => {
                    parser.report(LangError::new_syntax(
                        format!("expected lcurly or comma, got {}", parser.peek()),
                        parser.span(),
                    ));
                    loop {
                        match (parser.consume(Token::Comma), parser.peek()) {
                            (_, Token::LCurly) => {
                                break 'args args;
                            }
                            (true, _) => {
                                break;
                            }
                            _ => {
                                if parser.peek() == Token::EOF {
                                    span.set_end(parser.span().end());
                                    return Expr::new(ExprData::Error, span);
                                }
                                parser.next();
                            }
                        }
                    }
                }
            }
        }
    };

    let (stmts, expr, block_span) = parse_block_data(parser);
    span.set_end(block_span.end());

    Expr::new(ExprData::Fn(args, stmts, expr), span)
}

fn parse_class(parser: &mut Parser) -> Expr {
    let mut span = match parser.try_consume(Token::Obj) {
        Ok(span) => span,
        Err(_) => {
            unreachable!("should have class keyword");
        }
    };

    let name_span = match parser.try_consume(Token::Ident) {
        Ok(name_span) => name_span,
        Err(e) => {
            let mut span = e.span();
            parser.report(e);
            recover_to!(parser; after { Token::RCurly });
            span.set_end(parser.span().end());
            return Expr::new(ExprData::Error, span);
        }
    };

    let mut supers = Vec::new();

    if parser.consume(Token::Extends) {
        'supers: {
            loop {
                supers.push(parse_expr(parser));

                match (parser.consume(Token::Comma), parser.peek()) {
                    (_, Token::LCurly) => {
                        break 'supers;
                    }
                    (true, _) => {}
                    (false, _) => {
                        parser.report(LangError::new_syntax(
                            format!("expected lcurly or comma, got {}", parser.peek()),
                            parser.span(),
                        ));
                        loop {
                            match (parser.consume(Token::Comma), parser.peek()) {
                                (_, Token::LCurly) => {
                                    break 'supers;
                                }
                                (true, _) => {
                                    break;
                                }
                                _ => {
                                    if parser.peek() == Token::EOF {
                                        span.set_end(parser.span().end());
                                        return Expr::new(ExprData::Error, span);
                                    }
                                    parser.next();
                                }
                            }
                        }
                    }
                }
            }
        };
    }

    match parser.try_consume(Token::LCurly) {
        Ok(_) => {}
        Err(e) => {
            let mut span = e.span();
            parser.report(e);
            recover_to!(parser; after { Token::RCurly });
            span.set_end(parser.span().end());
            return Expr::new(ExprData::Error, span);
        }
    };

    let mut fields = Vec::new();

    if let Ok(rcurly_span) = parser.try_consume(Token::RCurly) {
        span.set_end(rcurly_span.end());
        return Expr::new(ExprData::Class(name_span, supers, fields), span);
    };

    'outer: loop {
        let ident_span = match parser.try_consume(Token::Ident) {
            Ok(ident_span) => ident_span,
            Err(e) => {
                parser.report(e);
                loop {
                    parser.next();
                    match parser.peek() {
                        Token::Ident => {
                            let span = parser.span();
                            parser.next();
                            break span;
                        }
                        Token::Equal => {
                            parser.next();
                            parse_expr(parser);
                            continue;
                        }
                        Token::RCurly | Token::EOF => {
                            break 'outer;
                        }
                        _ => {}
                    }
                }
            }
        };

        match parser.try_consume(Token::Equal) {
            Ok(_) => {}
            Err(e) => {
                parser.report(e);
                loop {
                    parser.next();
                    match parser.peek() {
                        Token::Equal => {
                            parser.next();
                            break;
                        }
                        Token::RCurly | Token::EOF => {
                            break 'outer;
                        }
                        _ => {}
                    }
                }
            }
        }

        let expr = parse_expr(parser);

        fields.push((ident_span, expr));

        if let Ok(rcurly_span) = parser.try_consume(Token::RCurly) {
            span.set_end(rcurly_span.end());
            break;
        }
    }

    Expr::new(ExprData::Class(name_span, supers, fields), span)
}
