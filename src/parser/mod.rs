use pest::{
    iterators::{Pair, Pairs},
    Parser,
};

use crate::{
    ast::{
        id,
        ops::{BinaryOp, UnaryOp},
        Expr, ExprData, LiteralKind, Stmt,
    },
    span::Span,
};

#[derive(pest_derive::Parser)]
#[grammar = "parser/parser.pest"]
pub struct LangParser;

pub fn parse(src: &str) -> Result<Vec<Stmt>, pest::error::Error<Rule>> {
    let program_pairs = match LangParser::parse(Rule::program, src)?.next() {
        Some(s) => s,
        None => return Ok(Vec::new()),
    }
    .into_inner();
    let mut ast = Vec::new();
    for stmt in program_pairs {
        match stmt.as_rule() {
            Rule::stmt => ast.push(parse_stmt(stmt)),
            Rule::EOI => break,
            rule => unreachable!("{rule:#?}"),
        }
    }
    Ok(ast)
}

pub fn parse_stmt(stmt: Pair<Rule>) -> Stmt {
    let inner = stmt.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::expr => {
            let expr = parse_expr(inner);
            Stmt::from(expr)
        }
        Rule::let_stmt => {
            let span = {
                let inner_span = inner.as_span();
                Span::from(inner_span.start()..inner_span.end())
            };
            let mut let_pairs = inner.into_inner();
            let ident = id(let_pairs.next().unwrap().as_str().to_string());
            let expr = parse_expr(let_pairs.next().unwrap());
            Stmt::new_let(ident, expr, span)
        }
        rule => unreachable!("{:?}", rule),
    }
}

pub fn parse_expr(expr: Pair<Rule>) -> Expr {
    lazy_static::lazy_static! {
    static ref PRATT: pest::pratt_parser::PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc, Op, PrattParser};

        PrattParser::new()
            .op(Op::infix(Rule::assign, Assoc::Right))
            .op(Op::infix(Rule::or, Assoc::Left))
            .op(Op::infix(Rule::and, Assoc::Left))
            .op(Op::infix(Rule::eq, Assoc::Left)
                | Op::infix(Rule::not_eq, Assoc::Left))
            .op(Op::infix(Rule::greater_eq, Assoc::Left)
                | Op::infix(Rule::lesser_eq, Assoc::Left)
                | Op::infix(Rule::lesser, Assoc::Left)
                | Op::infix(Rule::greater, Assoc::Left))
            .op(Op::infix(Rule::plus, Assoc::Left)
                | Op::infix(Rule::minus, Assoc::Left))
            .op(Op::infix(Rule::mul, Assoc::Left)
                | Op::infix(Rule::div, Assoc::Left))
            .op(Op::prefix(Rule::not)
                | Op::prefix(Rule::neg))
            .op(Op::postfix(Rule::call_args)
                | Op::postfix(Rule::index)
                | Op::postfix(Rule::access)
                | Op::postfix(Rule::call_self))
    };
    }

    PRATT
        .map_primary(parse_primary)
        .map_infix(|lhs, op, rhs| {
            let span = lhs.span.to(rhs.span.end());
            let op = match op.as_rule() {
                Rule::plus => BinaryOp::Add,
                Rule::minus => BinaryOp::Sub,
                Rule::mul => BinaryOp::Mul,
                Rule::div => BinaryOp::Div,
                Rule::eq => BinaryOp::Eq,
                Rule::not_eq => BinaryOp::NotEq,
                Rule::greater_eq => BinaryOp::GreaterEq,
                Rule::greater => BinaryOp::Greater,
                Rule::lesser_eq => BinaryOp::LesserEq,
                Rule::lesser => BinaryOp::Lesser,
                Rule::assign => BinaryOp::Assign,
                Rule::or => BinaryOp::Or,
                Rule::and => BinaryOp::And,
                rule => unreachable!("{:?}", rule),
            };
            Expr::new(ExprData::Op(op, Box::new(lhs), Box::new(rhs)), span)
        })
        .map_prefix(|op, expr| {
            let span = Span::from(op.as_span().start()..expr.span.end());
            let op = match op.as_rule() {
                Rule::not => UnaryOp::Not,
                Rule::neg => UnaryOp::Neg,
                rule => unreachable!("{:?}", rule),
            };
            Expr::new(ExprData::UnOp(op, Box::new(expr)), span)
        })
        .map_postfix(|expr, op| {
            let span = expr.span.to(op.as_span().end());
            let data = match op.as_rule() {
                Rule::access => {
                    let ident = id(op.into_inner().next().unwrap().as_str().to_string());
                    ExprData::Access(Box::new(expr), ident)
                }
                Rule::call_args => ExprData::Call(
                    Box::new(expr),
                    op.into_inner().map(|p| parse_expr(p)).collect(),
                ),
                Rule::call_self => {
                    // turns
                    // expr:func(arg1, arg2)
                    // into
                    // {
                    //    let func = expr;
                    //    func.func(func, arg1, arg2)
                    // }
                    // i have to do this in a hacky way because i don't
                    // want to have to reinterpret expr twice, and i store
                    // idents as spans

                    let mut call_self_pairs = op.into_inner();
                    let ident_pair = call_self_pairs.next().unwrap();
                    let func_ident = id(ident_pair.as_str().to_string());
                    let call_bind_name = id(String::from("@CALL_BIND"));

                    let expr_span = expr.span;

                    let let_bind = Stmt::new_let(call_bind_name.clone(), expr, span);

                    let access = Expr::new(
                        ExprData::Access(
                            Box::new(Expr::new(ExprData::Var(call_bind_name.clone()), expr_span)),
                            func_ident,
                        ),
                        span,
                    );

                    let mut args = vec![Expr::new(ExprData::Var(call_bind_name), expr_span)];
                    args.extend(call_self_pairs.map(|p| parse_expr(p)));

                    let call = Expr::new(ExprData::Call(Box::new(access), args), span);
                    ExprData::Block(vec![let_bind], Some(Box::new(call)))
                }
                Rule::index => ExprData::Index(
                    Box::new(expr),
                    Box::new(parse_expr(op.into_inner().next().unwrap())),
                ),
                rule => unreachable!("{:?}", rule),
            };
            Expr::new(data, span)
        })
        .parse(expr.into_inner())
}

pub fn parse_primary(primary: Pair<Rule>) -> Expr {
    let span = {
        let primary_span = primary.as_span();
        Span::from(primary_span.start()..primary_span.end())
    };
    match primary.as_rule() {
        Rule::expr => parse_expr(primary),
        Rule::r#true => Expr::new_lit(LiteralKind::True, span),
        Rule::r#false => Expr::new_lit(LiteralKind::False, span),
        Rule::none => Expr::new_lit(LiteralKind::None, span),
        Rule::ident => Expr::new(ExprData::Var(id(primary.as_str().to_string())), span),
        Rule::float => Expr::new_lit(LiteralKind::Float, span),
        Rule::integer => Expr::new_lit(LiteralKind::Int, span),
        Rule::string => Expr::new_lit(LiteralKind::Str, span),
        Rule::block => {
            let (stmts, expr) = parse_block_data(dbg!(primary.into_inner()));

            Expr::new(ExprData::Block(stmts, expr), span)
        }
        Rule::list_lit => {
            let exprs: Vec<Expr> = primary.into_inner().map(|expr| parse_expr(expr)).collect();

            Expr::new(ExprData::List(exprs), span)
        }
        Rule::function => {
            let mut func_pairs = primary.into_inner();
            let first = func_pairs.next().unwrap();
            let stmts;
            let expr;
            let mut args = Vec::new();
            match first.as_rule() {
                Rule::block => (stmts, expr) = parse_block_data(first.into_inner()),
                Rule::ident_lst => {
                    for arg in first.into_inner() {
                        args.push(id(arg.as_str().to_string()));
                    }
                    (stmts, expr) = parse_block_data(func_pairs.next().unwrap().into_inner());
                }
                _ => unreachable!(),
            }

            Expr::new(ExprData::Fn(args, stmts, expr), span)
        }
        Rule::r#if => {
            let mut if_pairs = primary.into_inner();
            let condition = parse_expr(if_pairs.next().unwrap());
            let block = parse_block(if_pairs.next().unwrap());
            let mut elifs = Vec::new();
            let mut r#else = None;
            loop {
                match if_pairs.next() {
                    Some(found_elif) if found_elif.as_rule() == Rule::elif => {
                        let mut elif_pairs = found_elif.into_inner();
                        let condition = parse_expr(elif_pairs.next().unwrap());
                        let block = parse_block(elif_pairs.next().unwrap());
                        elifs.push((condition, block));
                    }
                    Some(found_else) if found_else.as_rule() == Rule::r#else => {
                        let mut else_pairs = found_else.into_inner();
                        r#else = Some(Box::new(parse_block(else_pairs.next().unwrap())));
                        break;
                    }
                    None => break,
                    rule => unreachable!("{rule:?}"),
                }
            }

            fn build_chain(
                mut chain: impl Iterator<Item = (Expr, Expr)>,
                r#else: Option<Box<Expr>>,
                span: Span,
            ) -> Option<Box<Expr>> {
                match chain.next() {
                    Some((condition, block)) => Some(Box::new(Expr::new(
                        ExprData::If(
                            Box::new(condition),
                            Box::new(block),
                            build_chain(chain, r#else, span.clone()),
                        ),
                        span,
                    ))),
                    None => r#else,
                }
            }

            Expr::new(
                ExprData::If(
                    Box::new(condition),
                    Box::new(block),
                    build_chain(elifs.into_iter(), r#else, span),
                ),
                span,
            )
        }
        Rule::object => 'mtch: {
            let mut object_pairs = primary.into_inner();
            let name = id(object_pairs.next().unwrap().as_str().to_string());
            let mut fields = Vec::new();
            let mut supers = Vec::new();

            let next = match object_pairs.next() {
                Some(next) => next,
                None => break 'mtch Expr::new(ExprData::Class(name, supers, fields), span),
            };

            match next.as_rule() {
                Rule::ident_lst => {
                    for ident in next.into_inner() {
                        supers.push(Expr::new(
                            ExprData::Var(id(ident.as_str().to_owned())),
                            Span::from(ident.as_span().start()..ident.as_span().end()),
                        ));
                    }

                    loop {
                        let ident = match object_pairs.next() {
                            Some(next_ident) => id(next_ident.as_str().to_string()),
                            None => break,
                        };
                        let expr = parse_expr(object_pairs.next().unwrap());
                        fields.push((ident, expr));
                    }
                }
                Rule::ident => {
                    let mut ident = id(next.as_str().to_string());
                    loop {
                        let expr = parse_expr(object_pairs.next().unwrap());
                        fields.push((ident, expr));
                        ident = match object_pairs.next() {
                            Some(next_ident) => id(next_ident.as_str().to_string()),
                            None => break,
                        };
                    }
                }
                _ => unreachable!(),
            };

            Expr::new(ExprData::Class(name, supers, fields), span)
        }
        Rule::r#loop => {
            let mut loop_pairs = primary.into_inner();
            let block = parse_block(loop_pairs.next().unwrap());
            Expr::new(ExprData::Loop(Box::new(block)), span)
        }
        Rule::r#break => {
            let mut break_pairs = primary.into_inner();
            let expr = match break_pairs.next() {
                Some(expr) => Some(Box::new(parse_expr(expr))),
                None => None,
            };
            Expr::new(ExprData::Break(expr), span)
        }
        Rule::r#return => {
            let mut return_pairs = primary.into_inner();
            let expr = match return_pairs.next() {
                Some(expr) => Some(Box::new(parse_expr(expr))),
                None => None,
            };
            Expr::new(ExprData::Return(expr), span)
        }
        rule => unreachable!("{rule:#?}"),
    }
}

pub fn parse_block(block: Pair<Rule>) -> Expr {
    if block.as_rule() != Rule::block {
        panic!(
            "this should be a block, but is a {:?}\n{block:#?}",
            block.as_rule(),
        );
    }
    let span = Span::from(block.as_span().start()..block.as_span().end());
    let (stmts, expr) = parse_block_data(block.into_inner());
    Expr::new(ExprData::Block(stmts, expr), span)
}

pub fn parse_block_data(block_pairs: Pairs<Rule>) -> (Vec<Stmt>, Option<Box<Expr>>) {
    let mut expr = None;
    let stmts: Vec<Stmt> = block_pairs
        .map_while(|item| match item.as_rule() {
            Rule::stmt => Some(parse_stmt(item)),
            Rule::expr => {
                expr = Some(Box::new(parse_expr(item)));
                None
            }
            rule => unreachable!("{rule:#?}"),
        })
        .collect();
    (stmts, expr)
}
