use crate::{
    ast::{
        ops::{BinaryOp, UnaryOp},
        Expr, ExprData, Ident, LiteralKind,
    },
    error::{LangError, LangErrorData},
    span::Span,
    value::Value,
};

use super::IIRStmt;

pub struct IIRExpr {
    pub span: Span,
    data: IIRExprData,
}

impl IIRExpr {
    pub fn new(data: IIRExprData, span: Span) -> Self {
        Self { span, data }
    }

    pub fn try_from(other: Expr, src: &str) -> Result<Self, LangError> {
        Ok(IIRExpr {
            span: other.span,
            data: match other.data {
                ExprData::Op(op, lhs, rhs) => match (op, lhs.data) {
                    (BinaryOp::Assign, ExprData::Access(accessed, access_ident_span)) => {
                        IIRExprData::AccessSet(
                            Box::new(IIRExpr::try_from(*accessed, src)?),
                            access_ident_span,
                            Box::new(IIRExpr::try_from(*rhs, src)?),
                        )
                    }
                    (BinaryOp::Assign, ExprData::Index(expr, index)) => IIRExprData::IndexSet(
                        Box::new(IIRExpr::try_from(*expr, src)?),
                        Box::new(IIRExpr::try_from(*index, src)?),
                        Box::new(IIRExpr::try_from(*rhs, src)?),
                    ),
                    (_, data) => IIRExprData::Op(
                        op,
                        Box::new(Self::try_from(Expr::new(data, lhs.span), src)?),
                        Box::new(Self::try_from(*rhs, src)?),
                    ),
                },
                ExprData::UnOp(op, operand) => {
                    IIRExprData::UnOp(op, Box::new(Self::try_from(*operand, src)?))
                }
                ExprData::Lit(LiteralKind::Int) => {
                    IIRExprData::Const(Value::Int(src[other.span.range()].parse().map_err(
                        |pir| LangError::new(LangErrorData::ParseIntError(pir), other.span),
                    )?))
                }
                ExprData::Lit(LiteralKind::Float) => {
                    IIRExprData::Const(Value::Float(src[other.span.range()].parse().map_err(
                        |pfr| LangError::new(LangErrorData::ParseFloatError(pfr), other.span),
                    )?))
                }
                ExprData::Lit(LiteralKind::Str) => {
                    IIRExprData::Const(Value::new_str(process_escapes(&src[other.span.range()])))
                }
                ExprData::Lit(LiteralKind::True) => IIRExprData::Const(Value::TRUE),
                ExprData::Lit(LiteralKind::False) => IIRExprData::Const(Value::FALSE),
                ExprData::Lit(LiteralKind::None) => IIRExprData::Const(Value::None),
                ExprData::Var(ident) => IIRExprData::Var(ident),
                ExprData::Error => panic!("tried to convert ExprData::Error to IIRExprData"),
                ExprData::Block(old_stmts, old_expr) => {
                    let mut stmts = Vec::new();
                    for stmt in old_stmts {
                        stmts.push(IIRStmt::try_from(stmt, src)?);
                    }
                    let expr = match old_expr {
                        Some(old_expr) => Some(Box::new(IIRExpr::try_from(*old_expr, src)?)),
                        None => None,
                    };
                    IIRExprData::Block(stmts, expr)
                }
                ExprData::Fn(args, stmts, expr) => {
                    let mut iir_stmts = Vec::new();
                    for stmt in stmts {
                        iir_stmts.push(IIRStmt::try_from(stmt, src)?);
                    }
                    let iir_expr = match expr {
                        Some(expr) => Some(IIRExpr::try_from(*expr, src)?),
                        None => None,
                    };
                    IIRExprData::Const(Value::new_fn(args, iir_stmts, iir_expr))
                }
                ExprData::Call(callable, args) => {
                    let mut iir_args = Vec::new();
                    for arg in args {
                        iir_args.push(IIRExpr::try_from(arg, src)?);
                    }
                    IIRExprData::Call(Box::new(IIRExpr::try_from(*callable, src)?), iir_args)
                }
                ExprData::Class(name, supers, fields) => {
                    let iir_supers: Result<Vec<_>, LangError> = supers
                        .into_iter()
                        .map(|sup| IIRExpr::try_from(sup, src))
                        .collect();
                    let iir_supers = iir_supers?;

                    let iir_fields: Result<Vec<_>, LangError> = fields
                        .into_iter()
                        .map(|(span, field)| match IIRExpr::try_from(field, src) {
                            Ok(field) => Ok((span, field)),
                            Err(e) => Err(e),
                        })
                        .collect();
                    let iir_fields = iir_fields?;

                    IIRExprData::Class(name, iir_supers, iir_fields)
                }
                ExprData::Access(expr, span) => {
                    IIRExprData::Access(Box::new(IIRExpr::try_from(*expr, src)?), span)
                }
                ExprData::List(exprs) => {
                    let mut iir_exprs = Vec::new();
                    for expr in exprs {
                        iir_exprs.push(IIRExpr::try_from(expr, src)?);
                    }
                    IIRExprData::List(iir_exprs)
                }
                ExprData::Index(expr, index) => IIRExprData::Index(
                    Box::new(IIRExpr::try_from(*expr, src)?),
                    Box::new(IIRExpr::try_from(*index, src)?),
                ),
                ExprData::If(condition, block, r#else) => {
                    let r#else = match r#else {
                        Some(r#else) => Some(Box::new(IIRExpr::try_from(*r#else, src)?)),
                        None => None,
                    };

                    IIRExprData::If(
                        Box::new(IIRExpr::try_from(*condition, src)?),
                        Box::new(IIRExpr::try_from(*block, src)?),
                        r#else,
                    )
                }
                ExprData::Loop(block) => {
                    IIRExprData::Loop(Box::new(IIRExpr::try_from(*block, src)?))
                }
                ExprData::Break(expr) => IIRExprData::Break(match expr {
                    Some(expr) => Some(Box::new(IIRExpr::try_from(*expr, src)?)),
                    None => None,
                }),
            },
        })
    }

    pub fn new_const(value: Value, span: Span) -> Self {
        Self {
            span,
            data: IIRExprData::Const(value),
        }
    }

    pub fn new_var(ident: Ident, span: Span) -> Self {
        Self {
            span,
            data: IIRExprData::Var(ident),
        }
    }

    pub fn data(&self) -> &IIRExprData {
        &self.data
    }
}

pub enum IIRExprData {
    Op(BinaryOp, Box<IIRExpr>, Box<IIRExpr>),
    UnOp(UnaryOp, Box<IIRExpr>),
    Block(Vec<IIRStmt>, Option<Box<IIRExpr>>),
    Call(Box<IIRExpr>, Vec<IIRExpr>),
    Class(Ident, Vec<IIRExpr>, Vec<(Ident, IIRExpr)>),
    List(Vec<IIRExpr>),
    Const(Value),
    AccessSet(Box<IIRExpr>, Ident, Box<IIRExpr>),
    Access(Box<IIRExpr>, Ident),
    IndexSet(Box<IIRExpr>, Box<IIRExpr>, Box<IIRExpr>),
    Index(Box<IIRExpr>, Box<IIRExpr>),
    If(Box<IIRExpr>, Box<IIRExpr>, Option<Box<IIRExpr>>),
    Loop(Box<IIRExpr>),
    Break(Option<Box<IIRExpr>>),
    Var(Ident),
}

fn process_escapes(s: &str) -> String {
    let mut ret = String::new();
    let mut chars = s.chars();
    chars.next(); // skip first "

    loop {
        match chars.next().expect("should have hit end of string") {
            '\\' => match chars.next().expect("should have hit end of string") {
                'n' => ret.push('\n'),
                't' => ret.push('\t'),
                'r' => ret.push('\r'),
                next_c => ret.push(next_c),
            },
            '"' => {
                break;
            }
            c => {
                ret.push(c);
            }
        }
    }

    ret
}
