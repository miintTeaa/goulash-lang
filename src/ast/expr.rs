use std::fmt::Debug;

use crate::span::Span;

use super::{
    ops::{BinaryOp, UnaryOp},
    Stmt,
};

pub struct Expr {
    pub span: Span,
    pub(crate) data: ExprData,
}

impl Expr {
    pub fn new(data: ExprData, span: Span) -> Self {
        Self { span, data }
    }

    pub fn new_lit(value: ExprValueType, span: Span) -> Self {
        Self {
            span,
            data: ExprData::Lit(value),
        }
    }

    // pub fn span(&self) -> Span {
    //     self.span
    // }

    pub fn data(&self) -> &ExprData {
        &self.data
    }
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let span = self.span;
        match &self.data {
            ExprData::Op(op, lhs, rhs) => f
                .debug_tuple(&format!("Expr:BI[{op:?}]"))
                .field(&span)
                .field(&lhs)
                .field(&rhs)
                .finish(),
            ExprData::UnOp(op, expr) => f
                .debug_tuple(&format!("Expr:UN[{op:?}]"))
                .field(&span)
                .field(&expr)
                .finish(),
            ExprData::Lit(val) => write!(f, "Expr:LITERAL({span} {val:?})"),
            ExprData::Var => {
                write!(f, "Expr:VAR({span})")
            }
            ExprData::Error => write!(f, "Expr:ERROR({span})"),
            ExprData::Block(stmts, expr) => f
                .debug_tuple(&format!("Expr:BLOCK"))
                .field(&span)
                .field(&stmts)
                .field(&expr)
                .finish(),
            ExprData::Fn(args, stmts, expr) => f
                .debug_tuple(&format!("Expr:FN"))
                .field(&span)
                .field(&args)
                .field(&stmts)
                .field(&expr)
                .finish(),
            ExprData::Call(callable, args) => f
                .debug_tuple(&format!("Expr:CALL"))
                .field(&span)
                .field(&callable)
                .field(&args)
                .finish(),
        }
    }
}

#[derive(Debug)]
pub enum ExprData {
    Op(BinaryOp, Box<Expr>, Box<Expr>),
    UnOp(UnaryOp, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Lit(ExprValueType),
    Block(Vec<Stmt>, Option<Box<Expr>>),
    Fn(Vec<Span>, Vec<Stmt>, Option<Box<Expr>>),
    Var,
    Error,
}

#[derive(Debug)]
pub enum ExprValueType {
    Int,
    Float,
    Str,
    True,
    False,
    None,
}
