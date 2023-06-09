use std::{fmt::Debug};

use internment::ArcIntern;

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

    pub fn new_lit(value: LiteralKind, span: Span) -> Self {
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
                .debug_tuple(&format!("Expr:OP{op:?}[{span}]"))
                .field(&span)
                .field(&lhs)
                .field(&rhs)
                .finish(),
            ExprData::UnOp(op, expr) => f
                .debug_tuple(&format!("Expr:UN_OP{op:?}[{span}]"))
                .field(&expr)
                .finish(),
            ExprData::Lit(val) => write!(f, "Expr:LITERAL[{span}]({val:?})"),
            ExprData::Var(ident) => {
                write!(f, "Expr:VAR[{span}]({ident})")
            }
            ExprData::Error => write!(f, "Expr:ERROR[{span}]"),
            ExprData::Block(stmts, expr) => f
                .debug_tuple(&format!("Expr:BLOCK[{span}]"))
                .field(&stmts)
                .field(&expr)
                .finish(),
            ExprData::Fn(args, stmts, expr) => f
                .debug_tuple(&format!("Expr:FN[{span}]"))
                .field(&args)
                .field(&stmts)
                .field(&expr)
                .finish(),
            ExprData::Call(callable, args) => f
                .debug_tuple(&format!("Expr:CALL[{span}]"))
                .field(&callable)
                .field(&args)
                .finish(),
            ExprData::Class(name, supers, fields) => f
                .debug_tuple(&format!("Expr:CLASS[{span}]"))
                .field(name)
                .field(supers)
                .field(fields)
                .finish(),
            ExprData::Access(expr, ident_span) => f
                .debug_tuple(&format!("Expr:ACCESS[{span}]"))
                .field(expr)
                .field(ident_span)
                .finish(),
            ExprData::List(li) => f
                .debug_tuple(&format!("Expr:LIST[{span}]"))
                .field(li)
                .finish(),
            ExprData::Index(li, index) => f
                .debug_tuple(&format!("Expr:INDEX[{span}]"))
                .field(li)
                .field(index)
                .finish(),
            ExprData::If(condition, block, r#else) => f
                .debug_tuple(&format!("Expr:IF[{span}]"))
                .field(condition)
                .field(block)
                .field(r#else)
                .finish(),
            ExprData::Loop(block) => f
                .debug_tuple(&format!("Expr:LOOP[{span}]"))
                .field(block)
                .finish(),
            ExprData::Break(expr) => f
                .debug_tuple(&format!("Expr:BREAK[{span}]"))
                .field(expr)
                .finish(),
            ExprData::Return(expr) => f
                .debug_tuple(&format!("Expr:RETURN[{span}]"))
                .field(expr)
                .finish(),
        }
    }
}

pub type Ident = ArcIntern<String>;

pub fn id<'a>(s: impl AsRef<str>) -> ArcIntern<String> {
    ArcIntern::from_ref(s.as_ref())
}

#[derive(Debug)]
pub enum ExprData {
    Op(BinaryOp, Box<Expr>, Box<Expr>),
    UnOp(UnaryOp, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Lit(LiteralKind),
    Block(Vec<Stmt>, Option<Box<Expr>>),
    Loop(Box<Expr>),
    Class(Ident, Vec<Expr>, Vec<(Ident, Expr)>),
    Fn(Vec<Ident>, Vec<Stmt>, Option<Box<Expr>>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Access(Box<Expr>, Ident),
    List(Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Break(Option<Box<Expr>>),
    Return(Option<Box<Expr>>),
    Var(Ident),
    Error,
}

#[derive(Debug)]
pub enum LiteralKind {
    Int,
    Float,
    Str,
    True,
    False,
    None,
}
