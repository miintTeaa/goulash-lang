use crate::{
    ast::ops::{BinaryOp, UnaryOp},
    span::Span,
    value::Value,
};

pub struct IIRExpr {
    pub span: Span,
    data: IIRExprData,
}

impl IIRExpr {
    pub fn new(data: IIRExprData, span: Span) -> Self {
        Self { span, data }
    }

    pub fn new_const(value: Value, span: Span) -> Self {
        Self {
            span,
            data: IIRExprData::Const(value),
        }
    }

    pub fn new_var(span: Span) -> Self {
        Self {
            span,
            data: IIRExprData::Var,
        }
    }

    pub fn data(&self) -> &IIRExprData {
        &self.data
    }
}

pub enum IIRExprData {
    Op(BinaryOp, Box<IIRExpr>, Box<IIRExpr>),
    UnOp(UnaryOp, Box<IIRExpr>),
    Const(Value),
    Var,
}
