use crate::span::Span;

use super::expr::IIRExpr;

pub struct IIRStmt {
    pub span: Span,
    data: IIRStmtData,
}

impl IIRStmt {
    pub fn new(data: IIRStmtData, span: Span) -> Self {
        Self { span, data }
    }

    pub fn new_expr(expr: IIRExpr, span: Span) -> Self {
        Self {
            span,
            data: IIRStmtData::Expr(expr),
        }
    }

    pub fn new_print(expr: IIRExpr, span: Span) -> Self {
        Self {
            span,
            data: IIRStmtData::Print(expr),
        }
    }

    pub fn data(&self) -> &IIRStmtData {
        &self.data
    }
}

pub enum IIRStmtData {
    Let(Span, IIRExpr),
    Expr(IIRExpr),
    Print(IIRExpr),
}
