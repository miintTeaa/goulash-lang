use crate::{
    ast::{Ident, Stmt, StmtData},
    error::LangError,
    span::Span,
};

use super::expr::IIRExpr;

pub struct IIRStmt {
    pub span: Span,
    data: IIRStmtData,
}

impl IIRStmt {
    pub fn try_from(other: Stmt, src: &str) -> Result<Self, LangError> {
        Ok(Self {
            span: other.span,
            data: match other.data {
                StmtData::Let(ident, expr) => {
                    IIRStmtData::Let(ident, IIRExpr::try_from(expr, src)?)
                }
                StmtData::Expr(expr) => IIRStmtData::Expr(IIRExpr::try_from(expr, src)?),
                StmtData::Error => panic!("tried to convert StmtData::Error to IIRStmtData"),
            },
        })
    }

    pub fn new(data: IIRStmtData, span: Span) -> Self {
        Self { span, data }
    }

    pub fn new_expr(expr: IIRExpr, span: Span) -> Self {
        Self {
            span,
            data: IIRStmtData::Expr(expr),
        }
    }

    pub fn data(&self) -> &IIRStmtData {
        &self.data
    }
}

pub enum IIRStmtData {
    Let(Ident, IIRExpr),
    Expr(IIRExpr),
}
