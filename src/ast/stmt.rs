use std::fmt::Debug;

use crate::span::Span;

use super::expr::Expr;

pub struct Stmt {
    pub span: Span,
    pub(crate) data: StmtData,
}

impl Stmt {
    pub fn new_let(ident: Result<Span, Span>, rhs: Expr, span: Span) -> Self {
        Self {
            span,
            data: StmtData::Let(ident, rhs),
        }
    }

    pub fn data(&self) -> &StmtData {
        &self.data
    }
}

impl From<Expr> for Stmt {
    fn from(expr: Expr) -> Self {
        Self {
            span: expr.span,
            data: StmtData::Expr(expr),
        }
    }
}

impl Debug for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.data {
            StmtData::Let(sp, expr) => f
                .debug_tuple(&format!("Stmt:LET[{}]", self.span))
                .field(&format_args!("Ident:{sp:?}"))
                .field(expr)
                .finish(),
            StmtData::Expr(expr) => {
                if f.alternate() {
                    write!(f, "Stmt:{expr:#?}")
                } else {
                    write!(f, "Stmt:{expr:?}")
                }
            }
            StmtData::Error => {
                write!(f, "Stmt:ERROR[{}]", self.span)
            }
        }
    }
}

#[derive(Debug)]
pub enum StmtData {
    Let(Result<Span, Span>, Expr),
    Expr(Expr),
    Error,
}
