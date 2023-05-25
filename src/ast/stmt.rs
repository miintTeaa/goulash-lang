use std::fmt::Debug;

use crate::span::Span;

use super::expr::Expr;

pub struct Stmt {
    pub span: Span,
    data: StmtData,
}

impl Stmt {
    pub fn new(data: StmtData, span: Span) -> Self {
        Self { span, data }
    }

    pub fn data(&self) -> &StmtData {
        &self.data
    }
}

impl Debug for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.data {
            StmtData::Let(sp, expr) => {
                let mut tup = f.debug_tuple("Stmt:LET");
                tup.field(&self.span)
                    .field(&format_args!("Ident:{sp:?}"))
                    .field(expr)
                    .finish()
            }
            StmtData::Expr(expr) => {
                if f.alternate() {
                    write!(f, "Stmt:{expr:#?}")
                } else {
                    write!(f, "Stmt:{expr:?}")
                }
            }
            StmtData::Error => {
                write!(f, "Stmt:ERROR")
            }
            StmtData::Print(expr) => {
                if f.alternate() {
                    write!(f, "Stmt:PRINT:{expr:#?}")
                } else {
                    write!(f, "Stmt:PRINT:{expr:?}")
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum StmtData {
    Let(Result<Span, Span>, Expr),
    Expr(Expr),
    Print(Expr),
    Error,
}
