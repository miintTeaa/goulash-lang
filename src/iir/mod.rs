mod expr;
mod stmt;
pub mod visitor;

pub use expr::*;
pub use stmt::*;

use crate::{ast::Stmt, error::LangError};

pub fn build<'src>(src: &'src str, stmts: Vec<Stmt>) -> Result<Vec<IIRStmt>, Vec<LangError>> {
    let mut errors = Vec::new();
    let mut iir = Vec::new();

    for stmt in stmts {
        match IIRStmt::try_from(stmt, src) {
            Ok(o) => iir.push(o),
            Err(e) => errors.push(e),
        }
    }

    if errors.is_empty() {
        Ok(iir)
    } else {
        Err(errors)
    }
}
