use crate::{
    iir::{IIRExpr, IIRStmt, IIRStmtData},
    span::Span,
};

pub trait IIRStmtVisitor<T> {
    fn visit_stmt(&mut self, stmt: &IIRStmt) -> T {
        use IIRStmtData::*;
        match stmt.data() {
            Let(ident, expr) => self.visit_let(*ident, expr, stmt.span),
            Expr(expr) => self.visit_expr_stmt(expr, stmt.span),
        }
    }

    fn visit_expr_stmt(&mut self, expr: &IIRExpr, span: Span) -> T;
    fn visit_let(&mut self, ident: Span, expr: &IIRExpr, span: Span) -> T;
}
