use crate::{
    ast::{Expr, Stmt, StmtData},
    span::Span,
};

pub trait StmtVisitor<T> {
    fn visit_stmt(&mut self, stmt: &Stmt) -> T {
        use StmtData::*;
        match stmt.data() {
            Let(ident, expr) => self.visit_let(*ident, expr, stmt.span),
            Expr(expr) => self.visit_expr_stmt(expr, stmt.span),
            Print(expr) => self.visit_print(expr, stmt.span),
            Error => self.visit_error_stmt(stmt.span),
        }
    }

    fn visit_error_stmt(&mut self, span: Span) -> T;
    fn visit_expr_stmt(&mut self, expr: &Expr, span: Span) -> T;
    fn visit_print(&mut self, expr: &Expr, span: Span) -> T;
    fn visit_let(&mut self, ident: Result<Span, Span>, expr: &Expr, span: Span) -> T;
}
