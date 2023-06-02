use crate::{
    ast::ops::{BinaryOp, UnaryOp},
    iir::{IIRExpr, IIRExprData, IIRStmt},
    span::Span,
    value::Value,
};

pub trait IIRExprVisitor<T> {
    fn visit_expr(&mut self, expr: &IIRExpr) -> T {
        match expr.data() {
            IIRExprData::Op(op, lhs, rhs) => self.visit_binary_op(*op, lhs, rhs, expr.span),
            IIRExprData::UnOp(op, operand) => self.visit_unary_op(*op, operand, expr.span),
            IIRExprData::Const(val) => self.visit_const(val, expr.span),
            IIRExprData::Var => self.visit_var(expr.span),
            IIRExprData::Block(stmts, last_expr) => {
                self.visit_block(stmts, last_expr.as_deref(), expr.span)
            }
            IIRExprData::Call(expr, args) => self.visit_call(expr, args, expr.span),
            IIRExprData::Class(name, supers, fields) => {
                self.visit_class(*name, &*supers, &*fields, expr.span)
            }
            IIRExprData::Access(expr, access_ident) => {
                self.visit_access(expr, *access_ident, expr.span)
            }
            IIRExprData::AccessSet(lhs, access_ident, rhs) => {
                self.visit_access_set(lhs, *access_ident, rhs, expr.span)
            }
            IIRExprData::List(exprs) => self.visit_list(exprs),
            IIRExprData::Index(expr, index) => self.visit_index(expr, index),
            IIRExprData::IndexSet(expr, index, to) => self.visit_index_set(expr, index, to),
            IIRExprData::If(condition, block, r#else) => {
                self.visit_if(condition, block, r#else.as_deref())
            }
        }   
    }

    fn visit_var(&mut self, span: Span) -> T;
    fn visit_const(&mut self, val: &Value, span: Span) -> T;
    fn visit_binary_op(&mut self, op: BinaryOp, lhs: &IIRExpr, rhs: &IIRExpr, span: Span) -> T;
    fn visit_unary_op(&mut self, op: UnaryOp, operand: &IIRExpr, span: Span) -> T;
    fn visit_block(&mut self, stmts: &[IIRStmt], expr: Option<&IIRExpr>, span: Span) -> T;
    fn visit_call(&mut self, expr: &IIRExpr, args: &[IIRExpr], span: Span) -> T;
    fn visit_class(
        &mut self,
        name: Span,
        supers: &[IIRExpr],
        fields: &[(Span, IIRExpr)],
        span: Span,
    ) -> T;
    fn visit_access(&mut self, expr: &IIRExpr, access_ident: Span, span: Span) -> T;
    fn visit_access_set(
        &mut self,
        lhs: &IIRExpr,
        access_ident: Span,
        rhs: &IIRExpr,
        span: Span,
    ) -> T;
    fn visit_list(&mut self, exprs: &[IIRExpr]) -> T;
    fn visit_index(&mut self, expr: &IIRExpr, index: &IIRExpr) -> T;
    fn visit_index_set(&mut self, expr: &IIRExpr, index: &IIRExpr, to: &IIRExpr) -> T;
    fn visit_if(&mut self, condition: &IIRExpr, block: &IIRExpr, r#else: Option<&IIRExpr>) -> T;
}
