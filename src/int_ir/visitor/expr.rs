use crate::{
    ast::ops::{BinaryOp, UnaryOp},
    int_ir::{IIRExpr, IIRExprData},
    span::Span,
    value::Value,
};

pub trait IIRExprVisitor<T> {
    fn visit_expr(&mut self, expr: &IIRExpr) -> T {
        use IIRExprData::*;
        match expr.data() {
            Op(op, lhs, rhs) => self.visit_binary_op(*op, lhs, rhs, expr.span),
            UnOp(op, operand) => self.visit_unary_op(*op, operand, expr.span),
            Const(val) => self.visit_const(val, expr.span),
            Var => self.visit_var(expr.span),
        }
    }

    fn visit_var(&mut self, span: Span) -> T;
    fn visit_const(&mut self, val: &Value, span: Span) -> T;
    fn visit_binary_op(&mut self, op: BinaryOp, lhs: &IIRExpr, rhs: &IIRExpr, span: Span) -> T;
    fn visit_unary_op(&mut self, op: UnaryOp, operand: &IIRExpr, span: Span) -> T;
}
