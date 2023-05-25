use crate::ast::ops::BinaryOp;
use crate::ast::ops::UnaryOp;
use crate::ast::Expr;
use crate::ast::ExprData;
use crate::ast::ExprValueType;
use crate::ast::Stmt;
use crate::span::Span;

pub trait ExprVisitor<T> {
    fn visit_expr(&mut self, expr: &Expr) -> T {
        match expr.data() {
            ExprData::Op(op, lhs, rhs) => self.visit_binary_op(*op, lhs, rhs, expr.span),
            ExprData::UnOp(op, operand) => self.visit_unary_op(*op, operand, expr.span),
            ExprData::Lit(kind) => self.visit_literal(kind, expr.span),
            ExprData::Var => self.visit_var(expr.span),
            ExprData::Error => self.visit_error_expr(expr.span),
        }
    }

    fn visit_error_expr(&mut self, span: Span) -> T;
    fn visit_var(&mut self, span: Span) -> T;
    fn visit_literal(&mut self, kind: &ExprValueType, span: Span) -> T;
    fn visit_unary_op(&mut self, op: UnaryOp, operand: &Expr, span: Span) -> T;
    fn visit_binary_op(&mut self, op: BinaryOp, lhs: &Expr, rhs: &Expr, span: Span) -> T;
}
