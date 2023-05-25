mod scopes;

use scopes::ScopeStack;

use crate::{
    ast::ops::{BinaryOp, UnaryOp},
    int_ir::{
        visitor::{IIRExprVisitor, IIRStmtVisitor},
        IIRExpr, IIRExprData,
    },
    span::Span,
    types::Type,
    value::Value,
};

pub struct Interpreter<'src> {
    scopes: ScopeStack<Value>,
    src: &'src str,
}

impl<'src> Interpreter<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            scopes: ScopeStack::new(),
            src,
        }
    }
}

macro_rules! get_or_ret {
    ($val:expr) => {
        match $val {
            v @ IntpControlFlow::Ret(_) => return v,
            IntpControlFlow::Val(v) => v,
        }
    };
}

pub enum IntpControlFlow {
    Ret(Value),
    Val(Value),
}

impl<'src> IIRStmtVisitor<IntpControlFlow> for Interpreter<'src> {
    fn visit_expr_stmt(&mut self, expr: &IIRExpr, _span: Span) -> IntpControlFlow {
        self.visit_expr(expr);
        IntpControlFlow::Val(Value::None)
    }

    fn visit_print(&mut self, expr: &IIRExpr, _span: Span) -> IntpControlFlow {
        match self.visit_expr(expr) {
            v @ IntpControlFlow::Ret(_) => v,
            IntpControlFlow::Val(val) => {
                println!("{val}");
                IntpControlFlow::Val(Value::None)
            }
        }
    }

    fn visit_let(&mut self, ident: Span, expr: &IIRExpr, _span: Span) -> IntpControlFlow {
        match self.visit_expr(expr) {
            v @ IntpControlFlow::Ret(_) => v,
            IntpControlFlow::Val(val) => {
                self.scopes.decl(self.src[ident.range()].to_owned(), val);
                IntpControlFlow::Val(Value::None)
            }
        }
    }
}

impl<'src> IIRExprVisitor<IntpControlFlow> for Interpreter<'src> {
    fn visit_var(&mut self, span: Span) -> IntpControlFlow {
        match self.scopes.get(&self.src[span.range()]) {
            Some(val) => IntpControlFlow::Val(val.clone()),
            None => IntpControlFlow::Ret(Value::None),
        }
    }

    fn visit_const(&mut self, val: &Value, _span: Span) -> IntpControlFlow {
        IntpControlFlow::Val(val.clone())
    }

    fn visit_binary_op(
        &mut self,
        op: BinaryOp,
        lhs: &IIRExpr,
        rhs: &IIRExpr,
        _span: Span,
    ) -> IntpControlFlow {
        macro_rules! op {
            ($op_symbol:tt) => {{
                let lhs = get_or_ret!(self.visit_expr(lhs));
                let rhs = get_or_ret!(self.visit_expr(rhs));
                lhs $op_symbol rhs
            }};
        }
        use BinaryOp::*;
        match op {
            Add => op!(+),
            Sub => op!(-),
            Mul => op!(*),
            Div => op!(/),
            Assign => match lhs.data() {
                IIRExprData::Var => {
                    let rhs = get_or_ret!(self.visit_expr(rhs));
                    let ident = &self.src[lhs.span.range()];
                    let (old_val, scope_id) = match self.scopes.get_with_scope(ident) {
                        Some(v) => v,
                        None => return IntpControlFlow::Val(Value::None),
                    };
                    let rhs_type = rhs.get_type();
                    let old_type = old_val.get_type();
                    if rhs_type == Type::None || rhs_type == old_type {
                        self.scopes.set(ident, rhs);
                    } else if rhs_type != old_type {
                        self.scopes.del_from_scope(ident, scope_id);
                        self.scopes
                            .decl_in_scope(format!("{ident}_{}", rhs_type), rhs, scope_id)
                    }
                    IntpControlFlow::Val(Value::None)
                }
                _ => IntpControlFlow::Ret(Value::None),
            },
            Or => IntpControlFlow::Val(Value::Bool(
                get_or_ret!(self.visit_expr(lhs)).is_truthy()
                    || get_or_ret!(self.visit_expr(rhs)).is_truthy(),
            )),
            And => IntpControlFlow::Val(Value::Bool(
                get_or_ret!(self.visit_expr(lhs)).is_truthy()
                    && get_or_ret!(self.visit_expr(rhs)).is_truthy(),
            )),
        }
    }

    fn visit_unary_op(&mut self, op: UnaryOp, operand: &IIRExpr, _span: Span) -> IntpControlFlow {
        let operand = get_or_ret!(self.visit_expr(operand));
        match op {
            UnaryOp::Neg => -operand,
        }
    }
}
