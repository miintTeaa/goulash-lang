mod scopes;

use scopes::ScopeStack;

use crate::{
    ast::ops::{BinaryOp, UnaryOp},
    iir::{
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
    ($self:expr, $val:expr) => {
        match $self.visit_expr($val) {
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
        get_or_ret!(self, expr);
        IntpControlFlow::Val(Value::None)
    }

    fn visit_print(&mut self, expr: &IIRExpr, _span: Span) -> IntpControlFlow {
        println!("{}", get_or_ret!(self, expr));
        IntpControlFlow::Val(Value::None)
    }

    fn visit_let(&mut self, ident: Span, expr: &IIRExpr, _span: Span) -> IntpControlFlow {
        let val = get_or_ret!(self, expr);
        self.scopes.declare(self.src[ident.range()].to_owned(), val);
        IntpControlFlow::Val(Value::None)
    }
}

impl<'src> IIRExprVisitor<IntpControlFlow> for Interpreter<'src> {
    fn visit_var(&mut self, span: Span) -> IntpControlFlow {
        match self.scopes.find(&self.src[span.range()]) {
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
                let lhs = get_or_ret!(self, lhs);
                let rhs = get_or_ret!(self, rhs);
                lhs $op_symbol rhs
            }};
        }
        match op {
            BinaryOp::Add => op!(+),
            BinaryOp::Sub => op!(-),
            BinaryOp::Mul => op!(*),
            BinaryOp::Div => op!(/),
            BinaryOp::Eq => {
                let lhs = get_or_ret!(self, lhs);
                let rhs = get_or_ret!(self, rhs);
                IntpControlFlow::Val(Value::Bool(lhs == rhs))
            }
            BinaryOp::NotEq => {
                let lhs = get_or_ret!(self, lhs);
                let rhs = get_or_ret!(self, rhs);
                IntpControlFlow::Val(Value::Bool(lhs != rhs))
            }
            BinaryOp::Assign => match lhs.data() {
                IIRExprData::Var => {
                    let rhs = get_or_ret!(self, rhs);
                    let ident = &self.src[lhs.span.range()];
                    let (old_val, scope_id) = match self.scopes.find_with_scope_mut(ident) {
                        Some(v) => v,
                        None => return IntpControlFlow::Val(Value::None),
                    };
                    let rhs_type = rhs.get_type();
                    let old_type = old_val.get_type();
                    if rhs_type == Type::None || rhs_type == old_type {
                        *old_val = rhs;
                    } else if rhs_type != old_type {
                        self.scopes[scope_id].delete(ident);
                        self.scopes[scope_id].declare(format!("{ident}_{rhs_type}"), rhs)
                    }
                    IntpControlFlow::Val(Value::None)
                }
                _ => IntpControlFlow::Ret(Value::None),
            },
            BinaryOp::Or => IntpControlFlow::Val(Value::Bool(
                get_or_ret!(self, lhs).is_truthy() || get_or_ret!(self, rhs).is_truthy(),
            )),
            BinaryOp::And => IntpControlFlow::Val(Value::Bool(
                get_or_ret!(self, lhs).is_truthy() && get_or_ret!(self, rhs).is_truthy(),
            )),
        }
    }

    fn visit_unary_op(&mut self, op: UnaryOp, operand: &IIRExpr, _span: Span) -> IntpControlFlow {
        let operand = get_or_ret!(self, operand);
        match op {
            UnaryOp::Neg => -operand,
            UnaryOp::Not => IntpControlFlow::Val(Value::Bool(!operand.is_truthy())),
        }
    }
}
