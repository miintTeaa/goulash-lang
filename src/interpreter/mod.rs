mod scopes;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use scopes::ScopeStack;

use crate::{
    ast::ops::{BinaryOp, UnaryOp},
    iir::{
        visitor::{IIRExprVisitor, IIRStmtVisitor},
        IIRExpr, IIRExprData,
    },
    span::Span,
    types::Type,
    value::{FunctionData, ObjData, Value},
};

pub struct Interpreter<'src> {
    scopes: ScopeStack<Value>,
    src: &'src str,
}

impl<'src> Interpreter<'src> {
    pub fn new(src: &'src str) -> Self {
        let mut interpreter = Self {
            scopes: ScopeStack::new(),
            src,
        };

        interpreter.scopes.declare(
            "print".to_owned(),
            Value::Fn(Rc::new(FunctionData::new_raw(
                vec!["printed".to_owned()],
                Box::new(|interpreter: &mut Interpreter| {
                    println!(
                        "{}",
                        interpreter.scopes.find("printed").expect("should exist")
                    );
                    Value::None
                }),
            ))),
        );

        interpreter
    }

    pub fn scopes(&self) -> &ScopeStack<Value> {
        &self.scopes
    }

    pub fn scopes_mut(&mut self) -> &mut ScopeStack<Value> {
        &mut self.scopes
    }
}

macro_rules! visit_or_ret {
    ($self:expr, $val:expr) => {
        match $self.visit_expr($val) {
            v @ IntpControlFlow::Ret(_) => return v,
            IntpControlFlow::Val(v) => v,
        }
    };
}

macro_rules! get_or_ret {
    ($val:expr) => {
        match $val {
            v @ IntpControlFlow::Ret(_) => return v,
            IntpControlFlow::Val(v) => v,
        }
    };
}

#[derive(Debug)]
pub enum IntpControlFlow {
    Ret(Value),
    Val(Value),
}

impl<'src> IIRStmtVisitor<IntpControlFlow> for Interpreter<'src> {
    fn visit_expr_stmt(&mut self, expr: &IIRExpr, _span: Span) -> IntpControlFlow {
        visit_or_ret!(self, expr);
        IntpControlFlow::Val(Value::None)
    }

    fn visit_let(&mut self, ident: Span, expr: &IIRExpr, _span: Span) -> IntpControlFlow {
        let val = visit_or_ret!(self, expr);
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
            ($op_name:ident) => {{
                let lhs = visit_or_ret!(self, lhs);
                let rhs = visit_or_ret!(self, rhs);
                lhs.$op_name(rhs, self)
            }};
        }
        match op {
            BinaryOp::Add => op!(add),
            BinaryOp::Sub => op!(sub),
            BinaryOp::Mul => op!(mul),
            BinaryOp::Div => op!(div),
            BinaryOp::Eq => op!(eq),
            BinaryOp::NotEq => {
                let lhs = visit_or_ret!(self, lhs);
                let rhs = visit_or_ret!(self, rhs);
                lhs.eq(rhs, self)
            }
            BinaryOp::Greater => {
                op!(greater_than)
            }
            BinaryOp::GreaterEq => {
                let is_greater_result = get_or_ret!(op!(greater_than));
                if !is_greater_result.is_truthy() {
                    op!(eq)
                } else {
                    IntpControlFlow::Val(is_greater_result)
                }
            }
            BinaryOp::Lesser => get_or_ret!(op!(greater_than)).not(self),
            BinaryOp::LesserEq => {
                let is_lesser_result = get_or_ret!(get_or_ret!(op!(greater_than)).not(self));
                if is_lesser_result.is_truthy() {
                    op!(eq)
                } else {
                    IntpControlFlow::Val(is_lesser_result)
                }
            }
            BinaryOp::Assign => match lhs.data() {
                IIRExprData::Var => {
                    let rhs = visit_or_ret!(self, rhs);
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
                        let (mut name, _) = self.scopes[scope_id]
                            .delete(ident)
                            .expect("should still exist");
                        rhs.apply_suffix(&mut name);
                        self.scopes[scope_id].declare(name, rhs)
                    }
                    IntpControlFlow::Val(Value::None)
                }
                _ => IntpControlFlow::Ret(Value::None),
            },
            BinaryOp::Or => IntpControlFlow::Val(Value::Bool(
                visit_or_ret!(self, lhs).is_truthy() || visit_or_ret!(self, rhs).is_truthy(),
            )),
            BinaryOp::And => IntpControlFlow::Val(Value::Bool(
                visit_or_ret!(self, lhs).is_truthy() && visit_or_ret!(self, rhs).is_truthy(),
            )),
        }
    }

    fn visit_unary_op(&mut self, op: UnaryOp, operand: &IIRExpr, _span: Span) -> IntpControlFlow {
        let operand = visit_or_ret!(self, operand);
        match op {
            UnaryOp::Neg => operand.neg(self),
            UnaryOp::Not => operand.not(self),
        }
    }

    fn visit_block(
        &mut self,
        stmts: &[crate::iir::IIRStmt],
        expr: Option<&IIRExpr>,
        _span: Span,
    ) -> IntpControlFlow {
        self.scopes.push_scope();
        for stmt in stmts {
            match self.visit_stmt(stmt) {
                v @ IntpControlFlow::Ret(_) => {
                    self.scopes.pop_scope();
                    return v;
                }
                IntpControlFlow::Val(_) => (),
            }
        }
        let cflw = match expr {
            Some(expr) => self.visit_expr(expr),
            None => IntpControlFlow::Val(Value::None),
        };
        self.scopes.pop_scope();
        cflw
    }

    fn visit_call(&mut self, expr: &IIRExpr, args: &[IIRExpr], _span: Span) -> IntpControlFlow {
        match visit_or_ret!(self, expr) {
            Value::Fn(func_data) => {
                let mut arg_values = Vec::new();
                for arg in args {
                    arg_values.push(visit_or_ret!(self, arg));
                }
                func_data.call(arg_values, self)
            }
            _ => return IntpControlFlow::Ret(Value::None),
        }
    }

    fn visit_class(
        &mut self,
        name: Span,
        iir_supers: &[IIRExpr],
        iir_fields: &[(Span, IIRExpr)],
        _span: Span,
    ) -> IntpControlFlow {
        let mut fields = HashMap::default();
        for (span, expr) in iir_fields {
            match self.visit_expr(expr) {
                v @ IntpControlFlow::Ret(_) => return v,
                IntpControlFlow::Val(v) => fields.insert(self.src[span.range()].to_owned(), v),
            };
        }

        let mut supers = Vec::new();
        for sup in iir_supers {
            match visit_or_ret!(self, sup) {
                Value::Obj(o) => supers.push(o),
                _ => return IntpControlFlow::Ret(Value::None),
            }
        }

        IntpControlFlow::Val(Value::Obj(Rc::new(RefCell::new(ObjData::new(
            self.src[name.range()].to_owned(),
            supers,
            fields,
        )))))
    }

    fn visit_access(&mut self, expr: &IIRExpr, access_ident: Span, _span: Span) -> IntpControlFlow {
        let value = visit_or_ret!(self, expr);

        match value {
            Value::Obj(o) => match o.borrow().get_field(&self.src[access_ident.range()]) {
                Some(val) => IntpControlFlow::Val(val.clone()),
                None => IntpControlFlow::Val(Value::None),
            },
            _ => IntpControlFlow::Ret(Value::None),
        }
    }

    fn visit_access_set(
        &mut self,
        lhs: &IIRExpr,
        access_ident: Span,
        rhs: &IIRExpr,
        _span: Span,
    ) -> IntpControlFlow {
        let rhs_val = visit_or_ret!(self, rhs);
        let lhs_val = visit_or_ret!(self, lhs);
        match lhs_val {
            Value::Obj(o) => {
                o.borrow_mut()
                    .set_field(&self.src[access_ident.range()], rhs_val);
                IntpControlFlow::Val(Value::None)
            }
            _ => IntpControlFlow::Ret(Value::None),
        }
    }
}
