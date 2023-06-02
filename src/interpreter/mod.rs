mod scopes;

use std::{collections::HashMap, io::Write, process::exit};

use scopes::ScopeStack;

use crate::{
    ast::{
        id,
        ops::{BinaryOp, UnaryOp},
        Ident,
    },
    iir::{
        self,
        visitor::{IIRExprVisitor, IIRStmtVisitor},
        IIRExpr, IIRExprData,
    },
    parse,
    span::Span,
    types::Type,
    value::Value,
};

pub struct Interpreter {
    scopes: ScopeStack<Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut interpreter = Self {
            scopes: ScopeStack::new(),
        };

        interpreter.scopes.declare(
            id("__builtin__"),
            Value::new_fn_raw(vec![id("builtin_name")], |interpreter: &mut Interpreter| {
                let builtin_name = interpreter
                    .scopes
                    .find(&id("builtin_name"))
                    .expect("should exist")
                    .to_string();

                let builtin_name = String::from("@") + &builtin_name;

                match interpreter.scopes.find(&id(builtin_name)) {
                    Some(v) => v.clone(),
                    _ => Value::None,
                }
            }),
        );

        let std_src = include_str!("../std.gls");

        interpreter.scopes.declare(
            id("std"),
            Value::new_fn(
                vec![],
                match iir::build(
                    std_src,
                    match parse(std_src) {
                        Ok(o) => o,
                        Err(e) => panic!("{e}"),
                    },
                ) {
                    Ok(o) => o,
                    Err(e) => {
                        for error in e {
                            error.report("std", std_src);
                        }
                        panic!();
                    }
                },
                None,
            ),
        );

        interpreter.scopes.declare(
            id("@print"),
            Value::new_fn_raw(vec![id("printed")], |interpreter: &mut Interpreter| {
                let mut stdout = std::io::stdout().lock();
                let printed = interpreter
                    .scopes
                    .find(&id("printed"))
                    .expect("should exist");
                let buffer = format!("{printed}");
                let mut buffer = buffer.as_bytes();
                let mut written = 0;
                loop {
                    match stdout.write(buffer) {
                        Ok(new_written) => {
                            written += new_written;
                            if written >= buffer.len() {
                                break;
                            }
                            buffer = &buffer[new_written..];
                        }
                        Err(e) => match e.kind() {
                            std::io::ErrorKind::Interrupted => {
                                continue;
                            }
                            _ => return Value::FALSE,
                        },
                    }
                }
                match stdout.flush() {
                    Ok(_) => Value::TRUE,
                    Err(_) => Value::FALSE,
                }
            }),
        );

        interpreter.scopes.declare(
            id("@exit"),
            Value::new_fn_raw(vec![id("exit_data")], |interpreter: &mut Interpreter| {
                let value = interpreter
                    .scopes
                    .find_mut(&id("exit_data"))
                    .expect("should exist")
                    .clone();
                match value.to_int(interpreter) {
                    IntpControlFlow::Val(Value::Int(i)) => exit(i),
                    _ => {
                        eprintln!("Internal error");
                        exit(1)
                    }
                }
            }),
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
            b @ IntpControlFlow::Brk(_) => return b,
            IntpControlFlow::Val(v) => v,
        }
    };
}

macro_rules! get_or_ret {
    ($val:expr) => {
        match $val {
            v @ IntpControlFlow::Ret(_) => return v,
            b @ IntpControlFlow::Brk(_) => return b,
            IntpControlFlow::Val(v) => v,
        }
    };
}

#[derive(Debug)]
pub enum IntpControlFlow {
    Ret(Value),
    Val(Value),
    Brk(Value),
}

impl IIRStmtVisitor<IntpControlFlow> for Interpreter {
    fn visit_expr_stmt(&mut self, expr: &IIRExpr, _span: Span) -> IntpControlFlow {
        visit_or_ret!(self, expr);
        IntpControlFlow::Val(Value::None)
    }

    fn visit_let(&mut self, ident: &Ident, expr: &IIRExpr, _span: Span) -> IntpControlFlow {
        let val = visit_or_ret!(self, expr);
        self.scopes.declare(ident.clone(), val);
        IntpControlFlow::Val(Value::None)
    }
}

impl IIRExprVisitor<IntpControlFlow> for Interpreter {
    fn visit_var(&mut self, ident: Ident, _span: Span) -> IntpControlFlow {
        match self.scopes.find(&ident) {
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
                IIRExprData::Var(ident) => {
                    let rhs = visit_or_ret!(self, rhs);
                    let (old_val, scope_id) = match self.scopes.find_with_scope_mut(ident) {
                        Some(v) => v,
                        None => return IntpControlFlow::Val(Value::None),
                    };
                    let rhs_type = rhs.get_type();
                    let old_type = old_val.get_type();
                    if rhs_type == Type::None || rhs_type == old_type {
                        *old_val = rhs;
                    } else if rhs_type != old_type {
                        let (name, _) = self.scopes[scope_id]
                            .delete(ident)
                            .expect("should still exist");
                        self.scopes[scope_id].declare(id(rhs.apply_suffix(&name)), rhs)
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
                v @ IntpControlFlow::Brk(_) => {
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
        name: Ident,
        iir_supers: &[IIRExpr],
        iir_fields: &[(Ident, IIRExpr)],
        _span: Span,
    ) -> IntpControlFlow {
        let mut fields = HashMap::default();
        for (field_ident, expr) in iir_fields {
            fields.insert(field_ident.clone(), visit_or_ret!(self, expr));
        }

        let mut supers = Vec::new();
        for sup in iir_supers {
            match visit_or_ret!(self, sup) {
                Value::Obj(o) => supers.push(o),
                _ => return IntpControlFlow::Ret(Value::None),
            }
        }

        IntpControlFlow::Val(Value::new_obj(name, supers, fields))
    }

    fn visit_access(
        &mut self,
        expr: &IIRExpr,
        access_ident: Ident,
        _span: Span,
    ) -> IntpControlFlow {
        let value = visit_or_ret!(self, expr);

        match value {
            Value::Obj(o) => match o.borrow().get_field(access_ident) {
                Some(val) => IntpControlFlow::Val(val.clone()),
                None => IntpControlFlow::Val(Value::None),
            },
            _ => IntpControlFlow::Ret(Value::None),
        }
    }

    fn visit_access_set(
        &mut self,
        lhs: &IIRExpr,
        access_ident: Ident,
        rhs: &IIRExpr,
        _span: Span,
    ) -> IntpControlFlow {
        let rhs_val = visit_or_ret!(self, rhs);
        let lhs_val = visit_or_ret!(self, lhs);
        match lhs_val {
            Value::Obj(o) => {
                o.borrow_mut().set_field(access_ident, rhs_val);
                IntpControlFlow::Val(Value::None)
            }
            _ => IntpControlFlow::Ret(Value::None),
        }
    }

    fn visit_list(&mut self, exprs: &[IIRExpr], _span: Span) -> IntpControlFlow {
        let mut values = Vec::new();
        for expr in exprs {
            values.push(get_or_ret!(self.visit_expr(expr)));
        }
        IntpControlFlow::Val(Value::new_list(values))
    }

    fn visit_index(&mut self, expr: &IIRExpr, index: &IIRExpr, _span: Span) -> IntpControlFlow {
        visit_or_ret!(self, expr).index_by(visit_or_ret!(self, index), self)
    }

    fn visit_index_set(
        &mut self,
        expr: &IIRExpr,
        index: &IIRExpr,
        to: &IIRExpr,
        _span: Span,
    ) -> IntpControlFlow {
        visit_or_ret!(self, expr).index_by_set(
            visit_or_ret!(self, index),
            visit_or_ret!(self, to),
            self,
        )
    }

    fn visit_if(
        &mut self,
        condition: &IIRExpr,
        block: &IIRExpr,
        r#else: Option<&IIRExpr>,
        _span: Span,
    ) -> IntpControlFlow {
        if visit_or_ret!(self, condition).is_truthy() {
            self.visit_expr(block)
        } else if let Some(r#else) = r#else {
            self.visit_expr(r#else)
        } else {
            // oof
            IntpControlFlow::Val(Value::None)
        }
    }

    fn visit_loop(&mut self, block: &IIRExpr, _span: Span) -> IntpControlFlow {
        loop {
            match self.visit_expr(block) {
                v @ IntpControlFlow::Ret(_) => break v,
                IntpControlFlow::Brk(v) => break IntpControlFlow::Val(v),
                IntpControlFlow::Val(_) => {}
            }
        }
    }

    fn visit_break(&mut self, expr: Option<&IIRExpr>, _span: Span) -> IntpControlFlow {
        match expr {
            Some(v) => match self.visit_expr(v) {
                v @ IntpControlFlow::Ret(_) => v,
                v @ IntpControlFlow::Brk(_) => v,
                IntpControlFlow::Val(v) => IntpControlFlow::Brk(v),
            },
            None => IntpControlFlow::Brk(Value::None),
        }
    }

    fn visit_return(&mut self, expr: Option<&IIRExpr>, _span: Span) -> IntpControlFlow {
        match expr {
            Some(v) => match self.visit_expr(v) {
                v @ IntpControlFlow::Ret(_) => v,
                v @ IntpControlFlow::Brk(_) => v,
                IntpControlFlow::Val(v) => IntpControlFlow::Ret(v),
            },
            None => IntpControlFlow::Brk(Value::None),
        }
    }
}
