use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display},
    ops::{Add, Div, Mul, Neg, Sub},
    rc::Rc,
};

use crate::{
    iir::{
        visitor::{IIRExprVisitor, IIRStmtVisitor},
        IIRExpr, IIRStmt,
    },
    interpreter::{Interpreter, IntpControlFlow},
    types::Type,
};

#[derive(Debug)]
pub struct ObjData {
    name: String,
    supers: Vec<Rc<RefCell<ObjData>>>,
    fields: HashMap<String, Value>,
}

impl ObjData {
    pub fn new(
        name: String,
        supers: Vec<Rc<RefCell<ObjData>>>,
        fields: HashMap<String, Value>,
    ) -> Self {
        Self {
            name,
            supers,
            fields,
        }
    }

    pub fn get_field(&self, ident: &str) -> Option<Value> {
        match self.fields.get(ident) {
            Some(field) => Some(field.clone()),
            None => {
                for sup in &self.supers {
                    match sup.borrow().get_field(ident) {
                        Some(field) => return Some(field),
                        None => {}
                    }
                }
                None
            }
        }
    }

    pub fn set_field(&mut self, ident: &str, value: Value) -> Option<Value> {
        match self.fields.contains_key(ident) {
            true => self.fields.insert(ident.to_owned(), value),
            false => {
                for sup in &self.supers {
                    match sup.borrow_mut().set_field(ident, value.clone()) {
                        Some(field) => return Some(field),
                        None => {}
                    }
                }
                None
            }
        }
    }
}

pub type FunctionSignature = Box<dyn Fn(&mut Interpreter) -> Value>;

pub struct FunctionData {
    args: Vec<String>,
    callback: FunctionSignature,
}

impl FunctionData {
    pub fn new(args: Vec<String>, stmts: Vec<IIRStmt>, ending_expr: Option<IIRExpr>) -> Self {
        let callback = Box::new(move |interpreter: &mut Interpreter| {
            for stmt in &stmts {
                match interpreter.visit_stmt(stmt) {
                    IntpControlFlow::Ret(v) => return v,
                    IntpControlFlow::Val(_) => (),
                }
            }
            if let Some(expr) = ending_expr.as_ref() {
                match interpreter.visit_expr(expr) {
                    IntpControlFlow::Ret(v) | IntpControlFlow::Val(v) => return v,
                }
            }
            Value::None
        });
        Self { args, callback }
    }

    pub fn new_raw(args: Vec<String>, callback: FunctionSignature) -> Self {
        Self { args, callback }
    }

    pub fn call(&self, arg_values: Vec<Value>, interpreter: &mut Interpreter) -> IntpControlFlow {
        if arg_values.len() != self.args.len() {
            return IntpControlFlow::Ret(Value::None);
        }
        interpreter.scopes_mut().push_scope();
        let mut i = 0;
        for arg_value in arg_values {
            interpreter
                .scopes_mut()
                .declare(self.args[i].clone(), arg_value);
            i += 1;
        }
        let ret = (self.callback)(interpreter);
        interpreter.scopes_mut().pop_scope();
        IntpControlFlow::Val(ret)
    }
}

impl Debug for FunctionData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunctionData")
            .field("args", &self.args)
            .field("callback", &format!("{:p}", self.callback))
            .finish()
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    Float(f32),
    Str(Rc<String>),
    Bool(bool),
    Fn(Rc<FunctionData>),
    Obj(Rc<RefCell<ObjData>>),
    None,
}

impl Value {
    pub fn apply_suffix(&self, s: &mut String) {
        match self {
            Value::Int(_) => *s += "_int",
            Value::Float(_) => *s += "_float",
            Value::Str(_) => *s += "_str",
            Value::Bool(_) => *s += "_bool",
            Value::Fn(_) => *s += "_fn",
            Value::Obj(o) => s.extend(["_", &o.borrow().name]),
            Value::None => *s += "_none",
        };
    }

    pub fn get_type(&self) -> Type {
        match self {
            Value::Int(_) => Type::Int,
            Value::Float(_) => Type::Float,
            Value::Str(_) => Type::Str,
            Value::Bool(_) => Type::Bool,
            Value::Fn(_) => Type::Fn,
            Value::Obj(_) => Type::Obj,
            Value::None => Type::None,
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Int(i) => *i != 0,
            Value::Float(f) => *f != 0.0,
            Value::Str(s) => match &*s.to_ascii_lowercase() {
                // this is so dumb lol
                "true" => true,
                "false" => false,
                "yes" => true,
                "no" => false,
                "y" => true,
                "n" => false,
                _ => true,
            },
            Value::None => false,
            _ => true,
        }
    }
}

macro_rules! impl_op {
    (($op_name:ident, $op_fname:ident) => $op_symbol:tt) => {
        impl $op_name<&Value> for Value {
            type Output = IntpControlFlow;

            fn $op_fname(self, rhs: &Value) -> Self::Output {
                match (self, rhs) {
                    (Value::None, _) | (_, Value::None) => IntpControlFlow::Ret(Value::None),
                    (Value::Float(f), Value::Int(i)) => IntpControlFlow::Val(Value::Float(f $op_symbol *i as f32)),
                    (Value::Int(i), Value::Float(f)) => IntpControlFlow::Val(Value::Float(*f $op_symbol i as f32)),
                    (Value::Int(i1), Value::Int(i2)) => {
                        concat_idents::concat_idents!(wrapping_fn = wrapping_, $op_fname {
                            IntpControlFlow::Val(Value::Int(i1.wrapping_fn(*i2)))
                        })
                    }
                    (_, _) => IntpControlFlow::Ret(Value::None),
                }
            }
        }

        impl $op_name<Value> for Value {
            type Output = IntpControlFlow;

            fn $op_fname(self, rhs: Value) -> Self::Output {
                match (self, rhs) {
                    (Value::None, _) | (_, Value::None) => IntpControlFlow::Ret(Value::None),
                    (Value::Float(f), Value::Int(i)) => IntpControlFlow::Val(Value::Float(f $op_symbol i as f32)),
                    (Value::Int(i), Value::Float(f)) => IntpControlFlow::Val(Value::Float(f $op_symbol i as f32)),
                    (Value::Int(i1), Value::Int(i2)) => {
                        concat_idents::concat_idents!(wrapping_fn = wrapping_, $op_fname {
                            IntpControlFlow::Val(Value::Int(i1.wrapping_fn(i2)))
                        })
                    }
                    (_, _) => IntpControlFlow::Ret(Value::None),
                }
            }
        }
    };
}

impl_op!((Add, add) => +);
impl_op!((Sub, sub) => -);
impl_op!((Mul, mul) => *);
impl_op!((Div, div) => /);

impl Neg for Value {
    type Output = IntpControlFlow;

    fn neg(self) -> Self::Output {
        match self {
            Value::Int(i) => IntpControlFlow::Val(Value::Int(-i)),
            Value::Float(f) => IntpControlFlow::Val(Value::Float(-f)),
            _ => IntpControlFlow::Ret(Value::None),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::None, _) | (_, Self::None) => true,
            (Self::Int(i1), Self::Int(i2)) => i1 == i2,
            (Self::Float(f1), Self::Float(f2)) => f1 == f2,
            (Self::Int(i), Self::Float(f)) | (Self::Float(f), Self::Int(i)) => *i == *f as i32,
            (Self::Str(s1), Self::Str(s2)) => s1 == s2,
            (Self::Bool(b1), Self::Bool(b2)) => b1 == b2,
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{i}"),
            Value::Float(fl) => write!(f, "{fl}"),
            Value::Str(s) => write!(f, "{s}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Obj(o) => write!(f, "<obj {}>", o.borrow().name),
            Value::Fn(func) => write!(f, "<fn {:p}>", func.callback),
            Value::None => write!(f, "None"),
        }
    }
}
