use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display},
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

    pub fn to_int(&self, interpreter: &mut Interpreter) -> IntpControlFlow {
        match self {
            Value::Int(i) => IntpControlFlow::Val(Value::Int(*i)),
            Value::Float(f) => IntpControlFlow::Val(Value::Int(*f as i32)),
            Value::Str(s) => str_to_int(s),
            Value::Bool(b) => IntpControlFlow::Val(Value::Int(*b as i32)),
            Value::Fn(_) => IntpControlFlow::Ret(Value::None),
            Value::Obj(o) => match o.borrow().get_field("to_int") {
                Some(Value::Fn(f)) => f.call(vec![Value::Obj(o.clone())], interpreter),
                _ => IntpControlFlow::Ret(Value::None),
            },
            Value::None => IntpControlFlow::Val(Value::Int(0)),
        }
    }

    pub fn greater_than(self, rhs: Value, interpreter: &mut Interpreter) -> IntpControlFlow {
        match (self, rhs) {
            (Value::None, _) | (_, Value::None) => IntpControlFlow::Ret(Value::None),
            (Value::Float(f1), Value::Float(f2)) => IntpControlFlow::Val(Value::Bool(f1 > f2)),
            (Value::Float(f), Value::Int(i)) | (Value::Int(i), Value::Float(f)) => {
                IntpControlFlow::Val(Value::Bool(f > i as f32))
            }
            (Value::Int(i1), Value::Int(i2)) => IntpControlFlow::Val(Value::Bool(i1 > i2)),
            (o @ Value::Obj(_), other) | (other, o @ Value::Obj(_)) => {
                match o.to_int(interpreter) {
                    v @ IntpControlFlow::Ret(_) => return v,
                    IntpControlFlow::Val(v) => v,
                }
                .greater_than(other, interpreter)
            }
            (_, _) => IntpControlFlow::Ret(Value::None),
        }
    }
}

fn str_to_int(s: &str) -> IntpControlFlow {
    let mut val = 0;
    for byte in s.as_bytes() {
        match byte {
            b'0'..=b'9' => {
                val += (byte - b'0') as i32;
            }
            b' ' | b'\t' | b'\n' | 11 | 12 | 13 => {
                return IntpControlFlow::Ret(Value::None);
            }
            _ => {
                break;
            }
        }
    }
    IntpControlFlow::Val(Value::Int(val))
}

macro_rules! impl_op {
    ($op_fname:ident => ($op_symbol:tt, $op_obj_fname:literal)) => {
        impl Value {
            pub fn $op_fname(self, rhs: Value, interpreter: &mut Interpreter) -> IntpControlFlow {
                match (self, rhs) {
                    (Value::None, _) | (_, Value::None) => IntpControlFlow::Ret(Value::None),
                    (Value::Float(f1), Value::Float(f2)) =>
                        IntpControlFlow::Val(Value::Float(f1 $op_symbol f2)),
                    (Value::Float(f), Value::Int(i)) | (Value::Int(i), Value::Float(f)) =>
                        IntpControlFlow::Val(Value::Float(f $op_symbol i as f32)),
                    (Value::Int(i1), Value::Int(i2)) => {
                        concat_idents::concat_idents!(wrapping_fn = wrapping_, $op_fname {
                            IntpControlFlow::Val(Value::Int(i1.wrapping_fn(i2)))
                        })
                    }
                    (Value::Obj(o), rhs) => {
                        match o.borrow().get_field($op_obj_fname) {
                            Some(Value::Fn(f)) => {
                                f.call(vec![Value::Obj(o.clone()), rhs], interpreter)
                            }
                            _ => IntpControlFlow::Ret(Value::None),
                        }
                    }
                    (_, _) => IntpControlFlow::Ret(Value::None),
                }
            }
        }
    };

    ($op_fname:ident -> bool => ($op_symbol:tt, $op_obj_fname:literal)) => {
        impl Value {
            pub fn $op_fname(self, rhs: Value, interpreter: &mut Interpreter) -> IntpControlFlow {
                match (self, rhs) {
                    (Value::None, _) | (_, Value::None) => IntpControlFlow::Ret(Value::None),
                    (Value::Float(f1), Value::Float(f2)) => {
                        IntpControlFlow::Val(Value::Bool(f1 $op_symbol f2))
                    }
                    (Value::Float(f), Value::Int(i)) | (Value::Int(i), Value::Float(f)) =>
                        IntpControlFlow::Val(Value::Bool(f $op_symbol i as f32)),
                    (Value::Int(i1), Value::Int(i2)) => {
                        IntpControlFlow::Val(Value::Bool(i1 $op_symbol i2))
                    }
                    (Value::Obj(o), other) | (other, Value::Obj(o)) => {
                        match o.borrow().get_field($op_obj_fname) {
                            Some(Value::Fn(f)) => {
                                f.call(vec![Value::Obj(o.clone()), other], interpreter)
                            }
                            _ => IntpControlFlow::Ret(Value::None),
                        }
                    }
                    (_, _) => IntpControlFlow::Ret(Value::None),
                }
            }
        }
    };
}

impl_op!(add => (+, "op_add"));
impl_op!(sub => (-, "op_sub"));
impl_op!(mul => (*, "op_mul"));
impl_op!(div => (/, "op_div"));
impl_op!(eq -> bool => (==, "op_eq"));

impl Value {
    pub fn neg(self, interpreter: &mut Interpreter) -> IntpControlFlow {
        match self {
            Value::Int(i) => IntpControlFlow::Val(Value::Int(-i)),
            Value::Float(f) => IntpControlFlow::Val(Value::Float(-f)),
            Value::Obj(o) => match o.borrow_mut().get_field("op_neg") {
                Some(Value::Fn(f)) => f.call(vec![Value::Obj(o.clone())], interpreter),
                _ => IntpControlFlow::Ret(Value::None),
            },
            _ => IntpControlFlow::Ret(Value::None),
        }
    }

    pub fn not(self, interpreter: &mut Interpreter) -> IntpControlFlow {
        match self {
            Value::Obj(o) => match o.borrow().get_field("op_not") {
                Some(Value::Fn(f)) => f.call(vec![Value::Obj(o.clone())], interpreter),
                _ => match o.borrow().get_field("is_truthy") {
                    Some(Value::Fn(f)) => f.call(vec![Value::Obj(o.clone())], interpreter),
                    _ => IntpControlFlow::Val(Value::Bool(true)),
                },
            },
            value => IntpControlFlow::Val(Value::Bool(!value.is_truthy())),
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
