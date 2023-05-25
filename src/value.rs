use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display},
    ops::{Add, Div, Mul, Neg, Sub},
    rc::Rc,
};

use crate::{interpreter::IntpControlFlow, types::Type};

#[derive(Debug)]
pub struct ClassData {
    funcs: HashMap<String, Rc<FunctionData>>,
}

pub type FunctionSignature = Box<dyn Fn()>;

pub struct FunctionData {
    args: Vec<String>,
    callback: FunctionSignature,
}

impl Debug for FunctionData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunctionData")
            .field("args", &self.args)
            .field("callback", &format!("{:p}", self.callback))
            .finish()
    }
}

#[derive(Debug)]
pub struct ObjData {
    class: Rc<ClassData>,
    fields: HashMap<String, Value>,
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    Float(f32),
    Str(Rc<String>),
    Bool(bool),
    Fn(Rc<FunctionData>),
    Class(Rc<ClassData>),
    Obj(Rc<RefCell<ObjData>>),
    None,
}

impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            Value::Int(_) => Type::Int,
            Value::Float(_) => Type::Float,
            Value::Str(_) => Type::Str,
            Value::Bool(_) => Type::Bool,
            Value::Fn(_) => todo!(),
            Value::Class(_) => todo!(),
            Value::Obj(_) => todo!(),
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
            Value::Fn(_) => todo!(),
            Value::Class(_) => todo!(),
            Value::Obj(_) => todo!(),
            Value::None => write!(f, "None"),
        }
    }
}
