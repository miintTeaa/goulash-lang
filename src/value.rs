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

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;
        match self {
            Int(i) => write!(f, "{i}"),
            Float(fl) => write!(f, "{fl}"),
            Str(s) => write!(f, "{s}"),
            Bool(b) => write!(f, "{b}"),
            Fn(_) => todo!(),
            Class(_) => todo!(),
            Obj(_) => todo!(),
            None => write!(f, "None"),
        }
    }
}
