use std::{
    any::Any,
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{
    ast::{id, Ident},
    iir::{
        visitor::{IIRExprVisitor, IIRStmtVisitor},
        IIRExpr, IIRStmt,
    },
    interpreter::{Interpreter, IntpControlFlow},
    types::Type,
};

#[derive(Debug)]
pub struct ObjData {
    name: Ident,
    supers: Vec<Rc<RefCell<ObjData>>>,
    fields: HashMap<Ident, Value>,
}

impl ObjData {
    pub fn new(
        name: Ident,
        supers: Vec<Rc<RefCell<ObjData>>>,
        fields: HashMap<Ident, Value>,
    ) -> Self {
        Self {
            name,
            supers,
            fields,
        }
    }

    pub fn get_field(&self, ident: Ident) -> Option<Value> {
        match self.fields.get(&ident) {
            Some(field) => Some(field.clone()),
            None => {
                for sup in &self.supers {
                    match sup.borrow().get_field(ident.clone()) {
                        Some(field) => return Some(field),
                        None => {}
                    }
                }
                None
            }
        }
    }

    pub fn set_field(&mut self, ident: Ident, value: Value) -> Option<Value> {
        match self.fields.get_key_value(&ident) {
            Some((k, v)) => {
                let k = k.clone();
                if v.get_type() == Type::None {
                    self.fields.remove(&k)
                } else if v.get_type() != value.get_type() {
                    self.fields.remove(&k);
                    self.fields.insert(id(value.apply_suffix(&k)), value)
                } else {
                    self.fields.insert(ident.clone(), value)
                }
            }
            None => {
                for sup in &self.supers {
                    match sup.borrow_mut().set_field(ident.clone(), value.clone()) {
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
    args: Vec<Ident>,
    callback: FunctionSignature,
}

impl FunctionData {
    pub fn new(args: Vec<Ident>, stmts: Vec<IIRStmt>, ending_expr: Option<IIRExpr>) -> Self {
        let callback = Box::new(move |interpreter: &mut Interpreter| {
            for stmt in &stmts {
                match interpreter.visit_stmt(stmt) {
                    IntpControlFlow::Ret(v) => return v,
                    IntpControlFlow::Val(_) => (),
                    IntpControlFlow::Brk(_) => panic!("tried to break outside of loop"),
                }
            }
            if let Some(expr) = ending_expr.as_ref() {
                match interpreter.visit_expr(expr) {
                    IntpControlFlow::Ret(v) | IntpControlFlow::Val(v) => return v,
                    IntpControlFlow::Brk(_) => panic!("tried to break outside of loop"),
                }
            }
            Value::None
        });
        Self { args, callback }
    }

    pub fn new_raw(args: Vec<Ident>, callback: FunctionSignature) -> Self {
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

pub trait RustValue: Any + Debug {
    fn suffix(&self) -> &str;
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    Float(f32),
    Str(Rc<String>),
    Bool(bool),
    Fn(Rc<FunctionData>),
    List(Rc<RefCell<Vec<Value>>>),
    Obj(Rc<RefCell<ObjData>>),
    RustValue(Rc<dyn RustValue>),
    None,
}

impl Value {
    pub const TRUE: Value = Value::Bool(true);
    pub const FALSE: Value = Value::Bool(false);

    pub fn new_fn(args: Vec<Ident>, stmts: Vec<IIRStmt>, ending_expr: Option<IIRExpr>) -> Self {
        Self::Fn(Rc::new(FunctionData::new(args, stmts, ending_expr)))
    }

    pub fn new_list(values: Vec<Value>) -> Self {
        Self::List(Rc::new(RefCell::new(values)))
    }

    pub fn new_str(s: String) -> Self {
        Self::Str(Rc::new(s))
    }

    pub fn new_fn_raw(
        args: Vec<Ident>,
        callback: impl Fn(&mut Interpreter) -> Value + 'static,
    ) -> Self {
        Self::Fn(Rc::new(FunctionData::new_raw(args, Box::new(callback))))
    }

    pub fn new_obj(
        name: Ident,
        supers: Vec<Rc<RefCell<ObjData>>>,
        fields: HashMap<Ident, Value>,
    ) -> Self {
        Self::Obj(Rc::new(RefCell::new(ObjData::new(name, supers, fields))))
    }

    pub fn apply_suffix(&self, s: &str) -> String {
        let mut s = s.to_string();
        match self {
            Value::Int(_) => s += "_int",
            Value::Float(_) => s += "_float",
            Value::Str(_) => s += "_str",
            Value::Bool(_) => s += "_bool",
            Value::Fn(_) => s += "_fn",
            Value::Obj(o) => s.extend(["_", &o.borrow().name]),
            Value::List(_) => s += "_list",
            Value::None => s += "_none",
            Value::RustValue(rv) => s.extend(["_", rv.suffix()]),
        };
        s
    }

    pub fn get_type(&self) -> Type {
        match self {
            Value::Int(_) => Type::Int,
            Value::Float(_) => Type::Float,
            Value::Str(_) => Type::Str,
            Value::Bool(_) => Type::Bool,
            Value::Fn(_) => Type::Fn,
            Value::Obj(_) => Type::Obj,
            Value::List(_) => Type::List,
            Value::RustValue(_) => Type::Rv,
            Value::None => Type::None,
        }
    }

    pub fn index_by(&mut self, index: Value, interpreter: &mut Interpreter) -> IntpControlFlow {
        match (self, index) {
            (Value::List(li), index) => {
                let index = match index.to_int(interpreter) {
                    v @ IntpControlFlow::Ret(_) => return v,
                    v @ IntpControlFlow::Brk(_) => return v,
                    IntpControlFlow::Val(v) => match v {
                        Value::Int(i) => i as usize,
                        _ => return IntpControlFlow::Ret(Value::None),
                    },
                };
                match li.borrow().get(index) {
                    Some(v) => IntpControlFlow::Val(v.clone()),
                    None => IntpControlFlow::Ret(Value::None),
                }
            }
            (Value::Obj(obj), index) => {
                let f = match obj.borrow().get_field(id(String::from("index_by"))) {
                    Some(Value::Fn(f)) => f,
                    _ => return IntpControlFlow::Ret(Value::None),
                };
                f.call(vec![Value::Obj(obj.clone()), index], interpreter)
            }
            _ => IntpControlFlow::Ret(Value::None),
        }
    }

    pub fn index_by_set(
        &mut self,
        index: Value,
        to: Value,
        interpreter: &mut Interpreter,
    ) -> IntpControlFlow {
        match (self, index) {
            (Value::List(li), index) => {
                let index = match index.to_int(interpreter) {
                    v @ IntpControlFlow::Ret(_) => return v,
                    v @ IntpControlFlow::Brk(_) => return v,
                    IntpControlFlow::Val(v) => match v {
                        Value::Int(i) => i as usize,
                        _ => return IntpControlFlow::Ret(Value::None),
                    },
                };
                let mut li_borrow = li.borrow_mut();
                match li_borrow.get_mut(index) {
                    Some(v) => *v = to,
                    None => return IntpControlFlow::Ret(Value::None),
                };
                IntpControlFlow::Val(Value::None)
            }
            (Value::Obj(obj), index) => {
                let f = match obj.borrow().get_field(id("index_by_set")) {
                    Some(Value::Fn(f)) => f,
                    _ => return IntpControlFlow::Ret(Value::None),
                };
                f.call(vec![Value::Obj(obj.clone()), index, to], interpreter)
            }
            _ => IntpControlFlow::Ret(Value::None),
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
            Value::Obj(o) => match o.borrow().get_field(id("to_int")) {
                Some(Value::Fn(f)) => f.call(vec![Value::Obj(o.clone())], interpreter),
                _ => IntpControlFlow::Ret(Value::None),
            },
            _ => IntpControlFlow::Val(Value::Int(0)),
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
                    v @ IntpControlFlow::Brk(_) => return v,
                    IntpControlFlow::Val(v) => v,
                }
                .greater_than(other, interpreter)
            }
            (Value::List(l1), Value::List(l2)) => {
                IntpControlFlow::Val(Value::Bool(l1.borrow().len() > l2.borrow().len()))
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
                        match o.borrow().get_field(id($op_obj_fname)) {
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
                        match o.borrow().get_field(id($op_obj_fname)) {
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

impl Value {
    pub fn add(self, rhs: Value, interpreter: &mut Interpreter) -> IntpControlFlow {
        match (self, rhs) {
            (Value::None, _) => IntpControlFlow::Ret(Value::None),
            (Value::Float(f1), Value::Float(f2)) => IntpControlFlow::Val(Value::Float(f1 + f2)),
            (Value::Float(f), Value::Int(i)) | (Value::Int(i), Value::Float(f)) => {
                IntpControlFlow::Val(Value::Float(f + i as f32))
            }
            (Value::Int(i1), Value::Int(i2)) => {
                concat_idents::concat_idents!(wrapping_fn = wrapping_,add {
                  IntpControlFlow::Val(Value::Int(i1.wrapping_fn(i2)))
                })
            }
            (Value::Obj(o), rhs) => match o.borrow().get_field(id("op_add")) {
                Some(Value::Fn(f)) => f.call(vec![Value::Obj(o.clone()), rhs], interpreter),
                _ => IntpControlFlow::Ret(Value::None),
            },
            (Value::Str(s1), Value::Str(s2)) => {
                let mut s = (&*s1).to_owned();
                s += &*s2;
                IntpControlFlow::Val(Value::new_str(s))
            }
            (Value::Str(s1), rhs) => {
                let mut s = (&*s1).to_owned();
                s += &format!("{rhs}");
                IntpControlFlow::Val(Value::new_str(s))
            }
            (_, _) => IntpControlFlow::Ret(Value::None),
        }
    }
}
impl_op!(sub => (-, "op_sub"));
impl_op!(mul => (*, "op_mul"));
impl_op!(div => (/, "op_div"));
impl_op!(eq -> bool => (==, "op_eq"));

impl Value {
    pub fn neg(self, interpreter: &mut Interpreter) -> IntpControlFlow {
        match self {
            Value::Int(i) => IntpControlFlow::Val(Value::Int(-i)),
            Value::Float(f) => IntpControlFlow::Val(Value::Float(-f)),
            Value::Obj(o) => match o.borrow_mut().get_field(id("op_neg")) {
                Some(Value::Fn(f)) => f.call(vec![Value::Obj(o.clone())], interpreter),
                _ => IntpControlFlow::Ret(Value::None),
            },
            _ => IntpControlFlow::Ret(Value::None),
        }
    }

    pub fn not(self, interpreter: &mut Interpreter) -> IntpControlFlow {
        match self {
            Value::Obj(o) => match o.borrow().get_field(id("op_not")) {
                Some(Value::Fn(f)) => f.call(vec![Value::Obj(o.clone())], interpreter),
                _ => match o.borrow().get_field(id("is_truthy")) {
                    Some(Value::Fn(f)) => f.call(vec![Value::Obj(o.clone())], interpreter),
                    _ => IntpControlFlow::Val(Value::TRUE),
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
            Value::List(l) => {
                write!(f, "[")?;
                let l_borrow = l.borrow();
                let mut values = l_borrow.iter();
                if let Some(value) = values.next() {
                    write!(f, "{value}")?;
                }
                for value in values {
                    write!(f, ", {value}")?;
                }
                write!(f, "]")
            }
            Value::None => write!(f, "None"),
            Value::RustValue(rv) => write!(f, "<rv {:p}>", rv),
        }
    }
}
