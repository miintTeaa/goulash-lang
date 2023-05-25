use std::rc::Rc;

use crate::{
    ast::{
        ops::{BinaryOp, UnaryOp},
        Expr, ExprData, ExprValueType,
    },
    error::LangError,
    span::Span,
    value::Value,
};

pub struct IIRExpr {
    pub span: Span,
    data: IIRExprData,
}

impl IIRExpr {
    pub fn new(data: IIRExprData, span: Span) -> Self {
        Self { span, data }
    }

    pub fn try_from(other: Expr, src: &str) -> Result<Self, LangError> {
        Ok(IIRExpr {
            span: other.span,
            data: match other.data {
                ExprData::Op(op, lhs, rhs) => IIRExprData::Op(
                    op,
                    Box::new(Self::try_from(*lhs, src)?),
                    Box::new(Self::try_from(*rhs, src)?),
                ),
                ExprData::UnOp(op, operand) => {
                    IIRExprData::UnOp(op, Box::new(Self::try_from(*operand, src)?))
                }
                ExprData::Lit(ExprValueType::Int) => IIRExprData::Const(Value::Int(
                    src[other.span.range()]
                        .parse()
                        .map_err(|_| LangError::new_syntax("couldn't parse int", other.span))?,
                )),
                ExprData::Lit(ExprValueType::Float) => IIRExprData::Const(Value::Float(
                    src[other.span.range()]
                        .parse()
                        .map_err(|_| LangError::new_syntax("couldn't parse float", other.span))?,
                )),
                ExprData::Lit(ExprValueType::Str) => {
                    IIRExprData::Const(Value::Str(Rc::new(make_string(&src[other.span.range()]))))
                }
                ExprData::Lit(ExprValueType::True) => IIRExprData::Const(Value::Bool(true)),
                ExprData::Lit(ExprValueType::False) => IIRExprData::Const(Value::Bool(false)),
                ExprData::Lit(ExprValueType::None) => IIRExprData::Const(Value::None),
                ExprData::Var => IIRExprData::Var,
                ExprData::Error => panic!("tried to convert ExprData::Error to IIRExprData"),
            },
        })
    }

    pub fn new_const(value: Value, span: Span) -> Self {
        Self {
            span,
            data: IIRExprData::Const(value),
        }
    }

    pub fn new_var(span: Span) -> Self {
        Self {
            span,
            data: IIRExprData::Var,
        }
    }

    pub fn data(&self) -> &IIRExprData {
        &self.data
    }
}

pub enum IIRExprData {
    Op(BinaryOp, Box<IIRExpr>, Box<IIRExpr>),
    UnOp(UnaryOp, Box<IIRExpr>),
    Const(Value),
    Var,
}

fn make_string(s: &str) -> String {
    let mut ret = String::new();
    let mut chars = s.chars();
    chars.next(); // skip first "

    loop {
        match chars.next().expect("should have hit end of string") {
            '\\' => match chars.next().expect("should have hit end of string") {
                'n' => ret.push('\n'),
                't' => ret.push('\t'),
                'r' => ret.push('\r'),
                next_c => ret.push(next_c),
            },
            '"' => {
                break;
            }
            c => {
                ret.push(c);
            }
        }
    }

    ret
}
