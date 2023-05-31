use std::{
    fmt::Display,
    num::{ParseFloatError, ParseIntError},
};

use crate::{lexer::Token, span::Span};

#[derive(Debug)]
pub struct LangError {
    data: LangErrorData,
    span: Span,
}

impl LangError {
    pub fn new(data: LangErrorData, span: impl Into<Span>) -> Self {
        Self {
            data,
            span: span.into(),
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

impl Display for LangError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Error: [{}]: {}", &self.span, &self.data))
    }
}

#[derive(Debug)]
pub enum LangErrorData {
    ExpectedToken(Token),
    ExpectedOneOfGot(Vec<Token>, Token),
    ExpectedTokenGot(Token, Token),
    ExpectedPrimaryGot(Token),
    ParseIntError(ParseIntError),
    ParseFloatError(ParseFloatError),
    UnboundedString,
    BadChar,
}

impl Display for LangErrorData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LangErrorData::ExpectedToken(tk) => write!(f, "expected {tk}"),
            LangErrorData::ExpectedOneOfGot(tks, got) => {
                write!(f, "expected ")?;
                let mut tks_iter = tks.iter();
                if let Some(tk) = tks_iter.next() {
                    write!(f, "{tk}")?;
                }
                for _ in 0..(tks.len().saturating_sub(2)) {
                    write!(f, ", {}", tks_iter.next().unwrap())?;
                }
                if let Some(tk) = tks_iter.next() {
                    write!(f, " or {tk}")?;
                }
                write!(f, ", got {got}")
            }
            LangErrorData::UnboundedString => write!(f, "unbounded string"),
            LangErrorData::ExpectedTokenGot(tk, got) => write!(f, "expected {tk}, got {got}"),
            LangErrorData::ExpectedPrimaryGot(got) => write!(f, "expected value, got {got}"),
            LangErrorData::ParseIntError(pir) => write!(f, "{pir}"),
            LangErrorData::ParseFloatError(pfr) => write!(f, "{pfr}"),
            LangErrorData::BadChar => write!(f, "unknown symbol"),
        }
    }
}
