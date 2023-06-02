use std::{
    fmt::Display,
    num::{ParseFloatError, ParseIntError},
};

use crate::span::Span;

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
    ParseIntError(ParseIntError),
    ParseFloatError(ParseFloatError),
}

impl Display for LangErrorData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LangErrorData::ParseIntError(pir) => write!(f, "{pir}"),
            LangErrorData::ParseFloatError(pfr) => write!(f, "{pfr}"),
        }
    }
}
