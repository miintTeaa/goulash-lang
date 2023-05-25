use std::{fmt::Display, num::ParseIntError};

use crate::span::Span;

pub struct LangErrorBuilder {
    kind: LangErrorKind,
    msg: Option<String>,
    span: Option<Span>,
}

impl LangErrorBuilder {
    pub fn new(kind: LangErrorKind) -> Self {
        Self {
            kind,
            msg: None,
            span: None,
        }
    }

    pub fn new_syntax() -> Self {
        Self::new(LangErrorKind::SyntaxError)
    }

    pub fn new_type() -> Self {
        Self::new(LangErrorKind::TypeError)
    }

    pub fn with_span(mut self, span: impl Into<Span>) -> Self {
        self.span = Some(span.into());
        self
    }

    pub fn with_msg(mut self, msg: impl Into<String>) -> Self {
        self.msg = Some(msg.into());
        self
    }

    pub fn build(self) -> Option<LangError> {
        Some(LangError::new(self.kind, self.msg?, self.span?))
    }
}

#[derive(Debug)]
pub struct LangError {
    kind: LangErrorKind,
    msg: String,
    span: Span,
}

impl LangError {
    pub fn new(kind: LangErrorKind, msg: impl Into<String>, span: impl Into<Span>) -> Self {
        Self {
            kind,
            msg: msg.into(),
            span: span.into(),
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn new_syntax(msg: impl Into<String>, span: impl Into<Span>) -> LangError {
        Self::new(LangErrorKind::SyntaxError, msg, span)
    }

    pub fn new_type(msg: impl Into<String>, span: impl Into<Span>) -> LangError {
        Self::new(LangErrorKind::TypeError, msg, span)
    }
}

impl From<ParseIntError> for LangErrorBuilder {
    fn from(_value: ParseIntError) -> Self {
        LangErrorBuilder::new_syntax().with_msg("bad integer")
    }
}

impl Display for LangError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}[{}]: {}",
            &self.kind, &self.span, &self.msg
        ))
    }
}

#[derive(Debug)]
pub enum LangErrorKind {
    SyntaxError,
    TypeError,
}

impl Display for LangErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            LangErrorKind::SyntaxError => "Syntax Error",
            LangErrorKind::TypeError => "Type Error",
        })
    }
}
