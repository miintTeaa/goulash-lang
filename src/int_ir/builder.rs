use std::rc::Rc;

use crate::{
    ast::{
        ops::UnaryOp,
        visitor::{ExprVisitor, StmtVisitor},
        Expr, ExprValue, Stmt,
    },
    error::LangError,
    int_ir::IIRStmtData,
    span::Span,
    value::Value,
};

use super::{IIRExpr, IIRExprData, IIRStmt};

pub struct IIRBuilder<'src> {
    src: &'src str,
}

impl<'src> IIRBuilder<'src> {
    pub fn new(src: &'src str) -> Self {
        Self { src }
    }
}

impl<'src> StmtVisitor<Result<IIRStmt, LangError>> for IIRBuilder<'src> {
    fn visit_error_stmt(&mut self, span: Span) -> Result<IIRStmt, LangError> {
        panic!("Tried to build IIR with error at {span}")
    }

    fn visit_expr_stmt(&mut self, expr: &Expr, span: Span) -> Result<IIRStmt, LangError> {
        let expr = self.visit_expr(expr)?;
        Ok(IIRStmt::new_expr(expr, span))
    }

    fn visit_print(&mut self, expr: &Expr, span: Span) -> Result<IIRStmt, LangError> {
        let expr = self.visit_expr(expr)?;
        Ok(IIRStmt::new_print(expr, span))
    }

    fn visit_let(
        &mut self,
        ident: Result<Span, Span>,
        expr: &Expr,
        span: Span,
    ) -> Result<IIRStmt, LangError> {
        let ident = match ident {
            Ok(sp) => sp,
            Err(sp) => panic!("Tried to build IIR with error at {sp}"),
        };
        Ok(IIRStmt::new(
            IIRStmtData::Let(ident, self.visit_expr(expr)?),
            span,
        ))
    }
}

impl<'src> ExprVisitor<Result<IIRExpr, LangError>> for IIRBuilder<'src> {
    fn visit_error_expr(&mut self, span: Span) -> Result<IIRExpr, LangError> {
        panic!("Tried to build IIR with error at {span}")
    }

    fn visit_var(&mut self, span: Span) -> Result<IIRExpr, LangError> {
        Ok(IIRExpr::new_var(span))
    }

    fn visit_literal(&mut self, kind: &ExprValue, span: Span) -> Result<IIRExpr, LangError> {
        let val = match kind {
            ExprValue::Int => Value::Int(
                self.src[span.range()]
                    .parse()
                    .map_err(|_| LangError::new_syntax("couldn't parse int", span))?,
            ),
            ExprValue::Float => Value::Float(
                self.src[span.range()]
                    .parse()
                    .map_err(|_| LangError::new_syntax("couldn't parse float", span))?,
            ),
            ExprValue::Str => Value::Str(Rc::new(parse_str(&self.src[span.range()]))),
            ExprValue::True => Value::Bool(true),
            ExprValue::False => Value::Bool(false),
            ExprValue::None => Value::None,
        };
        Ok(IIRExpr::new_const(val, span))
    }

    fn visit_unary_op(
        &mut self,
        op: UnaryOp,
        operand: &Expr,
        span: Span,
    ) -> Result<IIRExpr, LangError> {
        Ok(IIRExpr::new(
            IIRExprData::UnOp(op, Box::new(self.visit_expr(operand)?)),
            span,
        ))
    }

    fn visit_binary_op(
        &mut self,
        op: crate::ast::ops::BinaryOp,
        lhs: &Expr,
        rhs: &Expr,
        span: Span,
    ) -> Result<IIRExpr, LangError> {
        Ok(IIRExpr::new(
            IIRExprData::Op(
                op,
                Box::new(self.visit_expr(lhs)?),
                Box::new(self.visit_expr(rhs)?),
            ),
            span,
        ))
    }
}

fn parse_str(s: &str) -> String {
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

pub fn build<'src>(src: &'src str, stmts: Vec<Stmt>) -> Result<Vec<IIRStmt>, Vec<LangError>> {
    let mut builder = IIRBuilder::new(src);
    let mut errors = Vec::new();
    let mut iir = Vec::new();

    for stmt in stmts {
        match builder.visit_stmt(&stmt) {
            Ok(o) => iir.push(o),
            Err(e) => errors.push(e),
        }
    }

    if errors.is_empty() {
        Ok(iir)
    } else {
        Err(errors)
    }
}
