use crate::{
    ast::Stmt,
    error::{LangError, LangErrorData},
    lexer::{Lexer, Token},
    span::Span,
};

pub struct Parser<'src> {
    src: &'src str,
    lexer: Lexer<'src>,
    peeked: Token,
    errors: Vec<LangError>,
    ast: Vec<Stmt>,
    can_report: bool,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> Self {
        let mut lexer = Lexer::new(src);
        let errors = Vec::new();
        let peeked = lexer.next();

        Self {
            src,
            lexer,
            peeked,
            errors,
            can_report: false,
            ast: Vec::new(),
        }
    }

    pub fn peek(&self) -> Token {
        self.peeked
    }

    pub fn next(&mut self) -> Token {
        let ret = self.peeked;
        self.peeked = self.lexer.next();
        ret
    }

    pub fn consume(&mut self, token: Token) -> bool {
        if self.peek() == token {
            self.next();
            true
        } else {
            false
        }
    }

    pub fn span(&self) -> Span {
        self.lexer.span()
    }

    pub fn src(&self) -> &'src str {
        self.src
    }

    pub fn is_empty(&self) -> bool {
        self.peek() == Token::EOF
    }

    pub fn try_consume(&mut self, token: Token) -> Result<Span, LangError> {
        if self.peek() == token {
            let span = self.span();
            self.next();
            Ok(span)
        } else {
            Err(LangError::new(
                LangErrorData::ExpectedTokenGot(token, self.peek()),
                self.span(),
            ))
        }
    }

    pub fn disable_reporting(&mut self) {
        self.can_report = false;
    }

    pub fn enable_reporting(&mut self) {
        self.can_report = true;
    }

    pub fn report(&mut self, error: LangError) {
        if self.can_report {
            self.errors.push(error)
        };
    }

    pub fn finalize(self) -> (Vec<LangError>, Vec<Stmt>) {
        (self.errors, self.ast)
    }

    pub fn to_errors(mut self) -> Vec<LangError> {
        self.errors.extend(self.lexer.to_errors());
        self.errors
    }

    pub fn push_stmt(&mut self, stmt: Stmt) {
        self.ast.push(stmt)
    }

    pub fn pop_stmt(&mut self) -> Option<Stmt> {
        self.ast.pop()
    }
}
