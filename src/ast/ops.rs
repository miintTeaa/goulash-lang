use std::fmt::Display;

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Assign,
    Div,

    Or,
    And,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Assign => "=",
            BinaryOp::Div => "/",
            BinaryOp::Or => "or",
            BinaryOp::And => "and",
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Neg,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            UnaryOp::Neg => "-",
        })
    }
}
