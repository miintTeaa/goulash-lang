use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Int,
    Float,
    Str,
    Bool,
    Fn,
    None,
    // todo!(Class, Obj)
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Type::Int => "int",
            Type::Float => "float",
            Type::Str => "str",
            Type::Bool => "bool",
            Type::None => "none",
            Type::Fn => "fn",
        })
    }
}
