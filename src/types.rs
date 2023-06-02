#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Int,
    Float,
    Str,
    Bool,
    Fn,
    Obj,
    List,
    None,
}
