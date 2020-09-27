#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    /// The `()` type
    Unit,
    /// The `bool` type
    Bool,
    /// The `i64` type
    I64,
    /// The `u8` type
    U8,
    /// The `[T]` type
    List(Box<Ty>),
    /// The `fn(...) -> ...` type
    Func(Box<FuncTy>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncTy {
    pub param_tys: Vec<Ty>,
    pub return_ty: Ty,
}
