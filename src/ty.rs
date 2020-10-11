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

pub trait PrimTy {
    /// Generates the type for a rust primitive type
    fn ty() -> Ty;
}

impl PrimTy for () {
    fn ty() -> Ty {
        Ty::Unit
    }
}

impl PrimTy for bool {
    fn ty() -> Ty {
        Ty::Bool
    }
}

impl PrimTy for i64 {
    fn ty() -> Ty {
        Ty::I64
    }
}

impl PrimTy for u8 {
    fn ty() -> Ty {
        Ty::U8
    }
}

impl<T: PrimTy> PrimTy for &[T] {
    fn ty() -> Ty {
        Ty::List(Box::new(T::ty()))
    }
}

impl<F: PrimFuncTy> PrimTy for F {
    fn ty() -> Ty {
        Ty::Func(Box::new(F::func_ty()))
    }
}

pub trait PrimFuncTy {
    fn func_ty() -> FuncTy;
}

impl<Ret> PrimFuncTy for fn() -> Ret
    where Ret: PrimTy,
{
    fn func_ty() -> FuncTy {
        FuncTy {
            param_tys: Vec::new(),
            return_ty: Ret::ty(),
        }
    }
}

macro_rules! impl_prim_func_ty {
    ($arg0:ident, $($arg:ident),*) => {
        impl<$arg0, $($arg),*, Ret> PrimFuncTy for fn($arg0, $($arg),*) -> Ret
            where $arg0: PrimTy,
                  $($arg: PrimTy,)*
                  Ret: PrimTy,
        {
            fn func_ty() -> FuncTy {
                FuncTy {
                    param_tys: vec![
                        $arg0::ty(),
                        $($arg::ty()),*
                    ],

                    return_ty: Ret::ty(),
                }
            }
        }
    };
}

impl_prim_func_ty!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z);
