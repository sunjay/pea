use thiserror::Error;
use ena::unify::UnifyValue;

use crate::nir;

use super::constraints::TyVar;

#[derive(Debug, Clone, Error)]
pub enum UnifyError {
}

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
    /// A type variable
    TyVar(TyVar),
}

impl From<&nir::Ty> for Ty {
    fn from(ty: &nir::Ty) -> Self {
        match ty {
            nir::Ty::Unit(_) => Ty::Unit,
            nir::Ty::Bool(_) => Ty::Bool,
            nir::Ty::I64(_) => Ty::I64,
            nir::Ty::U8(_) => Ty::U8,
            nir::Ty::List(ty) => Ty::List(Box::new((&ty.item_ty).into())),
            nir::Ty::Func(ty) => Ty::Func(Box::new((&**ty).into())),
        }
    }
}

impl UnifyValue for Ty {
    type Error = UnifyError;

    fn unify_values(ty1: &Self, ty2: &Self) -> Result<Self, Self::Error> {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncTy {
    pub param_tys: Vec<Ty>,
    pub return_ty: Ty,
}

impl From<&nir::FuncTy> for FuncTy {
    fn from(ty: &nir::FuncTy) -> Self {
        let nir::FuncTy {
            fn_token: _,
            paren_open_token: _,
            param_tys,
            paren_close_token: _,
            return_ty,
        } = ty;

        let param_tys = param_tys.iter().map(|param| param.into()).collect();

        // Default return type when not specified is `()`
        let return_ty = return_ty.as_ref().map(|ty| {
            let nir::ReturnTy {right_arrow_token: _, ty} = ty;
            ty.into()
        }).unwrap_or(Ty::Unit);

        Self {param_tys, return_ty}
    }
}
