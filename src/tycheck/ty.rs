use thiserror::Error;
use ena::unify::{NoError, UnifyValue};

use crate::nir;

use super::constraints::{ConstraintSet, TyVar};

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

impl From<&nir::FuncDecl> for Ty {
    fn from(ty: &nir::FuncDecl) -> Self {
        Ty::Func(Box::new(ty.into()))
    }
}

impl UnifyValue for Ty {
    type Error = NoError;

    fn unify_values(ty1: &Self, ty2: &Self) -> Result<Self, Self::Error> {
        use Ty::*;
        Ok(match (ty1, ty2) {
            (Unit, Unit) => Unit,
            (Bool, Bool) => Bool,
            (I64, I64) => I64,
            (U8, U8) => U8,

            (List(ty1), List(ty2)) => List(Box::new(Ty::unify_values(ty1, ty2)?)),
            (Func(ty1), Func(ty2)) => Func(Box::new(FuncTy::unify_values(ty1, ty2)?)),

            // Could return either one
            (&TyVar(ty_var), TyVar(_)) => {
                TyVar(ty_var)
            },

            (TyVar(_), ty) |
            (ty, TyVar(_)) => {
                ty.clone()
            },

            // Mismatched types
            _ => unreachable!("bug: mismatched types should already have been caught"),
        })
    }
}

impl Ty {
    /// Recursively unify this type with the given other type. The constraints set will be updated
    /// whenever type variables are equated with other types or variables.
    pub fn unify(self, other: Self, constraints: &mut ConstraintSet) -> Result<Self, UnifyError> {
        use Ty::*;
        Ok(match (self, other) {
            (Unit, Unit) => Unit,
            (Bool, Bool) => Bool,
            (I64, I64) => I64,
            (U8, U8) => U8,

            (List(ty1), List(ty2)) => List(Box::new(ty1.unify(*ty2, constraints)?)),
            (Func(ty1), Func(ty2)) => Func(Box::new(ty1.unify(*ty2, constraints)?)),

            (TyVar(ty_var1), TyVar(ty_var2)) => {
                constraints.ty_vars_unify(ty_var1, ty_var2)?;
                // Could return either one
                TyVar(ty_var1)
            },
            (TyVar(ty_var), ty) |
            (ty, TyVar(ty_var)) => {
                constraints.ty_var_is_ty(ty_var, ty.clone())?;
                ty
            },

            // Mismatched types
            _ => todo!(),
        })
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

impl From<&nir::FuncDecl> for FuncTy {
    fn from(ty: &nir::FuncDecl) -> Self {
        let nir::FuncDecl {
            fn_token: _,
            name: _,
            paren_open_token: _,
            params,
            paren_close_token: _,
            return_ty,
            body: _,
            scope: _,
        } = ty;

        let param_tys = params.iter()
            .map(|param| (&param.ty).into())
            .collect();

        // Default return type when not specified is `()`
        let return_ty = return_ty.as_ref().map(|ty| {
            let nir::ReturnTy {right_arrow_token: _, ty} = ty;
            ty.into()
        }).unwrap_or(Ty::Unit);

        Self {param_tys, return_ty}
    }
}

impl UnifyValue for FuncTy {
    type Error = NoError;

    fn unify_values(ty1: &Self, ty2: &Self) -> Result<Self, Self::Error> {
        assert_eq!(ty1.param_tys.len(), ty2.param_tys.len(),
            "bug: should have already checked arity");

        let param_tys = ty1.param_tys.iter()
            .zip(ty2.param_tys.iter())
            .map(|(ty1, ty2)| Ty::unify_values(ty1, ty2))
            .collect::<Result<Vec<_>, _>>()?;

        let return_ty = Ty::unify_values(&ty1.return_ty, &ty2.return_ty)?;

        Ok(Self {param_tys, return_ty})
    }
}

impl FuncTy {
    /// Recursively unify this type with the given other type. The constraints set will be updated
    /// whenever type variables are equated with other types or variables.
    pub fn unify(self, other: Self, constraints: &mut ConstraintSet) -> Result<Self, UnifyError> {
        if self.param_tys.len() != other.param_tys.len() {
            todo!()
        }

        let param_tys = self.param_tys.into_iter()
            .zip(other.param_tys)
            .map(|(ty1, ty2)| ty1.unify(ty2, constraints))
            .collect::<Result<Vec<_>, _>>()?;

        let return_ty = self.return_ty.unify(other.return_ty, constraints)?;

        Ok(Self {param_tys, return_ty})
    }
}
