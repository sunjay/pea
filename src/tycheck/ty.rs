use std::fmt;

use ena::unify::{NoError, UnifyValue};

use crate::{nir, source_files::Span, fmt_ctx::DisplayCtx, cwrite};

use super::constraints::TyVar;

#[derive(Debug, Clone)]
pub enum UnifyError {
    MismatchedTypes {
        ty1: Ty,
        ty2: Ty,
    },

    ArityMismatch {
        ty1: Ty,
        ty1_arity: usize,
        ty2: Ty,
        ty2_arity: usize,
    },
}

#[derive(Debug, Clone)]
pub enum Constraint {
    TyVarIsTy {ty_var: TyVar, ty: Ty},

    TyVarsUnify {ty_var1: TyVar, ty_var2: TyVar},
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TySpan {
    pub ty: Ty,
    /// The span associated with the type (if any)
    pub span: Option<Span>,
}

impl UnifyValue for TySpan {
    type Error = NoError;

    fn unify_values(ty1: &Self, ty2: &Self) -> Result<Self, Self::Error> {
        Ok(Self {
            ty: Ty::unify_values(&ty1.ty, &ty2.ty)?,
            // Could choose either span if both exist. Most important is that we keep at least one
            // span if either is set.
            span: ty2.span.or(ty1.span),
        })
    }
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
    /// A named type
    Named(nir::DefId),
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
            nir::Ty::Named(def) => Ty::Named(def.id),
        }
    }
}

impl From<&nir::FuncDecl> for Ty {
    fn from(ty: &nir::FuncDecl) -> Self {
        Ty::Func(Box::new(ty.into()))
    }
}

impl From<&crate::ty::Ty> for Ty {
    fn from(ty: &crate::ty::Ty) -> Self {
        match ty {
            crate::ty::Ty::Unit => Ty::Unit,
            crate::ty::Ty::Bool => Ty::Bool,
            crate::ty::Ty::I64 => Ty::I64,
            crate::ty::Ty::U8 => Ty::U8,
            crate::ty::Ty::List(ty) => Ty::List(Box::new((&**ty).into())),
            crate::ty::Ty::Func(ty) => Ty::Func(Box::new((&**ty).into())),
        }
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
    /// Recursively unify this type with the given other type.
    ///
    /// Collects substitutions/additional constraints found along the way so they can be applied
    /// afterwards (if the unification succeeds).
    pub fn unify(self, other: Self, constraints: &mut Vec<Constraint>) -> Result<Self, UnifyError> {
        use Ty::*;
        Ok(match (self, other) {
            (Unit, Unit) => Unit,
            (Bool, Bool) => Bool,
            (I64, I64) => I64,
            (U8, U8) => U8,

            (List(ty1), List(ty2)) => List(Box::new(ty1.unify(*ty2, constraints)?)),
            (Func(ty1), Func(ty2)) => Func(Box::new(ty1.unify(*ty2, constraints)?)),

            (TyVar(ty_var1), TyVar(ty_var2)) => {
                constraints.push(Constraint::TyVarsUnify {ty_var1, ty_var2});
                // Could return either one
                TyVar(ty_var1)
            },
            (TyVar(ty_var), ty) |
            (ty, TyVar(ty_var)) => {
                constraints.push(Constraint::TyVarIsTy {ty_var, ty: ty.clone()});
                ty
            },

            // Mismatched types
            (ty1, ty2) => {
                return Err(UnifyError::MismatchedTypes {ty1, ty2});
            },
        })
    }
}

impl DisplayCtx<nir::DefTable> for Ty {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &nir::DefTable) -> fmt::Result {
        use Ty::*;
        match self {
            Unit => cwrite!(f, ctx, "()"),
            Bool => cwrite!(f, ctx, "bool"),
            I64 => cwrite!(f, ctx, "i64"),
            U8 => cwrite!(f, ctx, "u8"),
            List(item_ty) => cwrite!(f, ctx, "[{}]", item_ty),
            Func(func) => cwrite!(f, ctx, "{}", func),
            &Named(def_id) => cwrite!(f, ctx, "{}", ctx.get(def_id).value),
            TyVar(_) => cwrite!(f, ctx, "_"),
        }
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

impl From<&crate::ty::FuncTy> for FuncTy {
    fn from(ty: &crate::ty::FuncTy) -> Self {
        let crate::ty::FuncTy {param_tys, return_ty} = ty;

        Self {
            param_tys: param_tys.iter().map(|ty| ty.into()).collect(),
            return_ty: return_ty.into(),
        }
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
    /// Recursively unify this type with the given other type.
    ///
    /// Collects substitutions/additional constraints found along the way so they can be applied
    /// afterwards (if the unification succeeds).
    pub fn unify(self, other: Self, constraints: &mut Vec<Constraint>) -> Result<Self, UnifyError> {
        if self.param_tys.len() != other.param_tys.len() {
            let ty1_arity = self.param_tys.len();
            let ty2_arity = other.param_tys.len();

            let ty1 = Ty::Func(Box::new(self));
            let ty2 = Ty::Func(Box::new(other));

            return Err(UnifyError::ArityMismatch {ty1, ty1_arity, ty2, ty2_arity});
        }

        let param_tys = self.param_tys.into_iter()
            .zip(other.param_tys)
            .map(|(ty1, ty2)| ty1.unify(ty2, constraints))
            .collect::<Result<Vec<_>, _>>()?;

        let return_ty = self.return_ty.unify(other.return_ty, constraints)?;

        Ok(Self {param_tys, return_ty})
    }
}

impl DisplayCtx<nir::DefTable> for FuncTy {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &nir::DefTable) -> fmt::Result {
        let Self {param_tys, return_ty} = self;

        cwrite!(f, ctx, "fn (")?;

        if let Some(param_ty) = param_tys.get(0) {
            cwrite!(f, ctx, "{}", param_ty)?;

            for param_ty in &param_tys[1..] {
                cwrite!(f, ctx, ", {}", param_ty)?;
            }
        }

        cwrite!(f, ctx, ")")?;

        if *return_ty != Ty::Unit {
            cwrite!(f, ctx, " -> {}", return_ty)?;
        }

        Ok(())
    }
}
