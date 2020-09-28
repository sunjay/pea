use std::collections::HashSet;

use ena::unify::{UnifyKey, InPlaceUnificationTable};

use crate::diagnostics::Diagnostics;

use super::{subst::Subst, ty::{Ty, UnifyError}};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyVar(u32);

impl UnifyKey for TyVar {
    /// None - type has not been determined yet
    /// Some(Ty) - the type that has been resolved so far for this variable
    type Value = Option<Ty>;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(id: u32) -> Self {
        TyVar(id)
    }

    fn tag() -> &'static str {
        "TyVar"
    }
}

#[derive(Debug, Default)]
pub struct ConstraintSet {
    /// The concrete types for each variable, collected along the way.
    ///
    /// A union-find implementation keeps track of which type variables are equal to each other
    /// and maintains that equivalence as values are updated.
    ty_var_table: InPlaceUnificationTable<TyVar>,
    /// The set of type variables which are allowed to default to `()` if ambiguous
    default_unit: HashSet<TyVar>,
}

impl ConstraintSet {
    /// Generates a fresh type variable and returns it
    pub fn fresh_type_var(&mut self) -> TyVar {
        self.ty_var_table.new_key(None)
    }

    /// Adds a constraint that asserts that the given type variable is the given type
    pub fn ty_var_is_ty(&mut self, ty_var: TyVar, ty: Ty) -> Result<(), UnifyError> {
        // HACK: We are actually unifying twice here, once for real, and once to please the API of ena
        let other_ty = self.ty_var_table.probe_value(ty_var);
        let ty = match other_ty {
            Some(other_ty) => ty.unify(other_ty, self)?,
            None => ty,
        };

        self.ty_var_table.union_value(ty_var, Some(ty));

        Ok(())
    }

    /// Adds a constraint that asserts that the given type variables unify
    pub fn ty_vars_unify(&mut self, ty_var1: TyVar, ty_var2: TyVar) -> Result<(), UnifyError> {
        // HACK: We are actually unifying twice here, once for real, and once to please the API of ena
        let ty1 = self.ty_var_table.probe_value(ty_var1);
        let ty2 = self.ty_var_table.probe_value(ty_var2);
        let ty = match (ty1, ty2) {
            (Some(ty1), Some(ty2)) => Some(ty1.unify(ty2, self)?),
            (Some(ty1), None) => Some(ty1),
            (None, Some(ty2)) => Some(ty2),
            (None, None) => None,
        };

        self.ty_var_table.union_value(ty_var1, ty);
        self.ty_var_table.union(ty_var1, ty_var2);

        Ok(())
    }

    /// Allows the given type variable to default to `()` if ambiguous
    pub fn ty_var_default_unit(&mut self, ty_var: TyVar) {
        self.default_unit.insert(ty_var);
    }

    /// Solves this constraint set, and returns a substitution that can be used to map all type
    /// variables to concrete types
    pub fn solve(self, _diag: &Diagnostics) -> Subst {
        let Self {mut ty_var_table, default_unit: _} = self;
        //TODO: Call functions in a separate `solve` module to resolve remaining constraints

        (0..ty_var_table.len() as u32).map(|id| {
            let ty_var = TyVar(id);
            let ty = ty_var_table.probe_value(ty_var)
                .expect("bug: not all types were inferred into concrete types");
            (ty_var, ty)
        }).collect()
    }
}
