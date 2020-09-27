use ena::unify::{UnifyKey, InPlaceUnificationTable};

use crate::{diagnostics::Diagnostics, ty::Ty};

use super::subst::Subst;

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
}

impl ConstraintSet {
    /// Generates a fresh type variable and returns it
    pub fn fresh_type_var(&mut self) -> TyVar {
        self.ty_var_table.new_key(None)
    }

    /// Solves this constraint set, and returns a substitution that can be used to map all type
    /// variables to concrete types
    pub fn solve(self, _diag: &Diagnostics) -> Subst {
        //TODO: Call functions in a separate `solve` module
        todo!()
    }
}
