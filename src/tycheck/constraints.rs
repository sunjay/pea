use crate::diagnostics::Diagnostics;

use super::subst::Subst;

#[derive(Debug, Default)]
pub struct ConstraintSet {
}

impl ConstraintSet {
    /// Solves this constraint set, and returns a substitution that can be used to map all type
    /// variables to concrete types
    pub fn solve(self, _diag: &Diagnostics) -> Subst {
        todo!()
    }
}
