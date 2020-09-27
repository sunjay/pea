mod tyir;
mod subst;
mod constraints;
mod infer;

use crate::{cgenir, nir, diagnostics::Diagnostics};

use constraints::ConstraintSet;
use infer::TyInfer;

/// Infers/checks the types in the given program
pub fn check_types(program: &nir::Program, diag: &Diagnostics) -> cgenir::Program {
    let mut constraints = ConstraintSet::default();

    let program = program.ty_infer(&mut constraints, diag);
    let subst = constraints.solve(diag);

    subst.apply(program)
}
