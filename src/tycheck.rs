mod ty;
mod tyir;
mod subst;
mod constraints;
mod solver;
mod infer;

use crate::{cgenir, nir, diagnostics::Diagnostics};

/// Infers/checks the types in the given program
pub fn check_types(program: &nir::Program, diag: &Diagnostics) -> cgenir::Program {
    let mut ctx = infer::Context::new(diag);

    let program = infer::infer_program(&mut ctx, program);
    let mut subst = ctx.constraints.solve(diag);

    subst.apply(program)
}
