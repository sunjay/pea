mod ty;
mod tyir;
mod subst;
mod constraints;
mod solver;
mod infer;

use crate::{cgenir, diagnostics::Diagnostics, nir, package::Package, prelude::PrimMethods};

/// Infers/checks the types in the given program
pub fn check_types(
    program: &nir::Program,
    prelude: &Package,
    prim_methods: &PrimMethods,
    diag: &Diagnostics,
) -> cgenir::Program {
    let mut ctx = infer::Context::new(prelude, prim_methods, &program.def_table, diag);

    let program = infer::infer_program(&mut ctx, program);
    let mut subst = ctx.into_constraints().solve(diag);

    subst.apply(program)
}
