//! Type inference algorithm

use crate::{diagnostics::Diagnostics, nir};

use super::{constraints::ConstraintSet, tyir};

pub trait TyInfer {
    type Output;

    fn ty_infer(&self, constraints: &mut ConstraintSet, diag: &Diagnostics) -> Self::Output;
}

impl TyInfer for nir::Program {
    type Output = tyir::Program;

    fn ty_infer(&self, _constraints: &mut ConstraintSet, _diag: &Diagnostics) -> Self::Output {
        todo!()
    }
}
