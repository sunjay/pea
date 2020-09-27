use thiserror::Error;
use ena::unify::UnifyValue;

use crate::nir;

#[derive(Debug, Clone, Error)]
pub enum UnifyError {
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Unit,
    Bool,
}

impl UnifyValue for Ty {
    type Error = UnifyError;

    fn unify_values(ty1: &Self, ty2: &Self) -> Result<Self, Self::Error> {
        todo!()
    }
}

impl From<&nir::Ty> for Ty {
    fn from(ty: &nir::Ty) -> Self {
        todo!()
    }
}
