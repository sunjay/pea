use super::tyir;

use crate::cgenir;

/// A mapping from type variable to concrete type
#[derive(Debug, Default)]
pub struct Subst {
}

impl Subst {
    pub fn apply(&self, _program: tyir::Program) -> cgenir::Program {
        todo!()
    }
}
