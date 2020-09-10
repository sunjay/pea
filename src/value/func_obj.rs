use std::fmt;
use std::sync::Arc;

use crate::bytecode::Bytecode;

#[derive(Debug, Clone)]
pub struct FuncObj {
    pub name: Arc<str>,
    pub code: Bytecode,
}

impl fmt::Display for FuncObj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}

impl FuncObj {
    pub fn new(name: Arc<str>) -> Self {
        Self {
            name,
            code: Default::default(),
        }
    }
}
