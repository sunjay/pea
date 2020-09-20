use std::fmt;
use std::sync::Arc;

use crate::{bytecode::Bytecode, source_files::SourceFiles};

use crate::gc;

#[derive(Debug, Clone)]
pub struct Func {
    pub name: Arc<str>,
    pub code: Bytecode,
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}

impl Func {
    pub fn new(name: Arc<str>) -> Self {
        Self::with_code(name, Default::default())
    }

    pub fn with_code(name: Arc<str>, code: Bytecode) -> Self {
        Self {name, code}
    }

    /// Prints the annotated bytecode this function to stderr
    pub fn print_annotated_bytecode(&self, source_files: &SourceFiles) {
        todo!()
    }
}

impl gc::Trace for Func {
    fn trace(&self) {
        let Self {name: _, code: _} = self;
    }
}
