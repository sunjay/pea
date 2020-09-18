#![deny(unused_must_use)]

mod debug_log;

pub mod diagnostics;
pub mod source_files;
pub mod ast;
pub mod parser;
pub mod nir;
pub mod resolve;

pub mod gc;
pub mod prim;
pub mod value;
pub mod bytecode;
pub mod codegen;
pub mod interpreter;

use std::fmt;
use std::sync::Arc;
use std::path::Path;

use parking_lot::RwLock;
use thiserror::Error;

use crate::{
    diagnostics::Diagnostics,
    source_files::{FileHandle, SourceFiles},
    interpreter::Interpreter,
};

#[derive(Debug, Error)]
pub struct ErrorsEmitted {
    /// The number of errors that were emitted
    emitted_errors: usize,
}

impl fmt::Display for ErrorsEmitted {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.emitted_errors {
            1 => write!(f, "aborting due to 1 previous error"),
            errors => write!(f, "aborting due to {} previous errors", errors),
        }
    }
}

macro_rules! check_errors {
    ($diag:expr) => {
        let diag = $diag;
        match diag.emitted_errors() {
            0 => {},
            emitted_errors => return Err(ErrorsEmitted {emitted_errors}),
        }
    };
}

/// Compiles the file at the given path and returns an interpreter that can be used to run it
pub fn compile_path<P: AsRef<Path>>(
    path: P,
    source_files: Arc<RwLock<SourceFiles>>,
    diag: &Diagnostics,
) -> Result<Interpreter, ErrorsEmitted> {
    let path = path.as_ref();
    let root_file = source_files.write().add_file(path);
    // Separate line because we need to make sure the write() lock isn't still held
    let root_file = match root_file {
        Ok(root_file) => root_file,
        Err(err) => {
            diag.error(format!("Could not read source file `{}`: {}", path.display(), err)).emit();
            check_errors!(diag);
            unreachable!(); // An error just occurred
        },
    };

    compile(root_file, source_files, diag)
}

/// Compiles the given file and returns an interpreter that can be used to run it
pub fn compile(
    root_file: FileHandle,
    source_files: Arc<RwLock<SourceFiles>>,
    diag: &Diagnostics,
) -> Result<Interpreter, ErrorsEmitted> {
    let program = {
        // New scope because we want to drop this lock guard as soon as possible
        let files = source_files.read();
        let tokens = parser::collect_tokens(files.source(root_file), diag);
        check_errors!(diag);
        parser::parse_program(&tokens, diag)
    };
    check_errors!(diag);

    let (program, def_table) = resolve::NameResolver::resolve(&program, diag);
    check_errors!(diag);

    let interpreter = codegen::Compiler::compile(&program, &def_table, diag);
    check_errors!(diag);

    Ok(interpreter)
}
