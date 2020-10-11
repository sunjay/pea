#![deny(unused_must_use)]

mod debug_log;

pub mod diagnostics;
pub mod source_files;
pub mod ast;
pub mod parser;
pub mod nir;
pub mod resolve;
pub mod ty;
pub mod cgenir;
pub mod tycheck;

pub mod gc;
pub mod prim;
pub mod value;
pub mod bytecode;
pub mod codegen;
pub mod interpreter;
pub mod package;
pub mod prelude;

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
    let mut packages = package::Packages::default();

    let mut consts = bytecode::Constants::default();
    let (prelude, prim_methods) = prelude::populate(packages.add_package(), &mut consts);

    let root_module = {
        // New scope because we want to drop this lock guard as soon as possible
        let files = source_files.read();
        let mod_name = files.mod_name(root_file);
        let tokens = parser::collect_tokens(files.source(root_file), diag);
        check_errors!(diag);
        parser::parse_module(mod_name.clone(), &tokens, diag)
    };
    check_errors!(diag);

    let pkg_id = packages.add_package();
    let program = resolve::NameResolver::resolve(pkg_id, &root_module, &prelude.root_module, diag);
    check_errors!(diag);

    let program = tycheck::check_types(&program, &prelude, &prim_methods, diag);
    check_errors!(diag);

    let def_consts = codegen::Compiler::compile(&program, &mut consts, &prelude, diag);
    check_errors!(diag);

    let mut interpreter = Interpreter::new(consts, diag.source_files().clone());
    call_main(&mut interpreter, &program, &def_consts, &diag);
    check_errors!(diag);

    Ok(interpreter)
}

/// Calls the `main` function
fn call_main(
    interpreter: &mut Interpreter,
    program: &cgenir::Program,
    def_consts: &package::DefConsts,
    diag: &Diagnostics,
) {
    // `main` must be declared in the root module, take zero arguments, and return `()`.
    let main_const_id = program.root_module.scope.lookup("main")
        .and_then(|def_id| def_consts.get(def_id));

    match main_const_id {
        Some(index) => {
            //TODO: Check that `main` is a function with zero arguments and returns `()`
            interpreter.call_main(index);
        },

        None => {
            diag.error("`main` function not found").emit();
        },
    }
}
