#![deny(unused_must_use)]

use std::process;
use std::path::Path;
use std::sync::Arc;

use parking_lot::RwLock;
use termcolor::ColorChoice;

use pea::{
    diagnostics::Diagnostics,
    source_files::SourceFiles,
    parser,
    bytecode::Code,
    codegen::ToBytecode,
};

macro_rules! quit {
    ($diag:expr, $($args:tt)*) => {
        {
            $diag.error(format!($($args)*)).emit();
            process::exit(1);
        }
    };
}

macro_rules! check_errors {
    ($diag:expr) => {
        let diag = $diag;
        match diag.emitted_errors() {
            0 => {},
            1 => quit!(diag, "aborting due to 1 previous error"),
            errors => quit!(diag, "aborting due to {} previous errors", errors),
        }
    };
}

fn main() {
    let program_path = Path::new("samples/run-pass/print_num.pea");
    let color_choice = ColorChoice::Always;

    let source_files = Arc::new(RwLock::new(SourceFiles::default()));
    let diag = Diagnostics::new(source_files.clone(), color_choice);

    // Need this separate statement so we don't hold the write() lock in the
    // error case and end up with a deadlock
    let root_file = source_files.write().add_file(&program_path);
    let root_file = root_file.unwrap_or_else(|err| {
        quit!(&diag, "Could not read source file `{}`: {}", program_path.display(), err)
    });

    let program = {
        // New scope because we want to drop this lock guard as soon as possible
        let files = source_files.read();
        let tokens = parser::collect_tokens(files.source(root_file), &diag);
        check_errors!(&diag);
        parser::parse_program(&tokens, &diag)
    };
    check_errors!(&diag);

    let mut code = Code::default();
    program.write_bytecode(&mut code);
    println!("{:?}", code);
}
