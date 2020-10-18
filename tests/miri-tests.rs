#![cfg(miri)]

use std::ffi::OsStr;
use std::path::Path;

use termcolor::ColorChoice;
use parking_lot::RwLock;

use pea::{
    gc::{self, Gc},
    source_files::SourceFiles,
    diagnostics::Diagnostics,
    interpreter::Status,
};

#[test]
fn run_pass_miri() {
    let tests_dir = Path::new("tests/run-pass");

    let test_files = tests_dir.read_dir()
        .unwrap_or_else(|err| panic!("Failed to read test files directory '{}': {}", tests_dir.display(), err));
    for entry in test_files {
        let entry = entry.unwrap_or_else(|err| panic!("Failed to read directory entry in '{}': {}", tests_dir.display(), err));
        let entry_path = entry.path();
        if entry_path.is_dir() || entry_path.extension() != Some(OsStr::new("pea")) {
            continue;
        }

        println!("[run-pass] Running {}", entry_path.display());

        let source_files = Gc::new(RwLock::new(SourceFiles::default()));
        let diag = Diagnostics::new(source_files.clone(), ColorChoice::Never);

        let mut interpreter = match pea::compile_path(&entry_path, source_files, &diag) {
            Ok(interpreter) => interpreter,
            Err(err) => {
                diag.error(format!("{}", err)).emit();
                panic!("Failed to compile {}", entry_path.display());
            },
        };

        loop {
            match interpreter.step() {
                Ok(Status::Running) => {},
                Ok(Status::Complete) => break,
                Err(err) => {
                    interpreter.print_call_stack();
                    eprintln!("error: {}", err);
                    break;
                },
            }
        }

        // Clean up any remaining memory allocated by the GC
        gc::sweep();
    }
}
