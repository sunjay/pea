use std::fs;
use std::env;
use std::ffi::OsStr;
use std::path::Path;
use std::sync::Arc;

use termcolor::ColorChoice;
use parking_lot::RwLock;

use pea::{
    source_files::SourceFiles,
    diagnostics::Diagnostics,
};

#[cfg(not(miri))]
const EXEC_PATH: &str = env!("CARGO_BIN_EXE_pea");

#[test]
fn integration_tests() {
    run_pass();

    #[cfg(not(miri))]
    ui();
}

fn run_pass() {
    // Pass the environment variable TESTRUNNER=overwrite to overwrite the stderr files
    let overwrite_expected_output = env::var("TESTRUNNER")
        .map(|val| val == "overwrite")
        .unwrap_or(false);

    let tests_dir = Path::new("../tests/run-pass");

    let test_files = tests_dir.read_dir()
        .unwrap_or_else(|err| panic!("Failed to read test files directory '{}': {}", tests_dir.display(), err));
    for entry in test_files {
        let entry = entry.unwrap_or_else(|err| panic!("Failed to read directory entry in '{}': {}", tests_dir.display(), err));
        let entry_path = entry.path();
        if entry_path.is_dir() || entry_path.extension() != Some(OsStr::new("pea")) {
            return;
        }

        println!("[run-pass] Running {}", entry_path.display());
        match interpret(&entry_path) {
            Ok((exec_path, stdout)) => {
                // The assembler currently doesn't generate output on success.
                // If this changes later we should probably save that expected
                // output and incorporate it into these tests.
                assert!(stdout.is_empty(), "Assembler generated output despite success for '{}'", entry_path.display());

                let exec_meta = fs::metadata(&exec_path)
                    .unwrap_or_else(|err| panic!("Failed to read metadata for '{}': {}", exec_path.display(), err));
                assert!(exec_meta.len() > 0, "Generated executable for '{}' should be non-empty", entry_path.display());

                println!("[run-pass] Assembler succeeded for {}", entry_path.display());
            },
            Err(err) => panic!("Assembler failed for '{}'\n--- ERROR MESSAGE START --\n{}--- ERROR MESSAGE END ---\n", entry_path.display(), err),
        }
    }
}

fn ui() {
    // Pass the environment variable TESTRUNNER=overwrite to overwrite the stderr files
    let overwrite_expected_output = env::var("TESTRUNNER")
        .map(|val| val == "overwrite")
        .unwrap_or(false);

    let tests_dir = Path::new("../tests/ui");

    let test_files = tests_dir.read_dir()
        .unwrap_or_else(|err| panic!("Failed to read test files directory '{}': {}", tests_dir.display(), err));
    for entry in test_files {
        let entry = entry.unwrap_or_else(|err| panic!("Failed to read directory entry in '{}': {}", tests_dir.display(), err));
        let entry_path = entry.path();
        if entry_path.is_dir() || entry_path.extension() != Some(OsStr::new("pea")) {
            return;
        }

        println!("[ui] Running {}", entry_path.display());
        match interpret(&entry_path) {
            Ok(_) => {
                panic!("Interpreter should have produced errors for '{}'", entry_path.display());
            },
            Err(stderr) => {
                // Check the stderr output against what's expected
                let stderr_file = entry_path.with_extension("stderr");

                if overwrite_expected_output {
                    fs::write(&stderr_file, &stderr)
                        .unwrap_or_else(|err| panic!("Failed to write expected output to '{}': {}", stderr_file.display(), err));
                    return;
                }

                let expected_stderr = fs::read_to_string(&stderr_file)
                    .unwrap_or_else(|err| panic!("Failed to open '{}': {}", stderr_file.display(), err));

                if stderr != expected_stderr {
                    panic!("Error for '{}' did not match '{}'", entry_path.display(), stderr_file.display());
                }

                println!("[ui] Finished running {}", entry_path.display());
            },
        }
    }
}

struct Output {
    stdout: Vec<u8>,
    stderr: Vec<u8>,
}

fn interpret(source_path: &Path) -> Result<Output, Output> {
    let mut stdout = Vec::new();

    let source_files = Arc::new(RwLock::new(SourceFiles::default()));
    let diag = Diagnostics::new(source_files.clone(), ColorChoice::Never);

    let interpreter = match pea::compile_path(source_path, source_files, &diag) {
        Ok(interpreter) => interpreter,
        Err(err) => {
            diag.error(format!("{}", err)).emit();
            let stderr = diag.output_stream().drain();
            return Err(Output {stdout, stderr})
        },
    };

    // The path to the executable that will be generated
    // Using temp file so this is reliably cleaned up
    let executable = NamedTempFile::new()
        .unwrap_or_else(|err| panic!("Failed to created temporary file: {}", err));

    #[cfg(not(miri))]
    let output = Command::new(EXEC_PATH)
        .arg(source_path)
        .arg("--color=never")
        .arg("-o")
        .arg(executable.path())
        .output()
        .unwrap_or_else(|err| panic!("Failed to run assembler: {}", err));

    // Check if assembler failed
    if !output.status.success() {
        let stderr = String::from_utf8(output.stderr)
            .unwrap_or_else(|err| panic!("Assembler stderr for '{}' was not valid UTF-8: {}", executable.path().display(), err));
        return Err(stderr);
    }

    let stdout = String::from_utf8(output.stdout)
        .unwrap_or_else(|err| panic!("Assembler stdout for '{}' was not valid UTF-8: {}", executable.path().display(), err));

    Ok((executable.into_temp_path(), stdout))
}
