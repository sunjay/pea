#![cfg(not(miri))]

use std::fs;
use std::env;
use std::ffi::OsStr;
use std::path::Path;
use std::process::Command;

use rayon::prelude::*;

const EXEC_PATH: &str = env!("CARGO_BIN_EXE_pea");

struct Output {
    stdout: String,
    stderr: String,
}

#[test]
fn run_pass() {
    // Pass the environment variable TESTRUNNER=overwrite to overwrite the stderr files
    let overwrite_expected_output = env::var("TESTRUNNER")
        .map(|val| val == "overwrite")
        .unwrap_or(false);

    let tests_dir = Path::new("tests/run-pass");

    let test_files = tests_dir.read_dir()
        .unwrap_or_else(|err| panic!("Failed to read test files directory '{}': {}", tests_dir.display(), err));
    test_files.par_bridge().panic_fuse().for_each(|entry| {
        let entry = entry.unwrap_or_else(|err| panic!("Failed to read directory entry in '{}': {}", tests_dir.display(), err));
        let entry_path = entry.path();
        if entry_path.is_dir() || entry_path.extension() != Some(OsStr::new("pea")) {
            return;
        }

        println!("[run-pass] Running {}", entry_path.display());
        match run_interpreter(&entry_path) {
            Ok(output) => {
                check_output(&entry_path, output, overwrite_expected_output);

                println!("[run-pass] Interpreter succeeded for {}", entry_path.display());
            },

            Err(Output {stderr, ..}) => {
                panic!("Failed to run '{}'\n--- ERROR MESSAGE START --\n{}--- ERROR MESSAGE END ---\n", entry_path.display(), stderr);
            },
        }
    });
}

#[test]
fn ui() {
    // Pass the environment variable TESTRUNNER=overwrite to overwrite the stderr files
    let overwrite_expected_output = env::var("TESTRUNNER")
        .map(|val| val == "overwrite")
        .unwrap_or(false);

    let tests_dir = Path::new("tests/ui");

    let test_files = tests_dir.read_dir()
        .unwrap_or_else(|err| panic!("Failed to read test files directory '{}': {}", tests_dir.display(), err));
    test_files.par_bridge().panic_fuse().for_each(|entry| {
        let entry = entry.unwrap_or_else(|err| panic!("Failed to read directory entry in '{}': {}", tests_dir.display(), err));
        let entry_path = entry.path();
        if entry_path.is_dir() || entry_path.extension() != Some(OsStr::new("pea")) {
            return;
        }

        println!("[ui] Running {}", entry_path.display());
        match run_interpreter(&entry_path) {
            Ok(_) => {
                panic!("Interpreter should have produced errors for '{}'", entry_path.display());
            },
            Err(output) => {
                check_output(&entry_path, output, overwrite_expected_output);

                println!("[ui] Finished running {}", entry_path.display());
            },
        }
    });
}

fn run_interpreter(source_path: &Path) -> Result<Output, Output> {
    let output = Command::new(EXEC_PATH)
        .arg(source_path)
        .arg("--color=never")
        .output()
        .unwrap_or_else(|err| panic!("Failed to run: {}", err));

    let status = output.status;

    let stdout = String::from_utf8(output.stdout)
        .unwrap_or_else(|err| panic!("stdout for '{}' was not valid UTF-8: {}", source_path.display(), err));
    let stderr = String::from_utf8(output.stderr)
        .unwrap_or_else(|err| panic!("stderr for '{}' was not valid UTF-8: {}", source_path.display(), err));

    let output = Output {stdout, stderr};

    if status.success() {
        Ok(output)
    } else {
        Err(output)
    }
}

fn check_output(entry_path: &Path, output: Output, overwrite: bool) {
    let Output {stdout, stderr} = output;

    // Check the output against what's expected
    let stdout_file = entry_path.with_extension("stdout");
    let stderr_file = entry_path.with_extension("stderr");

    if overwrite {
        fs::write(&stdout_file, &stdout)
            .unwrap_or_else(|out| panic!("Failed to write expected output to '{}': {}", stdout_file.display(), out));
        fs::write(&stderr_file, &stderr)
            .unwrap_or_else(|err| panic!("Failed to write expected output to '{}': {}", stderr_file.display(), err));
        return;
    }

    let expected_stdout = fs::read_to_string(&stdout_file)
        .unwrap_or_else(|out| panic!("Failed to open '{}': {}", stdout_file.display(), out));
    let expected_stderr = fs::read_to_string(&stderr_file)
        .unwrap_or_else(|err| panic!("Failed to open '{}': {}", stderr_file.display(), err));

    if stdout != expected_stdout {
        panic!("Output for '{}' did not match '{}'", entry_path.display(), stdout_file.display());
    }
    if stderr != expected_stderr {
        panic!("Error for '{}' did not match '{}'", entry_path.display(), stderr_file.display());
    }
}
