#![deny(unused_must_use)]

use std::process;
use std::path::PathBuf;
use std::sync::Arc;
use std::str::FromStr;

use parking_lot::RwLock;
use termcolor::ColorChoice;
use structopt::StructOpt;

use pea::{
    gc,
    diagnostics::Diagnostics,
    source_files::SourceFiles,
    parser,
    codegen,
    interpreter::Status,
};

/// A command line argument that configures the coloring of the output
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ColorArg(pub ColorChoice);

impl Default for ColorArg {
    fn default() -> Self {
        ColorArg(ColorChoice::Auto)
    }
}

impl ColorArg {
    /// Allowed values the argument
    pub const VARIANTS: &'static [&'static str] = &["auto", "always", "ansi", "never"];
}

impl FromStr for ColorArg {
    type Err = &'static str;

    fn from_str(src: &str) -> Result<ColorArg, &'static str> {
        match src {
            _ if src.eq_ignore_ascii_case("auto") => Ok(ColorArg(ColorChoice::Auto)),
            _ if src.eq_ignore_ascii_case("always") => Ok(ColorArg(ColorChoice::Always)),
            _ if src.eq_ignore_ascii_case("ansi") => Ok(ColorArg(ColorChoice::AlwaysAnsi)),
            _ if src.eq_ignore_ascii_case("never") => Ok(ColorArg(ColorChoice::Never)),
            _ => Err("valid values: auto, always, ansi, never"),
        }
    }
}

impl Into<ColorChoice> for ColorArg {
    fn into(self) -> ColorChoice {
        self.0
    }
}

#[derive(Debug, StructOpt)]
#[structopt(name = "pea", about)]
struct Options {
    /// The program to compile and run
    #[structopt(name = "input", parse(from_os_str))]
    program_path: PathBuf,

    /// Configure coloring of output
    #[structopt(long = "color", parse(try_from_str), default_value = "auto",
        possible_values = ColorArg::VARIANTS, case_insensitive = true)]
    pub color: ColorArg,
}

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
    let Options {program_path, color} = Options::from_args();

    let source_files = Arc::new(RwLock::new(SourceFiles::default()));
    let diag = Diagnostics::new(source_files.clone(), color.into());

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

    let mut interpreter = codegen::Compiler::compile(&program, &diag);
    check_errors!(&diag);

    while interpreter.step() == Status::Running {}

    // Clean up any remaining memory allocated by the GC
    gc::sweep();
}
