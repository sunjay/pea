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

fn main() {
    let Options {program_path, color} = Options::from_args();

    let source_files = Arc::new(RwLock::new(SourceFiles::default()));
    let diag = Diagnostics::new(source_files.clone(), color.into());

    let mut interpreter = pea::compile_path(program_path, source_files, &diag)
        .unwrap_or_else(|err| quit!(&diag, "{}", err));

    loop {
        match interpreter.step() {
            Ok(Status::Running) => {},
            Ok(Status::Complete) => break,
            Err(err) => {
                interpreter.print_call_stack();
                eprintln!("{}", err);
            },
        }
    }

    // Clean up any remaining memory allocated by the GC
    gc::sweep();
}
