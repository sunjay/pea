#![deny(unused_must_use)]

use std::path::Path;
use std::sync::Arc;

use anyhow::Context;
use parking_lot::RwLock;
use termcolor::ColorChoice;

use pea::{
    diagnostics::Diagnostics,
    source_files::SourceFiles,
};

fn main() -> anyhow::Result<()> {
    let source_path = Path::new("samples/run-pass/print_num.pea");
    let color_choice = ColorChoice::Always;

    let source_files = Arc::new(RwLock::new(SourceFiles::default()));

    source_files.write().add_file(source_path)
        .with_context(|| format!("Failed to read `{}`", source_path.display()))?;

    let diag = Diagnostics::new(source_files, color_choice);

    diag.error("test").emit();
    diag.warning("test").emit();
    diag.info("test").emit();
    diag.note("test").emit();
    diag.help("test").emit();

    Ok(())
}
