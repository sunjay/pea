use std::path::Path;

use anyhow::Context;
use pea::source_files::SourceFiles;

fn main() -> anyhow::Result<()> {
    let mut files = SourceFiles::default();

    let source_path = Path::new("samples/run-pass/print_num.pea");
    files.add_file(source_path)
        .with_context(|| format!("Failed to read `{}`", source_path.display()))?;

    println!("{:?}", files);

    Ok(())
}
