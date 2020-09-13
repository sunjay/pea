use std::io::{self, Write};

use termcolor::{StandardStream, ColorSpec, Color, WriteColor};

use crate::source_files::FilePos;

use super::Level;

fn level_prefix(level: Level) -> (&'static str, Color) {
    use Level::*;
    match level {
        Error => ("error", Color::Red),
        Warning => ("warning", Color::Yellow),
        Info => ("info", Color::White),
        Note => ("note", Color::Green),
        Help => ("help", Color::Blue),
    }
}

pub trait DiagnosticsWriter: Write + WriteColor {
    fn write_diag(&mut self, level: Level, pos: Option<FilePos>, message: &str) -> io::Result<()> {
        if let Some(FilePos {path, start_line, start_offset, end_line, end_offset}) = pos {
            if start_line == end_line && start_offset == end_offset {
                write!(self, "[{}:{}:{}] ", path.display(), start_line, start_offset)?;
            } else {
                // end offset is always one past the end
                write!(self, "[{}:{}:{}-{}:{}] ", path.display(), start_line, start_offset, end_line, end_offset)?;
            }
        }

        let (prefix, prefix_color) = level_prefix(level);
        self.set_color(ColorSpec::new().set_fg(Some(prefix_color)).set_bold(true))?;
        write!(self, "{}: ", prefix)?;
        self.reset()?;

        writeln!(self, "{}", message)
    }
}

impl DiagnosticsWriter for StandardStream {}

#[cfg(test)]
pub struct NullWriter;

#[cfg(test)]
impl NullWriter {
    pub fn new(_color_choice: termcolor::ColorChoice) -> Self {
        // This impl exists to silence an unused parameter warning
        Self
    }
}

#[cfg(test)]
impl Write for NullWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[cfg(test)]
impl WriteColor for NullWriter {
    fn supports_color(&self) -> bool {
        false
    }

    fn set_color(&mut self, _: &ColorSpec) -> io::Result<()> {
        Ok(())
    }

    fn reset(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[cfg(test)]
impl DiagnosticsWriter for NullWriter {}
