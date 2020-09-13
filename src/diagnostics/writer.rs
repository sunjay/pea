use std::io::{self, Write};

use termcolor::{StandardStream, ColorSpec, Color, WriteColor};

use crate::source_files::FilePos;

use super::Level;

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

#[cfg(test)]
pub struct BytesWriter {
    value: Vec<u8>,
}

#[cfg(test)]
impl BytesWriter {
    pub fn new(_color_choice: termcolor::ColorChoice) -> Self {
        BytesWriter {
            value: Vec::new(),
        }
    }

    /// Returns the entire value currently stored in the writer and resets it to an empty buffer
    pub fn drain(&mut self) -> Vec<u8> {
        use std::mem;

        mem::replace(&mut self.value, Vec::new())
    }
}

#[cfg(test)]
impl Write for BytesWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.value.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.value.flush()
    }
}

#[cfg(test)]
impl WriteColor for BytesWriter {
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
impl DiagnosticsWriter for BytesWriter {}
