mod writer;
mod diagnostic;

pub use diagnostic::*;

use std::borrow::Cow;
use std::sync::atomic::{AtomicUsize, Ordering};

use parking_lot::{Mutex, MutexGuard};
use termcolor::ColorChoice;

use crate::source_files::{Span, SharedSourceFiles};

#[cfg(not(test))]
pub type OutputStream = termcolor::StandardStream;
#[cfg(test)]
pub type OutputStream = writer::NullWriter;

pub struct Diagnostics {
    source_files: SharedSourceFiles,
    /// The stream where diagnostics will be written to
    out: Mutex<OutputStream>,
    /// The number of errors that have been emitted
    errors: AtomicUsize,
}

impl Diagnostics {
    pub fn new(source_files: SharedSourceFiles, color_choice: ColorChoice) -> Self {
        Self {
            source_files,
            #[cfg(not(test))]
            out: Mutex::new(termcolor::StandardStream::stderr(color_choice)),
            #[cfg(test)]
            out: Mutex::new(writer::NullWriter::new(color_choice)),
            errors: AtomicUsize::default(),
        }
    }

    pub fn source_files(&self) -> &SharedSourceFiles {
        &self.source_files
    }

    /// Returns a mutable handle to the output stream
    pub fn output_stream(&self) -> MutexGuard<OutputStream> {
        self.out.lock()
    }

    /// Returns the number of errors that have been emitted
    pub fn emitted_errors(&self) -> usize {
        self.errors.load(Ordering::SeqCst)
    }

    pub fn error<'a>(&'a self, message: impl Into<Cow<'a, str>>) -> DiagnosticWriter<'a> {
        self.level(Level::Error, message)
    }

    pub fn warning<'a>(&'a self, message: impl Into<Cow<'a, str>>) -> DiagnosticWriter<'a> {
        self.level(Level::Warning, message)
    }

    pub fn info<'a>(&'a self, message: impl Into<Cow<'a, str>>) -> DiagnosticWriter<'a> {
        self.level(Level::Info, message)
    }

    pub fn note<'a>(&'a self, message: impl Into<Cow<'a, str>>) -> DiagnosticWriter<'a> {
        self.level(Level::Note, message)
    }

    pub fn help<'a>(&'a self, message: impl Into<Cow<'a, str>>) -> DiagnosticWriter<'a> {
        self.level(Level::Help, message)
    }

    pub fn level<'a>(&'a self, level: Level, message: impl Into<Cow<'a, str>>) -> DiagnosticWriter<'a> {
        self.diagnostic_writer(Diagnostic {
            title: Message {
                level,
                label: message.into(),
            },
            fragments: Vec::new(),
        })
    }

    pub fn span_error<'a>(&'a self, span: Span, message: impl Into<Cow<'a, str>>) -> DiagnosticWriter<'a> {
        self.span_level(Level::Error, span, message)
    }

    pub fn span_warning<'a>(&'a self, span: Span, message: impl Into<Cow<'a, str>>) -> DiagnosticWriter<'a> {
        self.span_level(Level::Warning, span, message)
    }

    pub fn span_level<'a>(&'a self, level: Level, span: Span, message: impl Into<Cow<'a, str>>) -> DiagnosticWriter<'a> {
        let message = message.into();

        self.level(level, message.clone())
            .span_level(level, span, message)
    }

    fn diagnostic_writer<'a>(&'a self, data: Diagnostic<'a>) -> DiagnosticWriter<'a> {
        DiagnosticWriter {
            source_files: self.source_files.read(),
            out: self.out.lock(),
            errors: &self.errors,
            data,
        }
    }
}
