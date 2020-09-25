mod span;
mod file_source;
mod line_numbers;

pub use span::*;
pub use file_source::*;

use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};

use line_numbers::LineNumbers;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileHandle {
    /// The index of the first byte of the file in `SourceFiles::source`
    start: usize,
    /// The number of bytes in the file
    len: usize,
}

#[derive(Debug)]
struct File {
    path: PathBuf,
    /// The index into `SourceFiles::source` that represents the start of this file
    start_offset: usize,
    /// An index of the line numbers for all offsets in the file
    line_numbers: LineNumbers,
    /// The handle to this file
    handle: FileHandle,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FilePos<'a> {
    pub path: &'a Path,
    pub start_line: usize,
    pub start_offset: usize,
    pub end_line: usize,
    pub end_offset: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileLine<'a> {
    pub path: &'a Path,
    pub line: usize,
    pub bytes: &'a [u8],
}

#[derive(Debug, Default)]
pub struct SourceFiles {
    /// The source code of all files concatenated together.
    ///
    /// This allows span indexes to be uniquely identifiable
    source: Vec<u8>,
    /// Metadata about each file stored in `source`
    ///
    /// Sorted by the offset
    files: Vec<File>,
}

impl SourceFiles {
    /// Reads a file and adds it to the set of source files. Returns a handle to that file's
    /// contents.
    pub fn add_file<P: AsRef<Path>>(&mut self, path: P) -> io::Result<FileHandle> {
        let path = path.as_ref();
        let start = self.source.len();

        let mut file = fs::File::open(path)?;
        file.read_to_end(&mut self.source)?;

        let len = self.source.len() - start;
        Ok(self.create_handle(path, start, len))
    }

    /// Adds the given source to the set of source files. Returns a handle to that file's
    /// contents.
    pub fn add_source<P: AsRef<Path>>(&mut self, path: P, source: &[u8]) -> FileHandle {
        let path = path.as_ref();
        let start = self.source.len();

        self.source.extend(source);

        let len = source.len();
        self.create_handle(path, start, len)
    }

    fn create_handle(&mut self, path: &Path, start: usize, len: usize) -> FileHandle {
        let handle = FileHandle {start, len};
        let source = self.source(handle);
        let line_numbers = LineNumbers::new(source);
        self.files.push(File {
            path: path.to_path_buf(),
            start_offset: start,
            line_numbers,
            handle,
        });
        handle
    }

    /// Returns the handle containing the given span
    pub fn handle(&self, span: Span) -> FileHandle {
        self.file(span.start).handle
    }

    /// Returns the resolved file and position information for a span
    pub fn pos(&self, span: Span) -> FilePos {
        let File {path, line_numbers, ..} = self.file(span.start);
        let (start_line, start_offset) = line_numbers.number_offset(span.start);
        // Subtract 1 because end actually represents one past the end of the span
        let (end_line, end_offset) = line_numbers.number_offset(span.end - 1);

        FilePos {path, start_line, start_offset, end_line, end_offset}
    }

    /// Returns resolved file and line info for the line containing the given index
    pub fn line(&self, index: usize) -> FileLine {
        let File {path, line_numbers, handle, ..} = self.file(index);
        let (line, _) = line_numbers.number_offset(index);
        let (start, end) = line_numbers.line_indexes(line);
        let bytes = &self.source(*handle).bytes[start..end];

        FileLine {path, line, bytes}
    }

    /// Returns the path of the file whose source contains the given index
    pub fn path(&self, index: usize) -> &Path {
        &self.file(index).path
    }

    /// Returns the source for the given file handle
    pub fn source(&self, handle: FileHandle) -> FileSource {
        let FileHandle {start, len} = handle;
        FileSource {
            bytes: &self.source[start..start+len],
            offset: start,
        }
    }

    fn file(&self, index: usize) -> &File {
        let file_index = self.files.binary_search_by_key(&index, |file| file.start_offset)
            .unwrap_or_else(|index| index - 1);
        &self.files[file_index]
    }
}
