use std::ops::Range;

use super::Span;

/// The source for a file, represented as a slice of bytes and indexed from `start_index()` onwards
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileSource<'a> {
    /// A slice of `SourceFiles::source`
    pub(in super) bytes: &'a [u8],
    /// The offset at which the slice of bytes was extracted from `SourceFiles::source`
    pub(in super) offset: usize,
}

impl<'a> FileSource<'a> {
    /// Returns the first index into this slice
    pub fn start_index(&self) -> usize {
        self.offset
    }

    /// Returns the number of bytes in the file
    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    /// Returns the byte in this file at the given index, if any
    ///
    /// Note that the index MUST be greater than or equal to `start_index()` for this method to
    /// return anything.
    pub fn get(&self, index: usize) -> Option<u8> {
        let index = index - self.offset;
        self.bytes.get(index).copied()
    }

    /// Slices from the bytes of this file's source
    pub fn slice(&self, range: Range<usize>) -> &'a [u8] {
        let Self {bytes, offset} = self;
        let Range {start, end} = range;

        &bytes[start-offset..end-offset]
    }

    /// Iterates over the bytes of this file's source, yielding the offset for each one
    pub fn iter_bytes(&self) -> impl Iterator<Item=(usize, u8)> + '_ {
        self.bytes.iter().copied().enumerate().map(move |(index, byte)| (self.offset + index, byte))
    }

    /// Iterates over each line of the file's source and the line's span
    ///
    /// Each line will contain its trailing newline
    pub fn lines(&self) -> FileSourceLines {
        let &Self {bytes, offset} = self;
        FileSourceLines {bytes, offset, pos: 0}
    }
}

#[derive(Debug, Clone)]
pub struct FileSourceLines<'a> {
    /// A slice of `SourceFiles::source`
    bytes: &'a [u8],
    /// The offset at which the slice of bytes was extracted from `SourceFiles::source`
    offset: usize,
    /// The amount this iterator has moved forward
    pos: usize,
}

impl<'a> Iterator for FileSourceLines<'a> {
    type Item = (Span, &'a [u8]);

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.pos;
        while let Some(&ch) = self.bytes.get(self.pos) {
            self.pos += 1;

            if ch == b'\n' {
                break;
            }
        }

        if start == self.pos {
            return None;
        }

        let span = Span {
            start: self.offset + start,
            end: self.offset + self.pos,
        };

        Some((span, &self.bytes[start..self.pos]))
    }
}
