use std::ops::Range;

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
}
