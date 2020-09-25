use super::FileSource;

#[derive(Debug)]
pub struct LineNumbers {
    /// The index in `SourceFiles::source` of the first byte in each line
    ///
    /// The first byte of a line is defined as either the first byte in the file or a byte
    /// immediately after a b`\n`.
    offsets: Vec<usize>,
}

impl LineNumbers {
    pub fn new(source: FileSource) -> Self {
        // There is always at least one line, starting at offset 0
        let mut offsets = vec![source.start_index()];

        for (offset, ch) in source.iter_bytes() {
            if ch == b'\n' {
                // Each line starts right *after* each newline
                offsets.push(offset+1);
            }
        }

        // Push the offset for one past the end of the file. This allows us to index into `offsets`
        // without worrying about panicking since any result of `binary_search` for a valid index
        // into the file will have a corresponding value in `offsets`. (this simplifies some code)
        offsets.push(source.start_index() + source.len());

        Self {offsets}
    }

    /// Returns the (line number, offset) corresponding to the given index in the source file
    ///
    /// Both the line number and offset are 1-based
    pub fn number_offset(&self, index: usize) -> (usize, usize) {
        let line;
        let offset;

        match self.offsets.binary_search(&index) {
            // `index` corresponds to the first byte of a line
            Ok(i) => {
                line = i+1;
                offset = 1;
            },

            // `index` is on the line `i`
            Err(i) => {
                line = i;
                // the offset of the first byte of this line is at i-1 because `binary_search`
                // always finds the index *after* the first byte when `Err(_)` is returned
                offset = index - self.offsets[i-1] + 1;
            }
        }

        (line, offset)
    }

    /// Returns the (start index, end index) of this line
    pub fn line_indexes(&self, line: usize) -> (usize, usize) {
        (self.offsets[line-1], self.offsets[line])
    }
}
