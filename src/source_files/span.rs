/// A span of a source file
///
/// Invariants:
/// * Span must be of size >= 0, i.e. `end >= start`
/// * The `start` and `end` indexes MUST remain within the boundaries of a single file. That is, you
///   can never span two files at the same time.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    /// The start index of the span (inclusive)
    pub start: usize,
    /// The end index of the span (exclusive)
    pub end: usize,
}

impl Span {
    /// Creates a span from the start of `self` to the end of `other`
    pub fn to(self, other: Self) -> Self {
        assert!(other.end >= self.start, "bug: span should have at least zero size");

        Self {
            start: self.start,
            end: other.end,
        }
    }
}
