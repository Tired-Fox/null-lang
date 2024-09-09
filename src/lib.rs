use std::ops::Range;

pub mod token;

/// Span representing a range of bytes in source code
#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    /// Inclusive
    pub(crate) start: usize,
    /// Exclusive
    pub(crate) end: usize
}

impl Span {
    pub fn range(&self) -> Range<usize> {
        (*self).into()
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Span[{}..{}]", self.start, self.end)
    }
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

impl From<Span> for Range<usize> {
    fn from(value: Span) -> Self {
        value.start..value.end
    }
}
