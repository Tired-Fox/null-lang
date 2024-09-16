use std::{cmp::Ordering, ops::Range};

pub mod lex;
pub mod parse;
mod error;

pub use error::{Error, ErrorKind, Source, Label};

/// Span representing a range of bytes in source code
#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    /// Inclusive
    pub start: usize,
    /// Exclusive
    pub end: usize
}
impl Ord for Span {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.start.cmp(&other.start) {
            Ordering::Equal => self.end.cmp(&other.end),
            other => other
        }
    }
}
impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
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
