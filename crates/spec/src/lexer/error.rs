use crate::{error::{Diagnostic, Severity}, source::Chunk, Span};

#[derive(Debug)]
pub enum Error {
    SyntaxError(Chunk),
}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SyntaxError => write!(f, "invalid syntax")
        }
    }
}
impl std::error::Error for Error {}

impl Diagnostic for Error {
    fn chunk(&self) -> Option<&crate::source::Chunk> {
        
    }

    fn code<'code>(&'code self) -> Option<Box<dyn std::fmt::Display + 'code>> {
        match self {
            Self::SyntaxError => Some(Box::new("T001"))
        }
    }

    fn severity(&self) -> Option<Severity> {
        Some(Severity::Error)
    }
}
