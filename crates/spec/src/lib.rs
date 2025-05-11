use std::ops::Range;

pub mod lexer;
pub mod parser;
pub mod error;
pub mod source;

pub type Span = Range<usize>;
