mod token;
mod error;

use std::str::FromStr;

use crate::Span;
use token::Keyword;
pub use token::{TokenKind, Token};
pub use error::Error;

pub struct Tokenizer<'input> {
    src: &'input str,
    pointer: usize,
}

impl<'input> Tokenizer<'input> {
    pub fn new(src: &'input str) -> Self {
        Self {
            src,
            pointer: 0,
        }
    }

    /// Get the current line of the source the tokenizer is processing
    /// 
    /// `Zero Indexed`
    pub fn line(&self) -> usize {
        self.src[..self.pointer].lines().count()
    }

    /// Get the current column of the source the tokenizer is processing
    /// 
    /// `Zero Indexed`
    pub fn column(&self) -> usize {
        self.src[..self.pointer].rsplit_once('\n').unwrap().1.len()
    }

    pub fn current(&self) -> char {
        self.src[self.pointer..=self.pointer].chars().next().unwrap()
    }

    /// Move the pointer forward consuming all whitespace
    ///
    /// # Returns
    ///
    /// Slice of whitespace that was consumed
    fn consume_whitespace(&mut self) -> &'input str {
        let start = self.pointer;
        while self.pointer < self.src.len() {
            match self.current() {
                // Whitespace
                ' ' | '\n' | '\r' => self.pointer += 1,
                _ => break,
            }
        }
        &self.src[start..self.pointer]
    }

    /// Move the pointer foward consuming an identifier
    ///
    /// Assumes that a valid start to an identifier has already been checked.
    ///
    /// # Returns
    ///
    /// Consumed identifier
    fn consume_ident(&mut self) -> &'input str {
        let start = self.pointer;
        while self.pointer < self.src.len() {
            if !self.current().is_alphanumeric() {
                break;
            }
            self.pointer += 1;
        }
        &self.src[start..self.pointer]
    }
}

impl<'input> Iterator for Tokenizer<'input> {
    type Item = Result<Token<'input>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pointer >= self.src.len() {
            return None;
        }

        // Consume any leading whitespace
        self.consume_whitespace();

        match self.current() {
            'a'..='z' | 'A'..='Z' | '_' => {
                let ident = self.consume_ident();
                Some(Ok(match Keyword::from_str(ident) {
                    Ok(kw) => Token::keyword(kw, ident, self.pointer),
                    Err(_) => Token::ident(ident, self.pointer)
                }))
            },
            next => {
                println!("Delim({next})");
                self.pointer += 1;
                None
            }
        }
    }
}
