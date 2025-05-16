use std::{borrow::Cow, collections::{BTreeSet, VecDeque}};

use strum::VariantArray;
use token::{Operator, Token};

use crate::{error::Result, parser::Parser};

mod token;
mod error;

#[derive(Default)]
pub struct TokenCache<'token> {
    ident: BTreeSet<Cow<'token, str>>,
    string: BTreeSet<Cow<'token, str>>,
    number: BTreeSet<Cow<'token, str>>,
}

pub struct Tokenizer<'token, I> {
    parser: Parser<I>,

    position: usize,

    buffer: VecDeque<char>,
    cache: TokenCache<'token>,
}

impl<'token, I: Iterator<Item = u8>> Iterator for Tokenizer<'token, I> {
    type Item = Result<Token<'token>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(v) => v.map(Ok),
            Err(e) => Some(Err(e))
        }
    }
}

impl<'token, I: Iterator<Item = u8>> Tokenizer<'token, I> {
    pub fn new(stream: I) -> Self {
        Self {
            parser: Parser::from(stream),
            cache: Default::default(),
            position: 0,
            buffer: Default::default(),
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Result<Option<char>> {
        self.position += 1;
        if !self.buffer.is_empty() {
            return Ok(self.buffer.pop_front());
        }
        self.parser.next_code_point()
    }

    pub fn peek(&mut self) -> Result<Option<char>> {
        if !self.buffer.is_empty() {
            return Ok(self.buffer.front().copied());
        }

        match self.parser.next_code_point()? {
            Some(v) => {
                self.buffer.push_back(v);
                Ok(Some(v))
            },
            None => Ok(None)
        }
    }

    pub fn parse_whitespace(&mut self) -> Result<()> {
        let start = self.position;
        while matches!(self.peek()?, Some(cp) if cp.is_whitespace()) {
            _ = self.next()?;
        }
        println!("WS @ [{start}..{}]", self.position);
        Ok(())
    }

    pub fn parse_operator(&mut self) -> Result<Option<Token<'token>>> {
        let start = self.position;
        let mut pattern = String::new();
        let mut ops = Operator::VARIANTS.to_vec();

        while let Some(cp) = self.peek()? {
            if !Operator::contains(cp) {
                break;
            }

            pattern.push(cp);
            ops = ops.into_iter().filter(|op| op.as_ref().starts_with(&pattern)).collect::<Vec<_>>();

            if ops.is_empty() {
                return Err(error::Error::SyntaxError.into());
            } else if ops.len() == 1 {
                _ = self.next()?;
                break;
            }

            _ = self.next()?;
        }

        if ops.len() > 1 {
            return Err(error::Error::SyntaxError.into());
        }

        let op = ops.first().unwrap();

        if op.as_ref() == pattern {
            Ok(Some(Token::Operator(start..self.position, *op)))
        } else {
            Err(error::Error::SyntaxError.into())
        }
    }

    pub fn next_token(&mut self) -> Result<Option<Token<'token>>> {
        while let Some(codepoint) = self.peek()? {
            match codepoint {
                ' ' | '\r' | '\n' | '\t' => self.parse_whitespace()?,
                cp if Operator::contains(cp) => return self.parse_operator(),
                _ => return Err(error::Error::SyntaxError.into())
            }
        }

        Ok(None)
    }
}
