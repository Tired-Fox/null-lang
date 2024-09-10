mod token;

use std::str::FromStr;

use crate::{
    error,
    source,
    Span
};
use token::{Keyword, Operator};
pub use token::{TokenKind, Token};
use crate::error::{Error, ErrorKind};

pub struct Tokenizer<'input> {
    src: &'input str,
    rest: &'input str,
    byte: usize,
}

impl<'input> Tokenizer<'input> {
    pub fn new(src: &'input str) -> Self {
        Self {
            src,
            rest: &src[..1],
            byte: 0,
        }
    }

    /// Get the current line of the source the tokenizer is processing
    /// 
    /// `Zero Indexed`
    pub fn line(&self) -> usize {
        self.src[..self.byte].lines().count()
    }

    /// Get the current column of the source the tokenizer is processing
    /// 
    /// `Zero Indexed`
    pub fn column(&self) -> usize {
        self.src[..self.byte].rsplit_once('\n').unwrap().1.len()
    }

    /// Move the pointer forward consuming all whitespace
    ///
    /// # Returns
    ///
    /// Slice of whitespace that was consumed
    fn consume_whitespace(&mut self) -> &'input str {
        let start = self.byte;
        let chars = self.rest.chars();
        for c in chars {
            match c {
                ' ' | '\n' | '\r' => self.byte += c.len_utf8(),
                _ => break
            }
        }
        self.rest = &self.src[self.byte..];
        &self.src[start..self.byte]
    }

    /// Move the pointer foward consuming an identifier
    ///
    /// Assumes that a valid start to an identifier has already been checked.
    ///
    /// # Returns
    ///
    /// Consumed identifier
    fn consume_ident(&mut self) -> &'input str {
        let start = self.byte;
        let chars = self.rest.chars();
        for c in chars {
            if !c.is_alphanumeric() {
                break;
            }
            self.byte += c.len_utf8();
        }
        self.rest = &self.src[self.byte..];
        &self.src[start..self.byte]
    }

    /// Match a list of options of operators and return the first match progressing the pointer
    /// if there is a match
    fn one_of<const N: usize>(&mut self, options: [(&str, Operator); N], default: Operator) -> Operator {
        for (option, op) in options {
            if self.byte + option.len() < self.src.len() && &self.src[self.byte..self.byte + option.len()] == option {
                self.byte += option.len();
                self.rest = &self.src[self.byte..];
                return op;
            }
        }
        self.byte += 1;
        self.rest = &self.src[self.byte..];
        default
    }

    /// Move the pointer forward consuming an operator
    ///
    /// Assume that the current char is `"` and that check has already been done.
    ///
    /// # Returns
    ///
    /// Consumed string
    fn consume_string(&mut self) -> Result<Token<'input>, Error> {
        let mut chars = self.rest.chars();
        self.byte += chars.next().unwrap().len_utf8();

        let start = self.byte;
        let mut escaped = true;

        loop {
            let c = chars.next().ok_or(error!(ErrorKind::UnterminatedString, start..start))?;
            self.byte += c.len_utf8();
            self.rest = &self.src[self.byte..];
            match c {
                '\\' if !escaped => escaped = true,
                '"' if !escaped => break,
                _ => escaped = false,
            }
        }

        Ok(Token::string(&self.src[start..self.byte-1], self.byte))
    }

    /// Move the pointer forward consuming an operator
    /// 
    /// # Returns
    /// 
    /// Consumed operator
    fn consume_operator(&mut self) -> Option<Operator> {
        let c = self.rest.chars().next()?;
        match c {
            '+' => Some(self.one_of([("+=", Operator::PlusEqual)], Operator::Plus)),
            '-' => Some(self.one_of([("-=", Operator::MinusEqual)], Operator::Minus)),
            '*' => Some(self.one_of([("*=", Operator::MultiplyEqual)], Operator::Multiply)),
            '/' =>  Some(self.one_of([("/=", Operator::DivideEqual)], Operator::Divide)),
            '=' =>  Some(self.one_of([("==", Operator::EqualEqual)], Operator::Equal)),
            '%' =>  Some(self.one_of([("%=", Operator::ModuloEqual)], Operator::Modulo)),
            '&' =>  Some(self.one_of([("&=", Operator::AndEqual)], Operator::And)),
            '|' =>  Some(self.one_of([("||", Operator::LogicalOr), ("|=", Operator::OrEqual)], Operator::Or)),
            '!' =>  Some(self.one_of([("!=", Operator::BangEqual)], Operator::Bang)),
            '^' =>  Some(self.one_of([("^=", Operator::XorEqual)], Operator::Xor)),
            '~' =>  Some(self.one_of([("~=", Operator::NotEqual)], Operator::Not)),
            '<' =>  Some(self.one_of([("<<=", Operator::ShiftLeftEqual), ("<<", Operator::ShiftLeft), ("<=", Operator::LessThanEqual)], Operator::LessThan)),
            '>' =>  Some(self.one_of([(">>=", Operator::ShiftRightEqual), (">>", Operator::ShiftRight), (">=", Operator::GreaterThanEqual)], Operator::GreaterThan)),
            '.' =>  Some(self.one_of([("..", Operator::DotDot), ("..=", Operator::DotDotEqual)], Operator::Dot)),
            _ => None,
        }
    }

    /// Advance the pointer given a token
    fn advance(&mut self, token: Token<'input>) -> Option<Result<Token<'input>, Error>> {
        self.byte += token.repr.len();
        self.rest = &self.src[self.byte..];
        Some(Ok(token))
    }
}

impl<'input> Iterator for Tokenizer<'input> {
    type Item = Result<Token<'input>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        // Consume any leading whitespace
        self.consume_whitespace();

        let c = self.rest.chars().next()?;
        self.rest = &self.src[self.byte..];

        match c {
            'a'..='z' | 'A'..='Z' | '_' => {
                let ident = self.consume_ident();
                Some(Ok(match Keyword::from_str(ident) {
                    Ok(kw) => Token::keyword(kw, ident, self.byte),
                    Err(_) => Token::ident(ident, self.byte)
                }))
            },
            ':' => self.advance(Token::single(TokenKind::Colon, self.src, self.byte)),
            '\\' => self.advance(Token::single(TokenKind::Slash, self.src, self.byte)),
            ',' => self.advance(Token::single(TokenKind::Comma, self.src, self.byte)),
            '?' => self.advance(Token::single(TokenKind::Question, self.src, self.byte)),
            ';' => self.advance(Token::single(TokenKind::Semicolon, self.src, self.byte)),
            '[' => self.advance(Token::single(TokenKind::OpenBracket, self.src, self.byte)),
            ']' => self.advance(Token::single(TokenKind::CloseBracket, self.src, self.byte)),
            '(' => self.advance(Token::single(TokenKind::OpenParenthesis, self.src, self.byte)),
            ')' => self.advance(Token::single(TokenKind::CloseParenthesis, self.src, self.byte)),
            '{' => self.advance(Token::single(TokenKind::OpenBrace, self.src, self.byte)),
            '}' => self.advance(Token::single(TokenKind::CloseBrace, self.src, self.byte)),
            // TODO: parse a rune/char token
            '\'' => self.advance(Token::single(TokenKind::SingleQuote, self.src, self.byte)),
            '"' => Some(self.consume_string()),
            // TODO: Parse a number token
            '0'..='9' => {
                self.byte += c.len_utf8();
                self.rest = &self.src[self.byte..]; 
                Some(Err(error!(ErrorKind::InvalidSyntax, (self.byte-c.len_utf8()..self.byte, "unexpected character"))))
            },
            other => {
                let start = self.byte;
                if let Some(op) = self.consume_operator() {
                    return Some(Ok(Token::operator(op, &self.src[start..self.byte], self.byte)))
                }
                self.byte += other.len_utf8();
                self.rest = &self.src[self.byte..];
                Some(Err(error!(ErrorKind::InvalidSyntax, (self.byte-c.len_utf8()..self.byte, "unexpected character"))))
            }
        }
    }
}
