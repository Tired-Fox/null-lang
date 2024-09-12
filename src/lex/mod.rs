mod token;

use std::{borrow::Cow, cell::RefCell, rc::Rc, str::FromStr};

use token::{ByteSize, Float, Keyword, Number, Operator, Signed, Unsigned};
pub use token::{TokenKind, Token};
use crate::{Error, ErrorKind, error};

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

    pub fn inc(&mut self, value: char) {
        self.byte += value.len_utf8();
        self.rest = &self.src[self.byte..];
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
            match c {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => self.byte += c.len_utf8(),
                _ => break
            }
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

    /// Move the pointer forward consuming a string
    ///
    /// Assume that the current char is `"` and that check has already been done.
    ///
    /// # Returns
    ///
    /// Consumed string token
    fn consume_string(&mut self) -> Result<Token<'input>, Error> {
        let start = self.byte;
        self.inc(self.rest.chars().next().unwrap());
        let mut buffer = String::new();

        loop {
            match self.next_char(ErrorKind::UnterminatedString, start) {
                Err(e) => return Err(self.consume_bad_string(e)),
                Ok((_, "\"")) => break,
                Ok((value, _)) => buffer.push(value)
            }
        }
        Ok(Token::string(buffer, self.byte))
    }

    /// Consume until a closing `'` character or end of file
    fn consume_bad_char(&mut self, error: Error) -> Error {
        let mut chars = self.rest.chars();
        while let Some(c) = chars.next() {
            self.inc(chars.next().unwrap());
            if let '\'' = c { break }
        }
        error
    }

    /// Consume until a closing `"` character or end of file
    fn consume_bad_string(&mut self, error: Error) -> Error {
        let mut chars = self.rest.chars();
        while let Some(c) = chars.next() {
            self.inc(c);
            if let '"' = c { break }
        }
        error
    }

    /// TODO: Clean Up:
    /// - Better structure for early return errors consuming rest of char
    /// - Better structure for parsing escape patterns
    /// - Another method that parses and then consumes rest of input depending on string or char to
    /// try to recover for continued tokenization
    fn next_char(&mut self, unterminated: ErrorKind, beginning: usize) -> Result<(char, &'_ str), Error> {
        let start = self.byte;
        let mut chars = self.rest.chars();

        let c = chars.next().ok_or(error!(unterminated, [beginning..beginning]))?;
        self.inc(c);

        let value = match c {
            '\\' => {
                let c = chars.next().ok_or(error!(ErrorKind::InvalidEscapeSequence, [start..start]))?;
                self.inc(c);
                match c {
                    '\'' => '\'',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\\' => '\\',
                    '0' => '\0',
                    'u' => {
                        let a = chars.next().ok_or(error!(ErrorKind::InvalidEscapeSequence, [start..start]))?;
                        self.inc(a);
                        if a != '{' {
                            return Err(error!(ErrorKind::InvalidEscapeSequence, [(start..self.byte-1, "unicode escape must have the format `\\u{hex}`")]));
                        }

                        let mut buffer = String::new();
                        loop {
                            let a = match chars.next() {
                                Some(v) => v,
                                None => return Err(error!(ErrorKind::InvalidEscapeSequence, [start..start]))
                            };
                            self.inc(a);
                            match a {
                                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                                    buffer.push(a);
                                },
                                '}' => break,
                                _ => {
                                    return Err(error!(ErrorKind::InvalidEscapeSequence, [(start..self.byte, "unicode escape must have the format `\\u{hex}`")]));
                                },
                            }
                        }

                        if buffer.len() < 2 {
                            return Err(error!(ErrorKind::InvalidEscapeSequence, [(self.byte-buffer.len()-1..self.byte-1, "must be a 2 digits or more")]))
                        }

                        if buffer.len() > 6 {
                            return Err(error!(ErrorKind::InvalidEscapeSequence, [(self.byte-buffer.len()-1..self.byte-1, "must be 6 digits or less")]))
                        }

                        let digit = u32::from_str_radix(buffer.as_str(), 16).map_err(|_| {
                            error!(ErrorKind::InvalidEscapeSequence, [(self.byte-2..self.byte-1, "must be a valid hex digit")])
                        })?;
                        char::from_u32(digit)
                            .ok_or(error!(ErrorKind::InvalidEscapeSequence, [(self.byte-buffer.len()-1..self.byte-1, "must be a valid unicode character")]))?
                    },
                    'x' => {
                        let a = match chars.next() {
                            Some(v) => v,
                            None => return Err(error!(ErrorKind::InvalidEscapeSequence, [start..start])),
                        };
                        self.inc(a);
                        match a {
                            '0'..='9' | 'a'..='f' | 'A'..='F' => {
                                let b = match chars.next() {
                                    Some(v) => v,
                                    None => return Err(error!(ErrorKind::InvalidEscapeSequence, [start..start])),
                                };
                                self.inc(b);
                                match b {
                                    '0'..='9' | 'a'..='f' | 'A'..='F' => {
                                        let digit = u32::from_str_radix(&self.src[self.byte-2..self.byte], 16).map_err(|_| {
                                            error!(ErrorKind::InvalidEscapeSequence, [(self.byte-2..self.byte, "must be a valid hex digit")])
                                        })?;
                                        char::from_u32(digit)
                                            .ok_or(error!(ErrorKind::InvalidEscapeSequence, [(self.byte-2..self.byte, "must be a valid ansi character")]))?
                                    },
                                    _ => return Err(error!(ErrorKind::InvalidEscapeSequence, [(start..self.byte, "must be 2 hex digits")]))
                                }
                            },
                            _ => return Err(error!(ErrorKind::InvalidEscapeSequence, [(start..self.byte, "must be 2 hex digits")]))
                        }
                    }
                    _ => return Err(error!(ErrorKind::InvalidSyntax, [start..self.byte]))
                }
            },
            _ => c
        };

        Ok((value, &self.src[start..self.byte]))
    }

    /// Move the pointer forward consuming a char
    ///
    /// Assume that the current char is `'` and that check has already been done.
    ///
    /// # Returns
    ///
    /// Consumed char token
    fn consume_char(&mut self) -> Result<Token<'input>, Error> {
        // Consume leading `'`
        self.inc(self.rest.chars().next().unwrap());
        let start = self.byte;

        let value = match self.next_char(ErrorKind::UnterminatedChar, start) {
            Err(e) => return Err(self.consume_bad_char(e)),
            Ok((_, "'")) => return Err(error!(ErrorKind::InvalidSyntax, [start..self.byte])),
            Ok((value, _)) => value
        };

        let c = self.rest.chars().next().ok_or(error!(ErrorKind::UnterminatedChar, [start..start]))?;
        self.inc(c);
        if c != '\'' {
            return Err(error!(ErrorKind::UnterminatedChar, [start..self.byte-1]))
        }

        Ok(Token::char(value, &self.src[start..self.byte-1], self.byte))
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

    /// Move the pointer forward consuming a comment
    ///
    /// Assumes that the current character is a `/`
    ///
    /// # Returns
    ///
    /// Consumed comment, doccomment, or divide operator
    fn consume_comment_or_divide(&mut self) -> Option<Result<Token<'input>, Error>> {
        let mut chars = self.rest.chars();
        let c = chars.next().unwrap();
        self.inc(c);

        let mut chars = self.rest.chars();
        let c = match chars.next() {
            Some(v) => v,
            None => return self.advance(Token::single(TokenKind::Operator(Operator::Divide), &self.src[self.byte-1..self.byte], self.byte)),
        };

        match c {
            '/' => {
                self.inc(c);

                let start = self.byte;
                while let Some(c) = chars.next() {
                    self.inc(c);
                    match c {
                        '\n' => break,
                        _ => {}
                    }
                }

                let repr = &self.src[start..self.byte-1];
                if repr.starts_with("/") {
                    return Some(Ok(Token {
                        kind: TokenKind::DocComment,
                        span: (start+1..self.byte-1).into(),
                        repr: (&repr[1..]).into()
                    }))
                } else {
                    return Some(Ok(Token {
                        kind: TokenKind::Comment,
                        span: (start..self.byte-1).into(),
                        repr: repr.into()
                    }))
                }
            },
            '*' => {
                self.inc(c);

                let start = self.byte;

                let mut nested: usize = 1;
                let mut slash = false;
                let mut escaped = false;
                let mut star = false;

                while let Some(c) = chars.next() {
                    self.inc(c);
                    match c {
                        '\\' if !escaped => escaped = true,
                        '*' if !escaped => {
                            if slash { nested += 1 }
                            else { star = true }
                        },
                        '/' if !escaped => {
                            if star { nested = nested.saturating_sub(1); }
                            else { slash = true }
                            if nested == 0 { break }
                        }
                        _ => {
                            escaped = false;
                            star = false;
                            slash = false;
                        },
                    }
                }

                let repr = &self.src[start..self.byte-2];
                if repr.starts_with('*') {
                    return Some(Ok(Token {
                        kind: TokenKind::DocComment,
                        span: (start+1..self.byte-1).into(),
                        repr: (&repr[1..]).into()
                    }))
                } else {
                    return Some(Ok(Token {
                        kind: TokenKind::Comment,
                        span: (start..self.byte-2).into(),
                        repr: repr.into()
                    }))
                }
            },
            _ => {},
        }

        Some(Ok(Token::single(TokenKind::Operator(Operator::Divide), self.src, self.byte-1)))
    }

    fn produce_number(&mut self, start: usize, negative: bool, floating: bool, byte_size: ByteSize, buff: &str) -> Result<Token<'input>, Error> {
        match floating {
            true => {
                match Float::try_from((byte_size, buff)).map_err(|e| error!(ErrorKind::InvalidNumber, [(start..self.byte, e)])) {
                    Ok(v) => Ok(Token::float(v, &self.src[start..self.byte], self.byte)),
                    Err(e) => Err(e)
                }
            },
            false => match negative {
                true => {
                    match Signed::try_from((byte_size, buff)).map_err(|e| error!(ErrorKind::InvalidNumber, [(start..self.byte, e)])) {
                        Ok(v) => Ok(Token::signed(v, &self.src[start..self.byte], self.byte)),
                        Err(e) => Err(e)
                    }
                },
                false => {
                    match Unsigned::try_from((byte_size, buff)).map_err(|e| error!(ErrorKind::InvalidNumber, [(start..self.byte, e)])) {
                        Ok(v) => Ok(Token::unsigned(v, &self.src[start..self.byte], self.byte)),
                        Err(e) => Err(e)
                    }
                }
            },
        }
    }

    /// TODO: Parse a number token
    /// Move the pointer forward consuming a number literal
    ///
    /// Assumes the current character is a valid digit or `.`
    ///
    /// # Returns
    ///
    /// Consumed number with optional scientific notion and suffixed type.
    fn consume_number(&mut self) -> Option<Result<Token<'input>, Error>> {
        let mut chars = self.rest.chars();
        let start = self.byte;

        let mut buffer = String::new();
        let mut floating = false;
        let mut negative = false;
        let mut byte_size = ByteSize::default();

        //  NOTE: Infered defaults are i32 and f32. If the number is larger than either bump the type up
        //  with inference. The inference is mostly done here when lexing and figuring out the
        //  actual value.
        //
        //  Pattern: [\d.+-]
        //      \d*(.\d*)?([eE][+-]?[1-9]\d*)
        //
        // While next is digit ... collect
        //  1. If [i, u, f] then expect [8 16 32 64 128] and u & i also get `usize`
        //  2. If `.` then infer float
        //      While next is digit ... collect
        //       Check for condition 3
        //  3. If `e` | `E` then (+|-)?
        //      While next is digit ... collect
        //
        let first = match chars.next() {
            Some(v) => v,
            None => return Some(Err(error!(ErrorKind::Unkown, [(self.byte..self.byte, "expected a number")])))
        };

        match first {
            '+' | '-' => {
                buffer.push(first); 
                // TODO: Assert next char is a number or return the operator as is
                let second = match chars.next() {
                    Some(v) => v,
                    None => {
                        self.inc(first);
                        return Some(Ok(Token::operator(if first == '+' { Operator::Plus } else { Operator::Minus }, &self.src[self.byte-first.len_utf8()..self.byte], self.byte)));
                    }
                };
                if let '0'..='9' = second {
                    self.inc(first);
                    self.inc(second);
                    negative = first == '-';
                    buffer.push(second);
                } else {
                    if let Some(op) = self.consume_operator() {
                        return Some(Ok(Token::operator(op, &self.src[start..self.byte], self.byte)))
                    }
                    self.inc(first);
                    return Some(Err(error!(ErrorKind::InvalidSyntax, [(self.byte-first.len_utf8()..self.byte, "unexpected character")])));
                }
            },
            '.' => {
                buffer.push(first); 
                floating = true;
                // TODO: Assert next char is a number or return the operator as is
                let second = match chars.next() {
                    Some(v) => v,
                    None => {
                        self.inc(first);
                        return Some(Ok(Token::operator(Operator::Dot, &self.src[self.byte-first.len_utf8()..self.byte], self.byte)));
                    }
                };
                if let '0'..='9' = second {
                    self.inc(first);
                    self.inc(second);
                    buffer.push(second);
                } else {
                    if let Some(op) = self.consume_operator() {
                        return Some(Ok(Token::operator(op, &self.src[start..self.byte], self.byte)))
                    }
                    self.inc(first);
                    return Some(Err(error!(ErrorKind::InvalidSyntax, [(self.byte-first.len_utf8()..self.byte, "unexpected character")])));
                }
            },
            '0'..='9' => {
                self.inc(first);
                buffer.push(first);
            },
            _ => return Some(Err(error!(ErrorKind::InvalidNumber, [(self.byte-first.len_utf8()..self.byte, "must start with 0..9, `.`, `-`, or `+`")])))
        }

        // Consume digits
        while let Some(digit) = chars.next() {
            if !digit.is_ascii_digit() {
                break;
            }
            self.inc(digit);
            buffer.push(digit)
        }
        let mut chars = self.rest.chars();

        let next = match chars.next() {
            Some(v) => v,
            None => return Some(self.produce_number(start, negative, floating, byte_size, buffer.as_str()))
        };

        match next {
            '.' => {
                self.inc(next);
                floating = true;
                buffer.push(next);

                while let Some(digit) = chars.next() {
                    if !digit.is_ascii_digit() {
                        break;
                    }
                    self.inc(digit);
                    buffer.push(digit);
                }
                let mut chars = self.rest.chars();
                let next = match chars.next() {
                    Some(v) => v,
                    None => return Some(self.produce_number(start, negative, floating, byte_size, buffer.as_str()))
                };

                if let 'e' | 'E' = next {
                    self.inc(next);
                    buffer.push(next);

                    match chars.next() {
                        Some(v) if v.is_ascii_digit() => {
                            self.inc(v);
                            buffer.push(v);
                        },
                        v @ Some('+'|'-') => {
                            let v = v.unwrap();
                            self.inc(v);
                            buffer.push(v);

                            match chars.next() {
                                Some(v) if v.is_ascii_digit() => {
                                    self.inc(v);
                                    buffer.push(v);
                                },
                                _ => return Some(Err(error!(ErrorKind::InvalidNumber, [(self.byte-next.len_utf8()..self.byte, "expected a digit 1..9")])))
                            }
                        }
                        _ => return Some(Err(error!(ErrorKind::InvalidNumber, [(self.byte-next.len_utf8()..self.byte, "expected a digit 1..9 or `+` or `-`")])))
                    };
                    
                    while let Some(digit) = chars.next() {
                        if !digit.is_ascii_digit() {
                            break;
                        }
                        self.inc(digit);
                        buffer.push(digit);
                    }
                    return Some(self.produce_number(start, negative, floating, byte_size, buffer.as_str()))
                }

                Some(self.produce_number(start, negative, floating, byte_size, buffer.as_str()))
            },
            'i' | 'u' | 'f' => {
                let type_start = self.byte;
                let ident = self.consume_ident();

                if next == 'i' { negative = true }
                else if next == 'f' { floating = true }

                match ByteSize::from_str(ident).map_err(|e| error!(ErrorKind::InvalidNumber, [(type_start..self.byte, e)])) {
                    Ok(size) => byte_size = size,
                    Err(e) => return Some(Err(e))
                }

                Some(self.produce_number(start, negative, floating, byte_size, buffer.as_str()))
            },
            'e' | 'E' => {
                self.inc(next);
                buffer.push(next);

                match chars.next() {
                    Some(v) if v.is_ascii_digit() => {
                        self.inc(v);
                        buffer.push(v);
                    },
                    v @ Some('+'|'-') => {
                        let v = v.unwrap();
                        self.inc(v);
                        buffer.push(v);

                        match chars.next() {
                            Some(v) if v.is_ascii_digit() => {
                                self.inc(v);
                                buffer.push(v);
                            },
                            _ => return Some(Err(error!(ErrorKind::InvalidNumber, [(self.byte-next.len_utf8()..self.byte, "expected a digit 1..9")])))
                        }
                    }
                    _ => return Some(Err(error!(ErrorKind::InvalidNumber, [(self.byte-next.len_utf8()..self.byte, "expected a digit 1..9 or `+` or `-`")])))
                };
                
                while let Some(digit) = chars.next() {
                    if !digit.is_ascii_digit() {
                        break;
                    }
                    self.inc(digit);
                    buffer.push(digit);
                }
                Some(self.produce_number(start, negative, floating, byte_size, buffer.as_str()))
            },
            _ => {
                Some(self.produce_number(start, negative, floating, byte_size, buffer.as_str()))
            }
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

        match c {
            'a'..='z' | 'A'..='Z' | '_' => {
                let ident = self.consume_ident();
                Some(Ok(match Keyword::from_str(ident) {
                    Ok(kw) => Token::keyword(kw, ident, self.byte),
                    Err(_) => Token::ident(ident, self.byte)
                }))
            },
            ':' => self.advance(Token::single(TokenKind::Colon, self.src, self.byte)),
            // TODO: parse comment. Otherwise invalid character
            '\\' => self.advance(Token::single(TokenKind::Slash, self.src, self.byte)),
            '/' => self.consume_comment_or_divide(),
            ',' => self.advance(Token::single(TokenKind::Comma, self.src, self.byte)),
            '?' => self.advance(Token::single(TokenKind::Question, self.src, self.byte)),
            ';' => self.advance(Token::single(TokenKind::Semicolon, self.src, self.byte)),
            '[' => self.advance(Token::single(TokenKind::OpenBracket, self.src, self.byte)),
            ']' => self.advance(Token::single(TokenKind::CloseBracket, self.src, self.byte)),
            '(' => self.advance(Token::single(TokenKind::OpenParenthesis, self.src, self.byte)),
            ')' => self.advance(Token::single(TokenKind::CloseParenthesis, self.src, self.byte)),
            '{' => self.advance(Token::single(TokenKind::OpenBrace, self.src, self.byte)),
            '}' => self.advance(Token::single(TokenKind::CloseBrace, self.src, self.byte)),
            '\'' => Some(self.consume_char()),
            '"' => Some(self.consume_string()),
            '.' | '+' | '-' | '0'..='9' => self.consume_number(),
            other => {
                let start = self.byte;
                if let Some(op) = self.consume_operator() {
                    return Some(Ok(Token::operator(op, &self.src[start..self.byte], self.byte)))
                }
                self.inc(other);
                Some(Err(error!(ErrorKind::InvalidSyntax, [(self.byte-c.len_utf8()..self.byte, "unexpected character")])))
            }
        }
    }
}
