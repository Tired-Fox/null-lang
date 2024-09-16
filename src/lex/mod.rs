mod token;

use std::str::FromStr;

pub use token::{ByteSize, Float, Keyword, Operator, Signed, Unsigned, Number};
pub use token::{TokenKind, Token};
use crate::{Error, ErrorKind, error};

/// Check if a characters is an identifier valid character
fn is_ident(c: char) -> bool {
    matches!(c, 'a'..='z'|'A'..='Z'|'0'..='9'|'_')
}

/// Check if a characters is a valid whitespace character
fn is_whitespace(c: char) -> bool {
    matches!(c, '\r' | '\n' | '\t' | ' ')
}

pub struct Tokenizer<'input> {
    pub(crate) src: &'input str,
    pub(crate) byte: usize,
}

impl<'input> Tokenizer<'input> {
    pub fn new(src: &'input str) -> Self {
        Self { src, byte: 0 }
    }

    /// Consume the next character progressing the tokenizer 
    ///
    /// Automatically turns `\r\n` into `\n`
    pub(crate) fn consume(&mut self) -> Option<char> {
        let mut chars = self.src[self.byte..].chars();
        let char = chars.next();
        match char {
            Some(char) => {
                self.byte += char.len_utf8();
                if char == '\r' {
                    if let Some('\n') = chars.next() {
                        self.byte += 1;
                        return Some('\n');
                    }
                }
                Some(char)
            },
            None => None
        }
    }


    /// Consume the next `N` characters progressing the tokenizer
    ///
    /// Automatically turns `\r\n` into `\n`
    pub(crate) fn consume_n<const N: usize>(&mut self) -> [Option<char>;N] {
        let mut values = [None;N];
        let mut chars = self.src[self.byte..].chars().peekable();
        for value in values.iter_mut() {
            if let Some(c) = chars.next() {
                *value = Some(c);
                self.byte += c.len_utf8();

                if c == '\r' {
                    if let Some('\n') = chars.peek() {
                        self.byte += 1;
                        *value = chars.next();
                    }
                }
            }
        }
        values
    }

    /// Consume the next 'N' characters progressing the tokenizer
    ///
    /// Does not automatically turn `\r\n` into `\n`
    ///
    /// # Returns
    ///
    /// Characters as a slice of the source. This slice may be shorter
    /// than the specified length if the source does not contain enough characters
    pub(crate) fn consume_n_as_str(&mut self, n: usize) -> &'input str {
        let start = self.byte;
        self.byte += self.src[self.byte..].chars().take(n).map(|v| {
            v.len_utf8()
        }).sum::<usize>();
        &self.src[start..self.byte]
    }


    /// Consume the source until the predicate returns false. 
    ///
    /// Automatically turns `\r\n` into `\n`
    ///
    /// # Returns
    ///
    /// Consumed slice of the source text.
    pub(crate) fn consume_while<F>(&mut self, predicate: F) -> &'input str
    where
        F: Fn(char) -> bool
    {
        let start = self.byte;
        let mut chars = self.src[self.byte..].chars().peekable();
        while let Some(c) = chars.peek().copied() {
            if (c == '\r' && matches!(chars.peek(), Some('\n'))) || predicate(c) {
                let _ = chars.next();
                self.byte += c.len_utf8();
            } else {
                break;
            }
        }
        &self.src[start..self.byte]
    }

    /// Consume the source until the predicate returns false. 
    ///
    /// Automatically turns `\r\n` into `\n`
    ///
    /// # Returns
    ///
    /// Consumed slice of the source text.
    pub(crate) fn consume_while_state<S, F>(&mut self, mut state: S, predicate: F) -> &'input str
    where
        F: Fn(&mut S, char) -> bool
    {
        let start = self.byte;
        let mut chars = self.src[self.byte..].chars().peekable();
        while let Some(c) = chars.peek().copied() {
            if (c == '\r' && matches!(chars.peek(), Some('\n'))) || predicate(&mut state, c) {
                let _ = chars.next();
                self.byte += c.len_utf8();
            } else {
                break;
            }
        }
        &self.src[start..self.byte]
    }

    /// Consume the next character while progressing the tokenizer
    ///
    /// # Returns
    ///
    /// Decoded character. This character automatically has it's value lexed i.e. escape sequences
    /// to it's character representation.
    fn consume_decoded(&mut self) -> Result<(char, &'_ str), Error> {
        let start = self.byte;
        let value = match self.consume().ok_or(error!(ErrorKind::UnexpectedEof, [start..self.byte]))? {
            '\\' => {
                match self.consume().ok_or(error!(ErrorKind::InvalidEscapeSequence, [start..start]))? {
                    '\'' => '\'',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\\' => '\\',
                    '0' => '\0',
                    'u' => {
                        let a = self.consume().ok_or(error!(ErrorKind::InvalidEscapeSequence, [start..start]))?;
                        if a != '{' {
                            return Err(error!(ErrorKind::InvalidEscapeSequence, [(start..self.byte-1, "unicode escape must have the format `\\u{hex}`")]));
                        }

                        let mut buffer = String::new();
                        loop {
                            match self.consume() {
                                c @ Some('0'..='9' | 'a'..='f' | 'A'..='F') => {
                                    buffer.push(c.unwrap());
                                },
                                Some('}') => break,
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
                        let start_hex = self.byte;
                        match self.consume() {
                            Some('0'..='9' | 'a'..='f' | 'A'..='F') => {
                                match self.consume() {
                                    Some('0'..='9' | 'a'..='f' | 'A'..='F') => {
                                        let digit = u32::from_str_radix(&self.src[start_hex..self.byte], 16).map_err(|_| {
                                            error!(ErrorKind::InvalidEscapeSequence, [(start_hex..self.byte, "must be a valid hex digit")])
                                        })?;
                                        char::from_u32(digit)
                                            .ok_or(error!(ErrorKind::InvalidEscapeSequence, [(start_hex..self.byte, "must be a valid ansi character")]))?
                                    },
                                    _ => return Err(error!(ErrorKind::InvalidEscapeSequence, [(start..self.byte, "must be 2 hex digits")]))
                                }
                            },
                            _ => return Err(error!(ErrorKind::InvalidEscapeSequence, [(start..self.byte, "must be 2 hex digits")]))
                        }
                    }
                    c => c
                }
            },
            c => c
        };

        Ok((value, &self.src[start..self.byte]))
    }

    /// Consume the next character progressing the tokenizer 
    ///
    /// Automatically turns `\r\n` into `\n`
    ///
    /// # Returns
    /// A single character as a slice of the source
    pub(crate) fn consume_as_str(&mut self) -> &'input str {
        let start = self.byte;
        self.consume();
        &self.src[start..self.byte]
    }

    /// Fetch the next character without progressing the tokenizer
    ///
    /// Automatically turns `\r\n` into `\n`
    pub(crate) fn peek(&mut self) -> Option<char> {
        let mut chars = self.src[self.byte..].chars();
        let peek = chars.next();
        if let Some('\r') = peek {
            if let Some('\n') = chars.next() {
                return Some('\n');
            }
        }
        peek
    }

    /// Fetch the next 'N' characters without progressing the tokenizer
    ///
    /// Automatically turns `\r\n` into `\n`
    pub(crate) fn peek_n<const N: usize>(&mut self) -> [Option<char>;N] {
        let mut peeked: [Option<char>;N] = [None;N];
        let mut chars = self.src[self.byte..].chars().peekable();
        for peek in peeked.iter_mut() {
            *peek = chars.next();
            if let Some('\r') = peek {
                if let Some('\n') = chars.peek() {
                    *peek = chars.next();
                }
            }
        }
        peeked
    }

    /// Fetch the next 'N' characters without progressing the tokenizer
    ///
    /// Does not automatically turn `\r\n` into `\n`
    ///
    /// # Returns
    ///
    /// Peeks characters as a slice of the source. This slice may be shorter
    /// than the specified length if the source does not contain enough characters
    pub(crate) fn peek_n_as_str(&mut self, n: usize) -> &'input str {
        let end = self.src[self.byte..].chars().take(n).map(|v| v.len_utf8()).sum::<usize>();
        &self.src[self.byte..self.byte+end]
    }

    /// Fetch the `N`th character without progressing the tokenizer.
    ///
    /// Automatically turns `\r\n` into `\n`
    pub(crate) fn peek_nth(&mut self, pos: usize) -> Option<char> {
        let mut chars = self.src[self.byte..].chars();
        let peek = chars.nth(pos);
        if let Some('\r') = peek {
            if let Some('\n') = chars.next() {
                return Some('\n');
            }
        }
        peek
    }

    /// Get the current byte the tokenizer is processing
    pub fn byte(&self) -> usize {
        self.byte
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

    /// Match a list of options of operators and return the first match progressing the pointer
    /// if there is a match
    fn consume_one_of<T, const N: usize>(&mut self, options: [(&str, T); N]) -> Option<T> {
        for (option, op) in options {
            if self.peek_n_as_str(option.len()) == option {
                self.consume_n_as_str(option.len());
                return Some(op);
            }
        }
        None
    }

    /// Move the pointer forward consuming a char
    ///
    /// Assume that the current char is `'` and that check has already been done.
    ///
    /// # Returns
    ///
    /// Consumed char token
    fn consume_char(&mut self) -> Result<Token<'input>, Error> {
        let start = self.byte;
        // PERF: Ensure that first char is `'`
        self.consume();

        let value = match self.consume_decoded() {
            // PERF: Add help stating that char was not closed
            Err(e) => return Err(self.consume_bad_char(e)),
            Ok((_, "'")) => return Err(error!(ErrorKind::InvalidSyntax, [start..self.byte])),
            Ok((value, _)) => value
        };

        let c = self.consume().ok_or(error!(ErrorKind::UnterminatedChar, [start..start]))?;
        if c != '\'' {
            return Err(self.consume_bad_char(error!(ErrorKind::UnterminatedChar, [start..self.byte-1])))
        }

        Ok(Token::char(value, &self.src[start..self.byte], self.byte))
    }

    /// Move the pointer forward consuming a string
    ///
    /// Assume that the current char is `"` and that check has already been done.
    ///
    /// # Returns
    ///
    /// Consumed string token
    fn consume_string(&mut self) -> Result<Token<'input>, Error> {
        // PERF: Ensure that the first char is `"`
        self.consume();
        let mut buffer = String::new();

        loop {
            match self.consume_decoded() {
                // PERF: Add help stating that the string was not closed
                Err(e) => return Err(self.consume_bad_string(e)),
                Ok((_, "\"")) => break,
                Ok((value, _)) => buffer.push(value)
            }
        }
        Ok(Token::string(buffer, self.byte))
    }

    /// Consume until a closing `'` character or end of file
    fn consume_bad_char(&mut self, error: Error) -> Error {
        self.consume_while_state(false, |escaped, c| {
            match c {
                '\'' if !*escaped => return false,
                '\\' if !*escaped => *escaped = true,
                _ => *escaped = false
            }
            true
        });

        if self.byte < self.src.len() {
            self.byte += 1;
        }

        error
    }

    /// Consume until a closing `"` character or end of file
    fn consume_bad_string(&mut self, error: Error) -> Error {
        self.consume_while_state(false, |escaped, c| {
            match c {
                '"' if !*escaped => return false,
                '\\' if !*escaped => *escaped = true,
                _ => *escaped = false
            }
            true
        });
        if self.byte < self.src.len() {
            self.byte += 1;
        }
        error
    }

    /// Move the pointer forward consuming an operator
    /// 
    /// # Returns
    /// 
    /// Consumed operator
    fn consume_operator(&mut self) -> Option<Operator> {
        match self.peek()? {
            '+' => Some(self.consume_one_of([("+=", Operator::PlusEqual)]).unwrap_or({self.consume();Operator::Plus})),
            '-' => Some(self.consume_one_of([("-=", Operator::MinusEqual)]).unwrap_or({self.consume();Operator::Minus})),
            '*' => Some(self.consume_one_of([("*=", Operator::MultiplyEqual)]).unwrap_or({self.consume();Operator::Multiply})),
            '/' =>  Some(self.consume_one_of([("/=", Operator::DivideEqual)]).unwrap_or({self.consume();Operator::Divide})),
            '=' =>  Some(self.consume_one_of([("==", Operator::EqualEqual), ("=>", Operator::EqualGreaterThan)]).unwrap_or({self.consume();Operator::Equal})),
            '%' =>  Some(self.consume_one_of([("%=", Operator::ModuloEqual)]).unwrap_or({self.consume();Operator::Modulo})),
            '&' =>  Some(self.consume_one_of([("&=", Operator::AndEqual), ("&&", Operator::LogicalAnd), ("&~", Operator::AndNot)]).unwrap_or({self.consume();Operator::And})),
            '|' =>  Some(self.consume_one_of([("||", Operator::LogicalOr), ("|=", Operator::OrEqual)]).unwrap_or({self.consume();Operator::Or})),
            '!' =>  Some(self.consume_one_of([("!=", Operator::BangEqual)]).unwrap_or({self.consume();Operator::Bang})),
            '^' =>  Some(self.consume_one_of([("^=", Operator::XorEqual)]).unwrap_or({self.consume();Operator::Xor})),
            '~' =>  Some(self.consume_one_of([("~=", Operator::NotEqual)]).unwrap_or({self.consume();Operator::Not})),
            '<' =>  Some(self.consume_one_of([("<<=", Operator::ShiftLeftEqual), ("<<", Operator::ShiftLeft), ("<=", Operator::LessThanEqual)]).unwrap_or({self.consume();Operator::LessThan})),
            '>' =>  Some(self.consume_one_of([(">>=", Operator::ShiftRightEqual), (">>", Operator::ShiftRight), (">=", Operator::GreaterThanEqual)]).unwrap_or({self.consume();Operator::GreaterThan})),
            '.' =>  Some(self.consume_one_of([("..=", Operator::DotDotEqual), ("..", Operator::DotDot)]).unwrap_or({self.consume();Operator::Dot})),
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
        // PERF: Check that first char is `/`
        self.consume();

        let c = match self.peek() {
            Some(v) => v,
            None => return Some(Ok(Token::single(TokenKind::Operator(Operator::Divide), &self.src[self.byte-1..self.byte], self.byte))),
        };

        match c {
            '/' => {
                self.consume();

                let start = self.byte;
                let repr = self.consume_while(|c| c != '\n');
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
                self.consume();

                let start = self.byte;

                let mut nested: usize = 1;
                let mut slash = false;
                let mut escaped = false;
                let mut star = false;

                while let Some(c) = self.consume() {
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

        Some(Ok(Token::single(TokenKind::Operator(Operator::Divide), &self.src[self.byte-1..self.byte], self.byte)))
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

    /// Move the pointer forward consuming a number literal
    ///
    /// Assumes the current character is a valid digit or `.`
    ///
    /// # Returns
    ///
    /// Consumed number with optional scientific notion and suffixed type.
    fn consume_number(&mut self) -> Option<Result<Token<'input>, Error>> {
        let start = self.byte;

        let mut buffer = String::new();
        // Infer a i32 and f32 when parsing respectively
        let mut floating = false;
        let mut negative = false;
        let mut byte_size = ByteSize::default();

        //  Pattern: [\d.+-][\d_]*(.[\d_]*)?([eE][+-]?[1-9][\d_]*)
        match self.peek_n::<3>() {
            [Some('0'..='9'), _, _] => buffer.push(self.consume().unwrap()),
            [first @ Some('+'|'-'|'.'), Some('0'..='9'), _] => {
                if let Some('.') = first { floating = true }
                else if let Some('-') = first { negative = true }

                buffer.push_str(self.consume_n_as_str(2))
            },
            [first @ Some('+'|'-'), Some('.'), Some('0'..='9')] => {
                floating = true;
                if let Some('-') = first { negative = true }
                buffer.push_str(self.consume_n_as_str(3))
            },
            [Some('+'|'-'|'.'), _, _] => {
                if let Some(op) = self.consume_operator() {
                    return Some(Ok(Token::operator(op, &self.src[start..self.byte], self.byte)))
                }
                return Some(Err(error!(ErrorKind::InvalidSyntax, [(start..self.byte, "unexpected character")])));
            },
            _ => return Some(Err(error!(ErrorKind::InvalidNumber, [(start..self.byte, "expected a number")])))
        }

        // Consume digits
        while matches!(self.peek(), Some('0'..='9' | '_')) {
            let c = self.consume().unwrap();
            if c != '_' { buffer.push(c) }
        }

        match self.peek() {
            Some('.') => {
                floating = true;
                buffer.push(self.consume().unwrap());

                // Consume digits
                while matches!(self.peek(), Some('0'..='9' | '_')) {
                    let c = self.consume().unwrap();
                    if c != '_' { buffer.push(c) }
                }

                let next = match self.peek() {
                    Some(v) => v,
                    None => return Some(self.produce_number(start, negative, floating, byte_size, buffer.as_str()))
                };

                if let 'e' | 'E' = next {
                    let estart = self.byte;
                    buffer.push(self.consume().unwrap());
                    floating = true;

                    match self.peek_n::<2>() {
                        [Some('+'|'-'), Some('1'..='9')] => buffer.push_str(self.consume_n_as_str(2)),
                        [Some('1'..='9'), _] => buffer.push(self.consume().unwrap()),
                        _ => return Some(Err(error!(ErrorKind::InvalidNumber, [(estart..self.byte, "invalid scientific notation")])))
                    }
                    
                    // Consume digits
                    while matches!(self.peek(), Some('0'..='9' | '_')) {
                        let c = self.consume().unwrap();
                        if c != '_' { buffer.push(c) }
                    }

                    return Some(self.produce_number(start, negative, floating, byte_size, buffer.as_str()))
                }

                Some(self.produce_number(start, negative, floating, byte_size, buffer.as_str()))
            },
            v @ Some('i' | 'u' | 'f') => {
                let type_start = self.byte;
                let ident = self.consume_while(is_ident);

                if let Some('i') = v { negative = true }
                else if let Some('f') = v { floating = true }

                match ByteSize::from_str(ident).map_err(|e| error!(ErrorKind::InvalidNumber, [(type_start..self.byte, e)])) {
                    Ok(size) => byte_size = size,
                    Err(e) => return Some(Err(e))
                }

                Some(self.produce_number(start, negative, floating, byte_size, buffer.as_str()))
            },
            Some('e' | 'E') => {
                let estart = self.byte;
                buffer.push(self.consume().unwrap());
                floating = true;

                match self.peek_n::<2>() {
                    [Some('+'|'-'), Some('1'..='9')] => buffer.push_str(self.consume_n_as_str(2)),
                    [Some('1'..='9'), _] => buffer.push(self.consume().unwrap()),
                    _ => return Some(Err(error!(ErrorKind::InvalidNumber, [(estart..self.byte, "invalid scientific notation")])))
                }
                
                // Consume digits
                while matches!(self.peek(), Some('0'..='9' | '_')) {
                    let c = self.consume().unwrap();
                    if c != '_' { buffer.push(c) }
                }

                Some(self.produce_number(start, negative, floating, byte_size, buffer.as_str()))
            },
            _ => {
                Some(self.produce_number(start, negative, floating, byte_size, buffer.as_str()))
            }
        }
    }
}

impl<'input> Iterator for Tokenizer<'input> {
    type Item = Result<Token<'input>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        // Consume any leading whitespace
        self.consume_while(is_whitespace);

        match self.peek()? {
            'a'..='z' | 'A'..='Z' | '_' => {
                let ident = self.consume_while(is_ident);
                Some(Ok(match Keyword::from_str(ident) {
                    Ok(kw) => Token::keyword(kw, ident, self.byte),
                    Err(_) => Token::ident(ident, self.byte)
                }))
            },
            ':' => Some(Ok(Token::single(TokenKind::Colon, self.consume_as_str(), self.byte))),
            '\\' => {
                match self.peek_nth(1) {
                    Some(_) => {
                        let s = self.byte;
                        self.consume_n::<2>();
                        Some(Err(error!(ErrorKind::InvalidSyntax, [(s..self.byte, "unkown character")])))
                    }
                    None => {
                        self.consume();
                        Some(Err(error!(ErrorKind::InvalidSyntax, [self.byte-1..self.byte])))
                    }
                }
            },
            '/' => self.consume_comment_or_divide(),
            ',' => Some(Ok(Token::single(TokenKind::Comma, self.consume_as_str(), self.byte))),
            '?' => Some(Ok(Token::single(TokenKind::Question, self.consume_as_str(), self.byte))),
            ';' => Some(Ok(Token::single(TokenKind::Semicolon, self.consume_as_str(), self.byte))),
            '[' => Some(Ok(Token::single(TokenKind::OpenBracket, self.consume_as_str(), self.byte))),
            ']' => Some(Ok(Token::single(TokenKind::CloseBracket, self.consume_as_str(), self.byte))),
            '(' => Some(Ok(Token::single(TokenKind::OpenParenthesis, self.consume_as_str(), self.byte))),
            ')' => Some(Ok(Token::single(TokenKind::CloseParenthesis, self.consume_as_str(), self.byte))),
            '{' => Some(Ok(Token::single(TokenKind::OpenBrace, self.consume_as_str(), self.byte))),
            '}' => Some(Ok(Token::single(TokenKind::CloseBrace, self.consume_as_str(), self.byte))),
            '\'' => Some(self.consume_char()),
            '"' => Some(self.consume_string()),
            '.' | '+' | '-' | '0'..='9' => self.consume_number(),
            other => {
                let start = self.byte;
                if let Some(op) = self.consume_operator() {
                    return Some(Ok(Token::operator(op, &self.src[start..self.byte], self.byte)))
                }
                self.consume();
                Some(Err(error!(ErrorKind::InvalidSyntax, [(self.byte-other.len_utf8()..self.byte, "unexpected character")])))
            }
        }
    }
}

#[cfg(test)]
mod test {
    use std::borrow::Cow;

    use token::Number;

    use crate::source;

    use super::*;

    #[test]
    fn peek_helpers() {
        let source = "main :: fn () {\r\n  print('c');\r\n}";
        let mut tokenizer = Tokenizer::new(source);
        assert_eq!(tokenizer.peek(), Some('m'));
        assert_eq!(tokenizer.byte, 0);
        assert_eq!(tokenizer.peek_n::<4>(), [Some('m'), Some('a'), Some('i'), Some('n')]);
        assert_eq!(tokenizer.byte, 0);

        assert_eq!(tokenizer.peek_nth(15), Some('\n'));
        assert_eq!(tokenizer.byte, 0);

        assert_eq!(tokenizer.peek_n_as_str(4), "main");
        assert_eq!(tokenizer.byte, 0);
    }

    #[test]
    fn consume_helpers() {
        let source = "main :: fn () {\r\n  print('c');\r\n}";
        let mut tokenizer = Tokenizer::new(source);
        assert_eq!(tokenizer.consume(), Some('m'));
        assert_eq!(tokenizer.byte, 1);
        assert_eq!(tokenizer.consume_n::<3>(), [Some('a'), Some('i'), Some('n')]);
        assert_eq!(tokenizer.byte, 4);

        assert_eq!(tokenizer.consume_n::<14>()[11], Some('\n'));
        assert_eq!(tokenizer.byte, 19);

        assert_eq!(tokenizer.consume_while(is_ident), "print");
        assert_eq!(tokenizer.byte, 24);

        assert_eq!(tokenizer.consume_n_as_str(5), "('c')");
        assert_eq!(tokenizer.byte, 29);

        assert_eq!(tokenizer.consume_as_str(), ";");
        assert_eq!(tokenizer.byte, 30);

        let source = "=>";
        let mut tokenizer = Tokenizer::new(source);
        assert_eq!(tokenizer.consume_one_of([("=>", "=>"), ("=", "=")]), Some("=>"));
        assert_eq!(tokenizer.byte, 2);
    }

    #[test]
    fn consume_whitespace() {
        let source = "  \r\n\t \r";
        let mut tokenizer = Tokenizer::new(source);
        tokenizer.consume_while(is_whitespace);
        assert_eq!(tokenizer.byte, source.len());
    }

    #[test]
    fn consume_ident() {
        let source = "_name98";
        let mut tokenizer = Tokenizer::new(source);
        assert_eq!(tokenizer.consume_while(is_ident), "_name98");
        assert_eq!(tokenizer.byte, source.len());
    }

    macro_rules! number {
        ($num: literal as u8) => { TokenKind::Number(Number::Unsigned(Unsigned::U8($num))) };
        ($num: literal as u16) => { TokenKind::Number(Number::Unsigned(Unsigned::U16($num))) };
        ($num: literal as u32) => { TokenKind::Number(Number::Unsigned(Unsigned::U32($num))) };
        ($num: literal as u64) => { TokenKind::Number(Number::Unsigned(Unsigned::U64($num))) };
        ($num: literal as usize) => { TokenKind::Number(Number::Unsigned(Unsigned::USize($num))) };
        ($num: literal as u128) => { TokenKind::Number(Number::Unsigned(Unsigned::U128($num))) };
        ($num: literal as i8) => { TokenKind::Number(Number::Signed(Signed::I8($num))) };
        ($num: literal as i16) => { TokenKind::Number(Number::Signed(Signed::I16($num))) };
        ($num: literal as i32) => { TokenKind::Number(Number::Signed(Signed::I32($num))) };
        ($num: literal as i64) => { TokenKind::Number(Number::Signed(Signed::I64($num))) };
        ($num: literal as isize) => { TokenKind::Number(Number::Signed(Signed::ISize($num))) };
        ($num: literal as i128) => { TokenKind::Number(Number::Signed(Signed::I128($num))) };
        ($num: literal as f32) => { TokenKind::Number(Number::Float(Float::F32($num))) };
        ($num: literal as f64) => { TokenKind::Number(Number::Float(Float::F64($num))) };
    }

    macro_rules! token {
        (kind : $($kind: tt)+) => {
            Token { kind: $($kind)*, .. }
        };
        (repr : $repr: literal) => {
            Token { repr: Cow::Borrowed($repr), .. }
        };
    }

    #[test]
    fn consume_number() {
        macro_rules! assert_is_number {
            {
                $tokenizer: ident,
                [$($number: literal as $ty: ident);* $(;)?]
            } => {
                $(
                    $tokenizer.consume_while(is_whitespace);
                    assert!(matches!($tokenizer.consume_number(), Some(Ok(token! { kind: number!($number as $ty) }))));
                )*
            };
        }

        let source = "1 .1 -1 +1 -.1 12.32 1e10 1e+10 1e-10 1.1e10 1.1e+10 1.1e-10 1_100 1_000_000 \
        1u8 1u16 1u32 1u64 1usize 1u128 1i8 1i16 1i32 1i64 1isize 1i128 1f32 1f64";
        let mut tokenizer = Tokenizer::new(source);

        // TODO: Check that when it goes out of range of default it attempts at a larger size
        assert_is_number!{
            tokenizer,
            [
                1 as u32;
                0.1 as f32;
                -1 as i32;
                1 as u32;
                -0.1 as f32;
                12.32 as f32;
                1e10 as f32;
                1e10 as f32;
                1e-10 as f32;
                1.1e10 as f32;
                1.1e10 as f32;
                1.1e-10 as f32;
                1_100 as u32;
                1_000_000 as u32;
                1 as u8;
                1 as u16;
                1 as u32;
                1 as u64;
                1 as usize;
                1 as u128;
                1 as i8;
                1 as i16;
                1 as i32;
                1 as i64;
                1 as isize;
                1 as i128;
                1.0 as f32;
                1.0 as f64;
            ]
        }
    }

    #[test]
    fn consume_char() {
        let source = "'c' '\\'' '\\r' '\\n' '\\t' '\\0' '\\\\' '\\x1f' '\\u{1F60A}' 'ab' '";
        let mut tokenizer = Tokenizer::new(source);

        tokenizer.consume_while(is_whitespace);
        assert!(matches!(tokenizer.consume_char(), Ok(token! { kind: TokenKind::Char('c') })));

        tokenizer.consume_while(is_whitespace);
        assert!(matches!(tokenizer.consume_char(), Ok(token! { kind: TokenKind::Char('\'') })));

        tokenizer.consume_while(is_whitespace);
        assert!(matches!(tokenizer.consume_char(), Ok(token! { kind: TokenKind::Char('\r') })));

        tokenizer.consume_while(is_whitespace);
        assert!(matches!(tokenizer.consume_char(), Ok(token! { kind: TokenKind::Char('\n') })));

        tokenizer.consume_while(is_whitespace);
        assert!(matches!(tokenizer.consume_char(), Ok(token! { kind: TokenKind::Char('\t') })));

        tokenizer.consume_while(is_whitespace);
        assert!(matches!(tokenizer.consume_char(), Ok(token! { kind: TokenKind::Char('\0') })));

        tokenizer.consume_while(is_whitespace);
        assert!(matches!(tokenizer.consume_char(), Ok(token! { kind: TokenKind::Char('\\') })));

        tokenizer.consume_while(is_whitespace);
        assert!(matches!(tokenizer.consume_char(), Ok(token! { kind: TokenKind::Char('\x1f') })));

        tokenizer.consume_while(is_whitespace);
        assert!(matches!(tokenizer.consume_char(), Ok(token! { kind: TokenKind::Char('\u{1F60A}') })));

        tokenizer.consume_while(is_whitespace);
        assert!(tokenizer.consume_char().is_err());
        assert_eq!(tokenizer.byte, source.len()-2);

        tokenizer.consume_while(is_whitespace);
        assert!(tokenizer.consume_char().is_err());
        assert_eq!(tokenizer.byte, source.len());
    }

    #[test]
    fn consume_string() {
        let source = "\"c\\\"\\r\\n\\t\\0\\\\\\x1f\\u{1F60A}\" \"\\x \" \"";
        let mut tokenizer = Tokenizer::new(source);

        tokenizer.consume_while(is_whitespace);
        assert_eq!(tokenizer.consume_string().unwrap().repr, Cow::Borrowed("c\"\r\n\t\0\\\u{1f}😊"));

        tokenizer.consume_while(is_whitespace);
        assert!(tokenizer.consume_string().is_err());
        assert_eq!(tokenizer.byte, source.len() - 2);

        tokenizer.consume_while(is_whitespace);
        assert!(tokenizer.consume_string().is_err());
        assert_eq!(tokenizer.byte, source.len());
    }

    #[test]
    fn consume_operator() {
        macro_rules! assert_op {
            ($tokenizer: ident, $($op: ident),* $(,)?) => {
                $(
                    $tokenizer.consume_while(is_whitespace);
                    assert_eq!($tokenizer.consume_operator(), Some(Operator::$op));
                )*
            };
        }
        let source = "+ - * / = % & | ! ^ ~ &~ << >> . < > <= >= == != %= &= *= ~= += -= /= <<= >>= => .. ..= || && |= ^=";
        let mut tokenizer = Tokenizer::new(source);
    
        assert_op! {
            tokenizer,
            Plus,
            Minus,
            Multiply,
            Divide,
            Equal,
            Modulo,
            And,
            Or,
            Bang,
            Xor,
            Not,
            AndNot,
            ShiftLeft,
            ShiftRight,
            Dot,
            LessThan,
            GreaterThan,
            LessThanEqual,
            GreaterThanEqual,
            EqualEqual,
            BangEqual,
            ModuloEqual,
            AndEqual,
            MultiplyEqual,
            NotEqual,
            PlusEqual,
            MinusEqual,
            DivideEqual,
            ShiftLeftEqual,
            ShiftRightEqual,
            EqualGreaterThan,
            DotDot,
            DotDotEqual,
            LogicalOr,
            LogicalAnd,
            OrEqual,
            XorEqual,
        }
    }

    #[test]
    fn consume_comment_or_divide() {
        let source = "/* /*
*/ */
// some comment
/// Doc Comment
/** Doc Comment */
";
        let mut tokenizer = Tokenizer::new(source);

        tokenizer.consume_while(is_whitespace);
        assert!(matches!(tokenizer.consume_comment_or_divide(), Some(Ok(token! { kind: TokenKind::Comment }))));

        tokenizer.consume_while(is_whitespace);
        assert!(matches!(tokenizer.consume_comment_or_divide(), Some(Ok(token! { kind: TokenKind::Comment }))));

        tokenizer.consume_while(is_whitespace);
        let doc_comment = tokenizer.consume_comment_or_divide();
        assert!(matches!(doc_comment, Some(Ok(token! { kind: TokenKind::DocComment }))));
        assert_eq!(doc_comment.unwrap().unwrap().repr, Cow::Borrowed(" Doc Comment"));

        tokenizer.consume_while(is_whitespace);
        let doc_comment = tokenizer.consume_comment_or_divide();
        assert!(matches!(doc_comment, Some(Ok(token! { kind: TokenKind::DocComment }))));
        assert_eq!(doc_comment.unwrap().unwrap().repr, Cow::Borrowed(" Doc Comment "));
    }

    #[test]
    fn consume_decoded() {
        let source = "\\r\\n\\t\\0\\\\\\x1f\\u{1F60A}\\'";
        let mut tokenizer = Tokenizer::new(source);

        assert!(matches!(tokenizer.consume_decoded(), Ok(('\r', "\\r"))));
        assert!(matches!(tokenizer.consume_decoded(), Ok(('\n', "\\n"))));
        assert!(matches!(tokenizer.consume_decoded(), Ok(('\t', "\\t"))));
        assert!(matches!(tokenizer.consume_decoded(), Ok(('\0', "\\0"))));
        assert!(matches!(tokenizer.consume_decoded(), Ok(('\\', "\\\\"))));
        assert!(matches!(tokenizer.consume_decoded(), Ok(('\x1f', "\\x1f"))));
        assert!(matches!(tokenizer.consume_decoded(), Ok(('\u{1F60A}', "\\u{1F60A}"))));
        assert!(matches!(tokenizer.consume_decoded(), Ok(('\'', "\\'"))));
    }

    #[test]
    fn iterator() {
        let source = r#"main :: fn() {
    // Second argument will raise and error. Tests tokenizers recovery
    print("Hello 😊\u{203C}", "\x ");
}"#;

        let tokens = Tokenizer::new(source).filter_map(|t| t.ok().map(|v| v.kind)).collect::<Vec<_>>();
        assert_eq!(tokens, vec![
            TokenKind::Ident,
            TokenKind::Colon,
            TokenKind::Colon,
            TokenKind::Keyword(Keyword::Fn),
            TokenKind::OpenParenthesis,
            TokenKind::CloseParenthesis,
            TokenKind::OpenBrace,
            TokenKind::Comment,
            TokenKind::Ident,
            TokenKind::OpenParenthesis,
            TokenKind::String,
            TokenKind::Comma,
            TokenKind::CloseParenthesis,
            TokenKind::Semicolon,
            TokenKind::CloseBrace,
        ]);
    }
}
