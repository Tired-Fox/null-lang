use std::str::FromStr;

use crate::Span;

use super::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumIs, strum::EnumProperty)]
pub enum Keyword {
    Fn,
    Struct,
    Type,
    Union,
    Error,
    Public,
    Constant,
    Let,
    Null,
    Interface,
}

impl FromStr for Keyword {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "fn" => Self::Fn,
            "struct" => Self::Struct,
            "union" => Self::Union,
            "error" => Self::Error, 
            "pub" => Self::Public,
            "const" => Self::Constant,
            "let" => Self::Let,
            "null" => Self::Null,
            "interface" => Self::Interface,
            _ => return Err(Error::UnkownKeyword)
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumIs, strum::EnumProperty)]
pub enum Operator {
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Multiply,
    /// `/`
    Divide,
    /// `=`
    Equal,
    /// `%`
    Modulus,
    /// `\`
    Shash,
    /// `&`
    And,
    /// `|`
    Or,
    /// `!`
    Bang,
    /// `^`
    Xor,
    /// `~`
    Not,
    /// `&~`
    AndNot,
    /// `<<`
    ShiftLeft,
    /// `>>`
    ShiftRight,
    /// '.'
    Dot,
    /// `?`
    Question,

    /// ==
    EqualEqual,
    /// !=
    BangEqual,
    /// %=
    ModulusEqual,
    /// &=
    AndEqual,
    /// *=
    MultiplyEqual,
    /// ~=
    NotEqual,
    /// +=
    PlusEqual,
    /// -=
    MinusEqual,
    /// /=
    DivideEqual,
    /// <<=
    LeftShiftEqual,
    /// >>=
    RightShiftEqual,
    /// =>
    EqualGreaterThan,
    /// '..'
    DotDot,
    /// ..=
    DotDotEqual,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    String,
    Number,
    Ident,
    Keyword(Keyword),
    Operator(Operator),

    OpenParenthesis,
    CloseParenthesis,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token<'input> {
    kind: TokenKind,
    span: Span,
    repr: &'input str,
}

impl Token<'_> {
    pub fn keyword(keyword: Keyword, repr: &'_ str, pos: usize) -> Token<'_> {
        Token {
            kind: TokenKind::Keyword(keyword),
            span: Span::from(pos-repr.len()..pos),
            repr
        }
    }

    pub fn ident(repr: &'_ str, pos: usize) -> Token<'_> {
        Token {
            kind: TokenKind::Ident,
            span: Span::from(pos-repr.len()..pos),
            repr
        }
    }
}

#[cfg(test)]
mod test {
    use std::str::FromStr;

    use crate::token::{token::Keyword, Error};

    #[test]
    fn keyword_from_str() {
        assert!(matches!(Keyword::from_str("fn"), Ok(Keyword::Fn)));
        assert!(matches!(Keyword::from_str("struct"), Ok(Keyword::Struct)));
        assert!(matches!(Keyword::from_str("union"), Ok(Keyword::Union)));
        assert!(matches!(Keyword::from_str("error"), Ok(Keyword::Error))); 
        assert!(matches!(Keyword::from_str("pub"), Ok(Keyword::Public)));
        assert!(matches!(Keyword::from_str("const"), Ok(Keyword::Constant)));
        assert!(matches!(Keyword::from_str("let"), Ok(Keyword::Let)));
        assert!(matches!(Keyword::from_str("null"), Ok(Keyword::Null)));
        assert!(matches!(Keyword::from_str("interface"), Ok(Keyword::Interface)));

        assert!(matches!(Keyword::from_str("???"), Err(Error::UnkownKeyword)));
    }
}
