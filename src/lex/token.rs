use std::{borrow::Cow, str::FromStr};

use crate::Span;

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
    type Err = ();
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
            _ => return Err(())
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
    Modulo,
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
    /// `<`
    LessThan,
    /// `>`
    GreaterThan,
    /// `<=`
    LessThanEqual,
    /// `>=`
    GreaterThanEqual,

    /// ==
    EqualEqual,
    /// !=
    BangEqual,
    /// %=
    ModuloEqual,
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
    ShiftLeftEqual,
    /// >>=
    ShiftRightEqual,
    /// =>
    EqualGreaterThan,
    /// '..'
    DotDot,
    /// `..=`
    DotDotEqual,
    /// `||`
    LogicalOr,
    /// `&&`
    LogicalAnd,
    /// `|=`
    OrEqual,
    /// `^=`
    XorEqual,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    DocComment,
    Comment,
    String,
    Char(char),
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

    Comma,
    Colon,
    Question,
    Slash,
    Semicolon,
    DoubleQuote,
    SingleQuote,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'input> {
    pub(crate) kind: TokenKind,
    pub(crate) span: Span,
    pub(crate) repr: Cow<'input, str>,
}

impl<'input> Token<'input> {
    pub fn span(&self) -> Span {
        self.span 
    }

    pub fn repr(&self) -> &'_ str {
        self.repr.as_ref()
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub(crate) fn keyword(keyword: Keyword, repr: impl Into<Cow<'input, str>>, pos: usize) -> Token<'input> {
        let repr: Cow<'_, str> = repr.into();
        Token {
            kind: TokenKind::Keyword(keyword),
            span: Span::from(pos-repr.len()..pos),
            repr
        }
    }

    pub(crate) fn ident(repr: impl Into<Cow<'input, str>>, pos: usize) -> Token<'input> {
        let repr: Cow<'_, str> = repr.into();
        Token {
            kind: TokenKind::Ident,
            span: Span::from(pos-repr.len()..pos),
            repr
        }
    }

    pub(crate) fn operator(op: Operator, repr: impl Into<Cow<'input, str>>, pos: usize) -> Token<'input> {
        let repr: Cow<'_, str> = repr.into();
        Token {
            kind: TokenKind::Operator(op),
            span: Span::from(pos-repr.len()..pos),
            repr
        }
    }

    pub(crate) fn string(repr: impl Into<Cow<'input, str>>, pos: usize) -> Token<'input> {
        let repr: Cow<'_, str> = repr.into();
        Token {
            kind: TokenKind::String,
            span: Span::from(pos-repr.len()-2..pos),
            repr
        }
    }

    pub(crate) fn char(value: char, repr: impl Into<Cow<'input, str>>, pos: usize) -> Token<'input> {
        let repr: Cow<'_, str> = repr.into();
        Token {
            kind: TokenKind::Char(value),
            span: Span::from(pos-repr.len()-2..pos),
            repr
        }
    }

    pub(crate) fn single(kind: TokenKind, src: &'_ str, pos: usize) -> Token<'_> {
        Token {
            kind,
            span: Span::from(pos..pos + 1),
            repr: Cow::from(&src[pos..=pos]),
        }
    }
}

#[cfg(test)]
mod test {
    use std::str::FromStr;

    use crate::lex::token::Keyword;

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

        assert!(matches!(Keyword::from_str("???"), Err(())));
    }
}
