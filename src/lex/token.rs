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

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ByteSize {
    B8,
    B16,
    #[default]
    B32,
    B64,
    B128,
    BSize
}
impl FromStr for ByteSize {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "u8" | "i8" => Self::B8,
            "u16" | "i16" => Self::B16,
            "u32" | "i32" | "f32" => Self::B32,
            "u64" | "i64" | "f64" => Self::B64,
            "usize" | "isize" => Self::BSize,
            "u128" | "i128" => Self::B128,
            // PERF: Better error message based on input
            _ => return Err("invalid number type".into())
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumIs)]
pub enum Unsigned {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    USize(usize),
    U128(u128),
}
impl TryFrom<(ByteSize, &str)> for Unsigned {
    type Error = String;

    fn try_from((size, value): (ByteSize, &str)) -> Result<Self, Self::Error> {
        match size {
            ByteSize::B8 => value.parse().map(Unsigned::U8).map_err(|_| "out of range for u8".into()),
            ByteSize::B16 => value.parse().map(Unsigned::U16).map_err(|_| "out of range for u16".into()),
            ByteSize::B32 => value.parse().map(Unsigned::U32).map_err(|_| "out of range for u32".into()),
            ByteSize::B64 => value.parse().map(Unsigned::U64).map_err(|_| "out of range for u64".into()),
            ByteSize::BSize => value.parse().map(Unsigned::USize).map_err(|_| "out of range for usize".into()),
            ByteSize::B128 => value.parse().map(Unsigned::U128).map_err(|_| "out of range for u128".into()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumIs)]
pub enum Signed {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    ISize(isize),
    I128(i128),
}
impl TryFrom<(ByteSize, &str)> for Signed {
    type Error = String;

    fn try_from((size, value): (ByteSize, &str)) -> Result<Self, Self::Error> {
        match size {
            ByteSize::B8 => value.parse().map(Signed::I8).map_err(|_| "out of range for i8".into()),
            ByteSize::B16 => value.parse().map(Signed::I16).map_err(|_| "out of range for i16".into()),
            ByteSize::B32 => value.parse().map(Signed::I32).map_err(|_| "out of range for i32".into()),
            ByteSize::B64 => value.parse().map(Signed::I64).map_err(|_| "out of range for i64".into()),
            ByteSize::BSize => value.parse().map(Signed::ISize).map_err(|_| "out of range for isize".into()),
            ByteSize::B128 => value.parse().map(Signed::I128).map_err(|_| "out of range for i128".into()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, strum::EnumIs)]
pub enum Float {
    F32(f32),
    F64(f64),
}
impl TryFrom<(ByteSize, &str)> for Float {
    type Error = String;

    fn try_from((size, value): (ByteSize, &str)) -> Result<Self, Self::Error> {
        match size {
            ByteSize::B32 => value.parse().map(Float::F32).map_err(|_| "must be a valid f32".into()),
            ByteSize::B64 => value.parse().map(Float::F64).map_err(|_| "must be a valid f64".into()),
            // PERF: Better error message
            _ => Err("floats are only allowed to be 32 and 64 bits".into()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, strum::EnumIs)]
pub enum Number {
    Unsigned(Unsigned),
    Signed(Signed),
    Float(Float)
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    DocComment,
    Comment,
    String,
    Char(char),
    Number(Number),
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
    Semicolon,
    DoubleQuote,
    SingleQuote,
}

#[derive(Debug, Clone, PartialEq)]
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

    pub(crate) fn float(value: Float, repr: &'_ str, pos: usize) -> Token<'_> {
        Token {
            kind: TokenKind::Number(Number::Float(value)),
            span: (pos-repr.len()..pos).into(),
            repr: Cow::from(repr),
        }
    }

    pub(crate) fn unsigned(value: Unsigned, repr: &'_ str, pos: usize) -> Token<'_> {
        Token {
            kind: TokenKind::Number(Number::Unsigned(value)),
            span: (pos-repr.len()..pos).into(),
            repr: Cow::from(repr),
        }
    }

    pub(crate) fn signed(value: Signed, repr: &'_ str, pos: usize) -> Token<'_> {
        Token {
            kind: TokenKind::Number(Number::Signed(value)),
            span: (pos-repr.len()..pos).into(),
            repr: Cow::from(repr),
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
