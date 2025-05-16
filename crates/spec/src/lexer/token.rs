use std::borrow::Cow;

use strum::{EnumProperty, IntoEnumIterator};

use crate::Span;

#[derive(Debug)]
pub enum Token<'token> {
    Operator(Span, Operator),
    Keyword(Span, Keyword),
    Ident(Span, Cow<'token, str>),
    String(Span, Cow<'token, str>),
    Number(Span, Cow<'token, str>),
    Rune(Span, char),
    Character(Span, char),
}

impl Token<'_> {
    pub fn span(&self) -> Span {
        match self {
            Self::Operator(span, _) => span.clone(),
            Self::Keyword(span, _) => span.clone(),
            Self::Ident(span, _) => span.clone(),
            Self::String(span, _) => span.clone(),
            Self::Number(span, _) => span.clone(),
            Self::Rune(span, _) => span.clone(),
            Self::Character(span, _) => span.clone(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, strum::IntoStaticStr, strum::Display, strum::EnumIter)]
#[strum(serialize_all="lowercase")]
pub enum Keyword {
    Fn,
    For,
    If,
    Else,
    Struct,
    And,
    Or
}

impl Keyword {
    pub fn from(chars: &str) -> Option<Self> {
        Keyword::iter().find(|&kw| Into::<&'static str>::into(kw) == chars)
    }
}


/// `- + = [] {} () <> , . ? / \\ | _ ~ ! @ # $ % ^ & * ' " ~ : ;` `
///
/// `-= += *= /= != == <<= >>= << >> && || %= ^= ~=`
#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, strum::EnumProperty, strum::Display, strum::AsRefStr, strum::EnumIter, strum::VariantArray)]
pub enum Operator {
    #[strum(props(repr=":"))]
    Colon,
    #[strum(props(repr=";"))]
    SemiColon,
    #[strum(props(repr="-"))]
    Minus,
    #[strum(props(repr="+"))]
    Plus,
    #[strum(props(repr="="))]
    Equal,
    #[strum(props(repr="["))]
    LeftBracket,
    #[strum(props(repr="]"))]
    RightBracket,
    #[strum(props(repr="{"))]
    LeftBrace,
    #[strum(props(repr="}"))]
    RightBrace,
    #[strum(props(repr="("))]
    LeftParen,
    #[strum(props(repr=")"))]
    RightParen,
    #[strum(props(repr="<"))]
    LessThan,
    #[strum(props(repr=">"))]
    GreaterThan,
    #[strum(props(repr=","))]
    Comma,
    #[strum(props(repr="."))]
    Period,
    #[strum(props(repr="?"))]
    QuestionMark,
    #[strum(props(repr="/"))]
    Divide,
    #[strum(props(repr="\\"))]
    Slash,
    #[strum(props(repr="_"))]
    Underscore,
    #[strum(props(repr="`"))]
    Backtick,
    #[strum(props(repr="@"))]
    At,
    #[strum(props(repr="#"))]
    Hash,
    #[strum(props(repr="$"))]
    Dollar,
    #[strum(props(repr="%"))]
    Percent,
    #[strum(props(repr="!"))]
    Bang,
    #[strum(props(repr="*"))]
    Star,
    #[strum(props(repr="'"))]
    Quote,
    #[strum(props(repr="\""))]
    DoubleQuote,

    #[strum(props(repr="^"))]
    BitwiseXor,
    #[strum(props(repr="|"))]
    BitwiseOr,
    #[strum(props(repr="~"))]
    BitwiseNot,
    #[strum(props(repr="&"))]
    BitwiseAnd,
    #[strum(props(repr="<<"))]
    BitwiseShiftLeft,
    #[strum(props(repr=">>"))]
    BitwiseShiftRight,

    #[strum(props(repr="^="))]
    BitwiseXorEqual,
    #[strum(props(repr="|="))]
    BitwiseOrEqual,
    #[strum(props(repr="~="))]
    BitwiseNotEqual,
    #[strum(props(repr="&="))]
    BitwiseAndEqual,
    #[strum(props(repr="<<="))]
    BitwiseShiftLeftEqual,
    #[strum(props(repr=">>="))]
    BitwiseShiftRightEqual,

    #[strum(props(repr="-="))]
    MinusEqual,
    #[strum(props(repr="+="))]
    PlusEqual,
    #[strum(props(repr="*="))]
    StarEqual,
    #[strum(props(repr="/="))]
    DivideEqual,
    #[strum(props(repr="!="))]
    BangEqual,
    #[strum(props(repr="=="))]
    EqualEqual,
    #[strum(props(repr="%="))]
    PercentEqual,
    #[strum(props(repr=">="))]
    GreaterThanEqual,
    #[strum(props(repr="<="))]
    LessThanEqual,
}

impl Operator {
    pub fn from(chars: &str) -> Option<Self> {
        Operator::iter().find(|&op| op.get_str("repr").unwrap() == chars)
    }

    pub fn contains(codepoint: char) -> bool {
        "!@#$%^&*()-_+=`~<>,./?;:'\"[]{}|\\".contains(codepoint)
    }
}
