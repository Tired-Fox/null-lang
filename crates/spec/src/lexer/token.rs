use std::borrow::Cow;

pub enum Token<'token> {
    Operator(Operator),
    Keyword(Keyword),
    Ident(Cow<'token, str>),
    String(Cow<'token, str>),
    Number(Cow<'token, str>),
    Rune(char),
    Character(char),
}

pub enum Keyword {
    Fn,
    For,
    If,
    Else,
    Struct,
    And,
    Or
}


/// `- + = [] {} () <> , . ? / \\ | _ ~ ! @ # $ % ^ & * ' " ~` `
///
/// `-= += *= /= != == <<= >>= << >> && || %= ^= ~=`
pub enum Operator {
    Minus,
    Plus,
    Equal,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    LessThan,
    GreaterThan,
    Comma,
    Period,
    QuestionMark,
    Divide,
    Slash,
    Underscore,
    Backtick,
    At,
    Hash,
    Dollar,
    Percent,
    Bang,
    Star,
    Quote,
    DoubleQuote,

    BitwiseXor,
    BitwiseOr,
    BitwiseNot,
    BitwiseAnd,
    BitwiseShiftLeft,
    BitwiseShiftRight,

    BitwiseXorEqual,
    BitwiseOrEqual,
    BitwiseNotEqual,
    BitwiseAndEqual,
    BitwiseShiftLeftEqual,
    BitwiseShiftRightEqual,

    MinusEqual,
    PlusEqual,
    StarEqual,
    DivideEqual,
    BangEqual,
    EqualEqual,
    PercentEqual,
}
