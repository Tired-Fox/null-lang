use std::{borrow::Cow, ops::Range};

use crate::{error, lex::{Keyword, Token, TokenKind, Tokenizer, Number}, source};

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Bool(bool),
    Char(char),
    Number(Number),
    Str(Cow<'static, str>),
}

// TODO:
#[derive(Debug, Clone, PartialEq)]
pub struct Type;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'input> {
    Type(Type),
    Literal(Literal),
    Assignment {
        public: bool,
        constant: bool,
        ident: Cow<'input, str>,
        //_type: Option<Cow<'input, str>>,
        value: Box<Expr<'input>>,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ast<'input>(Vec<Expr<'input>>);

trait Parse<'input>: Sized {
    type Output;
    fn parse(tokenizer: &mut Tokenizer<'input>) -> Option<Result<Self::Output, crate::Error>>;
}

pub enum Either<A, B> {
    A(A),
    B(B),
}

impl<'input, A, B> Parse<'input> for Either<A, B>
where
    A: Parse<'input>,
    B: Parse<'input>,
{
    type Output = Either<A::Output, B::Output>;
    fn parse(tokenizer: &mut Tokenizer<'input>) -> Option<Result<Self::Output, crate::Error>> {
        match A::parse(tokenizer) {
            Some(Ok(a)) => return Some(Ok(Either::A(a))),
            Some(Err(err)) => return Some(Err(err)),
            None => {}
        }

        match B::parse(tokenizer) {
            Some(Ok(b)) => Some(Ok(Either::B(b))),
            Some(Err(err)) => Some(Err(err)),
            None => None,
        }
    } 
}

impl<'input> Parse<'input> for Token<'input> {
    type Output = Token<'input>;

    fn parse(tokenizer: &mut Tokenizer<'input>) -> Option<Result<Self::Output, crate::Error>> {
        tokenizer.next()
    }
}

pub struct Public;
pub struct Constant;
pub struct Identifier;
impl<'input> Parse<'input> for Constant {
    type Output = Token<'input>;

    fn parse(tokenizer: &mut Tokenizer<'input>) -> Option<Result<Self::Output, crate::Error>> {
        let pre = tokenizer.byte();
        let next = tokenizer.next();
        if !matches!(next, Some(Ok(Token { kind: TokenKind::Keyword(Keyword::Constant), .. }))) {
            return Some(Err(error!(crate::ErrorKind::InvalidSyntax, [(pre..tokenizer.byte(), "expected `const` keyword")])));
        }

        next
    }
}
impl<'input> Parse<'input> for Identifier {
    type Output = Token<'input>;

    fn parse(tokenizer: &mut Tokenizer<'input>) -> Option<Result<Self::Output, crate::Error>> {
        let pre = tokenizer.byte();
        let next = tokenizer.next();
        if !matches!(next, Some(Ok(Token { kind: TokenKind::Ident, .. }))) {
            return Some(Err(error!(crate::ErrorKind::InvalidSyntax, [(pre..tokenizer.byte(), "expected an identifier")])));
        }
        next
    }
}
impl<'input> Parse<'input> for Public {
    type Output = Token<'input>;

    fn parse(tokenizer: &mut Tokenizer<'input>) -> Option<Result<Self::Output, crate::Error>> {
        let pre = tokenizer.byte();
        let next = tokenizer.next();
        if !matches!(next, Some(Ok(Token { kind: TokenKind::Keyword(Keyword::Public), .. }))) {
            return Some(Err(error!(crate::ErrorKind::InvalidSyntax, [(pre..tokenizer.byte(), "expected `pub` keyword")])));
        }
        next
    }
}

trait Unravel: Sized {
    type Output;
    fn unravel(self, range: impl Into<Range<usize>>, message: impl std::fmt::Display) -> Result<Self::Output, crate::Error>;
}

impl<'input> Unravel for Option<Result<Token<'input>, crate::Error>> {
    type Output = Token<'input>;

    fn unravel(self, range: impl Into<Range<usize>>, message: impl std::fmt::Display) -> Result<Self::Output, crate::Error> {
        match self {
            Some(Ok(token)) => Ok(token),
            Some(Err(err)) => Err(err),
            None => Err(error!(crate::ErrorKind::InvalidSyntax, [(range.into(), message)])),
        }
    }
}

impl<A, B> Unravel for Option<Result<Either<A, B>, crate::Error>> {
    type Output = Either<A, B>;

    fn unravel(self, range: impl Into<Range<usize>>, message: impl std::fmt::Display) -> Result<Self::Output, crate::Error> {
        match self {
            Some(Ok(token)) => Ok(token),
            Some(Err(err)) => Err(err),
            None => Err(error!(crate::ErrorKind::InvalidSyntax, [(range.into(), message)])),
        }
    }
}

impl<'input> TryFrom<&str> for Ast<'input> {
    type Error = crate::Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        // TODO: Iterate and parse the tokens into expressions
        /*
            
            const x : u32 : 3;
            const x :: 3;
            x :: 3;

            <pub>? <const>? <identifier> : <type>? :  <value>

            <value>
                 - Const Literal
                 - Enum
                 - Struct
                 - Union
                 - Error
                 - Closure - fn(<key: type>,+) { <expr>;* }
        */
        let mut tokenizer = Tokenizer::new(value);

        let start = tokenizer.byte();

        let mut public = false;
        let mut ident = None;
        //let mut _type = None;

        match Either::<Public, Either<Constant, Identifier>>::parse(&mut tokenizer).unravel(start..tokenizer.byte(), "expected a statement") {
            Ok(Either::A(_)) => {
                println!("Public");
                public = true;
                let pre = tokenizer.byte();
                match Either::<Constant, Identifier>::parse(&mut tokenizer).unravel(pre..tokenizer.byte(), "expected a `const` keyword or an identifier") {
                    Ok(Either::A(Token { kind: TokenKind::Keyword(Keyword::Constant), .. })) => {
                        println!("Public Constant");
                        let pre = tokenizer.byte();
                        match Identifier::parse(&mut tokenizer).unravel(pre..tokenizer.byte(), "expected an identifier") {
                            Ok(Token { kind: TokenKind::Ident, repr, .. }) => {
                                println!("Public Constant Identifier");
                                ident = Some(repr.clone());
                            },
                            Err(err) => return Err(err),
                            _ => { unreachable!() }
                        }
                    },
                    Ok(Either::B(Token { kind: TokenKind::Ident, repr, .. })) => {
                        println!("Public Identifier");
                        ident = Some(repr.clone());
                    },
                    Err(err) => return Err(err),
                    _ => { unreachable!() }
                }

                let pre = tokenizer.byte();
                if !matches!(tokenizer.next(), Some(Ok(Token { kind: TokenKind::Colon, .. }))) {
                    return Err(error!(crate::ErrorKind::InvalidSyntax, [(pre..tokenizer.byte(), "expected `:` after identifier")]));
                }
                
                // TODO: <type>

                let pre = tokenizer.byte();
                if !matches!(tokenizer.next(), Some(Ok(Token { kind: TokenKind::Colon, .. }))) {
                    return Err(error!(crate::ErrorKind::InvalidSyntax, [(pre..tokenizer.byte(), "expected `:` after type")]));
                }

                // TODO: <expr>
            },
            Ok(Either::B(Either::A(Token { kind: TokenKind::Keyword(Keyword::Constant), .. }))) => {
                println!("Constant");
                let pre = tokenizer.byte();
                match Identifier::parse(&mut tokenizer).unravel(pre..tokenizer.byte(), "expected an identifier") {
                    Ok(Token { kind: TokenKind::Ident, repr, .. }) => {
                        println!("Constant Identifier");
                        ident = Some(repr.clone());
                    },
                    Err(err) => return Err(err),
                    _ => { unreachable!() }
                }

                let pre = tokenizer.byte();
                if !matches!(tokenizer.next(), Some(Ok(Token { kind: TokenKind::Colon, .. }))) {
                    return Err(error!(crate::ErrorKind::InvalidSyntax, [(pre..tokenizer.byte(), "expected `:` after identifier")]));
                }
                
                // TODO: <type>

                let pre = tokenizer.byte();
                if !matches!(tokenizer.next(), Some(Ok(Token { kind: TokenKind::Colon, .. }))) {
                    return Err(error!(crate::ErrorKind::InvalidSyntax, [(pre..tokenizer.byte(), "expected `:` after type")]));
                }
                
                // TODO: <expr>
            },
            Ok(Either::B(Either::B(Token { kind: TokenKind::Ident, repr, .. }))) => {
                println!("Identifier");
                ident = Some(repr.clone());

                let pre = tokenizer.byte();
                if !matches!(tokenizer.next(), Some(Ok(Token { kind: TokenKind::Colon, .. }))) {
                    return Err(error!(crate::ErrorKind::InvalidSyntax, [(pre..tokenizer.byte(), "expected `:` after identifier")]));
                }
                
                // TODO: <type>

                let pre = tokenizer.byte();
                if !matches!(tokenizer.next(), Some(Ok(Token { kind: TokenKind::Colon, .. }))) {
                    return Err(error!(crate::ErrorKind::InvalidSyntax, [(pre..tokenizer.byte(), "expected `:` after type")]));
                }
                
                // TODO: <expr>
            },
            Err(err) => return Err(err),
            _ => { unreachable!() }
        }

        println!("[pub: {public}] const {ident:?} :: <value>;");

        // identifier == Name of the variable
        // colon == :
        // type? == Optional explicit type
        // value == One of the many possible root level values
        Ok(Ast(Vec::new()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_const() {
        let source = "pub const x :: 3;";
        if let Err(err) = Ast::try_from(source) {
            println!("{}", err.with_source_code(source!(source)));
        }
    }
}

/*
Ast: List of statements (root level)
    - Expression: Conveys meaning (Literals, Variables, Operators, and function calls)
        - Statement
            - Expr + Semicolon
            - Struct (Create a new user defined type)
            - Enum
            - Union
            - Error
        - Block (list of expressions with a possible return value)
            - Array
            - Tuple
        - Loop
            - While
            - For

    ACCESS AND ASSIGNMENT:
        - Assignment
            - Constant (a :: 3, a: u32 = 3)
            - Let (a := 3, a: u32 = 3)
        - Literal (A literal constant value)
        - Path
        - Range
        - Unary (*a, !a: Access / manipulation of a value)
        - Binary (Math: a + b, a += b)
        - Reference (&a)
        - Cast

    CONTROL FLOW:
        - If
        - match
        - Return
        - Continue
        - Break
        - Defer
        - Closure
            - Paren (Group)
                - Field
            - Call

Return will return from the current block. This includes matches, branches, closures/functions.
    Loops will return from the parent block/context.
*/
