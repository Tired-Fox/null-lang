#[derive(Debug, Clone)]
pub enum Error {
    UnkownKeyword,
    UnexpectedCharacter(char),
}
impl std::error::Error for Error {}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnkownKeyword => write!(f, "unkown keyword"),
            Self::UnexpectedCharacter(c) => write!(f, "unkown character: {c}"),
        }
    }
}

