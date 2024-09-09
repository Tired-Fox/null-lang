#[derive(Debug, Clone)]
pub enum Error {
    UnkownKeyword,
    UnkownCharacter,
}
impl std::error::Error for Error {}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnkownKeyword => write!(f, "unkown keyword"),
            Self::UnkownCharacter => write!(f, "unkown character"),
        }
    }
}

