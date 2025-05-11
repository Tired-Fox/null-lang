use spec::{error::{Diagnostic, Result, Severity}, source::{Chunk, Context, Source}};

#[allow(dead_code)]
#[derive(Debug)]
struct Error {
    code: u32,
    message: String,
    chunk: Chunk,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}
impl std::error::Error for Error {}
impl Diagnostic for Error {
    fn code<'code>(&'code self) -> Option<Box<dyn std::fmt::Display + 'code>> {
        Some(Box::new(format!("E{}", self.code)))
    }
    fn severity(&self) -> Option<Severity> {
        Some(Severity::Error)
    }
    fn chunk(&self) -> Option<&Chunk> {
        Some(&self.chunk)
    }
}

static SOURCE: &str = r#"[workspace]
resolver = "2"
members = [
    "crtes/*"
]"#;

fn main() -> Result<()> {
    Err(Error {
        code: 115,
        message: "unknown directory 'crtes'".into(),
        chunk: SOURCE.chunk(45..50, Context::new(2, 1))?
    }.into())
}
