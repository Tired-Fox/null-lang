use crate::source::Chunk;

pub type Result<T> = std::result::Result<T, Error>;

pub struct Error {
    inner: Box<dyn Diagnostic>,
}
unsafe impl Sync for Error {}
unsafe impl Send for Error {}
impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (style, style_reset) = match self.inner.severity() {
            Some(Severity::Help) => ("\x1b[35m", "\x1b[39m"),
            Some(Severity::Warning) => ("\x1b[33m", "\x1b[39m"),
            Some(Severity::Error) => ("\x1b[31m", "\x1b[39m"),
            None => ("", ""),
        };

        if let Some(code) = self.inner.code() {
            write!(f, "{style}{code}{style_reset} {}", self.inner)?;
        } else {
            write!(f, "{style}{}{style_reset}", self.inner)?;
        }

        if let Some(chunk) = self.inner.chunk() {
            write!(f, "\n  ")?;
            if let Some(name) = chunk.name.as_deref() {
                write!(f, "{name} ")?;
            }
            writeln!(f, "[{}:{}]", chunk.line, chunk.column)?;

            let start = chunk.line.saturating_sub(chunk.context.before);
            let max_w_num = chunk.line.saturating_add(chunk.context.after).to_string().len();

            for (i, line) in String::from_utf8_lossy(&chunk.data).lines().enumerate() {
                if start + i == chunk.line {
                    let column = chunk.column.saturating_sub(1);
                    write!(f, "  {: >max_w$} | {}", start + i, &line[0..column], max_w=max_w_num)?;
                    write!(f, "{style}{}{style_reset}", &line[column..column + chunk.span.len()])?;
                    writeln!(f, "{}", &line[column + chunk.span.len()..])?;
                } else {
                    writeln!(f, "  {: >max_w$} | {line}", start + i, max_w=max_w_num)?;
                }
            }
        }

        // TODO: Severity, url, source, related

        Ok(())
    }
}
impl<D: Diagnostic + 'static> From<D> for Error {
    fn from(value: D) -> Self {
        Self {
            inner: Box::new(value),
        }
    }
}

pub enum Severity {
    Help,
    Warning,
    Error,
}

pub trait Diagnostic: std::error::Error {
    fn code<'code>(&'code self) -> Option<Box<dyn std::fmt::Display + 'code>> {
        None
    }

    fn severity(&self) -> Option<Severity> {
        None
    }

    fn url<'url>(&'url self) -> Option<Box<dyn std::fmt::Display + 'url>> {
        None
    }

    fn chunk(&self) -> Option<&Chunk> {
        None
    }

    fn related<'related>(
        &'related self,
    ) -> Option<Box<dyn Iterator<Item = &'related dyn Diagnostic> + 'related>> {
        None
    }
}

#[derive(Debug)]
pub enum InternalError {
    IoError(std::io::Error),
    OutOfBounds,
}
impl std::fmt::Display for InternalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IoError(error) => write!(f, "{error}"),
            Self::OutOfBounds => write!(f, "span is out of bounds of the source"),
        }
    }
}
impl std::error::Error for InternalError {}
impl Diagnostic for InternalError {
    fn code<'code>(&'code self) -> Option<Box<dyn std::fmt::Display + 'code>> {
        Some(Box::new("Internal"))
    }

    // TODO: URL
}
impl From<std::io::Error> for InternalError {
    fn from(value: std::io::Error) -> Self {
        Self::IoError(value)
    }
}
