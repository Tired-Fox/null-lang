use std::{
    io::{BufRead, BufReader, Seek},
    ops::Range,
    path::PathBuf,
};

use crate::error::InternalError;

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Context {
    pub(crate) before: usize,
    pub(crate) after: usize,
}
impl Context {
    pub fn new(before: usize, after: usize) -> Self {
        Self { before, after }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub struct Chunk {
    pub(crate) data: Vec<u8>,
    pub(crate) span: Range<usize>,
    pub(crate) line: usize,
    pub(crate) column: usize,
    pub(crate) height: usize,
    pub(crate) context: Context,
    pub(crate) name: Option<String>,
    pub(crate) language: Option<String>,
}

pub trait Source {
    fn chunk(&self, span: Range<usize>, context: Context) -> Result<Chunk, InternalError>;
}

impl Source for &str {
    fn chunk(&self, span: Range<usize>, context: Context) -> Result<Chunk, InternalError> {
        if span.end >= self.len() {
            return Err(InternalError::OutOfBounds);
        }

        let before = &self[0..span.start];
        let after = &self[span.end..];

        let start = if context.before > 0 {
            span.start.saturating_sub(
                before
                    .rsplit("\n")
                    .take(context.before + 1)
                    .collect::<Vec<_>>()
                    .join("\n")
                    .len(),
            )
        } else {
            span.start
        };

        let end = if context.after > 0 {
            (span.end
                + after
                    .split("\n")
                    .take(context.after + 1)
                    .collect::<Vec<_>>()
                    .join("\n")
                    .len())
            .min(self.len())
        } else {
            span.end
        };

        let line = before.lines().count();
        let column = before.rsplit("\n").next().unwrap().len();

        let slice = &self[start..end];
        let height = slice.lines().count();
        let before_height = self[start..span.start].lines().count().saturating_sub(1);
        Ok(Chunk {
            data: slice.as_bytes().to_vec(),
            span: span.start.saturating_sub(start + 1)
                ..span
                    .start
                    .saturating_sub(start + 1)
                    .saturating_add(span.len()),
            line,
            context: Context { before: before_height, after: height - before_height - 1 },
            column,
            height,
            name: None,
            language: None,
        })
    }
}

impl Source for PathBuf {
    fn chunk(&self, span: Range<usize>, context: Context) -> Result<Chunk, InternalError> {
        let mut file = std::fs::OpenOptions::new().read(true).open(self)?;

        let reader = BufReader::new(&mut file);
        let mut line: usize = 0;
        let mut bytes: usize = 0;
        for l in reader.lines() {
            line += 1;
            let length = bytes.saturating_add(l?.len() + 1);
            if length >= span.start {
                break;
            }
            bytes = length;
        }
        let column = span.start.saturating_sub(bytes);

        let diff_before = line.saturating_sub(context.before + 1);
        let before = line.saturating_sub(diff_before).min(context.before);

        file.rewind()?;
        let reader = BufReader::new(&mut file);
        let chunk = reader
            .lines()
            .map_while(Result::ok)
            .skip(diff_before)
            .take(before + context.after + 1)
            .collect::<Vec<_>>()
            .join("\n");
        let height = chunk.lines().count();

        let start = chunk
            .lines()
            .take(before)
            .collect::<Vec<_>>()
            .join("\n")
            .len()
            + column;

        Ok(Chunk {
            data: chunk.into_bytes(),
            span: start..start + span.len(),
            line,
            column,
            context: Context { before, after: height - before - 1 },
            height,
            name: Some(self.display().to_string()),
            language: None,
        })
    }
}

#[cfg(test)]
mod test {
    use tempdir::TempDir;

    use super::{Context, Source};

    static SOURCE: &str = r#"[workspace]
resolver = "2"
members = [
    "crtes/*"
]"#;

    #[test]
    fn source_str() {
        let chunk = SOURCE.chunk(
            45..50,
            Context {
                before: 2,
                after: 1,
            },
        );

        assert!(chunk.is_ok());
        let chunk = chunk.unwrap();
        assert!(chunk.data.len() == 42);
        assert!(chunk.span.start == 32);
        assert!(chunk.span.end == 37);
        assert!(chunk.line == 4);
        assert!(chunk.column == 6);
        assert!(chunk.height == 4);
        println!("{:?}", chunk.context);
        assert!(chunk.context == Context { before: 2, after: 1 });
        assert!(chunk.name.is_none());
        assert!(chunk.language.is_none());
    }

    #[test]
    fn source_file() -> std::io::Result<()> {
        let dir = TempDir::new("null-lang-tests")?;
        let file_path = dir.path().join("Cargo.toml");

        std::fs::write(&file_path, SOURCE)?;

        let chunk = file_path.chunk(
            45..50,
            Context {
                before: 2,
                after: 1,
            },
        );

        assert!(chunk.is_ok());
        let chunk = chunk.unwrap();
        assert!(chunk.data.len() == 42);
        assert!(chunk.span.start == 32);
        assert!(chunk.span.end == 37);
        assert!(chunk.line == 4);
        assert!(chunk.column == 6);
        assert!(chunk.height == 4);
        assert!(chunk.context == Context { before: 2, after: 1 });
        assert!(chunk.name == Some(file_path.display().to_string()));
        assert!(chunk.language.is_none());

        Ok(())
    }
}
