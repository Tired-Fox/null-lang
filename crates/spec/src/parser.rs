use crate::error::{InternalError, Result};

pub struct Parser<I> {
    inner: I,
    position: usize,
}

impl<I> Parser<I> {
    pub fn position(&self) -> usize {
        self.position
    }
}

impl<I: Iterator<Item = u8>> Iterator for Parser<I> {
    type Item = Result<char>;

    fn next(&mut self) -> Option<Self::Item> {
        match next_code_point(&mut self.inner) {
            Ok(v) => match v {
                Some(ch) => {
                    let ch = unsafe { char::from_u32_unchecked(ch) };
                    self.position += ch.len_utf8();
                    Some(Ok(ch))
                },
                None => None
            },
            Err(e) => Some(Err(e)),
        }
    }
}

impl<I> std::fmt::Debug for Parser<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Parser")
            .field("position", &self.position)
            .finish_non_exhaustive()
    }
}

impl<I> From<I> for Parser<I> {
    fn from(value: I) -> Self {
        Self {
            inner: value,
            position: 0,
        }
    }
}

impl<I> From<(usize, I)> for Parser<I> {
    fn from((position, value): (usize, I)) -> Self {
        Self {
            inner: value,
            position,
        }
    }
}

/// Reads the next code point out of a byte iterator (assuming a
/// UTF-8-like encoding).
#[inline]
pub fn next_code_point<I: Iterator<Item = u8>>(bytes: &mut I) -> Result<Option<u32>> {
    let x = match bytes.next() {
        Some(b) => b,
        None => return Ok(None)
    };

    if x < 128 {
        return Ok(Some(x as u32));
    }

    let init = (x & (0x7F >> 2)) as u32;
    let y = bytes.next().ok_or(InternalError::Utf8)?;
    let mut ch = (init << 6) | (y & 0x3F) as u32;

    if x >= 0xE0 {
        let z = bytes.next().ok_or(InternalError::Utf8)?;
        let y_z = (((y & 0x3F) as u32) << 6) | (z & 0x3F) as u32;
        ch = init << 12 | y_z;
        if x >= 0xF0 {
            let w = bytes.next().ok_or(InternalError::Utf8)?;
            ch = (init & 7) << 18 | ((y_z << 6) | (w & 0x3F) as u32);
        }
    }

    Ok(Some(ch))
}

#[cfg(test)]
mod test {
    use std::io::{BufReader, Read};

    use tempdir::TempDir;

    use crate::error::Result;

    use super::Parser;

    static SOURCE: &str = "The brown fox jumped over the lazy dog 😊";

    #[test]
    fn str_to_utf8_iter() {
        let parser = Parser::from(SOURCE.bytes());
        assert!(matches!(parser.collect::<Result<String>>().as_deref(), Ok(s) if s == SOURCE));
    }

    #[test]
    fn file_to_utf8_iter() -> Result<()> {
        let dir = TempDir::new("null-lang-tests")?;
        let file_path = dir.path().join("code.txt");
        std::fs::write(&file_path, SOURCE)?;

        let parser = Parser::from(
            BufReader::new(std::fs::OpenOptions::new().read(true).open(&file_path)?)
                .bytes()
                .map_while(std::result::Result::ok)
        );

        assert!(matches!(parser.collect::<Result<String>>().as_deref(), Ok(s) if s == SOURCE));

        Ok(())
    }
}
