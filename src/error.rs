use std::{cmp::Ordering, ops::Range, path::Path};

use codesnake::{Block, CodeWidth, LineIndex};
use owo_colors::{OwoColorize, Stream, Style};

use crate::Span; 

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label {
    pub at: Span,
    pub msg: Option<String>
}

impl Label {
    pub fn new<S: Into<Span>, M: std::fmt::Display>(at: S, msg: M) -> Self {
        Self {
            at: at.into(),
            msg: Some(msg.to_string())
        }
    }

    pub fn span(&self) -> Span {
        self.at
    }

    pub fn message(&self) -> Option<&str> {
        self.msg.as_deref()
    }
}

impl From<&Label> for codesnake::Label<Range<usize>, String> {
    fn from(value: &Label) -> Self {
        let range = value.span().range();
        match value.msg.as_ref() {
            Some(msg) => codesnake::Label::new(range).with_text(msg.clone()),
            None => codesnake::Label::new(range)
        }
    }
}

impl PartialOrd for Label {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.at.partial_cmp(&other.at) 
    }
}

impl Ord for Label {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl<S: Into<Span>> From<S> for Label {
    fn from(value: S) -> Self {
        Self {
            at: value.into(),
            msg: None
        }
    }
}

impl<S: Into<Span>, M: std::fmt::Display> From<(S, M)> for Label {
    fn from((span, msg): (S, M)) -> Self {
        Self {
            at: span.into(),
            msg: Some(msg.to_string())
        }
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Severity {
    #[default]
    Error,
    Warning,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Source {
    name: Option<String>,
    source: String,
}

impl Source {
    pub fn new<N: AsRef<Path>, S: std::fmt::Display>(name: Option<N>, source: S) -> Self {
        Self { 
            name: name.map(|v| v.as_ref().display().to_string()), 
            source: source.to_string()
        }
    }

    /// Get the line and column number of where the span starts
    pub fn start_pos(&self, span: Span) -> (usize, usize) {
        (
            self.source[..=span.start].lines().count().max(1),
            self.source[..=span.start].rsplit_once('\n').map(|v| v.1.len()).unwrap_or(1)
        )
    }

    /// Get the line and column number of where the span ends
    pub fn end_pos(&self, span: Span) -> (usize, usize) {
        (
            self.source[..span.end].lines().count().max(1),
            self.source[..span.end].rsplit_once('\n').map(|v| v.1.len() + 1).unwrap_or(1)
        )
    }

    /// Get the line and column pairs for where the span starts and ends
    pub fn pos(&self, span: Span) -> [(usize, usize);2] {
        [
            self.start_pos(span),
            self.end_pos(span),
        ]
    }

    pub fn line(&self, line: usize) -> Option<&str> {
        self.source.lines().nth(line.saturating_sub(1))
    }
}

#[derive(Default, Clone, thiserror::Error, PartialEq, Eq)]
#[error("{kind}")]
pub struct Error {
    kind: ErrorKind,
    level: Severity,
    labels: Vec<Label>,
    src: Option<Source>,
}

impl Error {
    pub fn error(kind: ErrorKind, labels: impl IntoIterator<Item=Label>) -> Self {
        Self {
            kind,
            labels: labels.into_iter().collect(),
            ..Default::default()
        }
    }

    pub fn warn(kind: ErrorKind, labels: impl IntoIterator<Item=Label>) -> Self {
        Self {
            kind,
            level: Severity::Warning,
            labels: labels.into_iter().collect(),
            ..Default::default()
        }
    }

    pub fn with_source_code(mut self, source: impl Into<Source>) -> Self {
        self.src = Some(source.into());
        self
    }

    pub fn code(&self) -> usize {
        self.kind.code()
    }

    pub fn source_code(&self) -> Option<&Source> {
        self.src.as_ref()
    }
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let color = match self.level {
            Severity::Error => {
                write!(f, "{}", format!("[E{:0>3}] Error: ", self.code()).if_supports_color(Stream::Stderr, |text| text.style(Style::default().bold().red())))?;
                Style::default().red()
            },
            Severity::Warning => {
                write!(f, "{}", "Warn: ".if_supports_color(Stream::Stderr, |text| text.style(Style::default().bold().yellow())))?;
                Style::default().yellow()
            }
        };

        write!(f, "{}", self.if_supports_color(Stream::Stderr, |text| text.bold()))?;

        if let (Some(src), false) = (self.source_code(), self.labels.is_empty()) {
            let idx = LineIndex::new(src.source.as_str());
            let mut labels = self.labels.iter().map(|l| l.span()).collect::<Vec<_>>();
            labels.sort();

            let (line, column) = src.start_pos(*labels.first().unwrap());

            match Block::new(
                &idx,
                self.labels
                    .iter()
                    .map(|label| {
                        codesnake::Label::from(label).with_style(move |t| t.style(color).to_string())
                    })
            ) {
                Some(block) => {
                    let block = block.map_code(|s| CodeWidth::new(s, s.len()));
                    writeln!(f, "\n{} {}", block.prologue(), match src.name.as_deref() {
                        Some(name) => format!("{name}:{line}:{column}"),
                        None => format!("??? {line}:{column}"),
                    })?;
                    write!(f, "{block}")?;
                    writeln!(f, "{}", block.epilogue())?;
                },
                None => {
                    write!(f, "Failed to build code block")?;
                    writeln!(f, "\n{}", match src.name.as_deref() {
                        Some(name) => format!("{name}:{line}:{column}"),
                        None => format!("??? {line}:{column}"),
                    })?;
                }
            }
        }
        writeln!(f)
    }
}

#[derive(Default, Debug, Clone, thiserror::Error, PartialEq, Eq)]
pub enum ErrorKind {
    #[error("unkown")]
    #[default]
    Unkown,
    #[error("invalid syntax")]
    InvalidSyntax,
    #[error("invalid escape sequence")]
    InvalidEscapeSequence,
    #[error("invalid number")]
    InvalidNumber,
    #[error("unterminated string")]
    UnterminatedString,
    #[error("unterminated char")]
    UnterminatedChar,
}
impl ErrorKind {
    pub fn code(&self) -> usize {
        match self {
            Self::Unkown => 0,
            Self::InvalidSyntax => 1,
            Self::InvalidEscapeSequence => 2,
            Self::InvalidNumber => 3,
            Self::UnterminatedString => 100,
            Self::UnterminatedChar => 101,
        }
    }
}

#[macro_export]
macro_rules! source {
    ($src:expr) => {
        $crate::Source::new(None::<String>, $src)
    };
    ($path: expr, $src:expr) => {
        $crate::Source::new(Some($path), $src)
    };
}

#[macro_export]
macro_rules! error {
    ($kind: expr) => {
        $crate::Error::error($kind, []) 
    };
    ($kind: expr, [$($label: expr),+ $(,)?] $(,)?) => {
        $crate::Error::error($kind, [$($label.into(),)+]) 
    };
}

#[macro_export]
macro_rules! warn {
    ($kind: expr) => {
        $crate::Error::warn($kind, []) 
    };
    ($kind: expr, [$($label: expr),+ $(,)?] $(,)?) => {
        $crate::Error::warn($kind, [$($label.into(),)+]) 
    };
}
