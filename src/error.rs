use std::{cmp::Ordering, collections::HashMap};

use owo_colors::{colors::xterm::Gray, OwoColorize, Stream, Style};

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
    pub fn new<N: std::fmt::Display, S: std::fmt::Display>(name: Option<N>, source: S) -> Self {
        Self { 
            name: name.map(|v| v.to_string()), 
            source: source.to_string()
        }
    }

    /// Get the line and column number of where the span starts
    pub fn start_pos(&self, span: Span) -> (usize, usize) {
        (
            self.source[..span.start].lines().count(),
            self.source[..span.start].rsplit_once('\n').unwrap().1.len() + 1
        )
    }

    /// Get the line and column number of where the span ends
    pub fn end_pos(&self, span: Span) -> (usize, usize) {
        (
            self.source[..span.end].lines().count(),
            self.source[..span.end].rsplit_once('\n').unwrap().1.len() + 1
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
        self.source.lines().nth(line)
    }
}

#[derive(Default, Clone, thiserror::Error, PartialEq, Eq)]
#[error("{kind}")]
pub struct Error {
    kind: ErrorKind,
    level: Severity,
    label: Option<Label>,
    src: Option<Source>,
}

impl Error {
    pub fn error(kind: ErrorKind, label: Option<Label>) -> Self {
        Self {
            kind,
            label,
            ..Default::default()
        }
    }

    pub fn warn(kind: ErrorKind, label: Option<Label>) -> Self {
        Self {
            kind,
            level: Severity::Warning,
            label,
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
        if let (Some(src), Some(label)) = (self.source_code(), self.label.as_ref()) {
            let (line, column) = src.start_pos(label.span());
            
            let [start, end] = src.pos(label.span());
            let lines = (start.0..=end.0).map(|v| src.line(v.saturating_sub(1)).unwrap()).collect::<Vec<_>>();
            let line_num_width = format!("{}", end.0).len();

            let then = '┊'.if_supports_color(Stream::Stderr, |text| text.fg::<Gray>()).to_string();
            let sep = '│'.if_supports_color(Stream::Stderr, |text| text.fg::<Gray>()).to_string();
            let spacer = (0..line_num_width).map(|_| ' ').collect::<String>();

            write!(f, "\n{spacer} {} {}",
                "╭─".if_supports_color(Stream::Stderr, |text| text.fg::<Gray>()),
                match src.name.as_deref() {
                    Some(name) => format!("{name}:{line}:{column}"),
                    None => format!("{line}:{column}"),
                },
            )?;
            
            let msg = label.message();
            let remain = end.1.saturating_sub(1).saturating_sub(start.1);
            let (left, right) = if remain == 0 {
                (0, 0)
            } else {
                (remain / 2, remain - (remain/2))
            };

            // TODO: Multi line span

            let line = lines.first().unwrap();
            write!(f, "\n{} {sep} {}",
                start.0,
                &line[..end.1.saturating_add(10).min(line.len())],
            )?;
            write!(f, "\n{spacer} {then} {}{}{}{}",
                (0..end.1-2).map(|_| ' ').collect::<String>(),
                (0..left).map(|_| '─').collect::<String>().if_supports_color(Stream::Stderr, |text| text.style(color)),
                match msg {
                    Some(_) => '┬',
                    None => '─',
                }.if_supports_color(Stream::Stderr, |text| text.style(color)),
                (0..right).map(|_| '─').collect::<String>().if_supports_color(Stream::Stderr, |text| text.style(color)),
            )?;
            if let Some(msg) = msg {
                write!(f, "\n{spacer} {then} {}{}{} {}",
                    (0..end.1-2).map(|_| ' ').collect::<String>(),
                    (0..left).map(|_| ' ').collect::<String>(),
                    "╰─".if_supports_color(Stream::Stderr, |text| text.style(color)),
                    msg
                )?;
            }

            write!(f, "\n{spacer} {}",
                "╰─".if_supports_color(Stream::Stderr, |text| text.fg::<Gray>()),
            )?;
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
}
impl ErrorKind {
    pub fn code(&self) -> usize {
        match self {
            Self::Unkown => 0,
            Self::InvalidSyntax { .. } => 1,
        }
    }
}

#[macro_export]
macro_rules! source {
    ($src:expr) => {
        $crate::error::Source::new(None::<String>, $src)
    };
    ($path: expr, $src:expr) => {
        $crate::error::Source::new(Some($path), $src)
    };
}
