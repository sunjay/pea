use std::sync::Arc;

use crate::source_files::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Keyword {
    //TODO: This should be removed when println stops being used as a keyword
    Println,
}

impl Keyword {
    pub fn from_str(s: &str) -> Option<Self> {
        use Keyword::*;
        Some(match s {
            "println" => Println,
            _ => return None,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i128),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    /// A keyword reserved by the language
    Keyword(Keyword),
    /// An identifier
    Ident(Arc<str>),
    /// A literal, e.g. `122`, `"abc"`, etc.
    Literal(Literal),

    /// The `(` symbol
    ParenOpen,
    /// The `)` symbol
    ParenClose,

    /// The `!` symbol
    Not,
    /// The `;` symbol
    Semicolon,

    /// End-of-file
    Eof,

    /// A placeholder for an invalid token, propagated to avoid cascading error messages and to
    /// allow the lexer to continue past errors
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}
