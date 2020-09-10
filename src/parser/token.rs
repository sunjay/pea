use std::fmt;
use std::sync::Arc;
use std::cmp::Ordering;

use crate::source_files::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Literal {
    Integer,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    /// A keyword reserved by the language
    Keyword(Keyword),
    /// An identifier
    Ident,
    /// A literal, e.g. `122`, `"abc"`, etc.
    Literal(Literal),

    /// The `(` symbol
    ParenOpen,
    /// The `)` symbol
    ParenClose,

    /// The `{` symbol
    BraceOpen,
    /// The `}` symbol
    BraceClose,

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
pub enum TokenValue {
    Ident(Arc<str>),
    Integer(i128),
}

impl From<i128> for TokenValue {
    fn from(value: i128) -> Self {
        TokenValue::Integer(value)
    }
}

impl From<Arc<str>> for TokenValue {
    fn from(value: Arc<str>) -> Self {
        TokenValue::Ident(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub value: Option<TokenValue>,
}

impl Token {
    /// Returns the data as an identifier or panics
    pub fn unwrap_ident(&self) -> &Arc<str> {
        match &self.value {
            Some(TokenValue::Ident(ident)) => ident,
            _ => panic!("bug: expected an identifier"),
        }
    }

    /// Returns the data as an integer and its suffix or panics
    pub fn unwrap_integer(&self) -> i128 {
        match &self.value {
            &Some(TokenValue::Integer(value)) => value,
            _ => panic!("bug: expected an integer"),
        }
    }
}

macro_rules! keywords {
    ($($variant:ident : $kw:literal)*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, Hash)]
        pub enum Keyword {
            $($variant),*
        }

        impl PartialOrd for Keyword {
            fn partial_cmp(&self, _: &Self) -> Option<Ordering> {
                // No inherent ordering between keywords
                Some(Ordering::Equal)
            }
        }

        impl Keyword {
            pub fn from_str(ident: &str) -> Option<Keyword> {
                use Keyword::*;
                match ident {
                    $($kw => Some($variant),)*
                    _ => None,
                }
            }
        }

        impl fmt::Display for Keyword {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                use Keyword::*;
                write!(f, "{}", match self {
                    $($variant => concat!("`", $kw, "`"),)*
                })
            }
        }
    };
}

keywords! {
    Fn : "fn"

    //TODO: This should be removed when println stops being used as a keyword
    Println : "println"
}