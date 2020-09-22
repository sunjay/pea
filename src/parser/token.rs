use std::fmt;
use std::sync::Arc;
use std::cmp::Ordering;

use crate::source_files::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Literal {
    Integer,
    Bytes,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Literal::*;
        match self {
            Integer => write!(f, "an integer"),
            Bytes => write!(f, "a byte string"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
    /// An identifier
    Ident,
    /// A keyword reserved by the language
    Keyword(Keyword),
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

    /// The `==` symbol
    EqualsEquals,
    /// The `!=` symbol
    NotEquals,
    /// The `>` symbol
    GreaterThan,
    /// The `>=` symbol
    GreaterThanEquals,
    /// The `<` symbol
    LessThan,
    /// The `<=` symbol
    LessThanEquals,

    /// The `=` symbol
    Equals,
    /// The `!` symbol
    Not,
    /// The `,` symbol
    Comma,
    /// The `;` symbol
    Semicolon,

    /// The `+` symbol
    Plus,
    /// The `-` symbol
    Minus,
    /// The `*` symbol
    Times,
    /// The `/` symbol
    Slash,
    /// The `%` symbol
    Percent,

    /// End-of-file
    Eof,

    /// A placeholder for an invalid token, propagated to avoid cascading error messages and to
    /// allow the lexer to continue past errors
    Error,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TokenKind::*;
        match self {
            Ident => write!(f, "an identifier"),
            Keyword(keyword) => write!(f, "{}", keyword),
            Literal(literal) => write!(f, "{}", literal),

            ParenOpen => write!(f, "`(`"),
            ParenClose => write!(f, "`)`"),

            BraceOpen => write!(f, "`{{`"),
            BraceClose => write!(f, "`}}`"),

            EqualsEquals => write!(f, "`==`"),
            NotEquals => write!(f, "`!=`"),
            GreaterThan => write!(f, "`>`"),
            GreaterThanEquals => write!(f, "`>=`"),
            LessThan => write!(f, "`<`"),
            LessThanEquals => write!(f, "`<=`"),

            Equals => write!(f, "`=`"),
            Not => write!(f, "`!`"),
            Comma => write!(f, "`,`"),
            Semicolon => write!(f, "`;`"),

            Plus => write!(f, "`+`"),
            Minus => write!(f, "`-`"),
            Times => write!(f, "`*`"),
            Slash => write!(f, "`/`"),
            Percent => write!(f, "`%`"),

            Eof => write!(f, "end of file"),

            Error => unreachable!("The Error token kind should not be formatted"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenValue {
    Ident(Arc<str>),
    Integer(i128),
    /// The unescaped characters of the byte string (without the surrounding double quotes)
    Bytes(Arc<[u8]>),
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

impl From<Arc<[u8]>> for TokenValue {
    fn from(value: Arc<[u8]>) -> Self {
        TokenValue::Bytes(value)
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

    /// Returns the data as a byte string or panics
    pub fn unwrap_bytes(&self) -> &Arc<[u8]> {
        match &self.value {
            Some(TokenValue::Bytes(value)) => value,
            _ => panic!("bug: expected a byte string"),
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
    Let : "let"
    Return : "return"
    True : "true"
    False : "false"

    //TODO: This should be removed when println stops being used as a keyword
    Println : "println"
}
