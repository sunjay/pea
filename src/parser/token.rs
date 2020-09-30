use std::fmt;
use std::sync::Arc;
use std::cmp::Ordering;

use crate::source_files::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Literal {
    Integer,
    BStr,
    Byte,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Literal::*;
        match self {
            Integer => write!(f, "an integer"),
            BStr => write!(f, "a byte string literal"),
            Byte => write!(f, "a byte literal"),
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

    /// The `[` symbol
    BracketOpen,
    /// The `]` symbol
    BracketClose,

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

    /// The `||` symbol
    OrOr,
    /// The `&&` symbol
    AndAnd,

    /// The `=` symbol
    Equals,
    /// The `!` symbol
    Not,
    /// The `,` symbol
    Comma,
    /// The `;` symbol
    Semicolon,
    /// The `:` symbol
    Colon,
    /// The `->` symbol
    RightArrow,
    /// The `.` symbol
    Dot,

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

    /// The `+=` symbol
    PlusEquals,
    /// The `-=` symbol
    MinusEquals,
    /// The `*=` symbol
    TimesEquals,
    /// The `/=` symbol
    SlashEquals,
    /// The `%=` symbol
    PercentEquals,

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

            BracketOpen => write!(f, "`[`"),
            BracketClose => write!(f, "`]`"),

            BraceOpen => write!(f, "`{{`"),
            BraceClose => write!(f, "`}}`"),

            EqualsEquals => write!(f, "`==`"),
            NotEquals => write!(f, "`!=`"),
            GreaterThan => write!(f, "`>`"),
            GreaterThanEquals => write!(f, "`>=`"),
            LessThan => write!(f, "`<`"),
            LessThanEquals => write!(f, "`<=`"),

            OrOr => write!(f, "`||`"),
            AndAnd => write!(f, "`&&`"),

            Equals => write!(f, "`=`"),
            Not => write!(f, "`!`"),
            Comma => write!(f, "`,`"),
            Semicolon => write!(f, "`;`"),
            Colon => write!(f, "`:`"),
            RightArrow => write!(f, "`->`"),
            Dot => write!(f, "`.`"),

            Plus => write!(f, "`+`"),
            Minus => write!(f, "`-`"),
            Times => write!(f, "`*`"),
            Slash => write!(f, "`/`"),
            Percent => write!(f, "`%`"),

            PlusEquals => write!(f, "`+=`"),
            MinusEquals => write!(f, "`-=`"),
            TimesEquals => write!(f, "`*=`"),
            SlashEquals => write!(f, "`/=`"),
            PercentEquals => write!(f, "`%=`"),

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
    BStr(Arc<[u8]>),
    /// The unescaped byte value of a byte literal
    Byte(u8),
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
        TokenValue::BStr(value)
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
    pub fn unwrap_bstr(&self) -> &Arc<[u8]> {
        match &self.value {
            Some(TokenValue::BStr(value)) => value,
            _ => panic!("bug: expected a byte string"),
        }
    }

    /// Returns the data as a byte literal or panics
    pub fn unwrap_byte(&self) -> u8 {
        match &self.value {
            &Some(TokenValue::Byte(value)) => value,
            _ => panic!("bug: expected a byte literal"),
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
    If : "if"
    Else : "else"
    While : "while"
    Loop : "loop"
    Break : "break"
    Continue : "continue"

    //TODO: This should be removed when println stops being used as a keyword
    Println : "println"
    //TODO: This should be removed when print stops being used as a keyword
    Print : "print"
}
