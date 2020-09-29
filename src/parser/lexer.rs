use std::sync::Arc;
use std::collections::HashSet;

use crate::diagnostics::Diagnostics;

use super::{
    scanner::Scanner,
    token::{self, Token, TokenKind, TokenValue},
};

use TokenKind::*;

pub struct Lexer<'a> {
    scanner: Scanner<'a>,
    diag: &'a Diagnostics,
    interned_strings: HashSet<Arc<str>>,
}

impl<'a> Lexer<'a> {
    pub fn new(scanner: Scanner<'a>, diag: &'a Diagnostics) -> Self {
        Self {
            scanner,
            diag,
            interned_strings: HashSet::new(),
        }
    }

    /// Returns the next token in the input
    pub fn next(&mut self) -> Token {
        self.ignore_whitespace_comments();

        let start = self.scanner.current_pos();
        let current_char = match self.scanner.next() {
            Some(current_char) => current_char,
            None => return self.empty_token(start, Eof),
        };

        // Denying this helps avoid missing a warning that is quite important here
        #[deny(unreachable_patterns)]
        match (current_char, self.scanner.peek()) {
            (b'(', _) => self.byte_token(start, ParenOpen),
            (b')', _) => self.byte_token(start, ParenClose),
            (b'[', _) => self.byte_token(start, BracketOpen),
            (b']', _) => self.byte_token(start, BracketClose),
            (b'{', _) => self.byte_token(start, BraceOpen),
            (b'}', _) => self.byte_token(start, BraceClose),

            (b'=', Some(b'=')) => self.next_token(start, EqualsEquals),
            (b'!', Some(b'=')) => self.next_token(start, NotEquals),
            (b'>', Some(b'=')) => self.next_token(start, GreaterThanEquals),
            (b'>', _) => self.byte_token(start, GreaterThan),
            (b'<', Some(b'=')) => self.next_token(start, LessThanEquals),
            (b'<', _) => self.byte_token(start, LessThan),

            (b'|', Some(b'|')) => self.next_token(start, OrOr),
            (b'&', Some(b'&')) => self.next_token(start, AndAnd),

            (b'=', _) => self.byte_token(start, Equals),
            (b'!', _) => self.byte_token(start, Not),
            (b',', _) => self.byte_token(start, Comma),
            (b';', _) => self.byte_token(start, Semicolon),
            (b':', _) => self.byte_token(start, Colon),
            (b'-', Some(b'>')) => self.next_token(start, RightArrow),

            (b'0' ..= b'9', _) |
            (b'+', Some(b'0' ..= b'9')) |
            (b'-', Some(b'0' ..= b'9')) => self.integer_lit(start, current_char),

            (b'+', Some(b'=')) => self.next_token(start, PlusEquals),
            (b'-', Some(b'=')) => self.next_token(start, MinusEquals),
            (b'*', Some(b'=')) => self.next_token(start, TimesEquals),
            (b'/', Some(b'=')) => self.next_token(start, SlashEquals),
            (b'%', Some(b'=')) => self.next_token(start, PercentEquals),

            (b'+', _) => self.byte_token(start, Plus),
            (b'-', _) => self.byte_token(start, Minus),
            (b'*', _) => self.byte_token(start, Times),
            (b'/', _) => self.byte_token(start, Slash),
            (b'%', _) => self.byte_token(start, Percent),

            (b'b', Some(b'"')) => self.bstr_lit(start),

            (b'a' ..= b'z', _) |
            (b'A' ..= b'Z', _) |
            (b'_', _) => self.ident(start),

            (ch, _) => {
                let token = self.byte_token(start, Error);
                self.diag.span_error(token.span, format!("unknown start of token `{}`", ch as char)).emit();
                token
            },
        }
    }

    fn ignore_whitespace_comments(&mut self) {
        while self.ignore_whitespace() || self.ignore_comments() {
            // Keep going until nothing is ignored anymore
        }
    }

    /// Returns true if any whitespace was ignored
    fn ignore_whitespace(&mut self) -> bool {
        let mut ignored = false;
        while let Some(ch) = self.scanner.peek() {
            if ch.is_ascii_whitespace() {
                self.scanner.next();
                ignored = true;
            } else {
                break;
            }
        }

        ignored
    }

    /// Returns true if any comments were ignored
    fn ignore_comments(&mut self) -> bool {
        let mut ignored = false;

        while let Some(ch) = self.scanner.peek() {
            let start = self.scanner.current_pos();
            match (ch, self.scanner.peek2()) {
                (b'/', Some(b'*')) => {
                    self.scanner.next2();
                    self.ignore_block_comment(start);
                },
                (b'/', Some(b'/')) => {
                    self.scanner.next2();
                    self.ignore_until_eol();
                },
                // Keep going until nothing is ignored anymore
                _ => break,
            }
            ignored = true;
        }

        ignored
    }

    /// Assuming the start of a block comment has been seen, ignores until the end is found
    ///
    /// Block comments may be nested, e.g. /* /* foo */ bar */
    fn ignore_block_comment(&mut self, start: usize) {
        // The count of unmatched `/*` seen
        let mut count = 1;
        while count > 0 {
            match (self.scanner.next(), self.scanner.next()) {
                (Some(b'/'), Some(b'*')) => count += 1,
                (Some(b'*'), Some(b'/')) => count -= 1,
                // If we reach EOF, stop iterating
                (None, _) | (_, None) => break,
                // Ignore all characters between the comment delimiters
                _ => {},
            }
        }

        //TODO: What happens in this code if too many */ are found?
        if count > 0 {
            let span = self.scanner.span(start, self.scanner.current_pos());
            self.diag.span_error(span, "unterminated block comment").emit();
        }
    }

    /// Ignores until the end of the line
    fn ignore_until_eol(&mut self) {
        while let Some(ch) = self.scanner.next() {
            if ch == b'\n' {
                break;
            }
        }
    }

    /// Parses an integer literal, given a starting digit or positive/negative sign
    fn integer_lit(&mut self, start: usize, start_byte: u8) -> Token {
        // If the start digit is zero, we may have a hex or binary literal
        let value = match (start_byte, self.scanner.peek()) {
            (b'0', Some(b'x')) => self.hex_lit_value(start),
            (b'0', Some(b'b')) => self.binary_lit_value(start),
            _ => self.decimal_lit_value(start, start_byte),
        };

        let value = match value {
            Ok(value) => value,
            Err(token) => return token,
        };

        if value < i64::MIN as i128 || value > u64::MAX as i128 {
            let token = self.token_to_current(start, Error, None);
            self.diag.span_error(token.span, "integer literal out of 64-bit range").emit();
            return token;
        }

        // An integer cannot be directly followed by an identifier with no whitespace in between
        if matches!(self.scanner.peek(), Some(b'a'..=b'z') | Some(b'A'..=b'Z')) {
            // Skip the first character
            self.scanner.next();
            // Try to avoid bogus errors by skipping the next numbers or identifiers
            let ignore_start = self.scanner.current_pos();
            match self.scanner.next() {
                Some(b'a'..=b'z') | Some(b'A'..=b'Z') => {
                    self.ident(ignore_start);
                },
                Some(current_byte@b'0'..=b'9') => {
                    self.integer_lit(ignore_start, current_byte);
                },
                _ => {},
            }

            let token = self.token_to_current(start, Error, None);
            self.diag.span_error(token.span, "invalid integer literal").emit();
            return token;
        }

        self.token_to_current(start, Literal(token::Literal::Integer), TokenValue::Integer(value))
    }

    fn hex_lit_value(&mut self, start: usize) -> Result<i128, Token> {
        // Skip `x` character
        self.scanner.next();

        let mut digits_buf = String::new();
        let digits = self.digits(true, Some(&mut digits_buf));
        if digits == 0 {
            let token = self.token_to_current(start, Error, None);
            self.diag.span_error(token.span, "invalid hexadecimal number literal").emit();
            return Err(token);
        }

        match i128::from_str_radix(&digits_buf, 16) {
            Ok(value) => Ok(value),
            Err(_) => {
                let token = self.token_to_current(start, Error, None);
                self.diag.span_error(token.span, "invalid hexadecimal number literal").emit();
                Err(token)
            },
        }
    }

    fn binary_lit_value(&mut self, start: usize) -> Result<i128, Token> {
        // Skip `b` character
        self.scanner.next();

        let mut digits_buf = String::new();
        let digits = self.digits(false, Some(&mut digits_buf));
        if digits == 0 {
            let token = self.token_to_current(start, Error, None);
            self.diag.span_error(token.span, "invalid binary number literal").emit();
            return Err(token);
        }

        match i128::from_str_radix(&digits_buf, 2) {
            Ok(value) => Ok(value),
            Err(_) => {
                let token = self.token_to_current(start, Error, None);
                self.diag.span_error(token.span, "invalid binary number literal").emit();
                Err(token)
            },
        }
    }

    /// Parses a decimal number literal assuming that either a digit or a negative sign has already
    /// been parsed
    fn decimal_lit_value(&mut self, start: usize, start_byte: u8) -> Result<i128, Token> {
        let mut digits_buf = String::new();

        // Add the start digit or negative sign
        digits_buf.push(start_byte as char);

        let digits = self.digits(false, Some(&mut digits_buf));
        if digits == 0 && !start_byte.is_ascii_digit() {
            let token = self.token_to_current(start, Error, None);
            self.diag.span_error(token.span, "invalid decimal number literal").emit();
            return Err(token);
        }

        match i128::from_str_radix(&digits_buf, 10) {
            Ok(value) => Ok(value),
            Err(_) => {
                let token = self.token_to_current(start, Error, None);
                self.diag.span_error(token.span, "invalid decimal number literal").emit();
                Err(token)
            },
        }
    }

    /// Advances the scanner until no more digits are found. Returns the number of digits found.
    ///
    /// The final, non-digit character is NOT consumed
    fn digits(&mut self, hex: bool, mut digit_buf: Option<&mut String>) -> usize {
        let mut digits = 0;
        while let Some(ch) = self.scanner.peek() {
            if ch.is_ascii_digit() || (hex && matches!(ch, b'a' ..= b'f' | b'A' ..= b'F')) {
                if let Some(digit_buf) = &mut digit_buf {
                    digit_buf.push(ch as char);
                }

                digits += 1;
                self.scanner.next();

            } else if ch == b'_' {
                // Skip underscores but don't count them as digits
                self.scanner.next();

            } else {
                break;
            }
        }

        digits
    }

    /// Parses a byte str, assuming that the opening 'b' character has already been parsed
    /// and that the next character is definitely the opening double quote '"'
    fn bstr_lit(&mut self, start: usize) -> Token {
        // Skip the opening quote
        self.scanner.next();

        let mut value = Vec::new();
        loop {
            let pos = self.scanner.current_pos();
            match self.scanner.next() {
                // Found closing quote
                Some(b'"') => break,
                Some(b'\\') => match self.bstr_lit_unescape_byte(pos) {
                    Ok(Some(ch)) => value.push(ch),
                    Ok(None) => {},
                    Err(token) => return token,
                },

                // Found next character
                Some(ch) => value.push(ch),

                None => {
                    let token = self.token_to_current(start, Error, None);
                    self.diag.span_error(token.span, "unterminated double quote string").emit();
                    return token;
                },
            }
        }

        // Note that the token span contains the quotes but the value does not
        let value: Arc<[u8]> = value.into();
        self.token_to_current(start, TokenKind::Literal(token::Literal::Bytes), TokenValue::Bytes(value))
    }

    /// Parses a byte string escape sequence, assuming that the starting backslash has already been
    /// parsed
    fn bstr_lit_unescape_byte(&mut self, lit_start: usize) -> Result<Option<u8>, Token> {
        Ok(match self.scanner.next() {
            Some(b'\\') => Some(b'\\'),
            Some(b'n') => Some(b'\n'),
            Some(b'r') => Some(b'\r'),
            Some(b't') => Some(b'\t'),
            Some(b'0') => Some(b'\0'),
            Some(b'\'') => Some(b'\''),
            Some(b'"') => Some(b'"'),

            // Hex escape
            Some(b'x') => match self.bstr_lit_byte_escape_hex(lit_start, b'"') {
                Ok(ch) => ch,
                Err(err) => {
                    // Error recovery: continue until the end quote
                    while let Some(ch) = self.scanner.next() {
                        if ch == b'"' {
                            break;
                        }
                    }
                    Err(err)?
                }
            },

            // Found backslash at the end of a line
            Some(b'\n') => {
                // Ignore all whitespace characters
                self.ignore_whitespace();

                None
            },

            Some(ch) => {
                let token = self.token_to_current(lit_start, Error, None);
                self.diag.span_error(token.span, format!("unknown character escape: `{}`", ch as char)).emit();

                // Error recovery: continue until the end quote
                while let Some(ch) = self.scanner.next() {
                    if ch == b'"' {
                        break;
                    }
                }
                return Err(token);
            },

            // Instead of emitting an error here for the EOF, we'll send up a value and let the
            // unterminated double quote string message handle the rest
            None => None,
        })
    }

    /// Parses a hex escape sequence `\x12' assuming that `\x` has been parsed already
    ///
    /// `lit_start` is the position of the backslash in the escape sequence. `lit_end_byte` is the
    /// byte that denotes the end of the literal this is in. `"` for byte strings and `'` for bytes.
    ///
    /// Returns None if any errors occur
    fn bstr_lit_byte_escape_hex(&mut self, lit_start: usize, lit_end_byte: u8) -> Result<Option<u8>, Token> {
        let digits_start = self.scanner.current_pos();

        // Get two hex digits
        for _ in 0..2 {
            match self.bstr_lit_escape_hex_digit(lit_start, lit_end_byte)? {
                Some(_) => {},
                None => return Ok(None),
            }
        }

        let digits = self.scanner.slice(digits_start, self.scanner.current_pos());

        let value = u8::from_str_radix(digits, 16)
            .expect("bug: should be a valid hex literal in the right range");
        Ok(Some(value))
    }

    fn bstr_lit_escape_hex_digit(&mut self, lit_start: usize, lit_end_byte: u8) -> Result<Option<u8>, Token> {
        if self.scanner.peek() == Some(lit_end_byte) {
            let token = self.token_to_current(lit_start, Error, None);
            self.diag.span_error(token.span, "numeric character escape is too short").emit();
            return Err(token);
        }

        Ok(match self.scanner.next() {
            Some(ch@b'0' ..= b'9') |
            Some(ch@b'a' ..= b'f') |
            Some(ch@b'A' ..= b'F') => {
                Some(ch)
            },

            Some(ch) => {
                let token = self.token_to_current(lit_start, Error, None);
                self.diag.span_error(token.span, format!("invalid character in numeric character escape: `{}`", ch as char)).emit();
                return Err(token);
            },

            // Propagate EOF and let the literal handle the error
            None => None,
        })
    }

    /// Parses an identifier, assuming that the first character has already been parsed
    ///
    /// Since the first character has already been parsed, this can never fail
    fn ident(&mut self, start: usize) -> Token {
        // We've already got a valid start character, so let's just look for further characters
        while let Some(ch) = self.scanner.peek() {
            if ch.is_ascii_alphanumeric() || ch == b'_' {
                self.scanner.next();
            } else {
                break;
            }
        }

        let value = self.scanner.slice(start, self.scanner.current_pos());
        match token::Keyword::from_str(value) {
            Some(kw) => self.token_to_current(start, Keyword(kw), None),
            None => {
                let value = self.intern_str(value);
                self.token_to_current(start, TokenKind::Ident, TokenValue::Ident(value))
            },
        }
    }

    fn empty_token(&self, start: usize, kind: TokenKind) -> Token {
        let span = self.scanner.empty_span(start);
        Token {kind, span, value: None}
    }

    fn byte_token(&self, start: usize, kind: TokenKind) -> Token {
        let span = self.scanner.byte_span(start);
        Token {kind, span, value: None}
    }

    fn next_token(&mut self, start: usize, kind: TokenKind) -> Token {
        let span = self.scanner.next_span(start);
        Token {kind, span, value: None}
    }

    fn token_to_current(&self, start: usize, kind: TokenKind, value: impl Into<Option<TokenValue>>) -> Token {
        let value = value.into();
        let span = self.scanner.span(start, self.scanner.current_pos());
        Token {kind, span, value}
    }

    fn intern_str(&mut self, value: &str) -> Arc<str> {
        match self.interned_strings.get(value) {
            Some(interned) => interned.clone(),
            None => {
                let interned: Arc<str> = value.into();
                self.interned_strings.insert(interned.clone());
                interned
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use parking_lot::RwLock;

    use crate::source_files::{Span, SourceFiles};

    macro_rules! t {
        ($kind:expr) => (
            Token {
                kind: $kind,
                span: Span {start: 0, end: 0},
                value: None,
            }
        );
        ($kind:expr, $value:expr) => (
            Token {
                kind: $kind,
                span: Span {start: 0, end: 0},
                value: Some($value),
            }
        );
    }

    macro_rules! ident {
        ($value:expr) => (
            t!(Ident, TokenValue::Ident($value.into()))
        );
    }

    macro_rules! bstr {
        ($value:expr) => (
            t!(Literal(token::Literal::Bytes), TokenValue::Bytes((&$value[..]).into()))
        );
    }

    macro_rules! kw {
        ($kw:ident) => (
            t!(Keyword(token::Keyword::$kw))
        );
    }

    macro_rules! int {
        ($value:expr) => (
            t!(Literal(token::Literal::Integer), TokenValue::Integer($value))
        );
    }

    macro_rules! expect_token {
        ($source:literal, $expected:expr) => {
            let source_files = Arc::new(RwLock::new(SourceFiles::default()));
            let root_file = source_files.write().add_source("test.rs", $source);
            let diag = Diagnostics::new(source_files.clone(), termcolor::ColorChoice::Auto);
            let files = source_files.read();
            let scanner = Scanner::new(files.source(root_file));
            let mut lexer = Lexer::new(scanner, &diag);
            let token = lexer.next();
            let expected = $expected;
            assert_eq!(token.kind, expected.kind);
            let token = lexer.next();
            assert_eq!(token.kind, Eof);
        };
    }

    macro_rules! expect_tokens {
        ($source:literal, $expected:expr) => {
            let source_files = Arc::new(RwLock::new(SourceFiles::default()));
            let root_file = source_files.write().add_source("test.rs", $source);
            let diag = Diagnostics::new(source_files.clone(), termcolor::ColorChoice::Auto);
            let files = source_files.read();
            let scanner = Scanner::new(files.source(root_file));
            let mut lexer = Lexer::new(scanner, &diag);
            let expected_tokens: &[Token] = $expected;
            for expected_token in expected_tokens {
                let token = lexer.next();
                assert_eq!(token.kind, expected_token.kind);
            }
            // Ensure that the input is exhausted
            let token = lexer.next();
            assert_eq!(token.kind, Eof);
        };
    }

    macro_rules! expect_error {
        ($source:literal) => {
            expect_token!($source, t!(Error));
        };
    }

    #[test]
    fn comments() {
        expect_tokens!(b"// 0xInvalidLit", &[]);
        expect_tokens!(b"/* 0xInvalid */", &[]);
        expect_tokens!(b"///wooooo", &[]);
        expect_tokens!(b"/*this // is a /* nested comment */ // */", &[]);
    }

    #[test]
    fn keywords() {
        expect_token!(b"println", kw!(Println));
    }

    #[test]
    fn not_symbol() {
        expect_token!(b"!", t!(Not));
    }

    #[test]
    fn semicolon_symbol() {
        expect_token!(b";", t!(Semicolon));
    }

    #[test]
    fn operators() {
        expect_token!(b"+", t!(Plus));
        expect_token!(b"-", t!(Minus));
        expect_token!(b"*", t!(Times));
        expect_token!(b"/", t!(Slash));
        expect_token!(b"%", t!(Percent));

        expect_token!(b"+=", t!(PlusEquals));
        expect_token!(b"-=", t!(MinusEquals));
        expect_token!(b"*=", t!(TimesEquals));
        expect_token!(b"/=", t!(SlashEquals));
        expect_token!(b"%=", t!(PercentEquals));

        expect_token!(b"=", t!(Equals));
        expect_token!(b"==", t!(EqualsEquals));
        expect_token!(b"!=", t!(NotEquals));
        expect_token!(b">=", t!(GreaterThanEquals));
        expect_token!(b">", t!(GreaterThan));
        expect_token!(b"<=", t!(LessThanEquals));
        expect_token!(b"<", t!(LessThan));
    }

    #[test]
    fn decimal_literals() {
        expect_token!(b"0", int!(0));
        expect_token!(b"000", int!(0));
        expect_token!(b"013", int!(013));
        expect_token!(b"123", int!(123));
        expect_token!(b"+123", int!(123));
        expect_token!(b"9_999", int!(9999));
        expect_token!(b"-9_999", int!(-9999));
        expect_token!(b"-9223372036854775808", int!(i64::MIN as i128));
        expect_token!(b"18446744073709551615", int!(u64::MAX as i128));
    }

    #[test]
    fn decimal_literals_invalid() {
        // out of range
        expect_error!(b"-9223372036854775809");
        expect_error!(b"18446744073709551616");

        // hex digits
        expect_error!(b"1844A674f4073709C551616");
    }

    #[test]
    fn hex_literals() {
        expect_token!(b"0x0", int!(0x0));
        expect_token!(b"0x000", int!(0x0));
        expect_token!(b"0x013", int!(0x013));
        expect_token!(b"0x123", int!(0x123));
        expect_token!(b"0x9999", int!(0x9999));
        expect_token!(b"0x030AfacbCDdef", int!(0x030AfacbCDdef));
        expect_token!(b"0xffff", int!(0xffff));
        expect_token!(b"0xffff_ffff_ffff_ffff", int!(u64::MAX as i128));
        expect_token!(b"0xFFFF_FFFF_FFFF_FFFF", int!(u64::MAX as i128));
    }

    #[test]
    fn hex_literals_invalid() {
        // out of range
        expect_error!(b"0x1_0000_0000_0000_0000");

        // cannot be negative
        expect_error!(b"-0x0");
        expect_error!(b"-0x1");

        // empty
        expect_error!(b"0x");

        // 'g' is not a letter between 'a' and 'f'
        expect_tokens!(b"0xg", &[t!(Error), ident!("g")]);
    }

    #[test]
    fn binary_literals() {
        expect_token!(b"0b0", int!(0b0));
        expect_token!(b"0b000", int!(0b000));
        expect_token!(b"0b0001", int!(0b0001));
        expect_token!(b"0b1", int!(0b1));
        expect_token!(b"0b11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111", int!(u64::MAX as i128));
    }

    #[test]
    fn binary_literals_invalid() {
        // out of range
        expect_error!(b"0b1_00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000");

        // cannot be negative
        expect_error!(b"-0b0");
        expect_error!(b"-0b1");

        // empty
        expect_error!(b"0b");

        // decimal or hex digit
        expect_error!(b"0b2");
        expect_tokens!(b"0bF", &[t!(Error), ident!("F")]);
    }

    #[test]
    fn byte_str_lits() {
        expect_token!(b"b\"\"", bstr!(b""));
        expect_token!(b"b\"abcdef\"", bstr!(b"abcdef"));
        expect_token!(b"b\"The quick brown fox \"", bstr!(b"The quick brown fox "));
        expect_token!(b"b\"\\n\\r\\t\\0\\'\\\"\\\\\\x123 \\
                        abc\"", bstr!(b"\n\r\t\0\'\"\\\x123 abc"));
    }

    #[test]
    fn byte_str_lits_invalid() {
        expect_error!(b"b\" \\q\"");
        expect_error!(b"b\" \\x\"");
        expect_error!(b"b\" \\x1\"");
        expect_error!(b"b\" \\xq \"");
        expect_error!(b"b\" \\x1q \"");

        expect_error!(b"b\"");
        expect_error!(b"b\"\\\"");
    }

    #[test]
    fn idents() {
        expect_token!(b"a", ident!("a"));
        expect_token!(b"bF", ident!("bF"));
        expect_token!(b"L1", ident!("L1"));
        expect_token!(b"_L1", ident!("_L1"));
        expect_token!(b"abc_efod_fso2190_123___", ident!("abc_efod_fso2190_123___"));
    }

    #[test]
    fn idents_invalid() {
        expect_error!(b"1ab132c");
    }
}
