use super::{
    ParseResult,
    ParseError,
    token::*,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TokenStream<'a> {
    input: &'a [Token],
}

impl<'a> From<&'a [Token]> for TokenStream<'a> {
    fn from(input: &'a [Token]) -> Self {
        Self {input}
    }
}

impl<'a> TokenStream<'a> {
    pub(in super) fn keyword(&mut self, keyword: Keyword) -> ParseResult<&Token> {
        self.match_kind(TokenKind::Keyword(keyword))
    }

    pub(in super) fn match_kind(&mut self, expected: TokenKind) -> ParseResult<&Token> {
        let token = self.advance();
        if token.kind == expected {
            Ok(token)
        } else {
            Err(ParseError::UnexpectedToken {
                expected: vec![expected],
                actual: token.clone(),
            })
        }
    }

    pub fn peek(&self) -> &Token {
        &self.input[0]
    }

    pub fn advance(&mut self) -> &Token {
        let token = &self.input[0];
        // Do not advance past the end, because input must always remain non-empty
        if token.kind != TokenKind::Eof {
            self.input = &self.input[1..];
        }
        token
    }

    pub fn check(&self, kind: TokenKind) -> bool {
        self.input[0].kind == kind
    }
}
