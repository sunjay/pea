use crate::ast;

use super::{Parser, ParseResult, TokenKind, Literal};

impl<'a> Parser<'a> {
    pub(in super) fn expr(&mut self) -> ParseResult<ast::Expr> {
        match self.input.peek().kind {
            TokenKind::Ident => self.call().map(ast::Expr::Call),
            TokenKind::Literal(Literal::Integer) => self.integer_literal().map(ast::Expr::Integer),
            _ => self.bstr_literal().map(ast::Expr::BStr),
        }
    }

    fn call(&mut self) -> ParseResult<ast::CallExpr> {
        let name = self.ident()?;
        let args = self.parens(|_| Ok([]))?;

        Ok(ast::CallExpr {name, args})
    }

    fn integer_literal(&mut self) -> ParseResult<ast::IntegerLiteral> {
        self.input.match_kind(TokenKind::Literal(Literal::Integer)).map(|token| ast::IntegerLiteral {
            value: token.unwrap_integer(),
            span: token.span,
        })
    }

    fn bstr_literal(&mut self) -> ParseResult<ast::BStr> {
        self.input.match_kind(TokenKind::Literal(Literal::Bytes)).map(|token| ast::BStr {
            value: token.unwrap_bytes().clone(),
            span: token.span,
        })
    }
}
