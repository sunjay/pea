use crate::ast;

use super::{Parser, ParseResult, TokenKind, Literal};

impl<'a> Parser<'a> {
    pub(in super) fn expr(&mut self) -> ParseResult<ast::Expr> {
        let mut lhs = self.atom()?;
        while self.input.peek().kind == TokenKind::ParenOpen {
            let paren_open_token = self.input.match_kind(TokenKind::ParenOpen)?.clone();
            let args = [];
            let paren_close_token = self.input.match_kind(TokenKind::ParenClose)?.clone();

            lhs = ast::Expr::Call(Box::new(ast::CallExpr {
                lhs,
                paren_open_token,
                args,
                paren_close_token,
            }));
        }

        Ok(lhs)
    }

    /// Parses "atomic" expressions that do not contain subexpressions and thus do not need to go
    /// through any operator precedence mechanisms
    fn atom(&mut self) -> ParseResult<ast::Expr> {
        match self.input.peek().kind {
            TokenKind::Ident => self.ident().map(ast::Expr::Ident),
            TokenKind::Literal(Literal::Integer) => self.integer_literal().map(ast::Expr::Integer),
            _ => self.bstr_literal().map(ast::Expr::BStr),
        }
    }

    fn integer_literal(&mut self) -> ParseResult<ast::IntegerLiteral> {
        self.input.match_kind(TokenKind::Literal(Literal::Integer)).map(|token| ast::IntegerLiteral {
            value: token.unwrap_integer(),
            span: token.span,
        })
    }

    fn bstr_literal(&mut self) -> ParseResult<ast::BStrLiteral> {
        self.input.match_kind(TokenKind::Literal(Literal::Bytes)).map(|token| ast::BStrLiteral {
            value: token.unwrap_bytes().clone(),
            span: token.span,
        })
    }
}
