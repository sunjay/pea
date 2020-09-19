use crate::ast;

use super::{TokenStream, ParseResult, TokenKind, Literal};

pub(in super) fn parse_expr<'a, 'b>(
    input: &'b mut TokenStream<'a>,
) -> ParseResult<ast::Expr> {
    let mut parser = ExprParser {input};
    parser.expr()
}

struct ExprParser<'a, 'b> {
    input: &'b mut TokenStream<'a>,
}

impl<'a, 'b> ExprParser<'a, 'b> {
    fn expr(&mut self) -> ParseResult<ast::Expr> {
        match self.input.peek().kind {
            TokenKind::Ident => self.call().map(ast::Expr::Call),
            TokenKind::Literal(Literal::Integer) => self.integer_literal().map(ast::Expr::Integer),
            _ => self.bstr_literal().map(ast::Expr::BStr),
        }
    }

    fn call(&mut self) -> ParseResult<ast::CallExpr> {
        let name = self.input.ident()?;
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

    fn parens<T>(
        &mut self,
        parser: impl FnOnce(&mut Self) -> ParseResult<T>,
    ) -> ParseResult<ast::Parens<T>> {
        let paren_open_token = self.input.match_kind(TokenKind::ParenOpen)?.clone();
        let value = parser(self)?;
        let paren_close_token = self.input.match_kind(TokenKind::ParenClose)?.clone();
        Ok(ast::Parens {paren_open_token, value, paren_close_token})
    }
}
