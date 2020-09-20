//! Parser for expressions
//!
//! See: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

use crate::ast;

use super::{Parser, ParseResult, TokenKind, Literal};

fn prefix_binding_power(kind: TokenKind) -> Result<(TokenKind, ast::UnaryOp, ((), u8)), TokenKind> {
    let (op, bp) = match kind {
        TokenKind::Plus => (ast::UnaryOp::Pos, ((), 9)),
        TokenKind::Minus => (ast::UnaryOp::Neg, ((), 9)),
        TokenKind::Not => (ast::UnaryOp::Not, ((), 9)),
        _ => return Err(kind),
    };

    Ok((kind, op, bp))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum PostfixOp {
    Call,
}

fn postfix_binding_power(kind: TokenKind) -> Result<(TokenKind, PostfixOp, (u8, ())), TokenKind> {
    let (op, bp) = match kind {
        TokenKind::ParenOpen => (PostfixOp::Call, (11, ())),
        _ => return Err(kind),
    };

    Ok((kind, op, bp))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum InfixOp {
    Assign,
    BinaryOp(ast::BinaryOp),
}

fn infix_binding_power(kind: TokenKind) -> Result<(TokenKind, InfixOp, (u8, u8)), TokenKind> {
    let (op, bp) = match kind {
        TokenKind::Equals => (InfixOp::Assign, (2, 1)),

        TokenKind::Plus => (InfixOp::BinaryOp(ast::BinaryOp::Add), (5, 6)),
        TokenKind::Minus => (InfixOp::BinaryOp(ast::BinaryOp::Sub), (5, 6)),
        TokenKind::Times => (InfixOp::BinaryOp(ast::BinaryOp::Mul), (7, 8)),
        TokenKind::Slash => (InfixOp::BinaryOp(ast::BinaryOp::Div), (7, 8)),
        TokenKind::Percent => (InfixOp::BinaryOp(ast::BinaryOp::Rem), (7, 8)),

        _ => return Err(kind),
    };

    Ok((kind, op, bp))
}

impl<'a> Parser<'a> {
    pub(in super) fn expr(&mut self) -> ParseResult<ast::Expr> {
        // "bp" = "binding power"
        self.expr_bp(0)
    }

    fn expr_bp(&mut self, min_bp: u8) -> ParseResult<ast::Expr> {
        let mut lhs = match prefix_binding_power(self.input.peek().kind) {
            Err(TokenKind::ParenOpen) => {
                let paren_open_token = self.input.match_kind(TokenKind::ParenOpen)?.clone();
                let expr = self.expr_bp(0)?;
                let paren_close_token = self.input.match_kind(TokenKind::ParenClose)?.clone();

                ast::Expr::Group(Box::new(ast::GroupExpr {
                    paren_open_token,
                    expr,
                    paren_close_token,
                }))
            },

            Ok((kind, op, ((), r_bp))) => {
                let op_token = self.input.match_kind(kind)?.clone();
                let expr = self.expr_bp(r_bp)?;

                ast::Expr::UnaryOp(Box::new(ast::UnaryOpExpr {op, op_token, expr}))
            },

            _ => self.atom()?,
        };

        while self.input.peek().kind != TokenKind::Eof {
            if let Ok((kind, op, (l_bp, ()))) = postfix_binding_power(self.input.peek().kind) {
                if l_bp < min_bp {
                    break;
                }

                let op_token = self.input.match_kind(kind)?.clone();
                lhs = match op {
                    PostfixOp::Call => {
                        let args = self.func_args()?;
                        let paren_close_token = self.input.match_kind(TokenKind::ParenClose)?.clone();
                        ast::Expr::Call(Box::new(ast::CallExpr {
                            lhs,
                            paren_open_token: op_token,
                            args,
                            paren_close_token,
                        }))
                    },
                };

                continue;
            }

            if let Ok((kind, op, (l_bp, r_bp))) = infix_binding_power(self.input.peek().kind) {
                if l_bp < min_bp {
                    break;
                }

                let op_token = self.input.match_kind(kind)?.clone();
                let rhs = self.expr_bp(r_bp)?;

                lhs = match op {
                    InfixOp::Assign => ast::Expr::Assign(Box::new(ast::AssignExpr {
                        lhs,
                        equals_token: op_token,
                        rhs,
                    })),

                    InfixOp::BinaryOp(op) => ast::Expr::BinaryOp(Box::new(ast::BinaryOpExpr {
                        lhs,
                        op,
                        op_token,
                        rhs,
                    })),
                };

                continue;
            }

            break;
        }

        Ok(lhs)
    }

    /// Parses comma separated expressions until the `)` token
    ///
    /// Trailing comma is allowed but optional. The `)` token is not consumed.
    fn func_args(&mut self) -> ParseResult<Vec<ast::Expr>> {
        let mut args = Vec::new();

        while self.input.peek().kind != TokenKind::ParenClose {
            let arg = self.expr()?;
            args.push(arg);

            if self.input.peek().kind != TokenKind::Comma {
                break;
            }

            self.input.match_kind(TokenKind::Comma)?;
        }

        Ok(args)
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
