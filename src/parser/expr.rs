//! Parser for expressions
//!
//! See: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
//!
//! # Precendence Table
//!
//! |  Prec  | Fixity  | Operators                          |
//! |--------|---------|------------------------------------|
//! | 31, 32 | infix   | `.`                                |
//! | 29, _  | postfix | `(...)`                            |
//! |  _, 25 | prefix  | `+`, `-`, `!`                      |
//! | 21, 22 | infix   | `*`, `/`, `%`                      |
//! | 19, 20 | infix   | `+`, `-`                           |
//! |  9, 10 | infix   | `==`, `!=`, `>`, `>=`, `<`, `<=`   |
//! |  7, 8  | infix   | `&&`                               |
//! |  5, 6  | infix   | `||`                               |
//! |  2, 1  | infix   | `=`, `+=`, `-=`, `*=`, `/=`, `%=`  |
//! |  _, 1  | prefix  | `return`, `break`, `continue`      |

use crate::ast;

use super::{Parser, ParseResult, ParseError, TokenKind, Literal, Keyword, MAX_ARGS};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum PrefixOp {
    Return,
    Break,
    Continue,
    UnaryOp(ast::UnaryOp),
}

fn prefix_binding_power(kind: TokenKind) -> Result<(TokenKind, PrefixOp, ((), u8)), TokenKind> {
    let (op, bp) = match kind {
        TokenKind::Keyword(Keyword::Return) => (PrefixOp::Return, ((), 1)),
        TokenKind::Keyword(Keyword::Break) => (PrefixOp::Break, ((), 1)),
        TokenKind::Keyword(Keyword::Continue) => (PrefixOp::Continue, ((), 1)),

        TokenKind::Plus => (PrefixOp::UnaryOp(ast::UnaryOp::Pos), ((), 25)),
        TokenKind::Minus => (PrefixOp::UnaryOp(ast::UnaryOp::Neg), ((), 25)),
        TokenKind::Not => (PrefixOp::UnaryOp(ast::UnaryOp::Not), ((), 25)),

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
        TokenKind::ParenOpen => (PostfixOp::Call, (29, ())),
        _ => return Err(kind),
    };

    Ok((kind, op, bp))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum InfixOp {
    Assign,
    /// Augmented assignment, e.g. `+=`, `-=`, etc.
    AugAssign(ast::BinaryOp),
    MethodCall,
    Or,
    And,
    BinaryOp(ast::BinaryOp),
}

fn infix_binding_power(kind: TokenKind) -> Result<(TokenKind, InfixOp, (u8, u8)), TokenKind> {
    let (op, bp) = match kind {
        TokenKind::Equals => (InfixOp::Assign, (2, 1)),
        TokenKind::PlusEquals => (InfixOp::AugAssign(ast::BinaryOp::Add), (2, 1)),
        TokenKind::MinusEquals => (InfixOp::AugAssign(ast::BinaryOp::Sub), (2, 1)),
        TokenKind::TimesEquals => (InfixOp::AugAssign(ast::BinaryOp::Mul), (2, 1)),
        TokenKind::SlashEquals => (InfixOp::AugAssign(ast::BinaryOp::Div), (2, 1)),
        TokenKind::PercentEquals => (InfixOp::AugAssign(ast::BinaryOp::Rem), (2, 1)),

        TokenKind::Dot => (InfixOp::MethodCall, (31, 32)),

        TokenKind::OrOr => (InfixOp::Or, (5, 6)),
        TokenKind::AndAnd => (InfixOp::And, (7, 8)),

        TokenKind::Plus => (InfixOp::BinaryOp(ast::BinaryOp::Add), (19, 20)),
        TokenKind::Minus => (InfixOp::BinaryOp(ast::BinaryOp::Sub), (19, 20)),

        TokenKind::Times => (InfixOp::BinaryOp(ast::BinaryOp::Mul), (21, 22)),
        TokenKind::Slash => (InfixOp::BinaryOp(ast::BinaryOp::Div), (21, 22)),
        TokenKind::Percent => (InfixOp::BinaryOp(ast::BinaryOp::Rem), (21, 22)),

        TokenKind::EqualsEquals => (InfixOp::BinaryOp(ast::BinaryOp::EqualsEquals), (9, 10)),
        TokenKind::NotEquals => (InfixOp::BinaryOp(ast::BinaryOp::NotEquals), (9, 10)),
        TokenKind::GreaterThan => (InfixOp::BinaryOp(ast::BinaryOp::GreaterThan), (9, 10)),
        TokenKind::GreaterThanEquals => (InfixOp::BinaryOp(ast::BinaryOp::GreaterThanEquals), (9, 10)),
        TokenKind::LessThan => (InfixOp::BinaryOp(ast::BinaryOp::LessThan), (9, 10)),
        TokenKind::LessThanEquals => (InfixOp::BinaryOp(ast::BinaryOp::LessThanEquals), (9, 10)),

        _ => return Err(kind),
    };

    Ok((kind, op, bp))
}

fn extract_lvalue(expr: ast::Expr) -> ParseResult<ast::LValueExpr> {
    use ast::Expr::*;
    Ok(match expr {
        Ident(ident) => ast::LValueExpr::Ident(ident),

        _ => return Err(ParseError::UnsupportedLValue {span: expr.span()}),
    })
}

impl<'a> Parser<'a> {
    pub(in super) fn expr(&mut self) -> ParseResult<ast::Expr> {
        let prev_allow_struct_literals = self.allow_struct_literals;

        // Default to allowing struct literals
        self.allow_struct_literals = true;
        let expr = self.expr_bp(0);
        self.allow_struct_literals = prev_allow_struct_literals;

        expr
    }

    pub(in super) fn expr_without_struct_literals(&mut self) -> ParseResult<ast::Expr> {
        let prev_allow_struct_literals = self.allow_struct_literals;

        self.allow_struct_literals = false;
        let expr = self.expr_bp(0);
        self.allow_struct_literals = prev_allow_struct_literals;

        expr
    }

    // "bp" = "binding power"
    fn expr_bp(&mut self, min_bp: u8) -> ParseResult<ast::Expr> {
        let mut lhs = match prefix_binding_power(self.input.peek().kind) {
            Err(TokenKind::ParenOpen) => {
                let paren_open_token = self.input.match_kind(TokenKind::ParenOpen)?.clone();
                if self.input.peek().kind == TokenKind::ParenClose {
                    let paren_close_token = self.input.match_kind(TokenKind::ParenClose)?.clone();

                    ast::Expr::Unit(ast::UnitLiteral {paren_open_token, paren_close_token})

                } else {
                    let expr = self.expr()?;

                    let paren_close_token = self.input.match_kind(TokenKind::ParenClose)?.clone();

                    ast::Expr::Group(Box::new(ast::GroupExpr {
                        paren_open_token,
                        expr,
                        paren_close_token,
                    }))
                }
            },

            Err(TokenKind::Keyword(Keyword::If)) => {
                let cond = self.cond()?;
                ast::Expr::Cond(Box::new(cond))
            },

            Ok((kind, op, ((), r_bp))) => {
                match op {
                    PrefixOp::Return => {
                        let return_token = self.input.match_kind(TokenKind::Keyword(Keyword::Return))?.clone();

                        // HACK: Try to parse an expression and then restore the input to its
                        // previous state if that fails. This simulates an "optional" value without
                        // us having to figure out how to lookahead for this.
                        let input = self.input;
                        let expr = match self.expr_bp(r_bp) {
                            Ok(expr) => Some(expr),
                            Err(_) => {
                                self.input = input;
                                None
                            },
                        };

                        ast::Expr::Return(Box::new(ast::ReturnExpr {return_token, expr}))
                    },

                    PrefixOp::Break => {
                        let break_token = self.input.match_kind(TokenKind::Keyword(Keyword::Break))?.clone();

                        ast::Expr::Break(ast::BreakExpr {break_token})
                    },

                    PrefixOp::Continue => {
                        let continue_token = self.input.match_kind(TokenKind::Keyword(Keyword::Continue))?.clone();

                        ast::Expr::Continue(ast::ContinueExpr {continue_token})
                    },

                    PrefixOp::UnaryOp(op) => {
                        let op_token = self.input.match_kind(kind)?.clone();
                        let expr = self.expr_bp(r_bp)?;

                        ast::Expr::UnaryOp(Box::new(ast::UnaryOpExpr {op, op_token, expr}))
                    },
                }
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

                lhs = match op {
                    InfixOp::Assign => {
                        let lvalue = extract_lvalue(lhs)?;
                        let rhs = self.expr_bp(r_bp)?;

                        ast::Expr::Assign(Box::new(ast::AssignExpr {
                            lvalue,
                            equals_token: op_token,
                            rhs,
                        }))
                    },

                    // Desugar `a += b` as `a = a + b`
                    //TODO: This desugaring will NOT work if `a` has side-effects
                    InfixOp::AugAssign(op) => {
                        let lvalue = extract_lvalue(lhs.clone())?;
                        let rhs = self.expr_bp(r_bp)?;

                        ast::Expr::Assign(Box::new(ast::AssignExpr {
                            lvalue,
                            equals_token: op_token.clone(),
                            rhs: ast::Expr::BinaryOp(Box::new(ast::BinaryOpExpr {
                                lhs,
                                op,
                                op_token,
                                rhs,
                            })),
                        }))
                    },

                    // A field access or method call
                    InfixOp::MethodCall => {
                        let name = self.ident()?;

                        // Parse method call arguments
                        let paren_open_token = self.input.match_kind(TokenKind::ParenOpen)?.clone();
                        let args = self.func_args()?;
                        let paren_close_token = self.input.match_kind(TokenKind::ParenClose)?.clone();

                        ast::Expr::MethodCall(Box::new(ast::MethodCallExpr {
                            lhs,
                            dot_token: op_token,
                            name,
                            paren_open_token,
                            args,
                            paren_close_token,
                        }))
                    },

                    InfixOp::Or => {
                        let rhs = self.expr_bp(r_bp)?;

                        ast::Expr::Or(Box::new(ast::OrExpr {
                            lhs,
                            oror_token: op_token,
                            rhs,
                        }))
                    },

                    InfixOp::And => {
                        let rhs = self.expr_bp(r_bp)?;

                        ast::Expr::And(Box::new(ast::AndExpr {
                            lhs,
                            andand_token: op_token,
                            rhs,
                        }))
                    },

                    InfixOp::BinaryOp(op) => {
                        let rhs = self.expr_bp(r_bp)?;

                        ast::Expr::BinaryOp(Box::new(ast::BinaryOpExpr {
                            lhs,
                            op,
                            op_token,
                            rhs,
                        }))
                    },
                };

                continue;
            }

            break;
        }

        Ok(lhs)
    }

    pub(in super) fn cond(&mut self) -> ParseResult<ast::Cond> {
        let if_token = self.input.match_kind(TokenKind::Keyword(Keyword::If))?.clone();

        // To avoid ambiguity, we disable struct literals inside conditions
        // e.g. In `if x { }`, `x { }` is a valid struct literal
        let if_cond = self.expr_without_struct_literals()?;

        let if_body = self.block()?;

        let mut else_if_clauses = Vec::new();
        let mut else_clause = None;
        while self.input.peek().kind == TokenKind::Keyword(Keyword::Else) {
            let else_token = self.input.match_kind(TokenKind::Keyword(Keyword::Else))?.clone();

            match self.input.peek().kind {
                TokenKind::Keyword(Keyword::If) => {
                    let if_token = self.input.match_kind(TokenKind::Keyword(Keyword::If))?.clone();
                    let cond = self.expr_without_struct_literals()?;
                    let body = self.block()?;

                    else_if_clauses.push(ast::ElseIfClause {else_token, if_token, cond, body});
                },

                TokenKind::BraceOpen => {
                    let body = self.block()?;
                    else_clause = Some(ast::ElseClause {else_token, body});
                    break;
                },

                _ => return Err(ParseError::UnexpectedToken {
                    expected: vec![TokenKind::Keyword(Keyword::If), TokenKind::BraceOpen],
                    actual: self.input.advance().clone(),
                }),
            }
        }

        Ok(ast::Cond {if_token, if_cond, if_body, else_if_clauses, else_clause})
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

        let nargs = args.len();
        if nargs > MAX_ARGS {
            let span = args[0].span().to(args[nargs-1].span());
            return Err(ParseError::TooManyArgs {span, nargs});
        }

        Ok(args)
    }

    /// Parses "atomic" expressions that do not contain subexpressions and thus do not need to go
    /// through any operator precedence mechanisms
    fn atom(&mut self) -> ParseResult<ast::Expr> {
        match self.input.peek().kind {
            TokenKind::Ident => {
                let name = self.ident()?;
                if self.allow_struct_literals && self.input.peek().kind == TokenKind::BraceOpen {
                    self.struct_literal(name).map(ast::Expr::StructLiteral)

                } else {
                    Ok(ast::Expr::Ident(name))
                }
            },

            TokenKind::Literal(Literal::Integer) => self.integer_literal().map(ast::Expr::Integer),

            TokenKind::Keyword(Keyword::True) |
            TokenKind::Keyword(Keyword::False) => self.bool_literal().map(ast::Expr::Bool),

            TokenKind::BracketOpen => self.list_literal(),

            TokenKind::Literal(Literal::Byte) => self.byte_literal().map(ast::Expr::Byte),

            _ => self.bstr_literal().map(ast::Expr::BStr),
        }
    }

    fn struct_literal(&mut self, name: ast::Ident) -> ParseResult<ast::StructLiteral> {
        let brace_open_token = self.input.match_kind(TokenKind::BraceOpen)?.clone();

        let mut fields = Vec::new();

        while self.input.peek().kind != TokenKind::BraceClose {
            let field = self.struct_literal_field()?;
            fields.push(field);

            if self.input.peek().kind != TokenKind::Comma {
                break;
            }

            self.input.match_kind(TokenKind::Comma)?;
        }

        let brace_close_token = self.input.match_kind(TokenKind::BraceClose)?.clone();

        Ok(ast::StructLiteral {name, brace_open_token, fields, brace_close_token})
    }

    fn struct_literal_field(&mut self) -> ParseResult<ast::StructLiteralField> {
        let name = self.ident()?;

        let (colon_token, expr) = match self.input.peek().kind {
            TokenKind::Colon => {
                let colon_token = self.input.match_kind(TokenKind::Colon)?.clone();
                let expr = self.expr()?;

                (Some(colon_token), expr)
            },

            // Field-shorthand syntax: `Foo {a}` desugars to `Foo {a: a}`
            _ => (None, ast::Expr::Ident(name.clone())),
        };

        Ok(ast::StructLiteralField {name, colon_token, expr})
    }

    fn integer_literal(&mut self) -> ParseResult<ast::IntegerLiteral> {
        self.input.match_kind(TokenKind::Literal(Literal::Integer)).map(|token| ast::IntegerLiteral {
            value: token.unwrap_integer(),
            span: token.span,
        })
    }

    fn bool_literal(&mut self) -> ParseResult<ast::BoolLiteral> {
        let (kind, value) = match self.input.peek().kind {
            kind@TokenKind::Keyword(Keyword::True) => (kind, true),
            kind@TokenKind::Keyword(Keyword::False) => (kind, false),
            _ => unreachable!(),
        };

        self.input.match_kind(kind).map(|token| ast::BoolLiteral {
            value,
            span: token.span,
        })
    }

    fn list_literal(&mut self) -> ParseResult<ast::Expr> {
        let bracket_open_token = self.input.match_kind(TokenKind::BracketOpen)?.clone();

        let mut items = Vec::new();
        if self.input.peek().kind != TokenKind::BracketClose {
            let item = self.expr()?;

            // Check if this is in fact a list repeat literal
            if self.input.peek().kind == TokenKind::Semicolon {
                let semicolon_token = self.input.match_kind(TokenKind::Semicolon)?.clone();
                let len = self.expr()?;
                let bracket_close_token = self.input.match_kind(TokenKind::BracketClose)?.clone();

                return Ok(ast::Expr::ListRepeat(Box::new(ast::ListRepeatLiteral {
                    bracket_open_token,
                    item,
                    semicolon_token,
                    len,
                    bracket_close_token,
                })));
            }

            items.push(item);
            if self.input.peek().kind == TokenKind::Comma {
                self.input.match_kind(TokenKind::Comma)?;

                while self.input.peek().kind != TokenKind::BracketClose {
                    let item = self.expr()?;
                    items.push(item);

                    if self.input.peek().kind != TokenKind::Comma {
                        break;
                    }

                    self.input.match_kind(TokenKind::Comma)?;
                }
            }
        }

        let bracket_close_token = self.input.match_kind(TokenKind::BracketClose)?.clone();

        Ok(ast::Expr::List(ast::ListLiteral {bracket_open_token, items, bracket_close_token}))
    }

    fn bstr_literal(&mut self) -> ParseResult<ast::BStrLiteral> {
        self.input.match_kind(TokenKind::Literal(Literal::BStr)).map(|token| ast::BStrLiteral {
            value: token.unwrap_bstr().clone(),
            span: token.span,
        })
    }

    fn byte_literal(&mut self) -> ParseResult<ast::ByteLiteral> {
        self.input.match_kind(TokenKind::Literal(Literal::Byte)).map(|token| ast::ByteLiteral {
            value: token.unwrap_byte(),
            span: token.span,
        })
    }
}
