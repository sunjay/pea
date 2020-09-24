mod scanner;
mod token;
mod lexer;
mod token_stream;
mod expr;

pub use token::*;

use std::fmt::Write;

use crate::{
    ast,
    source_files::{FileSource, Span},
    diagnostics::Diagnostics,
};

use lexer::Lexer;
use scanner::Scanner;
use token_stream::TokenStream;

pub fn collect_tokens(source: FileSource, diag: &Diagnostics) -> Vec<Token> {
    let scanner = Scanner::new(source);
    let mut lexer = Lexer::new(scanner, diag);

    let mut tokens = Vec::new();
    loop {
        let token = lexer.next();
        if token.kind == TokenKind::Eof {
            tokens.push(token);
            break;
        }
        tokens.push(token);
    }

    tokens
}

pub fn parse_program(input: &[Token], diag: &Diagnostics) -> ast::Program {
    let input = input.into();
    let mut parser = Parser {input, diag};
    parser.parse_program()
}

// Technically we can support up to 256 params, but we want to leave some space for locals too
const MAX_PARAMS: usize = 128;
// -1 for `self`
const MAX_ARGS: usize = MAX_PARAMS - 1;

#[derive(Debug, Clone)]
enum ParseError {
    UnexpectedToken {
        expected: Vec<TokenKind>,
        actual: Token,
    },

    UnsupportedLValue {
        span: Span,
    },

    DuplicateFuncParam {
        param: ast::Ident,
    },

    TooManyParams {
        span: Span,
        nparams: usize,
    },

    TooManyArgs {
        span: Span,
        nargs: usize,
    },
}

impl ParseError {
    fn emit(self, diag: &Diagnostics) {
        use ParseError::*;
        match self {
            UnexpectedToken {mut expected, actual} => {
                expected.sort_unstable();

                let mut message = String::new();
                match &expected[..] {
                    [] => unreachable!("bug: no parser should produce zero expected tokens"),
                    [tk] => write!(message, "expected {}", tk).unwrap(),
                    [tk1, tk2] => write!(message, "expected {} or {}", tk1, tk2).unwrap(),
                    kinds => {
                        write!(message, "expected one of ").unwrap();
                        for kind in &kinds[..kinds.len()-1] {
                            write!(message, "{}, ", kind).unwrap();
                        }
                        write!(message, "or {}", kinds[kinds.len()-1]).unwrap();
                    },
                }
                write!(message, ", found: {}", actual.kind).unwrap();
                diag.span_error(actual.span, message).emit();
            },

            UnsupportedLValue {span} => {
                diag.span_error(span, "unsupported left-hand side of assignment expression").emit();
            },

            DuplicateFuncParam {param} => {
                diag.error(format!("identifier `{}` is bound more than once in this parameter list", param.value))
                    .span_error(param.span, "used as parameter more than once")
                    .emit();
            },

            TooManyParams {span, nparams} => {
                diag.span_error(span, format!("functions may have up to {} parameters but {} were provided", MAX_PARAMS, nparams)).emit();
            },

            TooManyArgs {span, nargs} => {
                diag.span_error(span, format!("function calls may have up to {} arguments but {} were provided", MAX_ARGS, nargs)).emit();
            },
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

struct Parser<'a> {
    input: TokenStream<'a>,
    diag: &'a Diagnostics,
}

impl<'a> Parser<'a> {
    fn parse_program(&mut self) -> ast::Program {
        let mut decls = Vec::new();

        while !self.input.check(TokenKind::Eof) {
            match self.decl() {
                Ok(decl) => decls.push(decl),
                Err(err) => {
                    err.emit(self.diag);
                    break;
                },
            }
        }

        ast::Program {decls}
    }

    fn decl(&mut self) -> ParseResult<ast::Decl> {
        self.func_decl().map(ast::Decl::Func)
    }

    fn func_decl(&mut self) -> ParseResult<ast::FuncDecl> {
        let fn_token = self.input.match_kind(TokenKind::Keyword(Keyword::Fn))?.clone();
        let name = self.ident()?;

        let paren_open_token = self.input.match_kind(TokenKind::ParenOpen)?.clone();
        let params = self.func_params()?;
        let paren_close_token = self.input.match_kind(TokenKind::ParenClose)?.clone();
        let body = self.block()?;

        Ok(ast::FuncDecl {fn_token, name, paren_open_token, params, paren_close_token, body})
    }

    /// Parses comma separated function parameters until the `)` token
    ///
    /// Trailing comma is allowed but optional. The `)` token is not consumed.
    fn func_params(&mut self) -> ParseResult<Vec<ast::Ident>> {
        let mut params = Vec::new();

        while self.input.peek().kind != TokenKind::ParenClose {
            let param = self.ident()?;

            // Function parameters are not allowed to shadow each other
            if params.iter().any(|p: &ast::Ident| p.value == param.value) {
                return Err(ParseError::DuplicateFuncParam {param});
            }

            params.push(param);

            if self.input.peek().kind != TokenKind::Comma {
                break;
            }

            self.input.match_kind(TokenKind::Comma)?;
        }

        let nparams = params.len();
        if nparams > MAX_PARAMS {
            let span = params[0].span.to(params[nparams-1].span);
            return Err(ParseError::TooManyParams {span, nparams});
        }

        Ok(params)
    }

    fn block(&mut self) -> ParseResult<ast::Block> {
        let brace_open_token = self.input.match_kind(TokenKind::BraceOpen)?.clone();

        let mut stmts = Vec::new();
        let mut ret_expr = None;
        while !self.input.check(TokenKind::BraceClose) {
            match self.stmt() {
                Ok(stmt) => match stmt {
                    Ok(stmt) => stmts.push(stmt),
                    Err(expr) => {
                        ret_expr = Some(expr);
                        // Block ends at the return expression
                        break;
                    },
                },
                Err(err) => {
                    err.emit(self.diag);
                    // An error occurred, try to recover by finding the nearest statement terminator
                    while self.input.advance().kind != TokenKind::Semicolon {}
                },
            }
        }

        // HACK: (mild) Any expressions that are also statements must be "pulled out" of the list of
        // statements if they are the final statement in a block and there is no other final
        // expression. Without this, those statements would never be matched as the final expression
        // since the statement parser is run first.
        if ret_expr.is_none() {
            match stmts.last() {
                Some(ast::Stmt::Cond(_)) => if let Some(ast::Stmt::Cond(cond)) = stmts.pop() {
                    ret_expr = Some(ast::Expr::Cond(Box::new(cond)));
                },

                _ => {},
            }
        }

        let brace_close_token = self.input.match_kind(TokenKind::BraceClose)?.clone();

        Ok(ast::Block {brace_open_token, stmts, ret_expr, brace_close_token})
    }

    fn stmt(&mut self) -> ParseResult<Result<ast::Stmt, ast::Expr>> {
        match self.input.peek().kind {
            TokenKind::Keyword(Keyword::Println) => self.println_stmt().map(ast::Stmt::Println).map(Ok),
            TokenKind::Keyword(Keyword::Let) => self.var_decl_stmt().map(ast::Stmt::VarDecl).map(Ok),
            TokenKind::Keyword(Keyword::If) => self.cond_stmt().map(Ok),
            TokenKind::Keyword(Keyword::While) => self.while_loop_stmt().map(ast::Stmt::WhileLoop).map(Ok),
            _ => self.expr_stmt().map(|res| res.map(ast::Stmt::Expr)),
        }
    }

    fn println_stmt(&mut self) -> ParseResult<ast::PrintlnStmt> {
        let println_token = self.input.keyword(Keyword::Println)?.clone();
        let not_token = self.input.match_kind(TokenKind::Not)?.clone();

        let paren_open_token = self.input.match_kind(TokenKind::ParenOpen)?.clone();
        let expr = self.expr()?;
        let paren_close_token = self.input.match_kind(TokenKind::ParenClose)?.clone();

        let semicolon_token = self.input.match_kind(TokenKind::Semicolon)?.clone();

        Ok(ast::PrintlnStmt {
            println_token,
            not_token,
            paren_open_token,
            expr,
            paren_close_token,
            semicolon_token,
        })
    }

    fn var_decl_stmt(&mut self) -> ParseResult<ast::VarDeclStmt> {
        let let_token = self.input.keyword(Keyword::Let)?.clone();
        let name = self.ident()?;
        let equals_token = self.input.match_kind(TokenKind::Equals)?.clone();

        let expr = self.expr()?;

        let semicolon_token = self.input.match_kind(TokenKind::Semicolon)?.clone();

        Ok(ast::VarDeclStmt {let_token, name, equals_token, expr, semicolon_token})
    }

    /// Parses a conditional statement but allows it to be an expression statement if it ends with a
    /// semicolon
    fn cond_stmt(&mut self) -> ParseResult<ast::Stmt> {
        let cond = self.cond()?;

        if self.input.peek().kind == TokenKind::Semicolon {
            let expr = ast::Expr::Cond(Box::new(cond));
            let semicolon_token = self.input.match_kind(TokenKind::Semicolon)?.clone();
            Ok(ast::Stmt::Expr(ast::ExprStmt {expr, semicolon_token}))
        } else {
            Ok(ast::Stmt::Cond(cond))
        }
    }

    fn while_loop_stmt(&mut self) -> ParseResult<ast::WhileLoop> {
        let while_token = self.input.keyword(Keyword::While)?.clone();
        let cond = self.expr()?;
        let body = self.block()?;

        Ok(ast::WhileLoop {while_token, cond, body})
    }

    /// Parses an expression statement or just expression depending on whether the following token
    /// is a semicolon
    fn expr_stmt(&mut self) -> ParseResult<Result<ast::ExprStmt, ast::Expr>> {
        let expr = self.expr()?;

        if self.input.peek().kind == TokenKind::Semicolon {
            let semicolon_token = self.input.match_kind(TokenKind::Semicolon)?.clone();
            Ok(Ok(ast::ExprStmt {expr, semicolon_token}))
        } else {
            Ok(Err(expr))
        }
    }

    fn ident(&mut self) -> ParseResult<ast::Ident> {
        self.input.match_kind(TokenKind::Ident).map(|token| ast::Ident {
            value: token.unwrap_ident().clone(),
            span: token.span,
        })
    }
}
