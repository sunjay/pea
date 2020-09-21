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

#[derive(Debug, Clone)]
enum ParseError {
    UnexpectedToken {
        expected: Vec<TokenKind>,
        actual: Token,
    },

    UnsupportedLValue {
        span: Span,
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
        let params = ();
        let paren_close_token = self.input.match_kind(TokenKind::ParenClose)?.clone();
        let body = self.block()?;

        Ok(ast::FuncDecl {fn_token, name, paren_open_token, params, paren_close_token, body})
    }

    fn block(&mut self) -> ParseResult<ast::Block> {
        let brace_open_token = self.input.match_kind(TokenKind::BraceOpen)?.clone();

        let mut stmts = Vec::new();
        while !self.input.check(TokenKind::BraceClose) {
            match self.stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    err.emit(self.diag);
                    // An error occurred, try to recover by finding the nearest statement terminator
                    while self.input.advance().kind != TokenKind::Semicolon {}
                },
            }
        }

        let brace_close_token = self.input.match_kind(TokenKind::BraceClose)?.clone();

        Ok(ast::Block {brace_open_token, stmts, brace_close_token})
    }

    fn stmt(&mut self) -> ParseResult<ast::Stmt> {
        match self.input.peek().kind {
            TokenKind::Keyword(Keyword::Println) => self.println_stmt().map(ast::Stmt::Println),
            TokenKind::Keyword(Keyword::Let) => self.var_decl_stmt().map(ast::Stmt::VarDecl),
            _ => self.expr_stmt().map(ast::Stmt::Expr),
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

    fn expr_stmt(&mut self) -> ParseResult<ast::ExprStmt> {
        let expr = self.expr()?;
        let semicolon_token = self.input.match_kind(TokenKind::Semicolon)?.clone();

        Ok(ast::ExprStmt {expr, semicolon_token})
    }

    fn ident(&mut self) -> ParseResult<ast::Ident> {
        self.input.match_kind(TokenKind::Ident).map(|token| ast::Ident {
            value: token.unwrap_ident().clone(),
            span: token.span,
        })
    }
}
