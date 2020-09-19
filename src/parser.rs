mod scanner;
mod token;
mod lexer;
mod token_stream;
mod expr;

pub use token::*;

use crate::{
    ast,
    source_files::FileSource,
    diagnostics::Diagnostics,
};

use lexer::Lexer;
use scanner::Scanner;
use token_stream::TokenStream;
use expr::parse_expr;

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
        expected: TokenKind,
        found: Vec<TokenKind>,
    },
}

impl ParseError {
    fn emit(self, diag: &Diagnostics) {
        dbg!(self);
        todo!()
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
        let name = self.input.ident()?;

        let params = self.parens(|_| Ok(()))?;
        let body = self.block()?;

        Ok(ast::FuncDecl {fn_token, name, params, body})
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
            _ => self.expr_stmt().map(ast::Stmt::Expr),
        }
    }

    fn println_stmt(&mut self) -> ParseResult<ast::PrintlnStmt> {
        let println_token = self.input.keyword(Keyword::Println)?.clone();
        let not_token = self.input.match_kind(TokenKind::Not)?.clone();

        let expr = self.parens(|this| this.expr())?;

        self.input.match_kind(TokenKind::Semicolon)?;

        Ok(ast::PrintlnStmt {
            println_token,
            not_token,
            expr,
        })
    }

    fn expr_stmt(&mut self) -> ParseResult<ast::ExprStmt> {
        let expr = self.expr()?;
        let semicolon_token = self.input.match_kind(TokenKind::Semicolon)?.clone();

        Ok(ast::ExprStmt {expr, semicolon_token})
    }

    fn expr(&mut self) -> ParseResult<ast::Expr> {
        parse_expr(&mut self.input)
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
