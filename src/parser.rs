mod scanner;
mod token;
mod lexer;

pub use token::*;

use crate::{
    ast,
    source_files::FileSource,
    diagnostics::Diagnostics,
};

use lexer::Lexer;
use scanner::Scanner;

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
    input: &'a [Token],
    diag: &'a Diagnostics,
}

impl<'a> Parser<'a> {
    fn parse_program(&mut self) -> ast::Program {
        let mut decls = Vec::new();

        while !self.check(TokenKind::Eof) {
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
        let fn_token = self.match_kind(TokenKind::Keyword(Keyword::Fn))?.clone();
        let name = self.ident()?;

        let params = self.parens(|_| Ok(()))?;
        let body = self.block()?;

        Ok(ast::FuncDecl {fn_token, name, params, body})
    }

    fn block(&mut self) -> ParseResult<ast::Block> {
        let brace_open_token = self.match_kind(TokenKind::BraceOpen)?.clone();

        let mut stmts = Vec::new();
        while !self.check(TokenKind::BraceClose) {
            match self.stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    err.emit(self.diag);
                    // An error occurred, try to recover by finding the nearest statement terminator
                    while self.advance().kind != TokenKind::Semicolon {}
                },
            }
        }

        let brace_close_token = self.match_kind(TokenKind::BraceClose)?.clone();

        Ok(ast::Block {brace_open_token, stmts, brace_close_token})
    }

    fn stmt(&mut self) -> ParseResult<ast::Stmt> {
        match self.input[0].kind {
            TokenKind::Keyword(Keyword::Println) => self.println_stmt().map(ast::Stmt::Println),
            _ => self.expr_stmt().map(ast::Stmt::Expr),
        }
    }

    fn println_stmt(&mut self) -> ParseResult<ast::PrintlnStmt> {
        let println_token = self.keyword(Keyword::Println)?.clone();
        let not_token = self.match_kind(TokenKind::Not)?.clone();

        let expr = self.parens(|this| this.expr())?;

        self.match_kind(TokenKind::Semicolon)?;

        Ok(ast::PrintlnStmt {
            println_token,
            not_token,
            expr,
        })
    }

    fn expr_stmt(&mut self) -> ParseResult<ast::ExprStmt> {
        let expr = self.expr()?;
        let semicolon_token = self.match_kind(TokenKind::Semicolon)?.clone();

        Ok(ast::ExprStmt {expr, semicolon_token})
    }

    fn expr(&mut self) -> ParseResult<ast::Expr> {
        match self.input[0].kind {
            TokenKind::Ident => self.call().map(ast::Expr::Call),
            _ => self.integer_literal().map(ast::Expr::Integer),
        }
    }

    fn call(&mut self) -> ParseResult<ast::CallExpr> {
        let name = self.ident()?;
        let args = self.parens(|_| Ok([]))?;

        Ok(ast::CallExpr {name, args})
    }

    fn parens<T>(
        &mut self,
        parser: impl FnOnce(&mut Self) -> ParseResult<T>,
    ) -> ParseResult<ast::Parens<T>> {
        let paren_open_token = self.match_kind(TokenKind::ParenOpen)?.clone();
        let value = parser(self)?;
        let paren_close_token = self.match_kind(TokenKind::ParenClose)?.clone();
        Ok(ast::Parens {paren_open_token, value, paren_close_token})
    }

    fn integer_literal(&mut self) -> ParseResult<ast::IntegerLiteral> {
        self.match_kind(TokenKind::Literal(Literal::Integer)).map(|token| ast::IntegerLiteral {
            value: token.unwrap_integer(),
            span: token.span,
        })
    }

    fn ident(&mut self) -> ParseResult<ast::Ident> {
        self.match_kind(TokenKind::Ident).map(|token| ast::Ident {
            value: token.unwrap_ident().clone(),
            span: token.span,
        })
    }

    fn keyword(&mut self, keyword: Keyword) -> ParseResult<&Token> {
        self.match_kind(TokenKind::Keyword(keyword))
    }

    fn match_kind(&mut self, expected: TokenKind) -> ParseResult<&Token> {
        let token = self.advance();
        if token.kind == expected {
            Ok(token)
        } else {
            Err(ParseError::UnexpectedToken {
                expected,
                found: vec![token.kind.clone()],
            })
        }
    }

    fn advance(&mut self) -> &Token {
        let token = &self.input[0];
        // Do not advance past the end, because input must always remain non-empty
        if token.kind != TokenKind::Eof {
            self.input = &self.input[1..];
        }
        token
    }

    fn check(&self, kind: TokenKind) -> bool {
        self.input[0].kind == kind
    }
}
