use std::sync::Arc;

use crate::{source_files::Span, parser::Token};

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub decls: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Func(FuncDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDecl {
    pub fn_token: Token,
    pub name: Ident,
    pub params: Parens<(/* TODO */)>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub brace_open_token: Token,
    pub stmts: Vec<Stmt>,
    pub brace_close_token: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Println(PrintlnStmt),
    Expr(ExprStmt),
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrintlnStmt {
    /// The token for the `println` keyword
    pub println_token: Token,
    /// The token for the `!` symbol
    pub not_token: Token,
    /// The expression to be printed
    pub expr: Parens<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprStmt {
    pub expr: Expr,
    pub semicolon_token: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Call(CallExpr),
    Integer(IntegerLiteral),
    BStr(BStr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub name: Ident,
    pub args: Parens<[(); 0]>, //TODO
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parens<T> {
    pub paren_open_token: Token,
    pub value: T,
    pub paren_close_token: Token,
}

impl<T> Parens<T> {
    pub fn map<F, U>(&self, f: F) -> Parens<U>
        where F: FnOnce(&T) -> U,
    {
        let Self {paren_open_token, value, paren_close_token} = self;

        Parens {
            paren_open_token: paren_open_token.clone(),
            value: f(value),
            paren_close_token: paren_close_token.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub value: Arc<str>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BStr {
    pub value: Arc<[u8]>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntegerLiteral {
    pub value: i128,
    pub span: Span,
}
