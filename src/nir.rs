//! Nameless IR - An IR with all names replaced with `DefId`s.
//!
//! This is the result of name resolution. Names are still kept around so they can be retrieved
//! based on the `DefId` when we're generating errors.
//!
//! Note that this IR still contains field and method names as `Ident`s since those don't get
//! resolved until later when we know the types.

mod def_table;
mod scope;

pub use def_table::*;
pub use scope::*;

use crate::{ast, parser::Token, source_files::Span};

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub decls: Vec<Decl>,
    /// The root scope of the program
    pub scope: Scope,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Func(FuncDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDecl {
    pub fn_token: Token,
    pub name: DefSpan,
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
    VarDecl(VarDeclStmt),
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
    /// The token for the `;` symbol
    pub semicolon_token: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDeclStmt {
    /// The token for the `let` keyword
    pub let_token: Token,
    /// The variable name being declared
    pub name: DefSpan,
    /// The token for the `=` symbol
    pub equals_token: Token,
    /// The expression assigned to the variable
    pub expr: Expr,
    /// The token for the `;` symbol
    pub semicolon_token: Token,
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
    pub name: DefSpan,
    pub args: Parens<[(); 0]>, //TODO
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DefSpan {
    pub id: DefId,
    pub span: Span,
}

pub type Parens<T> = ast::Parens<T>;
pub type IntegerLiteral = ast::IntegerLiteral;
pub type BStr = ast::BStr;
