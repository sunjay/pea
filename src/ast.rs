use crate::{source_files::Span, parser::Token};

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Println(PrintlnStmt),
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
pub enum Expr {
    Integer(IntegerLiteral),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parens<T> {
    pub paren_open_token: Token,
    pub value: T,
    pub paren_close_token: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntegerLiteral {
    pub value: i128,
    pub span: Span,
}
