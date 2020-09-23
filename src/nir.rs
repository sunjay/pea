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
    pub paren_open_token: Token,
    pub params: Vec<DefSpan>,
    pub paren_close_token: Token,
    pub body: Block,
    /// The scope containing the function parameters
    pub scope: Scope,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub brace_open_token: Token,
    pub stmts: Vec<Stmt>,
    pub ret_expr: Option<Expr>,
    pub brace_close_token: Token,
    /// The variables declared in this scope
    pub scope: Scope,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Println(PrintlnStmt),
    VarDecl(VarDeclStmt),
    Expr(ExprStmt),
    Cond(Cond),
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrintlnStmt {
    pub println_token: Token,
    pub not_token: Token,
    pub paren_open_token: Token,
    pub expr: Expr,
    pub paren_close_token: Token,
    pub semicolon_token: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDeclStmt {
    pub let_token: Token,
    pub name: DefSpan,
    pub equals_token: Token,
    pub expr: Expr,
    pub semicolon_token: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprStmt {
    pub expr: Expr,
    pub semicolon_token: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Or(Box<OrExpr>),
    And(Box<AndExpr>),
    Cond(Box<Cond>),
    UnaryOp(Box<UnaryOpExpr>),
    BinaryOp(Box<BinaryOpExpr>),
    Assign(Box<AssignExpr>),
    Group(Box<GroupExpr>),
    Call(Box<CallExpr>),
    Return(Box<ReturnExpr>),
    Def(DefSpan),
    Integer(IntegerLiteral),
    Bool(BoolLiteral),
    BStr(BStrLiteral),
}

/// Short-circuiting `||` expression
#[derive(Debug, Clone, PartialEq)]
pub struct OrExpr {
    pub lhs: Expr,
    pub oror_token: Token,
    pub rhs: Expr,
}

/// Short-circuiting `&&` expression
#[derive(Debug, Clone, PartialEq)]
pub struct AndExpr {
    pub lhs: Expr,
    pub andand_token: Token,
    pub rhs: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cond {
    pub if_token: Token,
    pub if_cond: Expr,
    pub if_body: Block,
    pub else_if_clauses: Vec<ElseIfClause>,
    pub else_clause: Option<ElseClause>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElseIfClause {
    pub else_token: Token,
    pub if_token: Token,
    pub cond: Expr,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElseClause {
    pub else_token: Token,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOpExpr {
    pub op: UnaryOp,
    pub op_token: Token,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOpExpr {
    pub lhs: Expr,
    pub op: BinaryOp,
    pub op_token: Token,
    pub rhs: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LValueExpr {
    Def(DefSpan),
}

/// Assignment expression
#[derive(Debug, Clone, PartialEq)]
pub struct AssignExpr {
    pub lvalue: LValueExpr,
    pub equals_token: Token,
    pub rhs: Expr,
}

/// An expression in parens
#[derive(Debug, Clone, PartialEq)]
pub struct GroupExpr {
    pub paren_open_token: Token,
    pub expr: Expr,
    pub paren_close_token: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub lhs: Expr,
    pub paren_open_token: Token,
    pub args: Vec<Expr>,
    pub paren_close_token: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnExpr {
    pub return_token: Token,
    pub expr: Option<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DefSpan {
    pub id: DefId,
    pub span: Span,
}

pub type UnaryOp = ast::UnaryOp;
pub type BinaryOp = ast::BinaryOp;
pub type IntegerLiteral = ast::IntegerLiteral;
pub type BoolLiteral = ast::BoolLiteral;
pub type BStrLiteral = ast::BStrLiteral;
