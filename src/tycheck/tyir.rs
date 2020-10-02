//! An intermediate representation used during type checking to decorate the program with type
//! variables.

use std::sync::Arc;

use crate::{nir::{self, DefTable, Scope}, parser::Token};

use super::constraints::TyVar;

#[derive(Debug)]
pub struct Program {
    pub root_module: Module,
    pub def_table: Arc<DefTable>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: Arc<str>,
    pub decls: Vec<Decl>,
    /// The root scope of the module
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
    pub params: Vec<FuncParam>,
    pub paren_close_token: Token,
    pub right_arrow_token: Option<Token>,
    pub return_ty_var: TyVar,
    pub body: Block,
    /// The scope containing the function parameters
    pub scope: Scope,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncParam {
    pub name: DefSpan,
    pub colon_token: Token,
    pub ty_var: TyVar,
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
    Print(PrintStmt),
    VarDecl(VarDeclStmt),
    Expr(ExprStmt),
    Cond(Cond),
    WhileLoop(WhileLoop),
    Loop(Loop),
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
pub struct PrintStmt {
    pub print_token: Token,
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
    pub colon_token: Option<Token>,
    pub ty_var: TyVar,
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
pub struct WhileLoop {
    pub while_token: Token,
    pub cond: Expr,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Loop {
    pub loop_token: Token,
    pub body: Block,
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
    Break(BreakExpr),
    Continue(ContinueExpr),
    Def(DefSpan),
    Integer(IntegerLiteral),
    Bool(BoolLiteral),
    List(ListLiteral),
    ListRepeat(Box<ListRepeatLiteral>),
    BStr(BStrLiteral),
    Byte(ByteLiteral),
    Unit(UnitLiteral),
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

#[derive(Debug, Clone, PartialEq)]
pub struct ListLiteral {
    pub bracket_open_token: Token,
    pub items: Vec<Expr>,
    pub bracket_close_token: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ListRepeatLiteral {
    pub bracket_open_token: Token,
    pub item: Expr,
    pub semicolon_token: Token,
    pub len: Expr,
    pub bracket_close_token: Token,
}

pub type UnaryOp = nir::UnaryOp;
pub type BinaryOp = nir::BinaryOp;
pub type BreakExpr = nir::BreakExpr;
pub type ContinueExpr = nir::ContinueExpr;
pub type IntegerLiteral = nir::IntegerLiteral;
pub type BoolLiteral = nir::BoolLiteral;
pub type BStrLiteral = nir::BStrLiteral;
pub type ByteLiteral = nir::ByteLiteral;
pub type UnitLiteral = nir::UnitLiteral;

pub type DefSpan = nir::DefSpan;
