use std::fmt;
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
    pub paren_open_token: Token,
    pub params: (/* TODO */),
    pub paren_close_token: Token,
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
    pub name: Ident,
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
    UnaryOp(Box<UnaryOpExpr>),
    BinaryOp(Box<BinaryOpExpr>),
    Assign(Box<AssignExpr>),
    Group(Box<GroupExpr>),
    Call(Box<CallExpr>),
    Ident(Ident),
    Integer(IntegerLiteral),
    BStr(BStrLiteral),
}

impl Expr {
    pub fn span(&self) -> Span {
        use Expr::*;
        match self {
            UnaryOp(expr) => expr.span(),
            BinaryOp(expr) => expr.span(),
            Assign(expr) => expr.span(),
            Group(expr) => expr.span(),
            Call(expr) => expr.span(),
            Ident(expr) => expr.span,
            Integer(expr) => expr.span,
            BStr(expr) => expr.span,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Pos,
    Neg,
    Not,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use UnaryOp::*;
        match self {
            Pos => write!(f, "`+`"),
            Neg => write!(f, "`-`"),
            Not => write!(f, "`!`"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOpExpr {
    pub op: UnaryOp,
    pub op_token: Token,
    pub expr: Expr,
}

impl UnaryOpExpr {
    pub fn span(&self) -> Span {
        self.op_token.span.to(self.expr.span())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BinaryOp::*;
        match self {
            Add => write!(f, "`+`"),
            Sub => write!(f, "`-`"),
            Mul => write!(f, "`*`"),
            Div => write!(f, "`/`"),
            Rem => write!(f, "`%`"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOpExpr {
    pub lhs: Expr,
    pub op: BinaryOp,
    pub op_token: Token,
    pub rhs: Expr,
}

impl BinaryOpExpr {
    pub fn span(&self) -> Span {
        self.lhs.span().to(self.rhs.span())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LValueExpr {
    Ident(Ident),
}

impl LValueExpr {
    pub fn span(&self) -> Span {
        use LValueExpr::*;
        match self {
            Ident(ident) => ident.span,
        }
    }
}

/// Assignment expression
#[derive(Debug, Clone, PartialEq)]
pub struct AssignExpr {
    pub lvalue: LValueExpr,
    pub equals_token: Token,
    pub rhs: Expr,
}

impl AssignExpr {
    pub fn span(&self) -> Span {
        self.lvalue.span().to(self.rhs.span())
    }
}

/// An expression in parens
#[derive(Debug, Clone, PartialEq)]
pub struct GroupExpr {
    pub paren_open_token: Token,
    pub expr: Expr,
    pub paren_close_token: Token,
}

impl GroupExpr {
    pub fn span(&self) -> Span {
        self.paren_open_token.span.to(self.paren_close_token.span)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub lhs: Expr,
    pub paren_open_token: Token,
    pub args: Vec<Expr>,
    pub paren_close_token: Token,
}

impl CallExpr {
    pub fn span(&self) -> Span {
        self.lhs.span().to(self.paren_close_token.span)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub value: Arc<str>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BStrLiteral {
    pub value: Arc<[u8]>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntegerLiteral {
    pub value: i128,
    pub span: Span,
}
