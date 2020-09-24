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
    /// Guaranteed to not contain duplicate names (if no diagnostics were produced)
    pub params: Vec<Ident>,
    pub paren_close_token: Token,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub brace_open_token: Token,
    pub stmts: Vec<Stmt>,
    pub ret_expr: Option<Expr>,
    pub brace_close_token: Token,
}

impl Block {
    pub fn span(&self) -> Span {
        self.brace_open_token.span.to(self.brace_close_token.span)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Println(PrintlnStmt),
    VarDecl(VarDeclStmt),
    Expr(ExprStmt),
    Cond(Cond),
    WhileLoop(WhileLoop),
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
pub struct WhileLoop {
    pub while_token: Token,
    pub cond: Expr,
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
    Ident(Ident),
    Integer(IntegerLiteral),
    Bool(BoolLiteral),
    BStr(BStrLiteral),
}

impl Expr {
    pub fn span(&self) -> Span {
        use Expr::*;
        match self {
            Or(expr) => expr.span(),
            And(expr) => expr.span(),
            Cond(expr) => expr.span(),
            UnaryOp(expr) => expr.span(),
            BinaryOp(expr) => expr.span(),
            Assign(expr) => expr.span(),
            Group(expr) => expr.span(),
            Call(expr) => expr.span(),
            Return(expr) => expr.span(),
            Break(expr) => expr.span(),
            Continue(expr) => expr.span(),
            Ident(expr) => expr.span,
            Integer(expr) => expr.span,
            Bool(expr) => expr.span,
            BStr(expr) => expr.span,
        }
    }
}

/// Short-circuiting `||` expression
#[derive(Debug, Clone, PartialEq)]
pub struct OrExpr {
    pub lhs: Expr,
    pub oror_token: Token,
    pub rhs: Expr,
}

impl OrExpr {
    pub fn span(&self) -> Span {
        self.lhs.span().to(self.rhs.span())
    }
}

/// Short-circuiting `&&` expression
#[derive(Debug, Clone, PartialEq)]
pub struct AndExpr {
    pub lhs: Expr,
    pub andand_token: Token,
    pub rhs: Expr,
}

impl AndExpr {
    pub fn span(&self) -> Span {
        self.lhs.span().to(self.rhs.span())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cond {
    pub if_token: Token,
    pub if_cond: Expr,
    pub if_body: Block,
    pub else_if_clauses: Vec<ElseIfClause>,
    pub else_clause: Option<ElseClause>,
}

impl Cond {
    pub fn span(&self) -> Span {
        match (&self.else_clause, self.else_if_clauses.last()) {
            (Some(else_clause), _) => {
                self.if_token.span.to(else_clause.span())
            },

            (None, Some(else_if_clause)) => {
                self.if_token.span.to(else_if_clause.span())
            },

            (None, None) => self.if_token.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElseIfClause {
    pub else_token: Token,
    pub if_token: Token,
    pub cond: Expr,
    pub body: Block,
}

impl ElseIfClause {
    pub fn span(&self) -> Span {
        self.else_token.span.to(self.body.span())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElseClause {
    pub else_token: Token,
    pub body: Block,
}

impl ElseClause {
    pub fn span(&self) -> Span {
        self.else_token.span.to(self.body.span())
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

    EqualsEquals,
    NotEquals,
    GreaterThan,
    GreaterThanEquals,
    LessThan,
    LessThanEquals,
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

            EqualsEquals => write!(f, "`==`"),
            NotEquals => write!(f, "`!=`"),
            GreaterThan => write!(f, "`>`"),
            GreaterThanEquals => write!(f, "`>=`"),
            LessThan => write!(f, "`<`"),
            LessThanEquals => write!(f, "`<=`"),
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
pub struct ReturnExpr {
    pub return_token: Token,
    pub expr: Option<Expr>,
}

impl ReturnExpr {
    pub fn span(&self) -> Span {
        match &self.expr {
            Some(expr) => self.return_token.span.to(expr.span()),
            None => self.return_token.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BreakExpr {
    pub break_token: Token,
}

impl BreakExpr {
    pub fn span(&self) -> Span {
        self.break_token.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ContinueExpr {
    pub continue_token: Token,
}

impl ContinueExpr {
    pub fn span(&self) -> Span {
        self.continue_token.span
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

#[derive(Debug, Clone, PartialEq)]
pub struct BoolLiteral {
    pub value: bool,
    pub span: Span,
}
