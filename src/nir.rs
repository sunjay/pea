//! Nameless IR - An IR with all names replaced with `DefId`s.
//!
//! This is the result of name resolution. Names are still kept around so they can be retrieved
//! based on the `DefId` when we're generating errors.
//!
//! Note that this IR still contains field and method names as `Ident`s since those don't get
//! resolved until later when we know the types.

mod def_id;
mod def_table;
mod scope;

pub use def_id::*;
pub use def_table::*;
pub use scope::*;

use std::sync::Arc;

use crate::{ast, parser::Token, source_files::Span};

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
    pub return_ty: Option<ReturnTy>,
    pub body: Block,
    /// The scope containing the function parameters
    pub scope: Scope,
}

impl FuncDecl {
    pub fn span(&self) -> Span {
        self.fn_token.span.to(self.body.span())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncParam {
    pub name: DefSpan,
    pub colon_token: Token,
    pub ty: Ty,
}

impl FuncParam {
    pub fn span(&self) -> Span {
        self.name.span.to(self.ty.span())
    }
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

impl Block {
    pub fn span(&self) -> Span {
        self.brace_open_token.span.to(self.brace_close_token.span)
    }
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
    pub ty: Option<VarDeclTy>,
    pub equals_token: Token,
    pub expr: Expr,
    pub semicolon_token: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDeclTy {
    pub colon_token: Token,
    pub ty: Ty,
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
    MethodCall(Box<MethodCallExpr>),
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

impl Expr {
    pub fn span(&self) -> Span {
        use Expr::*;
        match self {
            Or(expr) => expr.span(),
            And(expr) => expr.span(),
            Cond(expr) => expr.span(),
            UnaryOp(expr) => expr.span(),
            BinaryOp(expr) => expr.span(),
            MethodCall(expr) => expr.span(),
            Assign(expr) => expr.span(),
            Group(expr) => expr.span(),
            Call(expr) => expr.span(),
            Return(expr) => expr.span(),
            Break(expr) => expr.span(),
            Continue(expr) => expr.span(),
            Def(expr) => expr.span,
            Integer(expr) => expr.span,
            Bool(expr) => expr.span,
            List(expr) => expr.span(),
            ListRepeat(expr) => expr.span(),
            BStr(expr) => expr.span,
            Byte(expr) => expr.span,
            Unit(expr) => expr.span(),
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
pub struct MethodCallExpr {
    pub lhs: Expr,
    pub dot_token: Token,
    pub name: Ident,
    pub paren_open_token: Token,
    pub args: Vec<Expr>,
    pub paren_close_token: Token,
}

impl MethodCallExpr {
    pub fn span(&self) -> Span {
        self.lhs.span().to(self.paren_close_token.span)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LValueExpr {
    Def(DefSpan),
}

impl LValueExpr {
    pub fn span(&self) -> Span {
        use LValueExpr::*;
        match self {
            Def(def) => def.span,
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
pub struct ListLiteral {
    pub bracket_open_token: Token,
    pub items: Vec<Expr>,
    pub bracket_close_token: Token,
}

impl ListLiteral {
    pub fn span(&self) -> Span {
        self.bracket_open_token.span.to(self.bracket_close_token.span)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ListRepeatLiteral {
    pub bracket_open_token: Token,
    pub item: Expr,
    pub semicolon_token: Token,
    pub len: Expr,
    pub bracket_close_token: Token,
}

impl ListRepeatLiteral {
    pub fn span(&self) -> Span {
        self.bracket_open_token.span.to(self.bracket_close_token.span)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Unit(UnitTy),
    Bool(Span),
    I64(Span),
    U8(Span),
    List(Box<ListTy>),
    Func(Box<FuncTy>),
}

impl Ty {
    pub fn span(&self) -> Span {
        use Ty::*;
        match self {
            Unit(ty) => ty.span(),
            &Bool(span) => span,
            &I64(span) => span,
            &U8(span) => span,
            List(ty) => ty.span(),
            Func(ty) => ty.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ListTy {
    pub bracket_open_token: Token,
    pub item_ty: Ty,
    pub bracket_close_token: Token,
}

impl ListTy {
    pub fn span(&self) -> Span {
        self.bracket_open_token.span.to(self.bracket_close_token.span)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncTy {
    pub fn_token: Token,
    pub paren_open_token: Token,
    pub param_tys: Vec<Ty>,
    pub paren_close_token: Token,
    pub return_ty: Option<ReturnTy>,
}

impl FuncTy {
    pub fn span(&self) -> Span {
        let end_span = self.return_ty.as_ref()
            .map(|ty| ty.span())
            .unwrap_or(self.paren_close_token.span);

        self.fn_token.span.to(end_span)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnTy {
    pub right_arrow_token: Token,
    pub ty: Ty,
}

impl ReturnTy {
    pub fn span(&self) -> Span {
        self.right_arrow_token.span.to(self.ty.span())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DefSpan {
    pub id: DefId,
    pub span: Span,
}

pub type UnaryOp = ast::UnaryOp;
pub type BinaryOp = ast::BinaryOp;
pub type BreakExpr = ast::BreakExpr;
pub type ContinueExpr = ast::ContinueExpr;
pub type IntegerLiteral = ast::IntegerLiteral;
pub type BoolLiteral = ast::BoolLiteral;
pub type BStrLiteral = ast::BStrLiteral;
pub type ByteLiteral = ast::ByteLiteral;
pub type UnitLiteral = ast::UnitLiteral;
pub type UnitTy = ast::UnitTy;
pub type Ident = ast::Ident;
