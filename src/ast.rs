use std::fmt;

use crate::{source_files::Span, parser::Token, gc::Gc};

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: Gc<str>,
    pub decls: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Struct(StructDecl),
    Func(FuncDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl {
    pub struct_token: Token,
    pub name: Ident,
    pub brace_open_token: Token,
    pub fields: Vec<StructDeclField>,
    pub brace_close_token: Token,
}

impl StructDecl {
    pub fn span(&self) -> Span {
        self.name.span.to(self.brace_close_token.span)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclField {
    pub name: Ident,
    pub colon_token: Token,
    pub ty: Ty,
}

impl StructDeclField {
    pub fn span(&self) -> Span {
        self.name.span.to(self.ty.span())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDecl {
    pub fn_token: Token,
    pub name: Ident,
    pub paren_open_token: Token,
    /// Guaranteed to not contain duplicate names (if no diagnostics were produced)
    pub params: Vec<FuncParam>,
    pub paren_close_token: Token,
    pub return_ty: Option<ReturnTy>,
    pub body: Block,
}

impl FuncDecl {
    pub fn span(&self) -> Span {
        self.fn_token.span.to(self.body.span())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncParam {
    pub name: Ident,
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
    pub name: Ident,
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

/// An infinite loop
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
    Ident(Ident),
    StructLiteral(StructLiteral),
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
            Ident(expr) => expr.span,
            StructLiteral(expr) => expr.span(),
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
    pub value: Gc<str>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BStrLiteral {
    pub value: Gc<[u8]>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ByteLiteral {
    pub value: u8,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteral {
    pub name: Ident,
    pub brace_open_token: Token,
    pub fields: Vec<StructLiteralField>,
    pub brace_close_token: Token,
}

impl StructLiteral {
    pub fn span(&self) -> Span {
        self.name.span.to(self.brace_close_token.span)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteralField {
    pub name: Ident,
    /// Set to `None` if this field was desugared from the struct literal field shorthand syntax
    pub colon_token: Option<Token>,
    pub expr: Expr,
}

impl StructLiteralField {
    pub fn span(&self) -> Span {
        self.name.span.to(self.expr.span())
    }
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

/// The `()` literal
#[derive(Debug, Clone, PartialEq)]
pub struct UnitLiteral {
    pub paren_open_token: Token,
    pub paren_close_token: Token,
}

impl UnitLiteral {
    pub fn span(&self) -> Span {
        self.paren_open_token.span.to(self.paren_close_token.span)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Unit(UnitTy),
    List(Box<ListTy>),
    Func(Box<FuncTy>),
    Named(Ident),
}

impl Ty {
    pub fn span(&self) -> Span {
        use Ty::*;
        match self {
            Unit(ty) => ty.span(),
            List(ty) => ty.span(),
            Func(ty) => ty.span(),
            Named(ty) => ty.span,
        }
    }
}

/// The `()` type
#[derive(Debug, Clone, PartialEq)]
pub struct UnitTy {
    pub paren_open_token: Token,
    pub paren_close_token: Token,
}

impl UnitTy {
    pub fn span(&self) -> Span {
        self.paren_open_token.span.to(self.paren_close_token.span)
    }
}

/// The `[a]` types
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
