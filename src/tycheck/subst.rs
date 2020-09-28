use std::{collections::HashMap, iter::FromIterator};

use super::{constraints::TyVar, tyir};

use crate::{cgenir};

#[derive(Debug)]
enum TyStatus {
    Unresolved(super::ty::Ty),
    Resolved(crate::ty::Ty),
}

/// A mapping from type variable to concrete type
#[derive(Debug)]
pub struct Subst {
    types: HashMap<TyVar, TyStatus>,
}

impl FromIterator<(TyVar, super::ty::Ty)> for Subst {
    fn from_iter<I: IntoIterator<Item=(TyVar, super::ty::Ty)>>(iter: I) -> Self {
        Self {
            types: iter.into_iter()
                .map(|(var, ty)| (var, TyStatus::Unresolved(ty)))
                .collect(),
        }
    }
}

impl Subst {
    /// Gets the fully resolved version of the type for a type variable with all inner type
    /// variables already substituted
    pub fn get_resolved(&mut self, ty_var: TyVar) -> crate::ty::Ty {
        let ty = self.types.get(&ty_var)
            .expect("bug: all type variables should be in this table");

        match ty {
            TyStatus::Unresolved(ty) => {
                let ty = ty.clone();
                let ty = self.resolve_ty(ty);
                self.types.insert(ty_var, TyStatus::Resolved(ty.clone()));
                ty
            },

            TyStatus::Resolved(ty) => ty.clone(),
        }
    }

    pub fn apply(&mut self, program: tyir::Program) -> cgenir::Program {
        program.apply_subst(self)
    }

    /// Resolves a `tycheck::ty::Ty` into a `crate::ty::Ty` by recursively resolving all variables
    pub fn resolve_ty(&mut self, ty: super::ty::Ty) -> crate::ty::Ty {
        use super::ty::Ty::*;
        match ty {
            Unit => crate::ty::Ty::Unit,
            Bool => crate::ty::Ty::Bool,
            I64 => crate::ty::Ty::I64,
            U8 => crate::ty::Ty::U8,
            List(ty) => crate::ty::Ty::List(Box::new(self.resolve_ty(*ty))),
            Func(ty) => crate::ty::Ty::Func(Box::new(self.resolve_func_ty(*ty))),
            TyVar(ty_var) => self.get_resolved(ty_var),
        }
    }

    /// Resolves a `tycheck::ty::FuncTy` into a `crate::ty::FuncTy` by recursively resolving all variables
    pub fn resolve_func_ty(&mut self, ty: super::ty::FuncTy) -> crate::ty::FuncTy {
        let param_tys = ty.param_tys.into_iter().map(|param| self.resolve_ty(param)).collect();
        let return_ty = self.resolve_ty(ty.return_ty);

        crate::ty::FuncTy {param_tys, return_ty}
    }
}

trait ApplySubst {
    type Output;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output;
}

impl<T: ApplySubst> ApplySubst for Vec<T> {
    type Output = Vec<<T as ApplySubst>::Output>;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        self.into_iter().map(|item| item.apply_subst(subst)).collect()
    }
}

impl<T: ApplySubst> ApplySubst for Option<T> {
    type Output = Option<<T as ApplySubst>::Output>;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        self.map(|item| item.apply_subst(subst))
    }
}

impl ApplySubst for TyVar {
    type Output = crate::ty::Ty;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        subst.get_resolved(self)
    }
}

impl ApplySubst for tyir::Program {
    type Output = cgenir::Program;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::Program {decls, scope} = self;

        let decls = decls.apply_subst(subst);

        cgenir::Program {decls, scope}
    }
}

impl ApplySubst for tyir::Decl {
    type Output = cgenir::Decl;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        use tyir::Decl::*;
        match self {
            Func(func) => cgenir::Decl::Func(func.apply_subst(subst)),
        }
    }
}

impl ApplySubst for tyir::FuncDecl {
    type Output = cgenir::FuncDecl;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::FuncDecl {
            fn_token,
            name,
            paren_open_token,
            params,
            paren_close_token,
            right_arrow_token,
            return_ty_var,
            body,
            scope,
        } = self;

        let params = params.apply_subst(subst);
        let return_ty = return_ty_var.apply_subst(subst);
        let body = body.apply_subst(subst);

        cgenir::FuncDecl {
            fn_token,
            name,
            paren_open_token,
            params,
            paren_close_token,
            right_arrow_token,
            return_ty,
            body,
            scope,
        }
    }
}

impl ApplySubst for tyir::FuncParam {
    type Output = cgenir::FuncParam;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::FuncParam {name, colon_token, ty_var} = self;

        let ty = ty_var.apply_subst(subst);

        cgenir::FuncParam {name, colon_token, ty}
    }
}

impl ApplySubst for tyir::Block {
    type Output = cgenir::Block;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::Block {brace_open_token, stmts, ret_expr, brace_close_token, scope} = self;

        let stmts = stmts.apply_subst(subst);
        let ret_expr = ret_expr.apply_subst(subst);

        cgenir::Block {brace_open_token, stmts, ret_expr, brace_close_token, scope}
    }
}

impl ApplySubst for tyir::Stmt {
    type Output = cgenir::Stmt;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        use tyir::Stmt::*;
        match self {
            Println(stmt) => cgenir::Stmt::Println(stmt.apply_subst(subst)),
            Print(stmt) => cgenir::Stmt::Print(stmt.apply_subst(subst)),
            VarDecl(stmt) => cgenir::Stmt::VarDecl(stmt.apply_subst(subst)),
            Expr(stmt) => cgenir::Stmt::Expr(stmt.apply_subst(subst)),
            Cond(stmt) => cgenir::Stmt::Cond(stmt.apply_subst(subst)),
            WhileLoop(stmt) => cgenir::Stmt::WhileLoop(stmt.apply_subst(subst)),
            Loop(stmt) => cgenir::Stmt::Loop(stmt.apply_subst(subst)),
        }
    }
}

impl ApplySubst for tyir::PrintlnStmt {
    type Output = cgenir::PrintlnStmt;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::PrintlnStmt {
            println_token,
            not_token,
            paren_open_token,
            expr,
            paren_close_token,
            semicolon_token,
        } = self;

        let expr = expr.apply_subst(subst);

        cgenir::PrintlnStmt {
            println_token,
            not_token,
            paren_open_token,
            expr,
            paren_close_token,
            semicolon_token,
        }
    }
}

impl ApplySubst for tyir::PrintStmt {
    type Output = cgenir::PrintStmt;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::PrintStmt {
            print_token,
            not_token,
            paren_open_token,
            expr,
            paren_close_token,
            semicolon_token,
        } = self;

        let expr = expr.apply_subst(subst);

        cgenir::PrintStmt {
            print_token,
            not_token,
            paren_open_token,
            expr,
            paren_close_token,
            semicolon_token,
        }
    }
}

impl ApplySubst for tyir::VarDeclStmt {
    type Output = cgenir::VarDeclStmt;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::VarDeclStmt {
            let_token,
            name,
            colon_token,
            ty_var,
            equals_token,
            expr,
            semicolon_token,
        } = self;

        let expr = expr.apply_subst(subst);
        let ty = ty_var.apply_subst(subst);

        cgenir::VarDeclStmt {
            let_token,
            name,
            colon_token,
            ty,
            equals_token,
            expr,
            semicolon_token,
        }
    }
}

impl ApplySubst for tyir::ExprStmt {
    type Output = cgenir::ExprStmt;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::ExprStmt {expr, semicolon_token} = self;
        let expr = expr.apply_subst(subst);
        cgenir::ExprStmt {expr, semicolon_token}
    }
}

impl ApplySubst for tyir::WhileLoop {
    type Output = cgenir::WhileLoop;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::WhileLoop {while_token, cond, body} = self;

        let cond = cond.apply_subst(subst);
        let body = body.apply_subst(subst);

        cgenir::WhileLoop {while_token, cond, body}
    }
}

impl ApplySubst for tyir::Loop {
    type Output = cgenir::Loop;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::Loop {loop_token, body} = self;

        let body = body.apply_subst(subst);

        cgenir::Loop {loop_token, body}
    }
}

impl ApplySubst for tyir::Expr {
    type Output = cgenir::Expr;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        use tyir::Expr::*;
        match self {
            Or(expr) => cgenir::Expr::Or(Box::new(expr.apply_subst(subst))),
            And(expr) => cgenir::Expr::And(Box::new(expr.apply_subst(subst))),
            Cond(cond) => cgenir::Expr::Cond(Box::new(cond.apply_subst(subst))),
            UnaryOp(expr) => cgenir::Expr::UnaryOp(Box::new(expr.apply_subst(subst))),
            BinaryOp(expr) => cgenir::Expr::BinaryOp(Box::new(expr.apply_subst(subst))),
            Assign(expr) => cgenir::Expr::Assign(Box::new(expr.apply_subst(subst))),
            Group(expr) => cgenir::Expr::Group(Box::new(expr.apply_subst(subst))),
            Call(call) => cgenir::Expr::Call(Box::new(call.apply_subst(subst))),
            Return(ret) => cgenir::Expr::Return(Box::new(ret.apply_subst(subst))),
            Break(value) => cgenir::Expr::Break(value),
            Continue(value) => cgenir::Expr::Continue(value),
            Def(name) => cgenir::Expr::Def(name),
            Integer(value) => cgenir::Expr::Integer(value),
            Bool(value) => cgenir::Expr::Bool(value),
            List(list) => cgenir::Expr::List(list.apply_subst(subst)),
            ListRepeat(list) => cgenir::Expr::ListRepeat(Box::new(list.apply_subst(subst))),
            BStr(value) => cgenir::Expr::BStr(value),
            Unit(value) => cgenir::Expr::Unit(value),
        }
    }
}

impl ApplySubst for tyir::OrExpr {
    type Output = cgenir::OrExpr;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::OrExpr {lhs, oror_token, rhs} = self;

        let lhs = lhs.apply_subst(subst);
        let rhs = rhs.apply_subst(subst);

        cgenir::OrExpr {lhs, oror_token, rhs}
    }
}

impl ApplySubst for tyir::AndExpr {
    type Output = cgenir::AndExpr;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::AndExpr {lhs, andand_token, rhs} = self;

        let lhs = lhs.apply_subst(subst);
        let rhs = rhs.apply_subst(subst);

        cgenir::AndExpr {lhs, andand_token, rhs}
    }
}

impl ApplySubst for tyir::Cond {
    type Output = cgenir::Cond;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::Cond {
            if_token,
            if_cond,
            if_body,
            else_if_clauses,
            else_clause,
        } = self;

        let if_cond = if_cond.apply_subst(subst);
        let if_body = if_body.apply_subst(subst);
        let else_if_clauses = else_if_clauses.apply_subst(subst);
        let else_clause = else_clause.apply_subst(subst);

        cgenir::Cond {
            if_token,
            if_cond,
            if_body,
            else_if_clauses,
            else_clause,
        }
    }
}

impl ApplySubst for tyir::ElseIfClause {
    type Output = cgenir::ElseIfClause;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::ElseIfClause {else_token, if_token, cond, body} = self;

        let cond = cond.apply_subst(subst);
        let body = body.apply_subst(subst);

        cgenir::ElseIfClause {else_token, if_token, cond, body}
    }
}

impl ApplySubst for tyir::ElseClause {
    type Output = cgenir::ElseClause;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::ElseClause {else_token, body} = self;

        let body = body.apply_subst(subst);

        cgenir::ElseClause {else_token, body}
    }
}

impl ApplySubst for tyir::UnaryOpExpr {
    type Output = cgenir::UnaryOpExpr;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::UnaryOpExpr {op, op_token, expr} = self;

        let expr = expr.apply_subst(subst);

        cgenir::UnaryOpExpr {op, op_token, expr}
    }
}

impl ApplySubst for tyir::BinaryOpExpr {
    type Output = cgenir::BinaryOpExpr;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::BinaryOpExpr {lhs, op, op_token, rhs} = self;

        let lhs = lhs.apply_subst(subst);
        let rhs = rhs.apply_subst(subst);

        cgenir::BinaryOpExpr {lhs, op, op_token, rhs}
    }
}

impl ApplySubst for tyir::AssignExpr {
    type Output = cgenir::AssignExpr;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::AssignExpr {lvalue, equals_token, rhs} = self;

        let lvalue = lvalue.apply_subst(subst);
        let rhs = rhs.apply_subst(subst);

        cgenir::AssignExpr {lvalue, equals_token, rhs}
    }
}

impl ApplySubst for tyir::LValueExpr {
    type Output = cgenir::LValueExpr;

    fn apply_subst(self, _subst: &mut Subst) -> Self::Output {
        use tyir::LValueExpr::*;
        match self {
            Def(name) => cgenir::LValueExpr::Def(name),
        }
    }
}

impl ApplySubst for tyir::GroupExpr {
    type Output = cgenir::GroupExpr;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::GroupExpr {paren_open_token, expr, paren_close_token} = self;

        let expr = expr.apply_subst(subst);

        cgenir::GroupExpr {paren_open_token, expr, paren_close_token}
    }
}

impl ApplySubst for tyir::CallExpr {
    type Output = cgenir::CallExpr;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::CallExpr {lhs, paren_open_token, args, paren_close_token} = self;

        let lhs = lhs.apply_subst(subst);
        let args = args.apply_subst(subst);

        cgenir::CallExpr {lhs, paren_open_token, args, paren_close_token}
    }
}

impl ApplySubst for tyir::ReturnExpr {
    type Output = cgenir::ReturnExpr;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::ReturnExpr {return_token, expr} = self;

        let expr = expr.apply_subst(subst);

        cgenir::ReturnExpr {return_token, expr}
    }
}

impl ApplySubst for tyir::ListLiteral {
    type Output = cgenir::ListLiteral;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::ListLiteral {bracket_open_token, items, bracket_close_token} = self;

        let items = items.apply_subst(subst);

        cgenir::ListLiteral {bracket_open_token, items, bracket_close_token}
    }
}

impl ApplySubst for tyir::ListRepeatLiteral {
    type Output = cgenir::ListRepeatLiteral;

    fn apply_subst(self, subst: &mut Subst) -> Self::Output {
        let tyir::ListRepeatLiteral {bracket_open_token, item, semicolon_token, len, bracket_close_token} = self;

        let item = item.apply_subst(subst);
        let len = len.apply_subst(subst);

        cgenir::ListRepeatLiteral {bracket_open_token, item, semicolon_token, len, bracket_close_token}
    }
}
