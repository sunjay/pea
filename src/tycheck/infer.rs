//! Type inference algorithm

use std::collections::HashMap;

use crate::{diagnostics::Diagnostics, nir::{self, DefId}, ty::Ty};

use super::{constraints::{ConstraintSet, TyVar}, tyir};

pub struct Context<'a> {
    pub diag: &'a Diagnostics,
    pub constraints: ConstraintSet,
    pub def_vars: HashMap<DefId, TyVar>,
}

impl<'a> Context<'a> {
    pub fn new(diag: &'a Diagnostics) -> Self {
        Self {
            diag,
            constraints: Default::default(),
            def_vars: Default::default(),
        }
    }

    pub fn fresh_type_var(&mut self) -> TyVar {
        self.constraints.fresh_type_var()
    }

    /// Creates a fresh type variable associated with the given `DefId`
    pub fn fresh_def_type_var(&mut self, def_id: DefId) -> TyVar {
        assert!(!self.def_vars.contains_key(&def_id),
            "bug: `DefId` was assigned to a type variable more than once");

        let ty_var = self.fresh_type_var();
        self.def_vars.insert(def_id, ty_var);
        ty_var
    }

    /// Adds a constraint that asserts that the given type variable is the given type
    pub fn ty_var_is_ty(&mut self, ty_var: TyVar, ty: Ty) {
        todo!()
    }

    /// Adds a constraint that asserts that the given type variables unify
    pub fn ty_vars_unify(&mut self, ty_var1: TyVar, ty_var2: TyVar) {
        todo!()
    }

    /// Allows the given type variable to default to `()` if ambiguous
    pub fn ty_var_default_unit(&mut self, ty_var: TyVar) {
        self.constraints.ty_var_default_unit(ty_var);
    }
}

pub fn infer_program(ctx: &mut Context, program: &nir::Program) -> tyir::Program {
    let nir::Program {decls, scope} = program;

    let decls = decls.iter().map(|decl| infer_decl(ctx, decl)).collect();
    let scope = scope.clone();

    tyir::Program {decls, scope}
}

fn infer_decl(ctx: &mut Context, decl: &nir::Decl) -> tyir::Decl {
    use nir::Decl::*;
    match decl {
        Func(func) => tyir::Decl::Func(infer_func_decl(ctx, func)),
    }
}

fn infer_func_decl(ctx: &mut Context, func: &nir::FuncDecl) -> tyir::FuncDecl {
    let nir::FuncDecl {
        fn_token,
        name,
        paren_open_token,
        params,
        paren_close_token,
        return_ty,
        body,
        scope,
    } = func;

    let fn_token = fn_token.clone();
    let name = name.clone();
    let paren_open_token = paren_open_token.clone();
    let params = params.iter().map(|param| infer_func_param(ctx, param)).collect();
    let paren_close_token = paren_close_token.clone();

    let return_ty_var = ctx.fresh_type_var();
    let (right_arrow_token, return_ty) = match return_ty {
        Some(nir::ReturnTy {right_arrow_token, ty}) => {
            (Some(right_arrow_token.clone()), ty.into())
        },

        None => {
            // Default return type is `()`
            (None, Ty::Unit)
        },
    };
    ctx.ty_var_is_ty(return_ty_var, return_ty);

    let body = infer_block(ctx, body, return_ty_var);
    let scope = scope.clone();

    tyir::FuncDecl {
        fn_token,
        name,
        paren_open_token,
        params,
        paren_close_token,
        right_arrow_token,
        return_ty_var,
        body,
        scope,
    }
}

fn infer_func_param(ctx: &mut Context, param: &nir::FuncParam) -> tyir::FuncParam {
    let nir::FuncParam {name, colon_token, ty} = param;

    let name = *name;
    let colon_token = colon_token.clone();
    let ty_var = ctx.fresh_def_type_var(name.id);
    ctx.ty_var_is_ty(ty_var, ty.into());

    tyir::FuncParam {name, colon_token, ty_var}
}

fn infer_block(ctx: &mut Context, block: &nir::Block, return_ty_var: TyVar) -> tyir::Block {
    let nir::Block {
        brace_open_token,
        stmts,
        ret_expr,
        brace_close_token,
        scope,
    } = block;
    let brace_open_token = brace_open_token.clone();
    let brace_close_token = brace_close_token.clone();

    let stmts = stmts.iter().map(|stmt| infer_stmt(ctx, stmt)).collect();

    let ret_expr = match ret_expr {
        Some(expr) => Some(infer_expr(ctx, expr, return_ty_var)),

        None => {
            // Blocks without a return expression must return `()`
            ctx.ty_var_is_ty(return_ty_var, Ty::Unit);

            None
        },
    };

    let scope = scope.clone();

    tyir::Block {
        brace_open_token,
        stmts,
        ret_expr,
        brace_close_token,
        scope,
    }
}

fn infer_stmt(ctx: &mut Context, stmt: &nir::Stmt) -> tyir::Stmt {
    use nir::Stmt::*;
    match stmt {
        Println(stmt) => tyir::Stmt::Println(infer_println_stmt(ctx, stmt)),
        Print(stmt) => tyir::Stmt::Print(infer_print_stmt(ctx, stmt)),
        VarDecl(stmt) => tyir::Stmt::VarDecl(infer_var_decl_stmt(ctx, stmt)),
        Expr(stmt) => tyir::Stmt::Expr(infer_expr_stmt(ctx, stmt)),
        Cond(stmt) => tyir::Stmt::Cond(infer_cond_stmt(ctx, stmt)),
        WhileLoop(stmt) => tyir::Stmt::WhileLoop(infer_while_loop(ctx, stmt)),
        Loop(stmt) => tyir::Stmt::Loop(infer_loop_stmt(ctx, stmt)),
    }
}

fn infer_println_stmt(ctx: &mut Context, stmt: &nir::PrintlnStmt) -> tyir::PrintlnStmt {
    let nir::PrintlnStmt {
        println_token,
        not_token,
        paren_open_token,
        expr,
        paren_close_token,
        semicolon_token,
    } = stmt;

    let println_token = println_token.clone();
    let not_token = not_token.clone();
    let paren_open_token = paren_open_token.clone();

    // `println` can print any type, so no need to constrain the type
    let return_ty_var = ctx.fresh_type_var();
    let expr = infer_expr(ctx, expr, return_ty_var);

    let paren_close_token = paren_close_token.clone();
    let semicolon_token = semicolon_token.clone();

    tyir::PrintlnStmt {
        println_token,
        not_token,
        paren_open_token,
        expr,
        paren_close_token,
        semicolon_token,
    }
}

fn infer_print_stmt(ctx: &mut Context, stmt: &nir::PrintStmt) -> tyir::PrintStmt {
    let nir::PrintStmt {
        print_token,
        not_token,
        paren_open_token,
        expr,
        paren_close_token,
        semicolon_token,
    } = stmt;

    let print_token = print_token.clone();
    let not_token = not_token.clone();
    let paren_open_token = paren_open_token.clone();

    // `print` can print any type, so no need to constrain the type
    let return_ty_var = ctx.fresh_type_var();
    let expr = infer_expr(ctx, expr, return_ty_var);

    let paren_close_token = paren_close_token.clone();
    let semicolon_token = semicolon_token.clone();

    tyir::PrintStmt {
        print_token,
        not_token,
        paren_open_token,
        expr,
        paren_close_token,
        semicolon_token,
    }
}

fn infer_var_decl_stmt(ctx: &mut Context, stmt: &nir::VarDeclStmt) -> tyir::VarDeclStmt {
    let nir::VarDeclStmt {let_token, name, ty, equals_token, expr, semicolon_token} = stmt;

    let let_token = let_token.clone();
    let name = *name;
    let equals_token = equals_token.clone();
    let semicolon_token = semicolon_token.clone();

    // Create a fresh variable for the type of this variable's expression
    let ty_var = ctx.fresh_def_type_var(name.id);
    // Optionally assert that the expression is the type annotated on the variable
    let colon_token = match ty {
        Some(nir::VarDeclTy {colon_token, ty}) => {
            ctx.ty_var_is_ty(ty_var, ty.into());

            Some(colon_token.clone())
        },

        None => None,
    };

    let expr = infer_expr(ctx, expr, ty_var);

    tyir::VarDeclStmt {let_token, name, colon_token, ty_var, equals_token, expr, semicolon_token}
}

fn infer_expr_stmt(ctx: &mut Context, stmt: &nir::ExprStmt) -> tyir::ExprStmt {
    let nir::ExprStmt {expr, semicolon_token} = stmt;

    // Expressions in expression statements may evaluate to any type
    //
    // The value of that type will be discarded due to the trailing semicolon at the end of the
    // statement
    let return_ty_var = ctx.fresh_type_var();
    let expr = infer_expr(ctx, expr, return_ty_var);

    let semicolon_token = semicolon_token.clone();

    tyir::ExprStmt {expr, semicolon_token}
}

fn infer_cond_stmt(ctx: &mut Context, stmt: &nir::Cond) -> tyir::Cond {
    // A cond stmt is exactly the same as a cond expression except that it is required to return `()`
    let return_ty_var = ctx.fresh_type_var();
    ctx.ty_var_is_ty(return_ty_var, Ty::Unit);

    infer_cond(ctx, stmt, return_ty_var)
}

fn infer_while_loop(ctx: &mut Context, while_loop: &nir::WhileLoop) -> tyir::WhileLoop {
    let nir::WhileLoop {while_token, cond, body} = while_loop;

    let while_token = while_token.clone();

    // The condition of a loop must be `bool`
    let cond_ty_var = ctx.fresh_type_var();
    ctx.ty_var_is_ty(cond_ty_var, Ty::Bool);

    let cond = infer_expr(ctx, cond, cond_ty_var);

    // The body of a while loop must be `()`
    let return_ty_var = ctx.fresh_type_var();
    ctx.ty_var_is_ty(return_ty_var, Ty::Unit);

    let body = infer_block(ctx, body, return_ty_var);

    tyir::WhileLoop {while_token, cond, body}
}

fn infer_loop_stmt(ctx: &mut Context, stmt: &nir::Loop) -> tyir::Loop {
    let nir::Loop {loop_token, body} = stmt;

    let loop_token = loop_token.clone();

    // The body of a loop must be `()`
    let return_ty_var = ctx.fresh_type_var();
    ctx.ty_var_is_ty(return_ty_var, Ty::Unit);

    let body = infer_block(ctx, body, return_ty_var);

    tyir::Loop {loop_token, body}
}

fn infer_expr(ctx: &mut Context, expr: &nir::Expr, return_ty_var: TyVar) -> tyir::Expr {
    use nir::Expr::*;
    match expr {
        Or(expr) => tyir::Expr::Or(Box::new(infer_or(ctx, expr, return_ty_var))),
        And(expr) => tyir::Expr::And(Box::new(infer_and(ctx, expr, return_ty_var))),
        Cond(cond) => tyir::Expr::Cond(Box::new(infer_cond(ctx, cond, return_ty_var))),
        UnaryOp(expr) => tyir::Expr::UnaryOp(Box::new(infer_unary_op(ctx, expr, return_ty_var))),
        BinaryOp(expr) => tyir::Expr::BinaryOp(Box::new(infer_binary_op(ctx, expr, return_ty_var))),
        Assign(expr) => tyir::Expr::Assign(Box::new(infer_assign(ctx, expr, return_ty_var))),
        Group(expr) => tyir::Expr::Group(Box::new(infer_group(ctx, expr, return_ty_var))),
        Call(call) => tyir::Expr::Call(Box::new(infer_call(ctx, call, return_ty_var))),
        Return(ret) => tyir::Expr::Return(Box::new(infer_return(ctx, ret, return_ty_var))),
        Break(value) => tyir::Expr::Break(infer_break(ctx, value, return_ty_var)),
        Continue(value) => tyir::Expr::Continue(infer_continue(ctx, value, return_ty_var)),
        Def(name) => tyir::Expr::Def(infer_def(ctx, name, return_ty_var)),
        Integer(value) => tyir::Expr::Integer(infer_integer(ctx, value, return_ty_var)),
        Bool(value) => tyir::Expr::Bool(infer_bool(ctx, value, return_ty_var)),
        List(list) => tyir::Expr::List(infer_list(ctx, list, return_ty_var)),
        ListRepeat(list) => tyir::Expr::ListRepeat(Box::new(infer_list_repeat(ctx, list, return_ty_var))),
        BStr(value) => tyir::Expr::BStr(infer_bstr(ctx, value, return_ty_var)),
        Unit(value) => tyir::Expr::Unit(infer_unit(ctx, value, return_ty_var)),
    }
}

fn infer_or(ctx: &mut Context, expr: &nir::OrExpr, return_ty_var: TyVar) -> tyir::OrExpr {
    let nir::OrExpr {lhs, oror_token, rhs} = expr;

    // Boolean expressions must be `bool`
    ctx.ty_var_is_ty(return_ty_var, Ty::Bool);

    let lhs = infer_expr(ctx, lhs, return_ty_var);
    let oror_token = oror_token.clone();
    let rhs = infer_expr(ctx, rhs, return_ty_var);

    tyir::OrExpr {lhs, oror_token, rhs}
}

fn infer_and(ctx: &mut Context, expr: &nir::AndExpr, return_ty_var: TyVar) -> tyir::AndExpr {
    let nir::AndExpr {lhs, andand_token, rhs} = expr;

    // Boolean expressions must be `bool`
    ctx.ty_var_is_ty(return_ty_var, Ty::Bool);

    let lhs = infer_expr(ctx, lhs, return_ty_var);
    let andand_token = andand_token.clone();
    let rhs = infer_expr(ctx, rhs, return_ty_var);

    tyir::AndExpr {lhs, andand_token, rhs}
}

fn infer_cond(ctx: &mut Context, cond: &nir::Cond, return_ty_var: TyVar) -> tyir::Cond {
    let nir::Cond {
        if_token,
        if_cond,
        if_body,
        else_if_clauses,
        else_clause,
    } = cond;

    let if_token = if_token.clone();

    // The condition must be `bool`
    let if_cond_ty_var = ctx.fresh_type_var();
    ctx.ty_var_is_ty(if_cond_ty_var, Ty::Bool);

    let if_cond = infer_expr(ctx, if_cond, if_cond_ty_var);
    let if_body = infer_block(ctx, if_body, return_ty_var);

    let else_if_clauses = else_if_clauses.iter().map(|clause| {
        let nir::ElseIfClause {else_token, if_token, cond, body} = clause;
        let else_token = else_token.clone();
        let if_token = if_token.clone();

        // The condition must be `bool`
        let cond_ty_var = ctx.fresh_type_var();
        ctx.ty_var_is_ty(cond_ty_var, Ty::Bool);

        let cond = infer_expr(ctx, cond, cond_ty_var);
        let body = infer_block(ctx, body, return_ty_var);

        tyir::ElseIfClause {else_token, if_token, cond, body}
    }).collect();

    let else_clause = match else_clause {
        Some(clause) => {
            let nir::ElseClause {else_token, body} = clause;

            let else_token = else_token.clone();
            let body = infer_block(ctx, body, return_ty_var);

            Some(tyir::ElseClause {else_token, body})
        },

        None => {
            // Any conditions without an `else` block must be `()` since all branches must produce
            // the same type and no other type can be produced for the non-existent else branch
            ctx.ty_var_is_ty(return_ty_var, Ty::Unit);

            None
        },
    };

    tyir::Cond {
        if_token,
        if_cond,
        if_body,
        else_if_clauses,
        else_clause,
    }
}

fn infer_unary_op(ctx: &mut Context, expr: &nir::UnaryOpExpr, return_ty_var: TyVar) -> tyir::UnaryOpExpr {
    let nir::UnaryOpExpr {op, op_token, expr} = expr;

    let op = *op;
    let op_token = op_token.clone();
    //TODO: (Soundness) Currently allows through any type, even if the operator is not supported.
    //  To fix: resolve this as a method call and assert the types properly.
    let expr = infer_expr(ctx, expr, return_ty_var);

    tyir::UnaryOpExpr {op, op_token, expr}
}

fn infer_binary_op(ctx: &mut Context, expr: &nir::BinaryOpExpr, return_ty_var: TyVar) -> tyir::BinaryOpExpr {
    let nir::BinaryOpExpr {lhs, op, op_token, rhs} = expr;

    //TODO: (Soundness) Currently allows through any type, even if the operator is not supported.
    //  To fix: resolve this as a method call and assert the types properly.

    let lhs = infer_expr(ctx, lhs, return_ty_var);
    let op = *op;
    let op_token = op_token.clone();
    let rhs = infer_expr(ctx, rhs, return_ty_var);

    tyir::BinaryOpExpr {lhs, op, op_token, rhs}
}

fn infer_assign(ctx: &mut Context, expr: &nir::AssignExpr, return_ty_var: TyVar) -> tyir::AssignExpr {
    let nir::AssignExpr {lvalue, equals_token, rhs} = expr;

    // Assignment always evaluates to `()`
    ctx.ty_var_is_ty(return_ty_var, Ty::Unit);

    // Both sides of the assignment must be the same type
    let assign_ty_var = ctx.fresh_type_var();

    let lvalue = infer_lvalue(ctx, lvalue, assign_ty_var);
    let equals_token = equals_token.clone();
    let rhs = infer_expr(ctx, rhs, assign_ty_var);

    tyir::AssignExpr {lvalue, equals_token, rhs}
}

fn infer_lvalue(ctx: &mut Context, lvalue: &nir::LValueExpr, return_ty_var: TyVar) -> tyir::LValueExpr {
    use nir::LValueExpr::*;
    match lvalue {
        Def(def) => tyir::LValueExpr::Def(infer_def(ctx, def, return_ty_var)),
    }
}

fn infer_group(ctx: &mut Context, expr: &nir::GroupExpr, return_ty_var: TyVar) -> tyir::GroupExpr {
    let nir::GroupExpr {paren_open_token, expr, paren_close_token} = expr;

    let paren_open_token = paren_open_token.clone();
    let expr = infer_expr(ctx, expr, return_ty_var);
    let paren_close_token = paren_close_token.clone();

    tyir::GroupExpr {paren_open_token, expr, paren_close_token}
}

fn infer_call(ctx: &mut Context, call: &nir::CallExpr, return_ty_var: TyVar) -> tyir::CallExpr {
    let nir::CallExpr {lhs, paren_open_token, args, paren_close_token} = call;

    todo!() //TODO: Generate function type with type variables
    // let lhs = infer_expr(ctx, lhs);
    // let paren_open_token = paren_open_token.clone();
    // let args = args.iter().map(|expr| self.resolve_expr(expr)).collect();
    // let paren_close_token = paren_close_token.clone();

    // tyir::CallExpr {lhs, paren_open_token, args, paren_close_token}
}

fn infer_return(ctx: &mut Context, ret: &nir::ReturnExpr, return_ty_var: TyVar) -> tyir::ReturnExpr {
    let nir::ReturnExpr {return_token, expr} = ret;

    let return_token = return_token.clone();

    // `return` can evaluate to any type, but we default to `()` in case there is nothing else to
    // constrain the type
    ctx.ty_var_default_unit(return_ty_var);

    //TODO: assert that return expression type is the same as the return type of the function we're in
    // let expr = match expr {
    //     Some(expr) => infer_expr()
    // }
    //
    // tyir::ReturnExpr {return_token, expr}
    todo!()
}

fn infer_break(ctx: &mut Context, expr: &nir::BreakExpr, return_ty_var: TyVar) -> tyir::BreakExpr {
    // `break` can evaluate to any type, but we default to `()` in case there is nothing else to
    // constrain the type
    ctx.ty_var_default_unit(return_ty_var);

    expr.clone()
}

fn infer_continue(ctx: &mut Context, expr: &nir::ContinueExpr, return_ty_var: TyVar) -> tyir::ContinueExpr {
    // `continue` can evaluate to any type, but we default to `()` in case there is nothing else to
    // constrain the type
    ctx.ty_var_default_unit(return_ty_var);

    expr.clone()
}

fn infer_def(ctx: &mut Context, expr: &nir::DefSpan, return_ty_var: TyVar) -> tyir::DefSpan {
    todo!()
}

fn infer_integer(ctx: &mut Context, expr: &nir::IntegerLiteral, return_ty_var: TyVar) -> tyir::IntegerLiteral {
    todo!()
}

fn infer_bool(ctx: &mut Context, expr: &nir::BoolLiteral, return_ty_var: TyVar) -> tyir::BoolLiteral {
    todo!()
}

fn infer_list(ctx: &mut Context, expr: &nir::ListLiteral, return_ty_var: TyVar) -> tyir::ListLiteral {
    todo!()
}

fn infer_list_repeat(ctx: &mut Context, expr: &nir::ListRepeatLiteral, return_ty_var: TyVar) -> tyir::ListRepeatLiteral {
    todo!()
}

fn infer_bstr(ctx: &mut Context, expr: &nir::BStrLiteral, return_ty_var: TyVar) -> tyir::BStrLiteral {
    todo!()
}

fn infer_unit(ctx: &mut Context, expr: &nir::UnitLiteral, return_ty_var: TyVar) -> tyir::UnitLiteral {
    todo!()
}
