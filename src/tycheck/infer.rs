//! Type inference algorithm

use std::collections::HashMap;

use crate::{
    cformat,
    gc::Gc,
    diagnostics::Diagnostics,
    nir::{self, DefId},
    package::Package,
    prelude::PrimMethods,
    source_files::Span,
};

use super::{constraints::{ConstraintSet, TyVar, UnifyErrorSpan}, ty::{FuncTy, Ty, TySpan}, tyir};

pub struct OperatorMethodNames {
    pub pos: Gc<str>,
    pub neg: Gc<str>,
    pub not: Gc<str>,

    pub add: Gc<str>,
    pub sub: Gc<str>,
    pub mul: Gc<str>,
    pub div: Gc<str>,
    pub rem: Gc<str>,

    pub eq: Gc<str>,
    pub ne: Gc<str>,
    pub gt: Gc<str>,
    pub ge: Gc<str>,
    pub lt: Gc<str>,
    pub le: Gc<str>,
}

pub struct Context<'a> {
    prelude: &'a Package,
    prim_methods: &'a PrimMethods,
    def_table: &'a nir::DefTable,
    diag: &'a Diagnostics,
    constraints: ConstraintSet,
    def_vars: HashMap<DefId, TyVar>,
    /// The type variable for the return type of the function currently being checked
    func_return_ty_var: Option<TyVar>,
    /// Cached versions of each operator method name (amortizes heap allocations)
    op_method_names: OperatorMethodNames,
}

impl<'a> Context<'a> {
    pub fn new(
        prelude: &'a Package,
        prim_methods: &'a PrimMethods,
        def_table: &'a nir::DefTable,
        diag: &'a Diagnostics,
    ) -> Self {
        Self {
            prelude,
            prim_methods,
            def_table,
            diag,
            constraints: Default::default(),
            def_vars: Default::default(),
            func_return_ty_var: None,
            op_method_names: OperatorMethodNames {
                pos: "pos".into(),
                neg: "neg".into(),
                not: "not".into(),

                add: "add".into(),
                sub: "sub".into(),
                mul: "mul".into(),
                div: "div".into(),
                rem: "rem".into(),

                // Not using real method names because we don't want these to actually be callable
                // like this. Eventually, we want to replace these with a single `cmp` method.
                eq: "==".into(),
                ne: "!=".into(),
                gt: ">".into(),
                ge: ">=".into(),
                lt: "<".into(),
                le: "<=".into(),
            },
        }
    }

    pub fn into_constraints(self) -> ConstraintSet {
        self.constraints
    }

    /// Returns the type variable associated with the given `DefId` (if any)
    pub fn def_type_var(&self, def_id: DefId) -> Option<TyVar> {
        self.def_vars.get(&def_id).copied()
    }

    /// Attempts to find a method with the given name for the given type
    pub fn lookup_method(&self, ty: TySpan, name: &str, span: Span) -> Option<DefId> {
        let PrimMethods {
            unit: unit_methods,
            bool: bool_methods,
            i64: i64_methods,
            u8: u8_methods,
            list: list_methods,
        } = &self.prim_methods;

        use Ty::*;
        let def_id = match &ty.ty {
            Unit => unit_methods.get(name),
            Bool => bool_methods.get(name),
            I64 => i64_methods.get(name),
            U8 => u8_methods.get(name),
            List(_) => list_methods.get(name),

            //TODO: Functions could support methods once we have polymorphism
            Func(_) => None,

            //TODO: Lookup method for named type
            Named(_def_id) => todo!(),

            //TODO: Not sure which case this can happen in, so leaving it until later
            TyVar(_) => unreachable!(),
        };

        def_id.or_else(|| {
            self.diag.span_error(span, cformat!(self.def_table, "no method named `{}` found for type `{}` in the current scope", name, ty.ty)).emit();

            None
        })
    }

    /// Returns the type that has been inferred so far for the given variable
    pub fn current_type(&mut self, ty_var: TyVar) -> Option<TySpan> {
        self.constraints.current_type(ty_var)
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
    pub fn ty_var_is_ty(&mut self, ty_var: TyVar, ty: Ty, span: impl Into<Option<Span>>) {
        match self.constraints.ty_var_is_ty(ty_var, ty, span) {
            Ok(()) => (),

            Err(err) => self.emit_unify_err(err),
        }
    }

    /// Adds a constraint that asserts that the given type variables unify
    pub fn ty_vars_unify(&mut self, ty_var1: TyVar, ty_var2: TyVar) {
        match self.constraints.ty_vars_unify(ty_var1, ty_var2) {
            Ok(()) => (),

            Err(err) => self.emit_unify_err(err),
        }
    }

    /// Allows the given type variable to default to `()` if ambiguous
    pub fn ty_var_default_unit(&mut self, ty_var: TyVar) {
        self.constraints.ty_var_default_unit(ty_var);
    }

    fn emit_unify_err(&self, err: UnifyErrorSpan) {
        use UnifyErrorSpan::*;
        match err {
            MismatchedTypes {ty1, ty2} => {
                let span = ty2.span.or(ty1.span)
                    .expect("bug: no span that can be used to emit error");

                self.diag.span_error(span, cformat!(self.def_table, "mismatched types: expected `{}`, found: `{}`", ty2.ty, ty1.ty)).emit();
            },
            ArityMismatch {ty1, ty1_arity, ty2, ty2_arity} => {
                let call_span = ty2.span.or(ty1.span)
                    .expect("bug: no span that can be used to emit error");
                let func_span = ty1.span.or(ty2.span)
                    .expect("bug: no span that can be used to emit error");

                self.diag.span_error(call_span, format!("wrong number of arguments: expected {}, found {}", ty1_arity, ty2_arity))
                    .span_note(func_span, cformat!(self.def_table, "function being called has signature `{}`", ty1.ty))
                    .emit();
            },
        }
    }
}

pub fn infer_program(ctx: &mut Context, program: &nir::Program) -> tyir::Program {
    let nir::Program {root_module, def_table} = program;

    let root_module = infer_module(ctx, root_module);
    let def_table = def_table.clone();

    tyir::Program {root_module, def_table}
}

fn infer_module(ctx: &mut Context, module: &nir::Module) -> tyir::Module {
    let nir::Module {name, decls, scope} = module;

    let name = name.clone();
    let decls = infer_decls(ctx, decls);
    let scope = scope.clone();

    tyir::Module {name, decls, scope}
}

fn infer_decls(ctx: &mut Context, decls: &[nir::Decl]) -> Vec<tyir::Decl> {
    // Insert the type of every decl so it can be used throughout the rest of the inference
    for decl in decls {
        use nir::Decl::*;
        match decl {
            Struct(_struct_decl) => {
                //TODO: Record struct fields
            },

            Func(func) => {
                let func_ty_var = ctx.fresh_def_type_var(func.name.id);
                ctx.ty_var_is_ty(func_ty_var, func.into(), func.name.span);
            },
        }
    }

    decls.iter().map(|decl| infer_decl(ctx, decl)).collect()
}

fn infer_decl(ctx: &mut Context, decl: &nir::Decl) -> tyir::Decl {
    use nir::Decl::*;
    match decl {
        Struct(struct_decl) => tyir::Decl::Struct(infer_struct_decl(ctx, struct_decl)),

        Func(func) => tyir::Decl::Func(infer_func_decl(ctx, func)),
    }
}

fn infer_struct_decl(ctx: &mut Context, struct_decl: &nir::StructDecl) -> tyir::StructDecl {
    let nir::StructDecl {
        struct_token,
        name,
        brace_open_token,
        fields,
        brace_close_token,
        scope,
    } = struct_decl;

    let struct_token = struct_token.clone();

    let name = name.clone();

    let brace_open_token = brace_open_token.clone();

    let fields = fields.iter()
        .map(|field| infer_struct_decl_field(ctx, field))
        .collect();

    let brace_close_token = brace_close_token.clone();

    let scope = scope.clone();

    tyir::StructDecl {
        struct_token,
        name,
        brace_open_token,
        fields,
        brace_close_token,
        scope,
    }
}

fn infer_struct_decl_field(ctx: &mut Context, field: &nir::StructDeclField) -> tyir::StructDeclField {
    let nir::StructDeclField {name, colon_token, ty} = field;

    let name = *name;
    let colon_token = colon_token.clone();
    let ty_var = ctx.fresh_def_type_var(name.id);
    ctx.ty_var_is_ty(ty_var, ty.into(), ty.span());

    tyir::StructDeclField {name, colon_token, ty_var}
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
    ctx.ty_var_is_ty(return_ty_var, return_ty, right_arrow_token.as_ref().map(|token| token.span));

    // Allow `return` expressions to assert their type against this function's return type
    assert!(ctx.func_return_ty_var.is_none(), "bug: did not pop function return type variable");
    ctx.func_return_ty_var = Some(return_ty_var);

    let body = infer_block(ctx, body, return_ty_var);
    let scope = scope.clone();

    // Reset the return type variable so it can't be used elsewhere
    ctx.func_return_ty_var = None;

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
    ctx.ty_var_is_ty(ty_var, ty.into(), ty.span());

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
        // Infer type of block from return expression
        Some(expr) => Some(infer_expr(ctx, expr, return_ty_var)),

        None => {
            // To support blocks ending in `return`, `break`, `continue`, etc. we do the following:
            //
            // If any of the direct child nodes of this block return `!`, the block returns `!`
            // Otherwise, it returns `()`.

            //TODO: This is kind of a hack. A more precise way to do this is to do control flow
            // analysis and look at the control flow graph to see if every control path returns the
            // same type. An example of a valid program that would fail to type check without proper
            // control flow analysis:
            //     fn foo(q: i32) -> i32 {
            //         let x = if q > 3 {
            //             return 1;
            //         } else {
            //             return 3;
            //         };
            //     }
            let reaches_end_of_block = block.stmts.iter().all(|stmt| match stmt {
                nir::Stmt::Expr(nir::ExprStmt {expr: nir::Expr::Return(_), ..}) |
                nir::Stmt::Expr(nir::ExprStmt {expr: nir::Expr::Break(_), ..}) |
                nir::Stmt::Expr(nir::ExprStmt {expr: nir::Expr::Continue(_), ..}) => false,

                _ => true,
            });

            if reaches_end_of_block {
                // Type must be `()` since that is the default return value of a block
                ctx.ty_var_is_ty(return_ty_var, Ty::Unit, None);
            }

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
            ctx.ty_var_is_ty(ty_var, ty.into(), ty.span());

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
    ctx.ty_var_is_ty(return_ty_var, Ty::Unit, None);

    infer_cond(ctx, stmt, return_ty_var)
}

fn infer_while_loop(ctx: &mut Context, while_loop: &nir::WhileLoop) -> tyir::WhileLoop {
    let nir::WhileLoop {while_token, cond, body} = while_loop;

    let while_token = while_token.clone();

    // The condition of a loop must be `bool`
    let cond_ty_var = ctx.fresh_type_var();
    ctx.ty_var_is_ty(cond_ty_var, Ty::Bool, None);

    let cond = infer_expr(ctx, cond, cond_ty_var);

    // The body of a while loop must be `()`
    let return_ty_var = ctx.fresh_type_var();
    ctx.ty_var_is_ty(return_ty_var, Ty::Unit, None);

    let body = infer_block(ctx, body, return_ty_var);

    tyir::WhileLoop {while_token, cond, body}
}

fn infer_loop_stmt(ctx: &mut Context, stmt: &nir::Loop) -> tyir::Loop {
    let nir::Loop {loop_token, body} = stmt;

    let loop_token = loop_token.clone();

    // The body of a loop must be `()`
    let return_ty_var = ctx.fresh_type_var();
    ctx.ty_var_is_ty(return_ty_var, Ty::Unit, None);

    let body = infer_block(ctx, body, return_ty_var);

    tyir::Loop {loop_token, body}
}

fn infer_expr(ctx: &mut Context, expr: &nir::Expr, return_ty_var: TyVar) -> tyir::Expr {
    use nir::Expr::*;
    match expr {
        Or(expr) => tyir::Expr::Or(Box::new(infer_or(ctx, expr, return_ty_var))),
        And(expr) => tyir::Expr::And(Box::new(infer_and(ctx, expr, return_ty_var))),
        Cond(cond) => tyir::Expr::Cond(Box::new(infer_cond(ctx, cond, return_ty_var))),
        UnaryOp(expr) => infer_unary_op(ctx, expr, return_ty_var),
        BinaryOp(expr) => infer_binary_op(ctx, expr, return_ty_var),
        MethodCall(expr) => infer_method_call(ctx, expr, return_ty_var),
        Assign(expr) => tyir::Expr::Assign(Box::new(infer_assign(ctx, expr, return_ty_var))),
        Group(expr) => tyir::Expr::Group(Box::new(infer_group(ctx, expr, return_ty_var))),
        Call(call) => tyir::Expr::Call(Box::new(infer_call(ctx, call, return_ty_var))),
        Return(ret) => tyir::Expr::Return(Box::new(infer_return(ctx, ret, return_ty_var))),
        Break(expr) => tyir::Expr::Break(infer_break(ctx, expr, return_ty_var)),
        Continue(expr) => tyir::Expr::Continue(infer_continue(ctx, expr, return_ty_var)),
        Def(name) => tyir::Expr::Def(infer_def(ctx, name, return_ty_var)),
        StructLiteral(lit) => tyir::Expr::StructLiteral(infer_struct_literal(ctx, lit, return_ty_var)),
        Integer(lit) => tyir::Expr::Integer(infer_integer(ctx, lit, return_ty_var)),
        Bool(lit) => tyir::Expr::Bool(infer_bool(ctx, lit, return_ty_var)),
        List(list) => tyir::Expr::List(infer_list(ctx, list, return_ty_var)),
        ListRepeat(list) => tyir::Expr::ListRepeat(Box::new(infer_list_repeat(ctx, list, return_ty_var))),
        BStr(lit) => tyir::Expr::BStr(infer_bstr(ctx, lit, return_ty_var)),
        Byte(lit) => tyir::Expr::Byte(infer_byte(ctx, lit, return_ty_var)),
        Unit(lit) => tyir::Expr::Unit(infer_unit(ctx, lit, return_ty_var)),
    }
}

fn infer_or(ctx: &mut Context, expr: &nir::OrExpr, return_ty_var: TyVar) -> tyir::OrExpr {
    let nir::OrExpr {lhs, oror_token, rhs} = expr;

    // Boolean expressions must be `bool`
    ctx.ty_var_is_ty(return_ty_var, Ty::Bool, None);

    let lhs = infer_expr(ctx, lhs, return_ty_var);
    let oror_token = oror_token.clone();
    let rhs = infer_expr(ctx, rhs, return_ty_var);

    tyir::OrExpr {lhs, oror_token, rhs}
}

fn infer_and(ctx: &mut Context, expr: &nir::AndExpr, return_ty_var: TyVar) -> tyir::AndExpr {
    let nir::AndExpr {lhs, andand_token, rhs} = expr;

    // Boolean expressions must be `bool`
    ctx.ty_var_is_ty(return_ty_var, Ty::Bool, None);

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
    ctx.ty_var_is_ty(if_cond_ty_var, Ty::Bool, None);

    let if_cond = infer_expr(ctx, if_cond, if_cond_ty_var);
    let if_body = infer_block(ctx, if_body, return_ty_var);

    let else_if_clauses = else_if_clauses.iter().map(|clause| {
        let nir::ElseIfClause {else_token, if_token, cond, body} = clause;
        let else_token = else_token.clone();
        let if_token = if_token.clone();

        // The condition must be `bool`
        let cond_ty_var = ctx.fresh_type_var();
        ctx.ty_var_is_ty(cond_ty_var, Ty::Bool, None);

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
            ctx.ty_var_is_ty(return_ty_var, Ty::Unit, None);

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

fn infer_unary_op(ctx: &mut Context, expr: &nir::UnaryOpExpr, return_ty_var: TyVar) -> tyir::Expr {
    let nir::UnaryOpExpr {op, op_token, expr} = expr;

    let method_name = match *op {
        nir::UnaryOp::Pos => ctx.op_method_names.pos.clone(),
        nir::UnaryOp::Neg => ctx.op_method_names.neg.clone(),
        nir::UnaryOp::Not => ctx.op_method_names.not.clone(),
    };

    let method_call = nir::MethodCallExpr {
        lhs: expr.clone(),
        dot_token: op_token.clone(),
        name: nir::Ident {
            value: method_name,
            span: op_token.span,
        },
        paren_open_token: op_token.clone(),
        args: Vec::new(),
        paren_close_token: op_token.clone(),
    };

    infer_method_call(ctx, &method_call, return_ty_var)
}

fn infer_binary_op(ctx: &mut Context, expr: &nir::BinaryOpExpr, return_ty_var: TyVar) -> tyir::Expr {
    let nir::BinaryOpExpr {lhs, op, op_token, rhs} = expr;

    let method_name = match *op {
        nir::BinaryOp::Add => ctx.op_method_names.add.clone(),
        nir::BinaryOp::Sub => ctx.op_method_names.sub.clone(),
        nir::BinaryOp::Mul => ctx.op_method_names.mul.clone(),
        nir::BinaryOp::Div => ctx.op_method_names.div.clone(),
        nir::BinaryOp::Rem => ctx.op_method_names.rem.clone(),

        nir::BinaryOp::EqualsEquals => ctx.op_method_names.eq.clone(),
        nir::BinaryOp::NotEquals => ctx.op_method_names.ne.clone(),
        nir::BinaryOp::GreaterThan => ctx.op_method_names.gt.clone(),
        nir::BinaryOp::GreaterThanEquals => ctx.op_method_names.ge.clone(),
        nir::BinaryOp::LessThan => ctx.op_method_names.lt.clone(),
        nir::BinaryOp::LessThanEquals => ctx.op_method_names.le.clone(),
    };

    let method_call = nir::MethodCallExpr {
        lhs: lhs.clone(),
        dot_token: op_token.clone(),
        name: nir::Ident {
            value: method_name,
            span: op_token.span,
        },
        paren_open_token: op_token.clone(),
        args: vec![rhs.clone()],
        paren_close_token: op_token.clone(),
    };

    infer_method_call(ctx, &method_call, return_ty_var)
}

fn infer_method_call(ctx: &mut Context, expr: &nir::MethodCallExpr, return_ty_var: TyVar) -> tyir::Expr {
    let nir::MethodCallExpr {
        lhs,
        dot_token: _,
        name,
        paren_open_token,
        args: method_args,
        paren_close_token,
    } = expr;
    let paren_open_token = paren_open_token.clone();
    let paren_close_token = paren_close_token.clone();

    // The "receiver" is the `self` argument of the method (i.e. the first argument)
    let receiver_ty_var = ctx.fresh_type_var();
    let receiver_arg = infer_expr(ctx, lhs, receiver_ty_var);

    // Uses the the receiver type to determine which method to call and returns a plain function
    // call that calls that method
    let method_def_id = match ctx.current_type(receiver_ty_var) {
        Some(ty) => match ctx.lookup_method(ty, &name.value, name.span) {
            Some(def_id) => def_id,

            None => {
                // Error recovery: return the receiver argument as-is so we can keep going
                return receiver_arg;
            },
        },

        None => {
            ctx.diag.span_error(lhs.span(), "the type of this value must be known in this context").emit();
            // Error recovery: return the receiver argument as-is so we can keep going
            return receiver_arg;
        },
    };

    // The receiver is the first argument of the generated function call
    let mut param_tys = vec![Ty::TyVar(receiver_ty_var)];
    let mut args = vec![receiver_arg];
    args.reserve(method_args.len());

    for arg in method_args {
        let arg_ty_var = ctx.fresh_type_var();
        param_tys.push(Ty::TyVar(arg_ty_var));

        args.push(infer_expr(ctx, arg, arg_ty_var));
    }

    let method_ty_var = ctx.fresh_type_var();
    // method must be a function type that takes the receiver + the additional args and returns the
    // same type as `return_ty_var`
    ctx.ty_var_is_ty(method_ty_var, Ty::Func(Box::new(FuncTy {
        param_tys,
        return_ty: Ty::TyVar(return_ty_var),
    })), lhs.span());

    let method_def = nir::DefSpan {id: method_def_id, span: name.span};
    let method = tyir::Expr::Def(infer_def(ctx, &method_def, method_ty_var));

    tyir::Expr::Call(Box::new(tyir::CallExpr {
        lhs: method,
        paren_open_token,
        args,
        paren_close_token,
    }))
}

fn infer_assign(ctx: &mut Context, expr: &nir::AssignExpr, return_ty_var: TyVar) -> tyir::AssignExpr {
    let nir::AssignExpr {lvalue, equals_token, rhs} = expr;

    // Assignment always evaluates to `()`
    ctx.ty_var_is_ty(return_ty_var, Ty::Unit, None);

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
    let paren_open_token = paren_open_token.clone();
    let paren_close_token = paren_close_token.clone();

    // Generate a function type for the lhs expression
    let mut param_tys = Vec::new();

    let args = args.iter().map(|arg| {
        let arg_ty_var = ctx.fresh_type_var();
        param_tys.push(Ty::TyVar(arg_ty_var));

        infer_expr(ctx, arg, arg_ty_var)
    }).collect();

    let lhs_ty_var = ctx.fresh_type_var();
    // lhs must be a function type that returns the same type as `return_ty_var` given the parameters
    ctx.ty_var_is_ty(lhs_ty_var, Ty::Func(Box::new(FuncTy {
        param_tys,
        return_ty: Ty::TyVar(return_ty_var),
    })), lhs.span());
    let lhs = infer_expr(ctx, lhs, lhs_ty_var);

    tyir::CallExpr {lhs, paren_open_token, args, paren_close_token}
}

fn infer_return(ctx: &mut Context, ret: &nir::ReturnExpr, return_ty_var: TyVar) -> tyir::ReturnExpr {
    let nir::ReturnExpr {return_token, expr} = ret;

    let return_token = return_token.clone();

    // `return` can evaluate to any type, but we default to `()` in case there is nothing else to
    // constrain the type
    ctx.ty_var_default_unit(return_ty_var);

    let func_return_ty_var = ctx.func_return_ty_var
        .expect("bug: should be inside a function");

    let expr = match expr {
        // Assert that return expression type is the same as the return type of this function
        Some(expr) => Some(infer_expr(ctx, expr, func_return_ty_var)),

        // If there is no expression, we return `()`
        None => {
            ctx.ty_var_is_ty(func_return_ty_var, Ty::Unit, return_token.span);

            None
        },
    };

    tyir::ReturnExpr {return_token, expr}
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

fn infer_def(ctx: &mut Context, def: &nir::DefSpan, return_ty_var: TyVar) -> tyir::DefSpan {
    // The type of this variable must match the type we are expected to return from the expr
    if let Some(ty_var) = ctx.def_type_var(def.id) {
        ctx.ty_vars_unify(ty_var, return_ty_var);

    } else if let Some(ty) = ctx.prelude.def_tys.get(def.id) {
        ctx.ty_var_is_ty(return_ty_var, ty.into(), def.span);

    } else {
        unreachable!("bug: DefId did not have an associated type variable");
    }

    *def
}

fn infer_struct_literal(ctx: &mut Context, lit: &nir::StructLiteral, return_ty_var: TyVar) -> tyir::StructLiteral {
    let nir::StructLiteral {name, brace_open_token, fields, brace_close_token} = lit;

    let name = *name;
    let brace_open_token = brace_open_token.clone();
    let fields = fields.iter()
        .map(|field| infer_struct_literal_field(ctx, field))
        .collect();
    let brace_close_token = brace_close_token.clone();

    // The type of a struct literal is the struct type itself
    ctx.ty_var_is_ty(return_ty_var, Ty::Named(name.id), lit.span());

    tyir::StructLiteral {name, brace_open_token, fields, brace_close_token}
}

fn infer_struct_literal_field(ctx: &mut Context, field: &nir::StructLiteralField) -> tyir::StructLiteralField {
    let nir::StructLiteralField {name, colon_token, expr} = field;

    let name = *name;
    let colon_token = colon_token.clone();

    let field_ty_var = ctx.def_type_var(name.id)
        .expect("bug: all fields should have a type variable already inserted");
    let expr = infer_expr(ctx, expr, field_ty_var);

    tyir::StructLiteralField {name, colon_token, expr}
}

fn infer_integer(ctx: &mut Context, expr: &nir::IntegerLiteral, return_ty_var: TyVar) -> tyir::IntegerLiteral {
    // Assert that the type must be an integer
    //TODO: May want to do something more complicated when multiple integral types are supported
    ctx.ty_var_is_ty(return_ty_var, Ty::I64, expr.span);

    expr.clone()
}

fn infer_bool(ctx: &mut Context, expr: &nir::BoolLiteral, return_ty_var: TyVar) -> tyir::BoolLiteral {
    // Assert that the type must be a bool
    ctx.ty_var_is_ty(return_ty_var, Ty::Bool, expr.span);

    expr.clone()
}

fn infer_list(ctx: &mut Context, expr: &nir::ListLiteral, return_ty_var: TyVar) -> tyir::ListLiteral {
    let nir::ListLiteral {bracket_open_token, items, bracket_close_token} = expr;

    let item_ty_var = ctx.fresh_type_var();
    // The list type with a currently unknown item type
    ctx.ty_var_is_ty(return_ty_var, Ty::List(Box::new(Ty::TyVar(item_ty_var))), expr.span());

    let bracket_open_token = bracket_open_token.clone();
    // Every item must be the same type
    let items = items.iter().map(|item| infer_expr(ctx, item, item_ty_var)).collect();
    let bracket_close_token = bracket_close_token.clone();

    tyir::ListLiteral {bracket_open_token, items, bracket_close_token}
}

fn infer_list_repeat(ctx: &mut Context, expr: &nir::ListRepeatLiteral, return_ty_var: TyVar) -> tyir::ListRepeatLiteral {
    let nir::ListRepeatLiteral {
        bracket_open_token,
        item,
        semicolon_token,
        len,
        bracket_close_token,
    } = expr;

    let bracket_open_token = bracket_open_token.clone();
    let semicolon_token = semicolon_token.clone();
    let bracket_close_token = bracket_close_token.clone();

    // Every item must be the same type
    let item_ty_var = ctx.fresh_type_var();
    // The list type with a currently unknown item type
    ctx.ty_var_is_ty(return_ty_var, Ty::List(Box::new(Ty::TyVar(item_ty_var))), expr.span());

    let item = infer_expr(ctx, item, item_ty_var);

    // The length must be an integer
    //TODO: Probably want this to be `uint` eventually
    let len_ty_var = ctx.fresh_type_var();
    ctx.ty_var_is_ty(len_ty_var, Ty::I64, None);
    let len = infer_expr(ctx, len, len_ty_var);

    tyir::ListRepeatLiteral {
        bracket_open_token,
        item,
        semicolon_token,
        len,
        bracket_close_token,
    }
}

fn infer_bstr(ctx: &mut Context, expr: &nir::BStrLiteral, return_ty_var: TyVar) -> tyir::BStrLiteral {
    // A bstr is of type `[u8]`
    ctx.ty_var_is_ty(return_ty_var, Ty::List(Box::new(Ty::U8)), expr.span);

    expr.clone()
}

fn infer_byte(ctx: &mut Context, expr: &nir::ByteLiteral, return_ty_var: TyVar) -> tyir::ByteLiteral {
    ctx.ty_var_is_ty(return_ty_var, Ty::U8, expr.span);

    expr.clone()
}

fn infer_unit(ctx: &mut Context, expr: &nir::UnitLiteral, return_ty_var: TyVar) -> tyir::UnitLiteral {
    // Assert that the type must be `()`
    ctx.ty_var_is_ty(return_ty_var, Ty::Unit, expr.span());

    expr.clone()
}
