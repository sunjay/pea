mod scope_stack;

use crate::{
    ast,
    nir,
    diagnostics::Diagnostics,
};

use scope_stack::ScopeStack;

pub struct NameResolver<'a> {
    diag: &'a Diagnostics,

    def_table: nir::DefTable,
    /// The stack of scopes, declarations are added to the top of the stack (last element)
    scope_stack: ScopeStack,
}

impl<'a> NameResolver<'a> {
    pub fn resolve(prog: &ast::Program, diag: &'a Diagnostics) -> nir::Program {
        let mut resolver = Self {
            diag,
            def_table: nir::DefTable::default(),
            scope_stack: ScopeStack::default(),
        };

        resolver.resolve_program(prog)
    }

    fn resolve_program(&mut self, prog: &ast::Program) -> nir::Program {
        let ast::Program {decls} = prog;

        let token = self.scope_stack.push();
        let decls = decls.iter().map(|decl| self.resolve_decl(decl)).collect();
        let scope = self.scope_stack.pop(token);

        nir::Program {decls, scope}
    }

    fn resolve_decl(&mut self, decl: &ast::Decl) -> nir::Decl {
        use ast::Decl::*;
        match decl {
            Func(func) => nir::Decl::Func(self.resolve_func_decl(func)),
        }
    }

    fn resolve_func_decl(&mut self, func: &ast::FuncDecl) -> nir::FuncDecl {
        let ast::FuncDecl {fn_token, name, params, body} = func;

        let fn_token = fn_token.clone();
        let name = self.declare_func(name);
        let params = params.clone(); //TODO
        let body = self.resolve_block(body);

        nir::FuncDecl {fn_token, name, params, body}
    }

    fn resolve_block(&mut self, block: &ast::Block) -> nir::Block {
        let ast::Block {brace_open_token, stmts, brace_close_token} = block;
        let brace_open_token = brace_open_token.clone();
        let brace_close_token = brace_close_token.clone();

        //TODO: Push new var scope

        let stmts = stmts.iter()
            .map(|stmt| self.resolve_stmt(stmt))
            .collect();

        //TODO: Pop var scope

        nir::Block {brace_open_token, stmts, brace_close_token}
    }

    fn resolve_stmt(&mut self, stmt: &ast::Stmt) -> nir::Stmt {
        use ast::Stmt::*;
        match stmt {
            Println(stmt) => nir::Stmt::Println(self.resolve_println_stmt(stmt)),
            Expr(stmt) => nir::Stmt::Expr(self.resolve_expr_stmt(stmt)),
        }
    }

    fn resolve_println_stmt(&mut self, stmt: &ast::PrintlnStmt) -> nir::PrintlnStmt {
        let ast::PrintlnStmt {println_token, not_token, expr} = stmt;
        let println_token = println_token.clone();
        let not_token = not_token.clone();
        let expr = expr.map(|expr| self.resolve_expr(expr));

        nir::PrintlnStmt {println_token, not_token, expr}
    }

    fn resolve_expr_stmt(&mut self, stmt: &ast::ExprStmt) -> nir::ExprStmt {
        let ast::ExprStmt {expr, semicolon_token} = stmt;
        let expr = self.resolve_expr(expr);
        let semicolon_token = semicolon_token.clone();
        nir::ExprStmt {expr, semicolon_token}
    }

    fn resolve_expr(&mut self, expr: &ast::Expr) -> nir::Expr {
        use ast::Expr::*;
        match expr {
            Call(call) => nir::Expr::Call(self.resolve_call(call)),
            Integer(value) => nir::Expr::Integer(value.clone()),
        }
    }

    fn resolve_call(&mut self, call: &ast::CallExpr) -> nir::CallExpr {
        let ast::CallExpr {name, args} = call;

        let name = self.lookup_func(name);
        let args = args.map(|args| args.clone()); //TODO: Walk args

        nir::CallExpr {name, args}
    }

    /// Declares a function name in the current scope
    ///
    /// The returned `DefSpan` preserves the `Span` of the given `Ident`
    fn declare_func(&mut self, name: &ast::Ident) -> nir::DefSpan {
        let id = self.def_table.insert(name.clone());
        self.scope_stack.top_mut().functions.insert(name.value.clone(), id);

        nir::DefSpan {
            id,
            span: name.span,
        }
    }

    /// Lookup a function name in the current scope or surrounding scopes
    ///
    /// Emits an error if the name cannot be resolved and returns a placeholder `DefId`
    ///
    /// The returned `DefSpan` preserves the `Span` of the given `Ident`
    fn lookup_func(&mut self, name: &ast::Ident) -> nir::DefSpan {
        match self.scope_stack.top().functions.get(&name.value) {
            Some(&id) => nir::DefSpan {
                id,
                span: name.span,
            },
            None => {
                self.diag.span_error(name.span, format!("cannot find function `{}` in this scope", name.value)).emit();

                // Error recovery: Insert this name and pretend it exists so we can continue past this
                self.declare_func(name)
            },
        }
    }
}
