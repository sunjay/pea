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
    pub fn resolve(prog: &ast::Program, diag: &'a Diagnostics) -> (nir::Program, nir::DefTable) {
        let mut resolver = Self {
            diag,
            def_table: nir::DefTable::default(),
            scope_stack: ScopeStack::default(),
        };

        let prog = resolver.resolve_program(prog);
        (prog, resolver.def_table)
    }

    fn resolve_program(&mut self, prog: &ast::Program) -> nir::Program {
        let ast::Program {decls} = prog;

        let token = self.scope_stack.push();
        let decls = self.resolve_decls(decls);
        let scope = self.scope_stack.pop(token);

        nir::Program {decls, scope}
    }

    fn resolve_decls(&mut self, decls: &[ast::Decl]) -> Vec<nir::Decl> {
        // Order of declaration doesn't matter, so go through and declare each name first
        for decl in decls {
            use ast::Decl::*;
            match decl {
                Func(func) => {
                    self.declare_func(&func.name);
                },
            }
        }

        decls.iter().map(|decl| self.resolve_decl(decl)).collect()
    }

    fn resolve_decl(&mut self, decl: &ast::Decl) -> nir::Decl {
        use ast::Decl::*;
        match decl {
            Func(func) => nir::Decl::Func(self.resolve_func_decl(func)),
        }
    }

    fn resolve_func_decl(&mut self, func: &ast::FuncDecl) -> nir::FuncDecl {
        let ast::FuncDecl {fn_token, name, paren_open_token, params, paren_close_token, body} = func;

        let fn_token = fn_token.clone();

        // Note that we are careful here to only look at the top scope instead of calling
        // `self.lookup` because that is where we expect the name to be defined
        let id = self.scope_stack.top().lookup(&name.value)
            .expect("bug: all decls should have already been defined in this scope");
        let name = nir::DefSpan {id, span: name.span};

        let paren_open_token = paren_open_token.clone();
        //TODO: Make a new scope for the function and put the params in it
        let params = params.clone(); //TODO
        let paren_close_token = paren_close_token.clone();
        let body = self.resolve_block(body);

        nir::FuncDecl {fn_token, name, paren_open_token, params, paren_close_token, body}
    }

    fn resolve_block(&mut self, block: &ast::Block) -> nir::Block {
        let ast::Block {brace_open_token, stmts, brace_close_token} = block;
        let brace_open_token = brace_open_token.clone();
        let brace_close_token = brace_close_token.clone();

        // Push a new scope for all the variables in the block
        let token = self.scope_stack.push();

        let stmts = stmts.iter()
            .map(|stmt| self.resolve_stmt(stmt))
            .collect();

        let scope = self.scope_stack.pop(token);

        nir::Block {brace_open_token, stmts, brace_close_token, scope}
    }

    fn resolve_stmt(&mut self, stmt: &ast::Stmt) -> nir::Stmt {
        use ast::Stmt::*;
        match stmt {
            Println(stmt) => nir::Stmt::Println(self.resolve_println_stmt(stmt)),
            VarDecl(stmt) => nir::Stmt::VarDecl(self.resolve_var_decl_stmt(stmt)),
            Expr(stmt) => nir::Stmt::Expr(self.resolve_expr_stmt(stmt)),
        }
    }

    fn resolve_println_stmt(&mut self, stmt: &ast::PrintlnStmt) -> nir::PrintlnStmt {
        let ast::PrintlnStmt {
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
        let expr = self.resolve_expr(expr);
        let paren_close_token = paren_close_token.clone();
        let semicolon_token = semicolon_token.clone();

        nir::PrintlnStmt {
            println_token,
            not_token,
            paren_open_token,
            expr,
            paren_close_token,
            semicolon_token,
        }
    }

    fn resolve_var_decl_stmt(&mut self, stmt: &ast::VarDeclStmt) -> nir::VarDeclStmt {
        let ast::VarDeclStmt {let_token, name, equals_token, expr, semicolon_token} = stmt;
        let let_token = let_token.clone();
        let equals_token = equals_token.clone();
        let semicolon_token = semicolon_token.clone();

        // Note that we are careful to resolve the expression before declaring the variable name
        // because otherwise we would allow `let a = a;` to slip through.
        let expr = self.resolve_expr(expr);
        let name = self.declare(name);

        nir::VarDeclStmt {let_token, name, equals_token, expr, semicolon_token}
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
            UnaryOp(expr) => todo!(),
            BinaryOp(expr) => todo!(),
            Assign(expr) => todo!(),
            Group(group) => todo!(),
            Call(call) => nir::Expr::Call(Box::new(self.resolve_call(call))),
            Ident(name) => nir::Expr::Def(self.lookup(name)),
            Integer(value) => nir::Expr::Integer(value.clone()),
            BStr(value) => nir::Expr::BStr(value.clone()),
        }
    }

    fn resolve_call(&mut self, call: &ast::CallExpr) -> nir::CallExpr {
        let ast::CallExpr {lhs, paren_open_token, args, paren_close_token} = call;

        let lhs = self.resolve_expr(lhs);
        let paren_open_token = paren_open_token.clone();
        let args = args.iter().map(|expr| self.resolve_expr(expr)).collect();
        let paren_close_token = paren_close_token.clone();

        nir::CallExpr {lhs, paren_open_token, args, paren_close_token}
    }

    fn declare_func(&mut self, name: &ast::Ident) -> nir::DefSpan {
        // Functions are not allowed to shadow other names in the same scope
        if let Some(orig) = self.scope_stack.top().lookup(&name.value) {
            let orig = self.def_table.get(orig);
            self.diag.error(format!("the name `{}` is defined multiple times", name.value))
                .span_info(orig.span, format!("previous definition of the value `{}` here", name.value))
                .span_error(name.span, format!("`{}` redefined here", name.value))
                .span_note(name.span, format!("`{}` must be defined only once in the value namespace of this module", name.value))
                .emit();

            // Error Recovery: continue past this point and allow the name to be redefined
        }

        self.declare(name)
    }

    /// Declares a name in the current scope
    ///
    /// The returned `DefSpan` preserves the `Span` of the given `Ident`
    fn declare(&mut self, name: &ast::Ident) -> nir::DefSpan {
        let id = self.def_table.insert(name.clone());
        self.scope_stack.top_mut().define(name.value.clone(), id);

        nir::DefSpan {
            id,
            span: name.span,
        }
    }

    /// Lookup a name in the current scope or surrounding scopes
    ///
    /// Emits an error if the name cannot be resolved and returns a placeholder `DefId`
    ///
    /// The returned `DefSpan` preserves the `Span` of the given `Ident`
    fn lookup(&mut self, name: &ast::Ident) -> nir::DefSpan {
        for scope in self.scope_stack.iter_top_down() {
            if let Some(id) = scope.lookup(&name.value) {
                return nir::DefSpan {
                    id,
                    span: name.span,
                };
            }
        }

        // No name found
        self.diag.span_error(name.span, format!("cannot find name `{}` in this scope", name.value)).emit();

        // Error recovery: Insert this name and pretend it exists so we can continue past this
        self.declare(name)
    }
}
