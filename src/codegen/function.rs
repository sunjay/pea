use std::convert::TryInto;

use crate::{
    nir,
    bytecode::{self, OpCode, ConstId},
    value::Value,
    gc::Gc,
};

use super::def_consts::DefConsts;

pub struct FunctionCompiler<'a> {
    consts: &'a mut bytecode::Constants,
    const_ids: &'a DefConsts,

    code: bytecode::Bytecode,
}

impl<'a> FunctionCompiler<'a> {
    pub fn compile(
        func: &nir::FuncDecl,
        consts: &'a mut bytecode::Constants,
        const_ids: &'a DefConsts,
    ) -> bytecode::Bytecode {
        let mut compiler = Self {
            consts,
            const_ids,

            code: Default::default(),
        };

        compiler.walk_func(func);
        compiler.code
    }

    fn walk_func(&mut self, func: &nir::FuncDecl) {
        let nir::FuncDecl {
            fn_token: _,
            name: _,
            paren_open_token: _,
            params: _,
            paren_close_token: _,
            body,
        } = func;

        for stmt in &body.stmts {
            self.walk_stmt(stmt);
        }

        // The default return value is unit
        self.code.write_instr(OpCode::ConstUnit);

        // Return from the function
        self.code.write_instr(OpCode::Return);
    }

    fn walk_stmt(&mut self, stmt: &nir::Stmt) {
        use nir::Stmt::*;
        match stmt {
            Println(stmt) => self.walk_println_stmt(stmt),
            VarDecl(stmt) => self.walk_var_decl_stmt(stmt),
            Expr(stmt) => self.walk_expr_stmt(stmt),
        }
    }

    fn walk_println_stmt(&mut self, stmt: &nir::PrintlnStmt) {
        let nir::PrintlnStmt {expr, ..} = stmt;

        self.walk_expr(expr);

        self.code.write_instr(OpCode::Print);
    }

    fn walk_var_decl_stmt(&mut self, stmt: &nir::VarDeclStmt) {
        todo!()
    }

    fn walk_expr_stmt(&mut self, stmt: &nir::ExprStmt) {
        let nir::ExprStmt {expr, ..} = stmt;

        self.walk_expr(&expr);

        // discard the expression value
        self.code.write_instr(OpCode::Pop);
    }

    fn walk_expr(&mut self, expr: &nir::Expr) {
        use nir::Expr::*;
        match expr {
            Call(call) => self.walk_call(call),
            Integer(lit) => self.walk_integer_literal(lit),
            BStr(lit) => self.walk_bstr_literal(lit),
        }
    }

    fn walk_call(&mut self, call: &nir::CallExpr) {
        let nir::CallExpr {name, paren_open_token: _, args, paren_close_token: _} = call;

        let func_const = self.func_const_id(name);
        self.code.write_instr_u16(OpCode::Constant, func_const.into_u16());

        //TODO: Walk argument exprs and push their values onto the stack

        let nargs = args.len().try_into()
            .expect("bug: should have validated that no more 255 arguments can be passed to a function");
        self.code.write_instr_u8(OpCode::Call, nargs);
    }

    fn walk_integer_literal(&mut self, lit: &nir::IntegerLiteral) {
        let &nir::IntegerLiteral {value, ..} = lit;

        //TODO: Check the range on the type of the value being produced
        let value = value.try_into()
            .expect("bug: values out of the 64-bit range are not currently supported");
        let index = self.consts.push(Value::I64(value));
        self.code.write_instr_u16(OpCode::Constant, index.into_u16());
    }

    fn walk_bstr_literal(&mut self, lit: &nir::BStr) {
        let nir::BStr {value, ..} = lit;

        let index = self.consts.push(Value::Bytes(Gc::new((**value).into())));
        self.code.write_instr_u16(OpCode::Constant, index.into_u16());
    }

    fn func_const_id(&self, name: &nir::DefSpan) -> ConstId {
        self.const_ids.get(name.id)
    }
}
