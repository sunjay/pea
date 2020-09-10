use std::convert::TryInto;

use crate::{
    ast,
    bytecode::{self, OpCode},
    value::Value,
};

pub struct FunctionCompiler<'a> {
    consts: &'a mut bytecode::Constants,
    code: bytecode::Bytecode,
}

impl<'a> FunctionCompiler<'a> {
    pub fn compile(
        func: &ast::FuncDecl,
        consts: &'a mut bytecode::Constants,
    ) -> bytecode::Bytecode {
        let mut compiler = Self {
            consts,
            code: Default::default(),
        };

        compiler.walk_func(func);
        compiler.code
    }

    fn walk_func(&mut self, func: &ast::FuncDecl) {
        let ast::FuncDecl {fn_token: _, name: _, params: _, body} = func;

        for stmt in &body.stmts {
            self.walk_stmt(stmt);
        }
    }

    fn walk_stmt(&mut self, stmt: &ast::Stmt) {
        use ast::Stmt::*;
        match stmt {
            Println(stmt) => self.walk_println_stmt(stmt),
        }
    }

    fn walk_println_stmt(&mut self, stmt: &ast::PrintlnStmt) {
        let ast::PrintlnStmt {expr, ..} = stmt;

        self.walk_expr(&expr.value);

        self.code.write_instr(OpCode::Print);
    }

    fn walk_expr(&mut self, expr: &ast::Expr) {
        use ast::Expr::*;
        match expr {
            Integer(lit) => self.walk_integer_literal(lit),
        }
    }

    fn walk_integer_literal(&mut self, lit: &ast::IntegerLiteral) {
        let &ast::IntegerLiteral {value, ..} = lit;

        //TODO: Handle this based on the type of the value being produced
        let value = value.try_into()
            .expect("bug: values out of the 64-bit range are not currently supported");
        let index = self.consts.push(Value::I64(value));
        self.code.write_instr_u16(OpCode::Constant, index.into_u16());
    }
}
