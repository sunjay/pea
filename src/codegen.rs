use std::convert::TryInto;

use crate::{
    ast,
    bytecode::{Code, OpCode},
    value::Value,
};

pub trait ToBytecode {
    fn write_bytecode(&self, code: &mut Code);
}

impl ToBytecode for ast::Program {
    fn write_bytecode(&self, code: &mut Code) {
        let Self {decls} = self;

        for decl in decls {
            decl.write_bytecode(code);
        }

        // Exit the program at the end
        code.write_instr(OpCode::Return);
    }
}

impl ToBytecode for ast::Decl {
    fn write_bytecode(&self, code: &mut Code) {
        use ast::Decl::*;
        match self {
            Func(decl) => decl.write_bytecode(code),
        }
    }
}

impl ToBytecode for ast::FuncDecl {
    fn write_bytecode(&self, code: &mut Code) {
        let Self {fn_token, name, params, body} = self;

        //TODO: Implement this properly
        for stmt in &body.stmts {
            stmt.write_bytecode(code);
        }
    }
}

impl ToBytecode for ast::Stmt {
    fn write_bytecode(&self, code: &mut Code) {
        use ast::Stmt::*;
        match self {
            Println(stmt) => stmt.write_bytecode(code),
        }
    }
}

impl ToBytecode for ast::PrintlnStmt {
    fn write_bytecode(&self, code: &mut Code) {
        let Self {expr, ..} = self;

        expr.write_bytecode(code);

        code.write_instr(OpCode::Print);
    }
}

impl<T: ToBytecode> ToBytecode for ast::Parens<T> {
    fn write_bytecode(&self, code: &mut Code) {
        self.value.write_bytecode(code);
    }
}

impl ToBytecode for ast::Expr {
    fn write_bytecode(&self, code: &mut Code) {
        use ast::Expr::*;
        match self {
            Integer(value) => value.write_bytecode(code),
        }
    }
}

impl ToBytecode for ast::IntegerLiteral {
    fn write_bytecode(&self, code: &mut Code) {
        let &Self {value, ..} = self;

        //TODO: Handle this based on the type of the value being produced
        let value = value.try_into()
            .expect("bug: values out of the 64-bit range are not currently supported");
        let index = code.push_constant(Value::I64(value));
        code.write_instr_u16(OpCode::Constant, index);
    }
}
