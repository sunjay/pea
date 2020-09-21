use std::convert::TryInto;
use std::collections::HashMap;

use crate::{
    nir,
    bytecode::{self, OpCode},
    value::Value,
    gc::Gc,
};

use super::def_consts::DefConsts;

pub struct FunctionCompiler<'a> {
    consts: &'a mut bytecode::Constants,
    const_ids: &'a DefConsts,

    code: bytecode::Bytecode,
    /// The frame offset of each local variable
    local_var_offsets: HashMap<nir::DefId, u8>,
    /// The offset from the frame pointer to be used for the next local variable that is defined
    next_frame_offset: u8,
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
            local_var_offsets: Default::default(),
            // The first value at the frame pointer is always the function itself
            next_frame_offset: 1,
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

        self.walk_block(body);

        // The default return value is unit
        self.code.write_instr(OpCode::ConstUnit, body.brace_close_token.span);

        // Return from the function
        self.code.write_instr(OpCode::Return, body.brace_close_token.span);
    }

    fn walk_block(&mut self, block: &nir::Block) {
        let nir::Block {stmts, ..} = block;

        let next_frame_offset = self.next_frame_offset;

        for stmt in stmts {
            self.walk_stmt(stmt);
        }

        //TODO: Pop local variables

        // Restore the next frame offset since all local variables are now popped
        self.next_frame_offset = next_frame_offset;
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
        let nir::PrintlnStmt {println_token, expr, ..} = stmt;

        self.walk_expr(expr);

        self.code.write_instr(OpCode::Print, println_token.span);
    }

    fn walk_var_decl_stmt(&mut self, stmt: &nir::VarDeclStmt) {
        let nir::VarDeclStmt {name, expr, ..} = stmt;

        debug_assert!(!self.local_var_offsets.contains_key(&name.id),
            "bug: two declared variables had the same `DefId` for some reason");
        self.local_var_offsets.insert(name.id, self.next_frame_offset);
        debug_assert!(self.next_frame_offset < 255, "bug: ran out of local var frame offsets");
        self.next_frame_offset += 1;

        // This expr will result in a stack effect = 1 (one item added to stack by the end). The
        // local var offset inserted above will correspond to the offset to that item from the frame
        // pointer.
        self.walk_expr(expr);
    }

    fn walk_expr_stmt(&mut self, stmt: &nir::ExprStmt) {
        let nir::ExprStmt {expr, semicolon_token} = stmt;

        self.walk_expr(&expr);

        // discard the expression value
        self.code.write_instr(OpCode::Pop, semicolon_token.span);
    }

    fn walk_expr(&mut self, expr: &nir::Expr) {
        use nir::Expr::*;
        match expr {
            UnaryOp(expr) => self.walk_unary_op(expr),
            BinaryOp(expr) => self.walk_binary_op(expr),
            Assign(expr) => self.walk_assign(expr),
            Group(expr) => self.walk_group(expr),
            Call(call) => self.walk_call(call),
            Def(def_id) => self.walk_def(def_id),
            Integer(lit) => self.walk_integer_literal(lit),
            BStr(lit) => self.walk_bstr_literal(lit),
        }
    }

    fn walk_unary_op(&mut self, expr: &nir::UnaryOpExpr) {
        todo!()
    }

    fn walk_binary_op(&mut self, expr: &nir::BinaryOpExpr) {
        todo!()
    }

    fn walk_assign(&mut self, expr: &nir::AssignExpr) {
        todo!()
    }

    fn walk_group(&mut self, expr: &nir::GroupExpr) {
        let nir::GroupExpr {paren_open_token: _, expr, paren_close_token: _} = expr;

        self.walk_expr(expr);
    }

    fn walk_call(&mut self, call: &nir::CallExpr) {
        let nir::CallExpr {lhs, paren_open_token, args, paren_close_token} = call;

        self.walk_expr(lhs);

        //TODO: Walk argument exprs and push their values onto the stack

        let nargs = args.len().try_into()
            .expect("bug: should have validated that no more 255 arguments can be passed to a function");
        let call_span = paren_open_token.span.to(paren_close_token.span);
        self.code.write_instr_u8(OpCode::Call, nargs, call_span);
    }

    fn walk_def(&mut self, def: &nir::DefSpan) {
        // Name resolution has already taken place, so this name should be either a variable or a
        // constant

        if let Some(&offset) = self.local_var_offsets.get(&def.id) {
            self.code.write_instr_u8(OpCode::GetLocal, offset, def.span);

        } else if let Some(const_id) = self.const_ids.get(def.id) {
            self.code.write_instr_u16(OpCode::Constant, const_id.into_u16(), def.span);

        } else {
            unreachable!("bug: name resolution should have caught undefined variable");
        }
    }

    fn walk_integer_literal(&mut self, lit: &nir::IntegerLiteral) {
        let &nir::IntegerLiteral {value, span} = lit;

        //TODO: Check the range on the type of the value being produced
        let value = value.try_into()
            .expect("bug: values out of the 64-bit range are not currently supported");
        let const_id = self.consts.push(Value::I64(value));
        self.code.write_instr_u16(OpCode::Constant, const_id.into_u16(), span);
    }

    fn walk_bstr_literal(&mut self, lit: &nir::BStrLiteral) {
        let nir::BStrLiteral {value, span} = lit;

        let const_id = self.consts.push(Value::Bytes(Gc::new((**value).into())));
        self.code.write_instr_u16(OpCode::Constant, const_id.into_u16(), *span);
    }
}
