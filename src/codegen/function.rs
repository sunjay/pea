use std::iter;
use std::convert::TryInto;
use std::collections::HashMap;

use crate::{
    nir,
    diagnostics::Diagnostics,
    bytecode::{self, OpCode},
    value::Value,
    gc::Gc,
};

use super::def_consts::DefConsts;

pub struct FunctionCompiler<'a> {
    consts: &'a mut bytecode::Constants,
    const_ids: &'a DefConsts,
    diag: &'a Diagnostics,

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
        diag: &'a Diagnostics,
    ) -> bytecode::Bytecode {
        let mut compiler = Self {
            consts,
            const_ids,
            diag,

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
            params,
            paren_close_token: _,
            body,
            scope: _,
        } = func;

        // Declare a variable for each parameter so we use the right stack slot. Note that no code
        // is generated for the parameters since they are implicitly provided thanks to our calling
        // convention.
        for param in params {
            self.declare_local(param);
        }

        self.walk_block(body);

        // Return from the function
        // Walking the block guarantees that there will be a return value for this to emit
        self.code.write_instr(OpCode::Return, body.brace_close_token.span);
    }

    fn walk_block(&mut self, block: &nir::Block) {
        let nir::Block {
            brace_open_token: _,
            stmts,
            ret_expr,
            brace_close_token,
            scope: _,
        } = block;

        let next_frame_offset = self.next_frame_offset;

        for stmt in stmts {
            self.walk_stmt(stmt);
        }

        // Generate the return value or default to unit
        match ret_expr {
            Some(expr) => self.walk_expr(expr),
            None => self.code.write_instr(OpCode::ConstUnit, brace_close_token.span),
        }

        // Pop local variables but keep the return value
        //
        // Note that we have to generate the return expression before we do this because otherwise
        // some of the local variables it uses may get popped.
        //
        // It's important to do this, especially for conditionals and loops because the frame
        // offsets we compute need to be correct whether the interpreter executes a given sub-block
        // or not. It would be bad if not executing an `if` body resulted in all future indexes
        // being incorrect.
        let nlocals = self.next_frame_offset - next_frame_offset;
        if nlocals > 0 {
            self.code.write_instr_u8(OpCode::BlockEnd, nlocals, brace_close_token.span);
        }

        // Restore the next frame offset since all local variables are now popped
        self.next_frame_offset = next_frame_offset;
    }

    fn walk_stmt(&mut self, stmt: &nir::Stmt) {
        use nir::Stmt::*;
        match stmt {
            Println(stmt) => self.walk_println_stmt(stmt),
            VarDecl(stmt) => self.walk_var_decl_stmt(stmt),
            Expr(stmt) => self.walk_expr_stmt(stmt),
            Cond(stmt) => self.walk_cond_stmt(stmt),
        }
    }

    fn walk_println_stmt(&mut self, stmt: &nir::PrintlnStmt) {
        let nir::PrintlnStmt {println_token, expr, ..} = stmt;

        self.walk_expr(expr);

        self.code.write_instr(OpCode::Print, println_token.span);
    }

    fn walk_var_decl_stmt(&mut self, stmt: &nir::VarDeclStmt) {
        let nir::VarDeclStmt {name, expr, ..} = stmt;

        self.declare_local(name);

        // This expr will result in a stack effect = 1 (one item added to stack by the end). The
        // local var offset inserted above will correspond to the offset to that item from the frame
        // pointer.
        self.walk_expr(expr);
    }

    fn walk_expr_stmt(&mut self, stmt: &nir::ExprStmt) {
        let nir::ExprStmt {expr, semicolon_token} = stmt;

        self.walk_expr(&expr);

        // discard the expression value
        self.code.write_instr_u8(OpCode::Pop, 1, semicolon_token.span);
    }

    fn walk_cond_stmt(&mut self, stmt: &nir::Cond) {
        self.walk_cond(stmt);

        // The span for the pop instruction is the last brace in the statement
        let span = match (&stmt.else_clause, stmt.else_if_clauses.last()) {
            (Some(else_clause), _) => {
                else_clause.body.brace_close_token.span
            },

            (None, Some(else_if_clause)) => {
                else_if_clause.body.brace_close_token.span
            },

            (None, None) => stmt.if_body.brace_close_token.span,
        };

        // Discard the expression value
        self.code.write_instr_u8(OpCode::Pop, 1, span);
    }

    fn walk_expr(&mut self, expr: &nir::Expr) {
        use nir::Expr::*;
        match expr {
            Or(expr) => self.walk_or(expr),
            And(expr) => self.walk_and(expr),
            Cond(cond) => self.walk_cond(cond),
            UnaryOp(expr) => self.walk_unary_op(expr),
            BinaryOp(expr) => self.walk_binary_op(expr),
            Assign(expr) => self.walk_assign(expr),
            Group(expr) => self.walk_group(expr),
            Call(call) => self.walk_call(call),
            Return(ret) => self.walk_return(ret),
            Def(def_id) => self.walk_def(def_id),
            Integer(lit) => self.walk_integer_literal(lit),
            Bool(lit) => self.walk_bool_literal(lit),
            BStr(lit) => self.walk_bstr_literal(lit),
        }
    }

    fn walk_or(&mut self, expr: &nir::OrExpr) {
        let nir::OrExpr {lhs, oror_token, rhs} = expr;

        self.walk_expr(lhs);

        // If lhs of `||` is true, immediately jump to the end and return that value (short-circuit)
        let end_patch = self.code.write_jump_patch(OpCode::JumpIfTrue, oror_token.span);

        // Pop off the lhs value
        self.code.write_instr_u8(OpCode::Pop, 1, oror_token.span);

        // Otherwise, evaluate rhs and return whatever its value is
        self.walk_expr(rhs);

        self.code.finish_jump_patch(end_patch);
    }

    fn walk_and(&mut self, expr: &nir::AndExpr) {
        let nir::AndExpr {lhs, andand_token, rhs} = expr;

        self.walk_expr(lhs);

        // If lhs of `&&` is false, immediately jump to the end and return that value (short-circuit)
        let end_patch = self.code.write_jump_patch(OpCode::JumpIfFalse, andand_token.span);

        // Pop off the lhs value
        self.code.write_instr_u8(OpCode::Pop, 1, andand_token.span);

        // Otherwise, evaluate rhs and return whatever its value is
        self.walk_expr(rhs);

        self.code.finish_jump_patch(end_patch);
    }

    fn walk_cond(&mut self, cond: &nir::Cond) {
        let nir::Cond {
            if_token: _,
            if_cond,
            if_body,
            else_if_clauses,
            else_clause,
        } = cond;

        // Generates:
        //       if_cond
        //       JumpIfFalse c1
        //       Pop 1
        //       if_body
        //       Jump end
        //   c1:
        //       Pop 1
        //       else_if_clauses[0].cond
        //       JumpIfFalse c2
        //       Pop 1
        //       else_if_clauses[0].body
        //       Jump end
        //   c2:
        //       Pop 1
        //       else_if_clauses[1].cond
        //       JumpIfFalse c3
        //       Pop 1
        //       else_if_clauses[1].body
        //       Jump end
        //   ...
        //   cn:
        //       Pop 1
        //       else_if_clauses[n].cond
        //       JumpIfFalse el
        //       Pop 1
        //       else_if_clauses[n].body
        //       Jump end
        //   el:
        //       Pop 1
        //       else_body
        //   end:
        //       ...
        //
        // Important Points:
        //
        // * Jump instructions do not pop the condition value (needed for && and ||)
        // * Condition that was just evaluated needs to be popped whether condition is true or false
        // * Since this is an expression, it needs to have a stack effect = 1 and that value should
        //   come from whichever block is chosen for execution
        // * If no block is executed (e.g. no `else` clause), we still need to generate `()`
        // * Jump instructions take a *relative* offset

        let conds = iter::once((if_cond, if_body))
            .chain(else_if_clauses.iter().map(|clause| (&clause.cond, &clause.body)));

        let mut end_patches = Vec::with_capacity(1 + else_if_clauses.len());

        for (cond, body) in conds {
            self.walk_expr(cond);

            let else_patch = self.code.write_jump_patch(OpCode::JumpIfFalse, body.brace_open_token.span);

            // Pop the condition value (if branch)
            self.code.write_instr_u8(OpCode::Pop, 1, body.brace_open_token.span);

            self.walk_block(body);

            let end_patch = self.code.write_jump_patch(OpCode::Jump, body.brace_close_token.span);
            end_patches.push(end_patch);

            self.code.finish_jump_patch(else_patch);

            // Pop the condition value (else branch)
            self.code.write_instr_u8(OpCode::Pop, 1, if_body.brace_open_token.span);
        }

        // Generate the else body
        // Note that the Pop just before the else body is already generated by the loop above
        match else_clause {
            Some(clause) => {
                let nir::ElseClause {else_token: _, body} = clause;
                self.walk_block(body);
            },

            None => {
                // Generate `()` in case `if` or `else if` condition is false

                let end_span = else_if_clauses.last()
                    .map(|clause| clause.body.brace_close_token.span)
                    .unwrap_or_else(|| if_body.brace_close_token.span);

                self.code.write_instr(OpCode::ConstUnit, end_span);
            },
        }

        // Finish off by patching all of the jumps to the end
        for end_patch in end_patches {
            self.code.finish_jump_patch(end_patch);
        }
    }

    fn walk_unary_op(&mut self, expr: &nir::UnaryOpExpr) {
        let nir::UnaryOpExpr {op, op_token, expr} = expr;

        self.walk_expr(expr);

        self.code.write_instr(match op {
            nir::UnaryOp::Pos => OpCode::Pos,
            nir::UnaryOp::Neg => OpCode::Neg,
            nir::UnaryOp::Not => OpCode::Not,
        }, op_token.span);
    }

    fn walk_binary_op(&mut self, expr: &nir::BinaryOpExpr) {
        let nir::BinaryOpExpr {lhs, op, op_token, rhs} = expr;

        self.walk_expr(lhs);
        self.walk_expr(rhs);

        self.code.write_instr(match op {
            nir::BinaryOp::Add => OpCode::Add,
            nir::BinaryOp::Sub => OpCode::Sub,
            nir::BinaryOp::Mul => OpCode::Mul,
            nir::BinaryOp::Div => OpCode::Div,
            nir::BinaryOp::Rem => OpCode::Rem,

            nir::BinaryOp::EqualsEquals => OpCode::EqualsEquals,
            nir::BinaryOp::NotEquals => OpCode::NotEquals,
            nir::BinaryOp::GreaterThan => OpCode::GreaterThan,
            nir::BinaryOp::GreaterThanEquals => OpCode::GreaterThanEquals,
            nir::BinaryOp::LessThan => OpCode::LessThan,
            nir::BinaryOp::LessThanEquals => OpCode::LessThanEquals,
        }, op_token.span);
    }

    fn walk_assign(&mut self, expr: &nir::AssignExpr) {
        let nir::AssignExpr {lvalue, equals_token: _, rhs} = expr;

        self.walk_expr(rhs);

        use nir::LValueExpr::*;
        match lvalue {
            Def(def) => match self.local_var_offsets.get(&def.id) {
                Some(&offset) => {
                    self.code.write_instr_u8(OpCode::SetLocal, offset, def.span)
                },

                None => {
                    self.diag.span_error(def.span, "invalid left-hand side of assignment").emit();
                },
            },
        }
    }

    fn walk_group(&mut self, expr: &nir::GroupExpr) {
        let nir::GroupExpr {paren_open_token: _, expr, paren_close_token: _} = expr;

        self.walk_expr(expr);
    }

    fn walk_call(&mut self, call: &nir::CallExpr) {
        let nir::CallExpr {lhs, paren_open_token, args, paren_close_token} = call;

        // Walk the lhs first so the function to be called is on the stack before the args
        self.walk_expr(lhs);

        // Walk the arguments in order so they get put on the stack left to right
        for arg in args {
            self.walk_expr(arg);
        }

        let nargs = args.len().try_into()
            .expect("bug: should have validated that no more than 255 arguments can be passed to a function");
        let call_span = paren_open_token.span.to(paren_close_token.span);
        self.code.write_instr_u8(OpCode::Call, nargs, call_span);
    }

    fn walk_return(&mut self, ret: &nir::ReturnExpr) {
        let nir::ReturnExpr {return_token, expr} = ret;

        // Generate the value to return
        match expr {
            Some(expr) => self.walk_expr(expr),
            // The default return value is `()` if none is specified
            None => self.code.write_instr(OpCode::ConstUnit, return_token.span),
        }

        // Return the value
        self.code.write_instr(OpCode::Return, return_token.span);
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

    fn walk_bool_literal(&mut self, lit: &nir::BoolLiteral) {
        let &nir::BoolLiteral {value, span} = lit;

        if value {
            self.code.write_instr(OpCode::ConstTrue, span);
        } else {
            self.code.write_instr(OpCode::ConstFalse, span);
        }
    }

    fn walk_bstr_literal(&mut self, lit: &nir::BStrLiteral) {
        let nir::BStrLiteral {value, span} = lit;

        let const_id = self.consts.push(Value::Bytes(Gc::new((**value).into())));
        self.code.write_instr_u16(OpCode::Constant, const_id.into_u16(), *span);
    }

    fn declare_local(&mut self, name: &nir::DefSpan) {
        debug_assert!(!self.local_var_offsets.contains_key(&name.id),
            "bug: two declared variables had the same `DefId` for some reason");
        self.local_var_offsets.insert(name.id, self.next_frame_offset);
        debug_assert!(self.next_frame_offset < 255, "bug: ran out of local var frame offsets");
        self.next_frame_offset += 1;
    }
}
