use std::iter;
use std::convert::TryInto;
use std::collections::HashMap;

use crate::{
    nir,
    cgenir,
    diagnostics::Diagnostics,
    bytecode::{self, OpCode, PatchJump, LoopCheckpoint},
    value::Value,
    gc::Gc,
};

use super::def_consts::DefConsts;

#[derive(Debug)]
struct LoopState {
    /// The checkpoint at the start of the loop, before the loop condition has executed
    loop_start: LoopCheckpoint,

    /// Patches that need to be resolved to the offset of the next instruction after the loop has
    /// completed (after ALL of the code for the loop)
    after_loop_patches: Vec<PatchJump>,
}

#[derive(Debug)]
struct BlockState {
    /// The frame offset at the start of this block
    start_frame_offset: u8,
    /// The loop in this block that is currently being walked
    ///
    /// If this is None, we are not currently in any loop
    current_loop: Option<LoopState>,
}

pub struct FunctionCompiler<'a> {
    consts: &'a mut bytecode::Constants,
    def_consts: &'a DefConsts,
    diag: &'a Diagnostics,

    code: bytecode::Bytecode,
    /// A stack of the state of each block, pushed as we enter a block and popped as we leave
    blocks: Vec<BlockState>,
    /// The frame offset of each local variable
    local_var_offsets: HashMap<nir::DefId, u8>,
    /// The offset from the frame pointer to be used for the next local variable that is defined
    next_frame_offset: u8,
}

impl<'a> FunctionCompiler<'a> {
    pub fn compile(
        func: &cgenir::FuncDecl,
        consts: &'a mut bytecode::Constants,
        def_consts: &'a DefConsts,
        diag: &'a Diagnostics,
    ) -> bytecode::Bytecode {
        let mut compiler = Self {
            consts,
            def_consts,
            diag,

            code: Default::default(),
            local_var_offsets: Default::default(),
            blocks: Default::default(),
            // The first value at the frame pointer is always the function itself
            next_frame_offset: 1,
        };

        compiler.walk_func(func);
        compiler.code
    }

    fn walk_func(&mut self, func: &cgenir::FuncDecl) {
        let cgenir::FuncDecl {
            fn_token: _,
            name: _,
            paren_open_token: _,
            params,
            paren_close_token: _,
            right_arrow_token: _,
            return_ty: _,
            body,
            scope: _,
        } = func;

        // Declare a variable for each parameter so we use the right stack slot. Note that no code
        // is generated for the parameters since they are implicitly provided thanks to our calling
        // convention.
        for param in params {
            let cgenir::FuncParam {name, ..} = param;
            self.declare_local(name);
        }

        self.walk_block(body);

        // Return from the function
        // Walking the block guarantees that there will be a return value for this to emit
        self.code.write_instr(OpCode::Return, body.brace_close_token.span);
    }

    fn walk_block(&mut self, block: &cgenir::Block) {
        let cgenir::Block {
            brace_open_token: _,
            stmts,
            ret_expr,
            brace_close_token,
            scope: _,
        } = block;

        self.blocks.push(BlockState {
            start_frame_offset: self.next_frame_offset,
            current_loop: None,
        });

        for stmt in stmts {
            self.walk_stmt(stmt);
        }

        // Generate the return value or default to unit
        match ret_expr {
            Some(expr) => self.walk_expr(expr),
            None => self.code.write_instr(OpCode::ConstUnit, brace_close_token.span),
        }

        // Finished with this block
        let BlockState {start_frame_offset, current_loop} = self.blocks.pop().expect("bug: block mismatch");
        assert!(current_loop.is_none(), "bug: should not have current loop at end of block");

        // Pop local variables but keep the return value
        //
        // Note that we have to generate the return expression before we do this because otherwise
        // some of the local variables it uses may get popped.
        //
        // It's important to do this, especially for conditionals and loops because the frame
        // offsets we compute need to be correct whether the interpreter executes a given sub-block
        // or not. It would be bad if not executing an `if` body resulted in all future indexes
        // being incorrect.
        let nlocals = self.next_frame_offset - start_frame_offset;
        if nlocals > 0 {
            self.code.write_instr_u8(OpCode::BlockEnd, nlocals, brace_close_token.span);
        }

        // Restore the next frame offset since all local variables are now popped
        self.next_frame_offset = start_frame_offset;
    }

    fn walk_stmt(&mut self, stmt: &cgenir::Stmt) {
        use cgenir::Stmt::*;
        match stmt {
            Println(stmt) => self.walk_println_stmt(stmt),
            Print(stmt) => self.walk_print_stmt(stmt),
            VarDecl(stmt) => self.walk_var_decl_stmt(stmt),
            Expr(stmt) => self.walk_expr_stmt(stmt),
            Cond(stmt) => self.walk_cond_stmt(stmt),
            WhileLoop(stmt) => self.walk_while_loop_stmt(stmt),
            Loop(stmt) => self.walk_loop_stmt(stmt),
        }
    }

    fn walk_println_stmt(&mut self, stmt: &cgenir::PrintlnStmt) {
        let cgenir::PrintlnStmt {println_token, expr, ..} = stmt;

        self.walk_expr(expr);

        self.code.write_instr(OpCode::Println, println_token.span);
    }

    fn walk_print_stmt(&mut self, stmt: &cgenir::PrintStmt) {
        let cgenir::PrintStmt {print_token, expr, ..} = stmt;

        self.walk_expr(expr);

        self.code.write_instr(OpCode::Print, print_token.span);
    }

    fn walk_var_decl_stmt(&mut self, stmt: &cgenir::VarDeclStmt) {
        let cgenir::VarDeclStmt {name, expr, ..} = stmt;

        self.declare_local(name);

        // This expr will result in a stack effect = 1 (one item added to stack by the end). The
        // local var offset inserted above will correspond to the offset to that item from the frame
        // pointer.
        self.walk_expr(expr);
    }

    fn walk_expr_stmt(&mut self, stmt: &cgenir::ExprStmt) {
        let cgenir::ExprStmt {expr, semicolon_token} = stmt;

        self.walk_expr(&expr);

        // discard the expression value
        self.code.write_instr_u8(OpCode::Pop, 1, semicolon_token.span);
    }

    fn walk_cond_stmt(&mut self, stmt: &cgenir::Cond) {
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

    fn walk_while_loop_stmt(&mut self, stmt: &cgenir::WhileLoop) {
        let cgenir::WhileLoop {while_token: _, cond, body} = stmt;

        let top_block = self.blocks.last_mut().expect("bug: should be in a block");
        assert!(top_block.current_loop.is_none(), "bug: overwrote loop");
        top_block.current_loop = Some(LoopState {
            // Record the address of the start of the loop
            loop_start: self.code.loop_checkpoint(),
            after_loop_patches: Vec::new(),
        });

        self.walk_expr(cond);

        // If the condition is false, jump to the end
        let end_patch = self.code.write_jump_patch(OpCode::JumpIfFalse, body.brace_open_token.span);

        // Pop the condition value
        self.code.write_instr_u8(OpCode::Pop, 1, body.brace_close_token.span);

        self.walk_block(body);

        // Pop the return value of the block
        self.code.write_instr_u8(OpCode::Pop, 1, body.brace_close_token.span);

        // The loop has completed
        let top_block = self.blocks.last_mut().expect("bug: should be in a block");
        let LoopState {
            loop_start,
            after_loop_patches,
        } = top_block.current_loop.take().expect("bug: should have had a loop");

        // Jump back to the start of the loop, just before the condition so it can run again
        self.code.write_loop(OpCode::Loop, loop_start, body.brace_close_token.span);

        self.code.finish_jump_patch(end_patch);

        // Pop the condition value
        self.code.write_instr_u8(OpCode::Pop, 1, body.brace_close_token.span);

        for patch in after_loop_patches {
            self.code.finish_jump_patch(patch);
        }
    }

    fn walk_loop_stmt(&mut self, stmt: &cgenir::Loop) {
        let cgenir::Loop {loop_token: _, body} = stmt;

        let top_block = self.blocks.last_mut().expect("bug: should be in a block");
        assert!(top_block.current_loop.is_none(), "bug: overwrote loop");
        top_block.current_loop = Some(LoopState {
            // Record the address of the start of the loop
            loop_start: self.code.loop_checkpoint(),
            after_loop_patches: Vec::new(),
        });

        self.walk_block(body);

        // Pop the return value of the block
        self.code.write_instr_u8(OpCode::Pop, 1, body.brace_close_token.span);

        // The loop has completed
        let top_block = self.blocks.last_mut().expect("bug: should be in a block");
        let LoopState {
            loop_start,
            after_loop_patches,
        } = top_block.current_loop.take().expect("bug: should have had a loop");

        // Jump back to the start of the loop so it can run again
        self.code.write_loop(OpCode::Loop, loop_start, body.brace_close_token.span);

        for patch in after_loop_patches {
            self.code.finish_jump_patch(patch);
        }
    }

    fn walk_expr(&mut self, expr: &cgenir::Expr) {
        use cgenir::Expr::*;
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
            Break(expr) => self.walk_break(expr),
            Continue(expr) => self.walk_continue(expr),
            Def(def_id) => self.walk_def(def_id),
            Integer(lit) => self.walk_integer_literal(lit),
            Bool(lit) => self.walk_bool_literal(lit),
            List(list) => self.walk_list(list),
            ListRepeat(list) => self.walk_list_repeat(list),
            BStr(lit) => self.walk_bstr_literal(lit),
            Byte(lit) => self.walk_byte_literal(lit),
            Unit(lit) => self.walk_unit_literal(lit),
        }
    }

    fn walk_or(&mut self, expr: &cgenir::OrExpr) {
        let cgenir::OrExpr {lhs, oror_token, rhs} = expr;

        self.walk_expr(lhs);

        // If lhs of `||` is true, immediately jump to the end and return that value (short-circuit)
        let end_patch = self.code.write_jump_patch(OpCode::JumpIfTrue, oror_token.span);

        // Pop off the lhs value
        self.code.write_instr_u8(OpCode::Pop, 1, oror_token.span);

        // Otherwise, evaluate rhs and return whatever its value is
        self.walk_expr(rhs);

        self.code.finish_jump_patch(end_patch);
    }

    fn walk_and(&mut self, expr: &cgenir::AndExpr) {
        let cgenir::AndExpr {lhs, andand_token, rhs} = expr;

        self.walk_expr(lhs);

        // If lhs of `&&` is false, immediately jump to the end and return that value (short-circuit)
        let end_patch = self.code.write_jump_patch(OpCode::JumpIfFalse, andand_token.span);

        // Pop off the lhs value
        self.code.write_instr_u8(OpCode::Pop, 1, andand_token.span);

        // Otherwise, evaluate rhs and return whatever its value is
        self.walk_expr(rhs);

        self.code.finish_jump_patch(end_patch);
    }

    fn walk_cond(&mut self, cond: &cgenir::Cond) {
        let cgenir::Cond {
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
                let cgenir::ElseClause {else_token: _, body} = clause;
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

    fn walk_unary_op(&mut self, expr: &cgenir::UnaryOpExpr) {
        let cgenir::UnaryOpExpr {op, op_token, expr} = expr;

        self.walk_expr(expr);

        self.code.write_instr(match op {
            cgenir::UnaryOp::Pos => OpCode::Pos,
            cgenir::UnaryOp::Neg => OpCode::Neg,
            cgenir::UnaryOp::Not => OpCode::Not,
        }, op_token.span);
    }

    fn walk_binary_op(&mut self, expr: &cgenir::BinaryOpExpr) {
        let cgenir::BinaryOpExpr {lhs, op, op_token, rhs} = expr;

        self.walk_expr(lhs);
        self.walk_expr(rhs);

        self.code.write_instr(match op {
            cgenir::BinaryOp::Add => OpCode::Add,
            cgenir::BinaryOp::Sub => OpCode::Sub,
            cgenir::BinaryOp::Mul => OpCode::Mul,
            cgenir::BinaryOp::Div => OpCode::Div,
            cgenir::BinaryOp::Rem => OpCode::Rem,

            cgenir::BinaryOp::EqualsEquals => OpCode::EqualsEquals,
            cgenir::BinaryOp::NotEquals => OpCode::NotEquals,
            cgenir::BinaryOp::GreaterThan => OpCode::GreaterThan,
            cgenir::BinaryOp::GreaterThanEquals => OpCode::GreaterThanEquals,
            cgenir::BinaryOp::LessThan => OpCode::LessThan,
            cgenir::BinaryOp::LessThanEquals => OpCode::LessThanEquals,
        }, op_token.span);
    }

    fn walk_assign(&mut self, expr: &cgenir::AssignExpr) {
        let cgenir::AssignExpr {lvalue, equals_token: _, rhs} = expr;

        self.walk_expr(rhs);

        use cgenir::LValueExpr::*;
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

    fn walk_group(&mut self, expr: &cgenir::GroupExpr) {
        let cgenir::GroupExpr {paren_open_token: _, expr, paren_close_token: _} = expr;

        self.walk_expr(expr);
    }

    fn walk_call(&mut self, call: &cgenir::CallExpr) {
        let cgenir::CallExpr {lhs, paren_open_token, args, paren_close_token} = call;

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

    fn walk_return(&mut self, ret: &cgenir::ReturnExpr) {
        let cgenir::ReturnExpr {return_token, expr} = ret;

        // Generate the value to return
        match expr {
            Some(expr) => self.walk_expr(expr),
            // The default return value is `()` if none is specified
            None => self.code.write_instr(OpCode::ConstUnit, return_token.span),
        }

        // Return the value
        self.code.write_instr(OpCode::Return, return_token.span);
    }

    fn walk_break(&mut self, expr: &cgenir::BreakExpr) {
        let cgenir::BreakExpr {break_token} = expr;

        // `break` needs to do two things:
        // 1. pop all local variables up to the start of the **loop body**
        // 2. jump to the point AFTER **all** of the generated code for the loop

        // The loop body block is in the block stack just **after** the block that contains the loop
        // state itself
        let loop_state;
        // This variable will get initialized to a different value, but we can't prove that to rustc
        let mut loop_body_start_frame_offset = 0;
        let mut block_stack = self.blocks.iter_mut().rev();
        loop {
            let block_state = match block_stack.next() {
                Some(block) => block,
                None => {
                    self.diag.span_error(break_token.span, "`break` outside of a loop").emit();
                    return;
                },
            };

            if let Some(current_loop) = &mut block_state.current_loop {
                loop_state = current_loop;
                break;
            }

            loop_body_start_frame_offset = block_state.start_frame_offset;
        }

        // Pop local variables up to the loop body
        let nlocals = self.next_frame_offset - loop_body_start_frame_offset;
        if nlocals > 0 {
            self.code.write_instr_u8(OpCode::Pop, nlocals, break_token.span);
        }

        // Jump to after the loop body
        let after_loop_patch = self.code.write_jump_patch(OpCode::Jump, break_token.span);

        loop_state.after_loop_patches.push(after_loop_patch);
    }

    fn walk_continue(&mut self, expr: &cgenir::ContinueExpr) {
        let cgenir::ContinueExpr {continue_token} = expr;

        // `continue` needs to do two things:
        // 1. pop all local variables up to the start of the **loop body**
        // 2. jump to the point just before the loop condition (same as if the end of the loop body
        //    is reached)

        // The loop body block is in the block stack just **after** the block that contains the loop
        // state itself
        let loop_state;
        // This variable will get initialized to a different value, but we can't prove that to rustc
        let mut loop_body_start_frame_offset = 0;
        let mut block_stack = self.blocks.iter_mut().rev();
        loop {
            let block_state = match block_stack.next() {
                Some(block) => block,
                None => {
                    self.diag.span_error(continue_token.span, "`continue` outside of a loop").emit();
                    return;
                },
            };

            if let Some(current_loop) = &mut block_state.current_loop {
                loop_state = current_loop;
                break;
            }

            loop_body_start_frame_offset = block_state.start_frame_offset;
        }

        // Pop local variables up to the loop body
        let nlocals = self.next_frame_offset - loop_body_start_frame_offset;
        if nlocals > 0 {
            self.code.write_instr_u8(OpCode::Pop, nlocals, continue_token.span);
        }

        // Jump to start of loop before condition
        self.code.write_loop(OpCode::Loop, loop_state.loop_start, continue_token.span);
    }

    fn walk_def(&mut self, def: &cgenir::DefSpan) {
        // Name resolution has already taken place, so this name should be either a variable or a
        // constant

        if let Some(&offset) = self.local_var_offsets.get(&def.id) {
            self.code.write_instr_u8(OpCode::GetLocal, offset, def.span);

        } else if let Some(const_id) = self.def_consts.get(def.id) {
            self.code.write_instr_u16(OpCode::Constant, const_id.into_u16(), def.span);

        } else {
            unreachable!("bug: name resolution should have caught undefined variable");
        }
    }

    fn walk_integer_literal(&mut self, lit: &cgenir::IntegerLiteral) {
        let &cgenir::IntegerLiteral {value, span} = lit;

        //TODO: Check the range on the type of the value being produced
        let value = value.try_into()
            .expect("bug: values out of the 64-bit range are not currently supported");
        let const_id = self.consts.push(Value::I64(value));
        self.code.write_instr_u16(OpCode::Constant, const_id.into_u16(), span);
    }

    fn walk_bool_literal(&mut self, lit: &cgenir::BoolLiteral) {
        let &cgenir::BoolLiteral {value, span} = lit;

        if value {
            self.code.write_instr(OpCode::ConstTrue, span);
        } else {
            self.code.write_instr(OpCode::ConstFalse, span);
        }
    }

    fn walk_list(&mut self, lit: &cgenir::ListLiteral) {
        let cgenir::ListLiteral {bracket_open_token: _, items, bracket_close_token} = lit;

        // Push each item onto the stack in order
        for item in items {
            self.walk_expr(item);
        }

        // Push the length onto the stack
        let len = items.len().try_into()
            .expect("bug: values out of the 64-bit range are not currently supported");
        let const_id = self.consts.push(Value::I64(len));
        self.code.write_instr_u16(OpCode::Constant, const_id.into_u16(), bracket_close_token.span);

        // Create the list
        self.code.write_instr(OpCode::List, bracket_close_token.span);
    }

    fn walk_list_repeat(&mut self, lit: &cgenir::ListRepeatLiteral) {
        let cgenir::ListRepeatLiteral {
            bracket_open_token: _,
            item,
            semicolon_token: _,
            len,
            bracket_close_token,
        } = lit;

        self.walk_expr(item);
        self.walk_expr(len);

        self.code.write_instr(OpCode::ListRepeat, bracket_close_token.span);
    }

    fn walk_bstr_literal(&mut self, lit: &cgenir::BStrLiteral) {
        let cgenir::BStrLiteral {value, span} = lit;

        let const_id = self.consts.push(Value::Bytes(Gc::new((**value).into())));
        self.code.write_instr_u16(OpCode::Constant, const_id.into_u16(), *span);
    }

    fn walk_byte_literal(&mut self, lit: &cgenir::ByteLiteral) {
        let &cgenir::ByteLiteral {value, span} = lit;

        let const_id = self.consts.push(Value::U8(value));
        self.code.write_instr_u16(OpCode::Constant, const_id.into_u16(), span);
    }

    fn walk_unit_literal(&mut self, lit: &cgenir::UnitLiteral) {
        let cgenir::UnitLiteral {paren_open_token, paren_close_token} = lit;

        self.code.write_instr(OpCode::ConstUnit, paren_open_token.span.to(paren_close_token.span));
    }

    fn declare_local(&mut self, name: &cgenir::DefSpan) {
        debug_assert!(!self.local_var_offsets.contains_key(&name.id),
            "bug: two declared variables had the same `DefId` for some reason");
        self.local_var_offsets.insert(name.id, self.next_frame_offset);
        debug_assert!(self.next_frame_offset < 255, "bug: ran out of local var frame offsets");
        self.next_frame_offset += 1;
    }
}
