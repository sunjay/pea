use crate::{ast, bytecode::ConstId, value::Value};

use super::{CallFrame, Interpreter, RuntimeError, RuntimeResult, Status};

#[inline]
pub fn call(ctx: &mut Interpreter, nargs: u8) -> RuntimeResult {
    let func = match ctx.peek(nargs as usize) {
        Value::Func(func) => func.clone(),
        _ => return Err(RuntimeError::NonFunctionCall),
    };

    if nargs != func.arity {
        return Err(RuntimeError::ArityMismatch {
            name: func.name.clone(),
            arity: func.arity,
            nargs,
        });
    }

    // Start with the arguments and the function itself on the start of the stack frame
    let frame_index = ctx.value_stack.len() - nargs as usize - 1;
    ctx.call_stack.push(CallFrame::new(func, frame_index))?;

    Ok(Status::Running)
}

#[inline]
pub fn ret(ctx: &mut Interpreter) -> RuntimeResult {
    // Pop the return value
    let return_value = ctx.pop();

    // Remove the top call frame
    let frame = ctx.call_stack.pop()
        .expect("bug: should always pop off at least one frame while program is running");

    // Reset the value stack to before this call
    ctx.value_stack.truncate(frame.frame_index);

    if ctx.call_stack.is_empty() {
        assert!(ctx.value_stack.is_empty(),
            "bug: value stack was not empty at the end of the program");

        return Ok(Status::Complete);
    }

    // Push the returned value
    ctx.value_stack.push(return_value);

    Ok(Status::Running)
}

#[inline]
pub fn const_unit(ctx: &mut Interpreter) -> RuntimeResult {
    ctx.value_stack.push(Value::Unit);

    Ok(Status::Running)
}

#[inline]
pub fn constant(ctx: &mut Interpreter, index: u16) -> RuntimeResult {
    // Safety: If the bytecode is compiled correctly, this will always be a valid
    // constant ID
    let id = unsafe { ConstId::new_unchecked(index) };
    ctx.value_stack.push(ctx.consts.get(id).clone());

    Ok(Status::Running)
}

pub fn get_local(ctx: &mut Interpreter, fp_offset: u8) -> RuntimeResult {
    // Must have at least one call frame to get to this point
    let frame = unsafe { ctx.call_stack.top_unchecked() };

    let value = ctx.value_stack[frame.frame_index+fp_offset as usize].clone();
    ctx.value_stack.push(value);

    Ok(Status::Running)
}

pub fn set_local(ctx: &mut Interpreter, fp_offset: u8) -> RuntimeResult {
    let value = ctx.pop();

    // Must have at least one call frame to get to this point
    let frame = unsafe { ctx.call_stack.top_unchecked() };
    ctx.value_stack[frame.frame_index+fp_offset as usize] = value;

    // Replace the popped value (needed because assignment is an expression and expressions must
    // have a stack effect = 1)
    ctx.value_stack.push(Value::Unit);

    Ok(Status::Running)
}

#[inline]
pub fn pop(ctx: &mut Interpreter, n: u8) -> RuntimeResult {
    for _ in 0..n {
        ctx.pop();
    }

    Ok(Status::Running)
}

#[inline]
pub fn print(ctx: &mut Interpreter) -> RuntimeResult {
    println!("{}", ctx.pop());

    Ok(Status::Running)
}

#[inline]
pub fn neg(ctx: &mut Interpreter) -> RuntimeResult {
    unary_op(ctx, Value::neg, ast::UnaryOp::Neg)
}

#[inline]
pub fn pos(ctx: &mut Interpreter) -> RuntimeResult {
    unary_op(ctx, Value::pos, ast::UnaryOp::Pos)
}

#[inline]
pub fn not(ctx: &mut Interpreter) -> RuntimeResult {
    unary_op(ctx, Value::not, ast::UnaryOp::Not)
}

#[inline(always)]
fn unary_op(
    ctx: &mut Interpreter,
    f: impl FnOnce(Value) -> Option<Value>,
    op: ast::UnaryOp,
) -> RuntimeResult {
    let value = ctx.pop();
    let typ = value.typ();

    let result = f(value).ok_or_else(|| RuntimeError::UnsupportedUnaryOp {op, typ})?;
    ctx.value_stack.push(result);

    Ok(Status::Running)
}

#[inline]
pub fn add(ctx: &mut Interpreter) -> RuntimeResult {
    binary_op(ctx, Value::add, ast::BinaryOp::Add)
}

#[inline]
pub fn sub(ctx: &mut Interpreter) -> RuntimeResult {
    binary_op(ctx, Value::sub, ast::BinaryOp::Sub)
}

#[inline]
pub fn mul(ctx: &mut Interpreter) -> RuntimeResult {
    binary_op(ctx, Value::mul, ast::BinaryOp::Mul)
}

#[inline(always)]
fn binary_op(
    ctx: &mut Interpreter,
    f: impl FnOnce(Value, Value) -> Option<Value>,
    op: ast::BinaryOp,
) -> RuntimeResult {
    // Note that the values are on the stack in **reverse** order (rhs, then lhs)
    let rhs = ctx.pop();
    let lhs = ctx.pop();

    let lhs_type = lhs.typ();
    let rhs_type = rhs.typ();

    let result = f(lhs, rhs)
        .ok_or_else(|| RuntimeError::UnsupportedBinaryOp {op, lhs_type, rhs_type})?;
    ctx.value_stack.push(result);

    Ok(Status::Running)
}

#[inline]
pub fn div(ctx: &mut Interpreter) -> RuntimeResult {
    try_binary_op(ctx, Value::div, ast::BinaryOp::Div)
}

#[inline]
pub fn rem(ctx: &mut Interpreter) -> RuntimeResult {
    try_binary_op(ctx, Value::rem, ast::BinaryOp::Rem)
}

#[inline(always)]
fn try_binary_op(
    ctx: &mut Interpreter,
    f: impl FnOnce(Value, Value) -> Option<Result<Value, RuntimeError>>,
    op: ast::BinaryOp,
) -> RuntimeResult {
    // Note that the values are on the stack in **reverse** order (rhs, then lhs)
    let rhs = ctx.pop();
    let lhs = ctx.pop();

    let lhs_type = lhs.typ();
    let rhs_type = rhs.typ();

    let result = f(lhs, rhs)
        .ok_or_else(|| RuntimeError::UnsupportedBinaryOp {op, lhs_type, rhs_type})??;
    ctx.value_stack.push(result);

    Ok(Status::Running)
}
