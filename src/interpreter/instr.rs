use crate::{bytecode::ConstId, value::Value};

use super::{CallFrame, Interpreter, RuntimeError, RuntimeResult, Status};

#[inline]
pub fn call(ctx: &mut Interpreter, nargs: u8) -> RuntimeResult {
    let nargs = nargs as usize;

    let func = match ctx.peek(nargs) {
        Value::Func(func) => func.clone(),
        _ => return Err(RuntimeError::NonFunctionCall),
    };

    //TODO: Validate that the function was passed the correct number of arguments

    // Start with the arguments and the function itself on the start of the stack frame
    let frame_index = ctx.value_stack.len() - nargs - 1;
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

#[inline]
pub fn pop(ctx: &mut Interpreter) -> RuntimeResult {
    ctx.pop();

    Ok(Status::Running)
}

#[inline]
pub fn print(ctx: &mut Interpreter) -> RuntimeResult {
    println!("{}", ctx.pop());

    Ok(Status::Running)
}

#[inline]
pub fn neg(ctx: &mut Interpreter) -> RuntimeResult {
    todo!()
}

#[inline]
pub fn pos(ctx: &mut Interpreter) -> RuntimeResult {
    todo!()
}

#[inline]
pub fn not(ctx: &mut Interpreter) -> RuntimeResult {
    todo!()
}

#[inline]
pub fn add(ctx: &mut Interpreter) -> RuntimeResult {
    todo!()
}

#[inline]
pub fn sub(ctx: &mut Interpreter) -> RuntimeResult {
    todo!()
}

#[inline]
pub fn mul(ctx: &mut Interpreter) -> RuntimeResult {
    todo!()
}

#[inline]
pub fn div(ctx: &mut Interpreter) -> RuntimeResult {
    todo!()
}

#[inline]
pub fn rem(ctx: &mut Interpreter) -> RuntimeResult {
    todo!()
}
