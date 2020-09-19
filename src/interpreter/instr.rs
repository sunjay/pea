use crate::{bytecode::ConstId, value::Value};

use super::{CallFrame, Interpreter, RuntimeError, RuntimeResult, Status};

#[inline]
pub fn call(ctx: &mut Interpreter, nargs: u8) -> RuntimeResult {
    let nargs = nargs as usize;

    let func = match ctx.peek(nargs) {
        Value::Func(func) => func.clone(),
        _ => return Err(RuntimeError::NonFunctionCall),
    };

    // Start with the arguments on the start of the stack frame
    let frame_index = ctx.value_stack.len() - nargs;
    ctx.call_stack.push(CallFrame::new(func, frame_index));

    Ok(Status::Running)
}

#[inline]
pub fn ret(ctx: &mut Interpreter) -> RuntimeResult {
    let result = ctx.pop();

    // Remove the top call frame
    ctx.call_stack.pop();
    if ctx.call_stack.is_empty() {
        return Ok(Status::Complete);
    }

    //TODO: Pop off any local variables (incl. func args) from the value stack
    //ctx.value_stack.set_len(ctx.value_stack.len() - num_locals)

    // push the returned value
    ctx.value_stack.push(result);

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
