use std::iter;

use crate::{bytecode::ConstId, value::{Value, DeepClone}, gc::Gc};

use super::{CallFrame, Interpreter, RuntimeError, RuntimeResult, Status};

#[inline]
pub fn call(ctx: &mut Interpreter, nargs: u8) -> RuntimeResult {
    match ctx.peek(nargs as usize) {
        Value::Func(func) => {
            let func = func.clone();

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
        },

        Value::NativeFunc(func) => {
            let func = func.clone();

            if nargs != func.arity {
                return Err(RuntimeError::ArityMismatch {
                    name: func.name.clone(),
                    arity: func.arity,
                    nargs,
                });
            }

            // NOTE: This code must produce exactly the same state in `ctx` as `ret` would
            //   Not checking if the call stack is empty because a native function cannot be `main`.

            let args_index = ctx.value_stack.len() - nargs as usize;
            //TODO: Would be nice if there was a way to avoid allocating here
            let args = ctx.value_stack.drain(args_index..).collect();

            // Remove the function itself from the top of the stack
            ctx.pop();

            let return_value = func.call(ctx, args)?;

            // Push the returned value
            ctx.value_stack.push(return_value);

            Ok(Status::Running)
        },

        _ => return Err(RuntimeError::NonFunctionCall),
    }
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
pub fn jump(ctx: &mut Interpreter, offset: u16) -> RuntimeResult {
    // Must have at least one call frame to get to this point
    let frame = unsafe { ctx.call_stack.top_unchecked_mut() };
    // Safety: This jump should be valid if the bytecode is compiled correctly
    unsafe { frame.cursor.add(offset as usize); }

    Ok(Status::Running)
}

#[inline]
pub fn jump_back(ctx: &mut Interpreter, offset: u16) -> RuntimeResult {
    // Must have at least one call frame to get to this point
    let frame = unsafe { ctx.call_stack.top_unchecked_mut() };
    // Safety: This jump should be valid if the bytecode is compiled correctly
    unsafe { frame.cursor.sub(offset as usize); }

    Ok(Status::Running)
}

#[inline]
pub fn jump_if_true(ctx: &mut Interpreter, offset: u16) -> RuntimeResult {
    let cond_value = ctx.peek(0).to_bool()
        .ok_or(RuntimeError::ConditionMustBeBool)?;

    if cond_value {
        jump(ctx, offset)
    } else {
        Ok(Status::Running)
    }
}

#[inline]
pub fn jump_if_false(ctx: &mut Interpreter, offset: u16) -> RuntimeResult {
    let cond_value = ctx.peek(0).to_bool()
        .ok_or(RuntimeError::ConditionMustBeBool)?;

    if !cond_value {
        jump(ctx, offset)
    } else {
        Ok(Status::Running)
    }
}

#[inline]
pub fn const_unit(ctx: &mut Interpreter) -> RuntimeResult {
    ctx.value_stack.push(Value::Unit);

    Ok(Status::Running)
}

#[inline]
pub fn const_true(ctx: &mut Interpreter) -> RuntimeResult {
    ctx.value_stack.push(Value::Bool(true));

    Ok(Status::Running)
}

#[inline]
pub fn const_false(ctx: &mut Interpreter) -> RuntimeResult {
    ctx.value_stack.push(Value::Bool(false));

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
pub fn list(ctx: &mut Interpreter) -> RuntimeResult {
    // Compiler should guarantee that this argument is a non-negative number
    let len = ctx.pop().unwrap_i64() as usize;

    let list = ctx.value_stack.drain(ctx.value_stack.len()-len..).collect();
    ctx.value_stack.push(Value::List(Gc::new(list)));

    Ok(Status::Running)
}

#[inline]
pub fn list_repeat(ctx: &mut Interpreter) -> RuntimeResult {
    let len = ctx.pop();
    let len = len.to_i64()
        .ok_or_else(|| RuntimeError::ListRepeatLenMismatchedTypes {typ: len.typ()})?;

    let len = match len {
        0 ..= i64::MAX => len as usize,
        _ => return Err(RuntimeError::ListRepeatLenNegative),
    };

    let item = ctx.pop();

    let list = iter::repeat_with(|| item.deep_clone()).take(len).collect();
    ctx.value_stack.push(Value::List(Gc::new(list)));

    Ok(Status::Running)
}

#[inline]
pub fn get_local(ctx: &mut Interpreter, fp_offset: u8) -> RuntimeResult {
    // Must have at least one call frame to get to this point
    let frame = unsafe { ctx.call_stack.top_unchecked() };

    let value = ctx.value_stack[frame.frame_index+fp_offset as usize].clone();
    ctx.value_stack.push(value);

    Ok(Status::Running)
}

#[inline]
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
pub fn block_end(ctx: &mut Interpreter, n: u8) -> RuntimeResult {
    let result = ctx.pop();

    for _ in 0..n {
        ctx.pop();
    }

    ctx.value_stack.push(result);

    Ok(Status::Running)
}

#[inline]
pub fn println(ctx: &mut Interpreter) -> RuntimeResult {
    println!("{}", ctx.pop());

    Ok(Status::Running)
}

#[inline]
pub fn print(ctx: &mut Interpreter) -> RuntimeResult {
    print!("{}", ctx.pop());

    Ok(Status::Running)
}
