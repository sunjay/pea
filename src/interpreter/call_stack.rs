use std::ptr;

use thiserror::Error;

use crate::{
    prim,
    bytecode::BytecodeCursor,
    gc::{Gc, Trace},
};

#[derive(Debug, Clone, Error)]
#[error("stack overflow: call stack grew past maximum of {0} call frames")]
pub struct StackOverflow(usize);

#[derive(Debug)]
pub struct CallFrame {
    /// A pointer to the function being run (owned by the interpreter)
    pub(in super) func: Gc<prim::Func>,
    /// The index of the first slot in the value stack that belongs to this frame (frame pointer)
    pub(in super) frame_index: usize,
    /// The address in func.code of the next bytecode instruction to execute
    pub(in super) cursor: BytecodeCursor,
}

impl Trace for CallFrame {
    fn trace(&self) {
        let Self {func, frame_index: _, cursor: _} = self;

        func.trace();
    }
}

impl CallFrame {
    pub fn new(func: Gc<prim::Func>, frame_index: usize) -> Self {
        Self {
            func,
            frame_index,
            // Start from the first byte
            cursor: BytecodeCursor::default(),
        }
    }
}

#[derive(Debug)]
pub struct CallStack {
    stack: Vec<CallFrame>,
    /// A cached pointer to the top of the call stack
    ///
    /// Safety: This may get invalidated if the stack changes capacity
    top_frame: *mut CallFrame,
}

impl Trace for CallStack {
    fn trace(&self) {
        // `top_frame` points to data in stack, so we can ignore it and just trace stack
        let Self {stack, top_frame: _} = self;

        stack.trace();
    }
}

impl CallStack {
    /// Initialize a new call stack with the given capacity
    pub fn new(capacity: usize) -> Self {
        Self {
            stack: Vec::with_capacity(capacity),
            top_frame: ptr::null_mut(),
        }
    }

    /// Returns true if no call frames have been added to the call stack
    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    /// Iterate through the frames in the stack, top to bottom
    pub fn iter_top_down(&self) -> impl Iterator<Item=&CallFrame> {
        self.stack.iter().rev()
    }

    /// Returns a reference to the top frame of the call stack
    ///
    /// # Safety
    ///
    /// The call stack must have at least one call frame or this will result in UB.
    pub unsafe fn top_unchecked(&self) -> &CallFrame {
        &*self.top_frame
    }

    /// Returns a mutable reference to the top frame of the call stack
    ///
    /// # Safety
    ///
    /// The call stack must have at least one call frame or this will result in UB.
    pub unsafe fn top_unchecked_mut(&mut self) -> &mut CallFrame {
        &mut *self.top_frame
    }

    /// Push a new call frame onto the top of the call stack
    ///
    /// Performance: This function can never trigger a heap allocation
    pub fn push(&mut self, frame: CallFrame) -> Result<(), StackOverflow> {
        if self.stack.len() >= self.stack.capacity() {
            return Err(StackOverflow(self.stack.capacity()));
        }

        self.stack.push(frame);

        self.top_frame = self.stack.last_mut()
            .map(|frame| frame as *mut CallFrame)
            .unwrap_or(ptr::null_mut());

        Ok(())
    }

    /// Pop the top call frame from the call stack (if any)
    ///
    /// Performance: This function can never trigger a heap allocation
    pub fn pop(&mut self) -> Option<CallFrame> {
        let frame = self.stack.pop()?;

        self.top_frame = self.stack.last_mut()
            .map(|frame| frame as *mut CallFrame)
            .unwrap_or(ptr::null_mut());

        Some(frame)
    }
}
