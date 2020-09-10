use std::mem;

use crate::{
    prim,
    bytecode::{Constants, ConstId, OpCode},
    value::Value,
    gc::Gc,
};

/// The current status of the interpreter
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Status {
    Running,
    Complete,
}

#[derive(Debug)]
pub struct CallFrame {
    /// A pointer to a function owned by the interpreter
    func: Gc<prim::Func>,
    /// The index of the first slot in the value stack that belongs to this frame (frame pointer)
    frame_index: usize,
    /// The address in func.code of the next bytecode instruction to execute
    next_instr: usize,
}

impl CallFrame {
    pub fn new(func: Gc<prim::Func>, frame_index: usize) -> Self {
        Self {
            func: func.into(),
            frame_index,
            // Start from the first byte
            next_instr: 0,
        }
    }
}

#[derive(Debug)]
pub struct Interpreter {
    consts: Constants,
    call_stack: Vec<CallFrame>,
    value_stack: Vec<Value>,
}

impl Interpreter {
    pub fn new(consts: Constants) -> Self {
        Self {
            consts,
            call_stack: Vec::with_capacity(64),
            // Start with an initial capacity so programs can avoid allocating too often and so that
            // many small programs don't allocate more than once
            value_stack: Vec::with_capacity(256),
        }
    }

    /// Pushes the first call frame onto the call stack
    ///
    /// The given constant index should point to a function that takes zero arguments
    pub fn call_main(&mut self, const_index: ConstId) {
        assert!(self.call_stack.is_empty(),
            "bug: main can only be initialized before the interpreter has begun");

        let func = self.consts.get(const_index).unwrap_func().clone();

        // main starts at the first item in the stack
        let frame_index = self.value_stack.len();
        self.call_stack.push(CallFrame::new(func, frame_index));
    }

    pub fn step(&mut self) -> Status {
        // Safety: `next_instr` will be in bounds and the transmute is safe assuming that the
        // bytecode is compiled correctly and assuming there is at least one instruction. If the
        // compiler accidentally creates a jump to some arbitrary value, this can cause UB.
        let next_op: u8 = unsafe { self.read_next_byte() };
        let next_op: OpCode = unsafe { mem::transmute(next_op) };

        use OpCode::*;
        match next_op {
            Return => {
                let result = self.pop();

                // Remove the top call frame
                self.call_stack.pop();
                if self.call_stack.is_empty() {
                    return Status::Complete;
                }

                //TODO: Pop off any local variables from the value stack
                //self.value_stack.set_len(self.value_stack.len() - num_locals)

                // push the returned value
                self.value_stack.push(result);
            },

            ConstUnit => {
                self.value_stack.push(Value::Unit);
            },

            Constant => {
                let index = self.read_u16();
                // Safety: If the bytecode is compiled correctly, this will always be a valid
                // constant ID
                let id = unsafe { ConstId::new_unchecked(index) };
                self.value_stack.push(self.consts.get(id).clone());
            },

            Pop => {
                self.pop();
            },

            Print => {
                println!("{}", self.pop());
            },
        }

        Status::Running
    }

    /// Returns the next byte from the code segment of the call frame at the top of the call stack.
    ///
    /// # Safety
    ///
    /// No bounds checking is performed. The next address must be valid within the code segment. The
    /// function associated with this call frame must still be alive. There must be at least one
    /// call frame.
    #[inline]
    unsafe fn read_next_byte(&mut self) -> u8 {
        let mut frame = self.top_call_frame_unchecked_mut();
        let addr = frame.next_instr;
        frame.next_instr += mem::size_of::<u8>();

        frame.func.code.get_unchecked(addr)
    }

    /// Retrieves the top call frame
    ///
    /// # Safety
    ///
    /// Assumes that there is at least one stack frame
    #[inline]
    unsafe fn top_call_frame_unchecked_mut(&mut self) -> &mut CallFrame {
        //TODO: If we cache a pointer to the top frame we can avoid this calculation + index
        let index = self.call_stack.len().saturating_sub(1);
        self.call_stack.get_unchecked_mut(index)
    }

    fn read_u16(&mut self) -> u16 {
        let mut bytes = [0; 2];
        // Safety: This is safe assuming that the bytecode was compiled correctly
        bytes[0] = unsafe { self.read_next_byte() };
        bytes[1] = unsafe { self.read_next_byte() };
        u16::from_le_bytes(bytes)
    }

    fn pop(&mut self) -> Value {
        // Stack should always have a value if the bytecode is compiled correctly
        self.value_stack.pop()
            .expect("bug: stack underflow")
    }
}
