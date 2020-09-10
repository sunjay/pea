mod func_obj_ptr;

use std::mem;

use crate::{bytecode::{Constants, OpCode}, value::{Value, FuncObj}};

use func_obj_ptr::FuncObjPtr;

/// The current status of the interpreter
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Status {
    Running,
    Complete,
}

#[derive(Debug)]
pub struct CallFrame {
    /// A pointer to a function owned by the interpreter
    func: FuncObjPtr,
    /// The index of the first slot in the value stack that belongs to this frame (frame pointer)
    frame_index: usize,
    /// The address in func.code of the next bytecode instruction to execute
    next_instr: usize,
}

impl CallFrame {
    pub fn new(func: &mut FuncObj, frame_index: usize) -> Self {
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
    pub fn call_main(&mut self, const_index: u16) {
        assert!(self.call_stack.is_empty(),
            "bug: main can only be initialized before the interpreter has begun");

        // Safety: compiler should generate a valid constant index
        let func = unsafe { self.consts.get_unchecked(const_index) }.unwrap_obj();
        let mut func = func.lock();
        let func = func.unwrap_func_mut().into();

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
                //TODO: Return a value
                //let result = self.pop();

                // Remove the top call frame
                self.call_stack.pop();
                if self.call_stack.is_empty() {
                    return Status::Complete;
                }

                //TODO: Pop off any local variables from the value stack
                //self.value_stack.set_len(self.value_stack.len() - num_locals)

                //TODO:
                // push the returned value
                //self.value_stack.push(result);
            },

            Constant => {
                let index = self.read_u16();
                self.value_stack.push(unsafe { self.consts.get_unchecked(index).clone() });
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

        let func = frame.func.get_unchecked();
        func.code.get_unchecked(addr)
    }

    /// Retrieves the top call frame
    ///
    /// # Safety
    ///
    /// Assumes that there is at least one stack frame
    #[inline]
    unsafe fn top_call_frame_unchecked_mut(&mut self) -> &mut CallFrame {
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
