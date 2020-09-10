use std::mem;

use crate::{bytecode::{Bytecode, Constants, OpCode}, value::Value};

/// The current status of the interpreter
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Status {
    Running,
    Complete,
}

#[derive(Debug)]
pub struct Interpreter {
    code: Bytecode,
    consts: Constants,
    /// The address in code.bytes of the next bytecode instruction to execute
    next_instr: usize,
    /// The stack of values being operated on
    stack: Vec<Value>,
}

impl Interpreter {
    pub fn new(code: Bytecode, consts: Constants) -> Self {
        assert!(!code.is_empty(), "bug: bytecode chunk cannot be interpreted when empty");

        Self {
            code,
            consts,
            // Start from the first byte
            next_instr: 0,
            // Start with an initial capacity so programs can avoid allocating too often and so that
            // many small programs don't allocate more than once
            stack: Vec::with_capacity(256),
        }
    }

    pub fn step(&mut self) -> Status {
        // Safety: `next_instr` will be in bounds and the transmute is safe assuming that the
        // bytecode is compiled correctly and assuming there is at least one instruction. If the
        // compiler accidentally creates a jump to some arbitrary value, this can cause UB.
        let next_op: u8 = unsafe { self.code.get_unchecked(self.next_instr) };
        let next_op: OpCode = unsafe { mem::transmute(next_op) };
        self.next_instr += mem::size_of::<u8>();

        use OpCode::*;
        match next_op {
            Return => {
                return Status::Complete;
            },

            Constant => {
                let index = self.read_u16();
                self.stack.push(unsafe { self.consts.get_unchecked(index).clone() });
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

    fn read_u16(&mut self) -> u16 {
        let mut bytes = [0; 2];
        // Safety: This is safe assuming that the bytecode was compiled correctly
        bytes[0] = unsafe { self.code.get_unchecked(self.next_instr) };
        bytes[1] = unsafe { self.code.get_unchecked(self.next_instr+1) };
        self.next_instr += 2;
        u16::from_le_bytes(bytes)
    }

    fn pop(&mut self) -> Value {
        // Stack should always have a value if the bytecode is compiled correctly
        self.stack.pop()
            .expect("bug: stack underflow")
    }
}
