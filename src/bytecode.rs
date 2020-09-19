use crate::{value::Value, gc::Trace};

/// A compiled chunk of bytecode, including both opcodes and operand bytes
#[derive(Debug, Default, Clone)]
pub struct Bytecode(Vec<u8>);

impl Bytecode {
    /// Writes an instruction opcode into the bytecode chunk with no arguments
    pub fn write_instr(&mut self, opcode: OpCode) {
        self.0.push(opcode as u8);
    }

    /// Writes an instruction opcode into the bytecode chunk with a single u8 argument
    pub fn write_instr_u8(&mut self, opcode: OpCode, arg: u8) {
        self.0.push(opcode as u8);
        self.0.push(arg);
    }

    /// Writes an instruction opcode into the bytecode chunk with a single u16 argument
    pub fn write_instr_u16(&mut self, opcode: OpCode, arg: u16) {
        self.0.push(opcode as u8);
        self.0.extend(&arg.to_le_bytes());
    }

    /// Returns true if this chunk of bytecode is empty
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Returns the number of bytes in this chunk of bytecode
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns the byte at the given index
    ///
    /// # Safety
    ///
    /// No bounds checking is performed.
    pub unsafe fn get_unchecked(&self, index: usize) -> u8 {
        *self.0.get_unchecked(index)
    }
}

/// An ID guaranteed to refer to a valid constant in the constant pool
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConstId(u16);

impl ConstId {
    /// Creates a new constant ID from the given value
    ///
    /// # Safety
    ///
    /// You must guarantee that the given value is a valid index intot he constant pool.
    pub unsafe fn new_unchecked(value: u16) -> Self {
        ConstId(value)
    }

    /// Returns the primitive value of this ID
    pub fn into_u16(self) -> u16 {
        self.0
    }
}

/// A pool of all constants in the program, referenced by index
#[derive(Debug, Default, Clone)]
pub struct Constants(Vec<Value>);

impl Trace for Constants {
    fn trace(&self) {
        self.0.trace();
    }
}

impl Constants {
    /// Pushes a constant value into the table of constants and returns its 16-bit index
    pub fn push(&mut self, value: Value) -> ConstId {
        let index = self.0.len();
        if index >= u16::MAX as usize {
            panic!("bug: compiler does not support more than {} constant values in a single chunk of bytecode", u16::MAX);
        }

        self.0.push(value);

        ConstId(index as u16)
    }

    /// Replaces the given constant with the given value
    pub fn replace(&mut self, id: ConstId, value: Value) {
        let ConstId(index) = id;
        // Safety: All ConstIds are guaranteed to be valid indexes
        let slot = unsafe { self.0.get_unchecked_mut(index as usize) };
        *slot = value;
    }

    /// Retrieves a constant using its ID
    pub fn get(&self, id: ConstId) -> &Value {
        let ConstId(index) = id;
        // Safety: All ConstIds are guaranteed to be valid indexes
        unsafe { self.0.get_unchecked(index as usize) }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum OpCode {
    /// Calls a function with the given number of arguments (up to 255).
    ///
    /// The number of arguments is specified by the next byte in the bytecode. The arguments should
    /// appear after the function to call on the stack. This will pop one value (the function) + the
    /// number of arguments off the stack.
    Call,

    /// Return from the current function or exit the program if we are already at the bottom of the
    /// call stack
    Return,

    /// Push the unit value onto the stack
    ConstUnit,

    /// Load a constant value from the constants array and push it onto the stack
    ///
    /// The next two bytes in the bytecode represent the constant index. They will be interpreted as
    /// an unsigned 16-bit integer (little-endian byte order).
    Constant,

    /// Pops the value at the top of the stack, discarding it
    Pop,

    /// Pops and prints the value at the top of the stack
    //TODO: Get rid of this and make printing an ordinary function call
    Print,
}
