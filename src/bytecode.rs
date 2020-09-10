use crate::value::Value;

/// A compiled chunk of bytecode, including both opcodes and operand bytes
#[derive(Debug, Default, Clone)]
pub struct Bytecode(Vec<u8>);

impl Bytecode {
    /// Writes an instruction opcode into the bytecode chunk with no arguments
    pub fn write_instr(&mut self, opcode: OpCode) {
        self.0.push(opcode as u8);
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

/// A table of constants, referenced by index
#[derive(Debug, Default, Clone)]
pub struct Constants(Vec<Value>);

impl Constants {
    /// Pushes a constant value into the table of constants and returns its 16-bit index
    pub fn push(&mut self, value: Value) -> u16 {
        let index = self.0.len();
        if index >= u16::MAX as usize {
            panic!("bug: compiler does not support more than {} constant values in a single chunk of bytecode", u16::MAX);
        }

        self.0.push(value);

        index as u16
    }

    /// Retrieves a constant from the table of constants
    ///
    /// # Safety
    ///
    /// No bounds checking takes place so the index must be valid.
    pub unsafe fn get_unchecked(&self, index: u16) -> &Value {
        self.0.get_unchecked(index as usize)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum OpCode {
    /// Return from the current function or exit the program if we are already at the bottom of the
    /// call stack
    Return,

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
