use crate::value::Value;

/// A compiled chunk of bytecode and any other information needed to run it, including metadata
#[derive(Debug, Default, Clone, PartialEq)]
pub struct Code {
    /// The complete bytecode including opcodes and operands
    pub bytes: Vec<u8>,
    /// A table of constants that may be referenced by index
    pub constants: Vec<Value>,
}

impl Code {
    /// Writes an instruction opcode into the bytecode chunk with no arguments
    pub fn write_instr(&mut self, opcode: OpCode) {
        self.bytes.push(opcode as u8);
    }

    /// Writes an instruction opcode into the bytecode chunk with a single u16 argument
    pub fn write_instr_u16(&mut self, opcode: OpCode, arg: u16) {
        self.bytes.push(opcode as u8);
        self.bytes.extend(&arg.to_le_bytes());
    }

    /// Pushes a constant value into the table of constants and returns its 16-bit index
    pub fn push_constant(&mut self, value: Value) -> u16 {
        let index = self.constants.len();
        if index >= u16::MAX as usize {
            panic!("bug: compiler does not support more than {} constant values in a single chunk of bytecode", u16::MAX);
        }

        self.constants.push(value);

        index as u16
    }

    /// Retrieves a constant from the table of constants
    ///
    /// # Safety
    ///
    /// No bounds checking takes place so the index must be valid.
    pub unsafe fn constant(&self, index: u16) -> &Value {
        self.constants.get_unchecked(index as usize)
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
