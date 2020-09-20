use std::fmt;
use std::sync::Arc;

use crate::{bytecode::{Bytecode, BytecodeCursor, OpCode}, source_files::SourceFiles};

use crate::gc;

#[derive(Debug, Clone)]
pub struct Func {
    pub name: Arc<str>,
    pub code: Bytecode,
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}

impl Func {
    pub fn new(name: Arc<str>) -> Self {
        Self::with_code(name, Default::default())
    }

    pub fn with_code(name: Arc<str>, code: Bytecode) -> Self {
        Self {name, code}
    }

    /// Prints the annotated bytecode this function to stderr
    pub fn print_annotated_bytecode(&self, source_files: &SourceFiles) {
        let mut cursor = BytecodeCursor::default();

        // Safety: If the bytecode is compiled correctly, this should all be valid
        let read_opcode = |cursor: &mut BytecodeCursor| unsafe {
            cursor.read_opcode_unchecked(&self.code)
        };
        let read_u8 = |cursor: &mut BytecodeCursor| unsafe {
            cursor.read_u8_unchecked(&self.code)
        };
        let read_u16 = |cursor: &mut BytecodeCursor| unsafe {
            cursor.read_u16_unchecked(&self.code)
        };

        eprintln!("{}:", self.name);
        while cursor.can_read_further(&self.code) {
            let offset = cursor.offset();
            let opcode: OpCode = read_opcode(&mut cursor);

            eprint!("  {:04} ", offset);

            use OpCode::*;
            match opcode {
                Call => eprintln!("call(nargs={})", read_u8(&mut cursor)),
                Return => eprintln!("return()"),
                ConstUnit => eprintln!("const_unit()"),
                Constant => eprintln!("const(const_id={})", read_u16(&mut cursor)),
                Pop => eprintln!("pop()"),
                Print => eprintln!("print()"),
            }
        }
        eprintln!();
    }
}

impl gc::Trace for Func {
    fn trace(&self) {
        let Self {name: _, code: _} = self;
    }
}
