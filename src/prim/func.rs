use std::fmt;
use std::sync::Arc;
use std::io::Write;

use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

use crate::{gc, bytecode::{Bytecode, BytecodeCursor, OpCode}, source_files::SourceFiles};

#[derive(Debug, Clone)]
pub struct Func {
    pub name: Arc<str>,
    pub arity: u8,
    pub code: Bytecode,
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}

impl Func {
    pub fn new(name: Arc<str>, arity: u8) -> Self {
        Self::with_code(name, arity, Default::default())
    }

    pub fn with_code(name: Arc<str>, arity: u8, code: Bytecode) -> Self {
        Self {name, arity, code}
    }

    /// Prints the annotated bytecode this function to stderr
    pub fn print_annotated_bytecode(&self, _source_files: &SourceFiles) {
        // Safety: If the bytecode is compiled correctly, this should all be valid
        let read_opcode = |cursor: &mut BytecodeCursor| unsafe {
            cursor.read_opcode_span_unchecked(&self.code)
        };
        let read_u8 = |cursor: &mut BytecodeCursor| unsafe {
            cursor.read_u8_unchecked(&self.code)
        };
        let read_u16 = |cursor: &mut BytecodeCursor| unsafe {
            cursor.read_u16_unchecked(&self.code)
        };

        macro_rules! cwrite {
            ($out:ident, $($t:tt)*) => {
                write!($out, $($t)*).expect("IO error")
            };
            ($out:ident) => {
                write!($out).expect("IO error")
            };
        }

        macro_rules! cwriteln {
            ($out:ident, $($t:tt)*) => {
                writeln!($out, $($t)*).expect("IO error")
            };
            ($out:ident) => {
                writeln!($out).expect("IO error")
            };
        }

        //TODO: Figure out how to get stream from `diag` here
        let mut out = StandardStream::stderr(ColorChoice::Auto);

        cwriteln!(out, "{}:", self.name);
        let mut cursor = BytecodeCursor::default();
        while cursor.can_read_further(&self.code) {
            let offset = cursor.offset();
            //TODO: Actually figure out how to print the source line for the given span
            let (opcode, _span) = read_opcode(&mut cursor);

            out.set_color(ColorSpec::new().set_fg(Some(Color::Cyan))).expect("IO error");
            cwrite!(out, "  {:04} ", offset);
            out.reset().expect("IO error");

            use OpCode::*;
            match opcode {
                Call => cwriteln!(out, "call(nargs={})", read_u8(&mut cursor)),
                Return => cwriteln!(out, "return()"),

                ConstUnit => cwriteln!(out, "const_unit()"),
                ConstTrue => cwriteln!(out, "const_true()"),
                ConstFalse => cwriteln!(out, "const_false()"),
                Constant => cwriteln!(out, "const(const_id={})", read_u16(&mut cursor)),

                GetLocal => cwriteln!(out, "get_local(fp_offset={})", read_u8(&mut cursor)),
                SetLocal => cwriteln!(out, "set_local(fp_offset={})", read_u8(&mut cursor)),

                Pop => cwriteln!(out, "pop(n={})", read_u8(&mut cursor)),
                BlockEnd => cwriteln!(out, "block_end(n={})", read_u8(&mut cursor)),

                Print => cwriteln!(out, "print()"),

                Neg => cwriteln!(out, "neg()"),
                Pos => cwriteln!(out, "pos()"),
                Not => cwriteln!(out, "not()"),
                Add => cwriteln!(out, "add()"),
                Sub => cwriteln!(out, "sub()"),
                Mul => cwriteln!(out, "mul()"),
                Div => cwriteln!(out, "div()"),
                Rem => cwriteln!(out, "rem()"),
            }
        }
        cwriteln!(out);
    }
}

impl gc::Trace for Func {
    fn trace(&self) {
        let Self {name: _, arity: _, code: _} = self;
    }
}
