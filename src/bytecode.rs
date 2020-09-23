use std::fmt;
use std::mem;
use std::collections::HashMap;
use std::slice::SliceIndex;
use std::convert::TryInto;

use crate::{gc::Trace, source_files::Span, value::Value};

/// A unique token used to backpatch an argument of an instruction after it has already been written
/// into the bytecode
#[derive(Debug)]
#[must_use]
pub struct PatchJump {
    /// The address of the `u16` value to overwrite
    addr: usize,
}

/// A compiled chunk of bytecode, including both opcodes and operand bytes
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Bytecode {
    bytes: Vec<u8>,
    /// Map of offset into `bytes` to the corresponding `Span`
    ///
    /// Spans are only inserted for each `OpCode`.
    spans: HashMap<usize, Span>,
}

impl Bytecode {
    /// Writes an instruction opcode into the bytecode chunk with no arguments
    pub fn write_instr(&mut self, opcode: OpCode, span: Span) {
        let offset = self.bytes.len();
        self.bytes.push(opcode as u8);
        self.spans.insert(offset, span);
    }

    /// Writes an instruction opcode into the bytecode chunk with a single u8 argument
    pub fn write_instr_u8(&mut self, opcode: OpCode, arg: u8, span: Span) {
        self.write_instr(opcode, span);
        self.bytes.push(arg);
    }

    /// Writes an instruction opcode into the bytecode chunk with a single u16 argument
    pub fn write_instr_u16(&mut self, opcode: OpCode, arg: u16, span: Span) {
        self.write_instr(opcode, span);
        self.bytes.extend(&arg.to_le_bytes());
    }

    /// Writes an instruction opcode into the bytecode chunk with a single u16 argument that is
    /// backpatched in later using `finish_patch`.
    pub fn write_jump_patch(&mut self, opcode: OpCode, span: Span) -> PatchJump {
        self.write_instr(opcode, span);

        let addr = self.bytes.len();
        // Put a placeholder value that will be backpatched later
        self.bytes.extend(&0u16.to_le_bytes());

        PatchJump {addr}
    }

    /// Completes a backpatch at the current end of the bytecode
    pub fn finish_jump_patch(&mut self, patch: PatchJump) {
        let PatchJump {addr} = patch;
        let value_end_addr = addr + mem::size_of::<u16>();

        let last_addr = self.len();
        let jump_offset: u16 = (last_addr - value_end_addr).try_into()
            .expect("bug: offset is greater than u16::MAX");

        self.bytes[addr..value_end_addr].copy_from_slice(&jump_offset.to_le_bytes());
    }

    /// Returns true if this chunk of bytecode is empty
    pub fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }

    /// Returns the number of bytes in this chunk of bytecode
    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    /// Returns the byte at the given index
    pub fn get<I: SliceIndex<[u8]>>(&self, index: I) -> &<I as SliceIndex<[u8]>>::Output {
        &self.bytes[index]
    }

    /// Returns the byte at the given index
    ///
    /// # Safety
    ///
    /// No bounds checking is performed.
    pub unsafe fn get_unchecked<I: SliceIndex<[u8]>>(&self, index: I) -> &<I as SliceIndex<[u8]>>::Output {
        self.bytes.get_unchecked(index)
    }

    /// Returns the `Span` for the `OpCode` at the given offset
    ///
    /// # Panics
    ///
    /// A `Span` is only recorded for each `OpCode`, so if this does not correspond to a byte
    /// position that has an `OpCode`, this method will panic.
    pub fn span(&self, index: usize) -> Span {
        self.spans.get(&index).copied()
            .expect("bug: attempt to get a span for a non-opcode byte offset")
    }
}

/// A cursor into a chunk of bytecode
///
/// This does not explicitly keep ownership or even a reference to the bytecode being read, so it is
/// up to the user of the API to ensure that this cursor is only used with a single chunk of
/// bytecode.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BytecodeCursor {
    /// The next byte position that will be read
    next_pos: usize,
}

impl BytecodeCursor {
    /// Returns the current offset of the cursor in the bytecode
    pub fn offset(&self) -> usize {
        self.next_pos
    }

    /// Returns true if it is safe to continue reading at least one more byte of the given bytecode
    /// using this cursor
    pub fn can_read_further(&self, code: &Bytecode) -> bool {
        self.next_pos < code.len()
    }

    /// Adds the given value to the cursor position
    ///
    /// # Safety
    ///
    /// No overflow checks are performed. The value after adding must end up at a valid index in the
    /// bytecode for this to avoid UB.
    pub unsafe fn add(&mut self, offset: usize) {
        //TODO: Could use `unchecked_add` once that is stable
        self.next_pos = self.next_pos.wrapping_add(offset);
    }

    /// Subtracts the given value from the cursor position
    ///
    /// # Safety
    ///
    /// No overflow checks are performed. The value after subtracting must end up at a valid index
    /// in the bytecode for this to avoid UB.
    pub unsafe fn sub(&mut self, offset: usize) {
        //TODO: Could use `unchecked_sub` once that is stable
        self.next_pos = self.next_pos.wrapping_sub(offset);
    }

    /// Advances the cursor to read a `u8` from the given bytecode and then interprets that value as
    /// an `OpCode`
    ///
    /// # Safety
    ///
    /// No bounds checking is performed, so you must guarantee that there are enough bytes left to
    /// successfully read this value. It is UB for the value of the byte to be outside the valid
    /// range of values for `OpCode`.
    pub unsafe fn read_opcode_unchecked(&mut self, code: &Bytecode) -> OpCode {
        mem::transmute(self.read_u8_unchecked(code))
    }

    /// Reads both an `OpCode` and its corresponding `Span`
    ///
    /// See `read_opcode_unchecked` for more details.
    pub unsafe fn read_opcode_span_unchecked(&mut self, code: &Bytecode) -> (OpCode, Span) {
        let span = code.span(self.next_pos);
        let opcode = self.read_opcode_unchecked(code);
        (opcode, span)
    }

    /// Advances the cursor to read a `u8` from the given bytecode
    ///
    /// # Safety
    ///
    /// No bounds checking is performed, so you must guarantee that there are enough bytes left to
    /// successfully read this value.
    pub unsafe fn read_u8_unchecked(&mut self, code: &Bytecode) -> u8 {
        let addr = self.next_pos;
        self.next_pos += mem::size_of::<u8>();
        *code.get_unchecked(addr)
    }

    /// Advances the cursor to read a `u16` from the given bytecode
    ///
    /// # Safety
    ///
    /// No bounds checking is performed, so you must guarantee that there are enough bytes left to
    /// successfully read this value.
    pub unsafe fn read_u16_unchecked(&mut self, code: &Bytecode) -> u16 {
        let addr = self.next_pos;
        self.next_pos += mem::size_of::<u16>();
        let bytes = code.get_unchecked(addr..self.next_pos);
        // Safety: we just sliced the correct number of bytes
        // See: https://github.com/rust-lang/rust/blob/f68e08933d8f519a9655934fedebbc509661b219/library/core/src/array/mod.rs#L148-L161
        let bytes = *(bytes.as_ptr() as *const [u8; 2]);

        u16::from_le_bytes(bytes)
    }
}

/// An ID guaranteed to refer to a valid constant in the constant pool
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConstId(u16);

impl fmt::Display for ConstId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

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

    /// Iterates through each constant in an unspecified order
    pub fn iter(&self) -> impl Iterator<Item=(ConstId, &Value)> {
        (0..).zip(self.0.iter()).map(|(id, value)| (ConstId(id), value))
    }
}

/// The valid bytecode instruction opcodes
///
/// Any required arguments are listed with the opcode below. The arguments are expected to appear in
/// the bytecode after the opcode with no additional padding. All values are interpreted as
/// little-endian. For example, the two bytes for a `u16` will be read and then interpreted in
/// little-endian byte order.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum OpCode {
    /// Calls a function with the given number of arguments (up to 255).
    ///
    /// # Arguments
    ///
    /// * `u8` - The number of arguments to call the function with. The top of the value stack
    ///   is expected to have the function being called, followed by each argument. These values
    ///   should be arranged such that popping the value stack first retrieves the arguments in
    ///   reverse order and then the function being called itself.
    Call,
    /// Return from the current function or exit the program if we are already at the bottom of the
    /// call stack.
    ///
    /// This will reset the value stack to the frame pointer of the call frame that was just popped.
    /// That will result in all local variables from the popped frame being removed. It will then
    /// push the returned value on to the top of the stack.
    Return,

    /// Add the given address to the instruction pointer.
    ///
    /// # Arguments
    ///
    /// * `u16` - The amount to add to the instruction pointer
    Jump,
    /// Peek at the value at the top of the stack and add the given address to the instruction
    /// pointer if the value is true.
    ///
    /// Note that the value at the top of the stack is not removed or modified in any way.
    ///
    /// # Arguments
    ///
    /// * `u16` - The amount to add to the instruction pointer if the value is true
    JumpIfTrue,
    /// Peek at the value at the top of the stack and add the given address to the instruction
    /// pointer if the value is false.
    ///
    /// Note that the value at the top of the stack is not removed or modified in any way.
    ///
    /// # Arguments
    ///
    /// * `u16` - The amount to add to the instruction pointer if the value is false
    JumpIfFalse,

    /// Push the unit value `()` onto the top of the stack.
    ConstUnit,
    /// Push the bool value `true` onto the top of the stack.
    ConstTrue,
    /// Push the bool value `false` onto the top of the stack.
    ConstFalse,

    /// Load a constant value from the constants table and push it onto the top of the stack.
    ///
    /// # Arguments
    ///
    /// * `u16` - The ID of the constant to read.
    Constant,

    /// Loads a value from the stack and pushes a copy of it onto the top of the stack.
    ///
    /// # Arguments
    ///
    /// * `u8` - The index of the value on the stack relative to the frame pointer.
    GetLocal,
    /// Pops the value at the top of the stack and assigns it to the given stack slot. Pushes `()`
    /// onto the stack to replace the popped value.
    ///
    /// # Arguments
    ///
    /// * `u8` - The index of the slot on the stack relative to the frame pointer.
    SetLocal,

    /// Pops values starting from the top of the stack, discarding them.
    ///
    /// # Arguments
    ///
    /// * `u8` - The number of values to pop
    Pop,
    /// Pops the top item off the stack, pops the given number of items, then pushes the top item
    /// back onto the stack
    ///
    /// # Arguments
    ///
    /// * `u8` - The number of values to pop
    BlockEnd,

    /// Pops and prints the value at the top of the stack.
    //TODO: Get rid of this and make printing an ordinary function call
    Print,

    /// Pops the top value from the stack, negates it, and then pushes the result back onto the
    /// stack.
    Neg,
    /// Pops the top value from the stack, applies the positive operator to it, and then pushes
    /// the result back onto the stack.
    Pos,
    /// Pops the top value from the stack, applies the not operator to it, and then pushes the
    /// result back onto the stack.
    Not,

    /// Pops the top two values from the stack, adds them, and then pushes the result back onto the
    /// top of the stack.
    Add,
    /// Pops the top two values from the stack, subtracts them, and then pushes the result back onto
    /// the top of the stack.
    Sub,
    /// Pops the top two values from the stack, multiplies them, and then pushes the result back
    /// onto the top of the stack.
    Mul,
    /// Pops the top two values from the stack, divides them, and then pushes the result back onto
    /// the top of the stack.
    Div,
    /// Pops the top two values from the stack, computes the remainder of the second popped value
    /// from the first popped value, and then pushes the result back onto the top of the stack.
    Rem,

    /// Pops the top two values from the stack, applies the `==` operator, and then pushes the
    /// result back onto the top of the stack.
    EqualsEquals,
    /// Pops the top two values from the stack, applies the `!=` operator, and then pushes the
    /// result back onto the top of the stack.
    NotEquals,
    /// Pops the top two values from the stack, applies the `>` operator, and then pushes the
    /// result back onto the top of the stack.
    GreaterThan,
    /// Pops the top two values from the stack, applies the `>=` operator, and then pushes the
    /// result back onto the top of the stack.
    GreaterThanEquals,
    /// Pops the top two values from the stack, applies the `<` operator, and then pushes the
    /// result back onto the top of the stack.
    LessThan,
    /// Pops the top two values from the stack, applies the `<=` operator, and then pushes the
    /// result back onto the top of the stack.
    LessThanEquals,
}
