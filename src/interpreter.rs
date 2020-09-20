mod execute;
mod instr;

use std::mem;
use std::sync::Arc;

use thiserror::Error;
use parking_lot::RwLock;

use crate::{
    gc_debug,
    prim,
    gc::{self, Gc, Trace},
    bytecode::{Constants, ConstId, OpCode},
    source_files::SourceFiles,
    value::Value,
};

use execute::Execute;

/// The current status of the interpreter
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Status {
    Running,
    Complete,
}

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("attempt to call a value that is not a function")]
    NonFunctionCall,
}

pub type RuntimeResult = Result<Status, RuntimeError>;

#[derive(Debug)]
pub struct CallFrame {
    /// A pointer to the function being run (owned by the interpreter)
    func: Gc<prim::Func>,
    /// The index of the first slot in the value stack that belongs to this frame (frame pointer)
    frame_index: usize,
    /// The address in func.code of the next bytecode instruction to execute
    next_instr: usize,
}

impl Trace for CallFrame {
    fn trace(&self) {
        let Self {func, frame_index: _, next_instr: _} = self;

        func.trace();
    }
}

impl CallFrame {
    pub fn new(func: Gc<prim::Func>, frame_index: usize) -> Self {
        Self {
            func,
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
    source_files: Arc<RwLock<SourceFiles>>,
}

impl Trace for Interpreter {
    fn trace(&self) {
        let Self {consts, call_stack, value_stack, source_files: _} = self;
        consts.trace();
        call_stack.trace();
        value_stack.trace();
    }
}

impl Interpreter {
    pub fn new(consts: Constants, source_files: Arc<RwLock<SourceFiles>>) -> Self {
        Self {
            consts,
            call_stack: Vec::with_capacity(64),
            // Start with an initial capacity so programs can avoid allocating too often and so that
            // many small programs don't allocate more than once
            value_stack: Vec::with_capacity(256),
            source_files,
        }
    }

    /// Pushes the first call frame onto the call stack
    ///
    /// The given constant index should point to a function that takes zero arguments
    pub fn call_main(&mut self, const_index: ConstId) {
        assert!(self.call_stack.is_empty(),
            "bug: main can only be initialized before the interpreter has begun");

        let func = self.consts.get(const_index).unwrap_func();

        // main starts at the first item in the stack
        let frame_index = self.value_stack.len();
        self.call_stack.push(CallFrame::new(func.clone(), frame_index));
    }

    /// Prints the call stack to stderr
    pub fn print_call_stack(&self) {
        eprintln!("stack backtrace:");
        for (i, frame) in self.call_stack.iter().rev().enumerate() {
            //TODO: Use `frame.next_instr` to print the exact line that was running
            //  Will need to store `Span` and `diag` somewhere to do that.

            let func = &frame.func;
            eprintln!("{:>4}: {}", i, func.name);
        }
    }

    /// Prints the table of constants to stderr
    pub fn print_constants(&self) {
        eprintln!("Constants:\n");
        eprintln!("| ID    | Value");
        eprintln!("|-------|-----------------------");
        for (id, constant) in self.consts.iter() {
            eprintln!("| {:<5} | {}", id, constant);
        }
    }

    /// Prints the annotated bytecode of every function constant to stderr
    pub fn print_all_annotated_bytecode(&self) {
        eprintln!("Bytecode:\n");

        let source_files = self.source_files.read();
        for (_, constant) in self.consts.iter() {
            match constant {
                Value::Func(func) => {
                    func.print_annotated_bytecode(&source_files);
                },
                _ => {},
            }
        }
    }

    /// Executes the next bytecode instruction
    ///
    /// Returns the status of the interpreter after running the instruction. This method should no
    /// longer be called once `Status::Complete` has been returned.
    pub fn step(&mut self) -> Result<Status, RuntimeError> {
        // Trigger garbage collection if that is necessary
        if cfg!(feature = "gc_stress_test") || gc::needs_collect() {
            self.collect_garbage();
        }

        // Safety: `next_instr` will be in bounds and the transmute is safe assuming that the
        // bytecode is compiled correctly and assuming there is at least one instruction. If the
        // compiler accidentally creates a jump to some arbitrary value, this can cause UB.
        let next_op: u8 = unsafe { self.read_next_byte() };
        let next_op: OpCode = unsafe { mem::transmute(next_op) };

        use OpCode::*;
        match next_op {
            Call => instr::call.run(self),
            Return => instr::ret.run(self),
            ConstUnit => instr::const_unit.run(self),
            Constant => instr::constant.run(self),
            Pop => instr::pop.run(self),
            Print => instr::print(self),
        }
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

    fn read_u8(&mut self) -> u8 {
        // Safety: This is safe assuming that the bytecode was compiled correctly
        unsafe { self.read_next_byte() }
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

    /// Peek at the value n slots back in the value stack
    fn peek(&self, n: usize) -> &Value {
        &self.value_stack[self.value_stack.len() - n - 1]
    }

    /// Trigger garbage collection by first recursively marking/tracing all roots and then calling
    /// `gc::sweep()`
    fn collect_garbage(&self) {
        gc_debug!("--- GC BEGIN ---");
        self.trace();
        gc::sweep();
        gc_debug!("--- GC END ---");
    }
}
