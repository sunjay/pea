mod execute;
mod instr;
mod call_stack;

pub use call_stack::*;

use std::sync::Arc;

use thiserror::Error;
use parking_lot::RwLock;

use crate::{
    gc_debug,
    ast,
    bytecode::{ConstId, Constants, OpCode},
    gc::{self, Trace},
    source_files::SourceFiles,
    value::{self, Value},
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

    #[error(transparent)]
    StackOverflow(#[from] call_stack::StackOverflow),

    #[error("attempt to divide by zero")]
    DivideByZero,

    #[error("attempt to calculate the remainder with a divisor of zero")]
    RemainderByZero,

    #[error("unsupported unary operator {op} for type `{typ}`")]
    UnsupportedUnaryOp {
        op: ast::UnaryOp,
        typ: value::Type,
    },

    #[error("unsupported binary operator {op} for types `{lhs_type}` and `{rhs_type}`")]
    UnsupportedBinaryOp {
        op: ast::BinaryOp,
        lhs_type: value::Type,
        rhs_type: value::Type,
    },

    #[error("wrong number of arguments for function `{name}`: expected {arity}, got {nargs}")]
    ArityMismatch {
        name: Arc<str>,
        arity: u8,
        nargs: u8,
    },

    #[error("conditions must evaluate to a value of type `bool`")]
    ConditionMustBeBool,

    #[error("list repeat length argument must be a number, found `{typ}`")]
    ListRepeatLenMismatchedTypes {
        typ: value::Type,
    },
    #[error("list repeat length argument must be a non-negative number")]
    ListRepeatLenNegative,
}

pub type RuntimeResult = Result<Status, RuntimeError>;

#[derive(Debug)]
pub struct Interpreter {
    consts: Constants,
    call_stack: CallStack,
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
            call_stack: CallStack::new(64),
            // Start with an initial capacity so programs can avoid allocating too often and so that
            // many small programs don't allocate more than once
            value_stack: Vec::with_capacity(256),
            source_files,
        }
    }

    /// Pushes the first call frame onto the call stack
    ///
    /// The given constant ID should point to a function that takes zero arguments
    pub fn call_main(&mut self, id: ConstId) {
        assert!(self.call_stack.is_empty(),
            "bug: main can only be initialized before the interpreter has begun");

        // Delegate to the call instruction since it will do all necessary validations for us and
        // then update the interpreter state to be consistent with our calling convention
        self.value_stack.push(self.consts.get(id).clone());

        instr::call(self, 0)
            .expect("bug: failed to call `main` function while setting up interpreter");
    }

    /// Prints the call stack to stderr
    pub fn print_call_stack(&self) {
        eprintln!("stack backtrace:");
        for (i, frame) in self.call_stack.iter_top_down().enumerate() {
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

        // Safety: This is safe assuming that the bytecode is compiled correctly. There must be at
        // least one call stack frame and at least one bytecode instruction in the current frame's
        // function. If the compiler accidentally creates a jump to some arbitrary position, this
        // can cause UB.
        let frame = unsafe { self.call_stack.top_unchecked_mut() };
        let next_op = unsafe { frame.cursor.read_opcode_unchecked(&frame.func.code) };

        use OpCode::*;
        match next_op {
            Call => instr::call.run(self),
            Return => instr::ret.run(self),

            Jump => instr::jump.run(self),
            Loop => instr::jump_back.run(self),
            JumpIfTrue => instr::jump_if_true.run(self),
            JumpIfFalse => instr::jump_if_false.run(self),

            ConstUnit => instr::const_unit.run(self),
            ConstTrue => instr::const_true.run(self),
            ConstFalse => instr::const_false.run(self),
            Constant => instr::constant.run(self),

            List => instr::list.run(self),
            ListRepeat => instr::list_repeat.run(self),

            GetLocal => instr::get_local.run(self),
            SetLocal => instr::set_local.run(self),

            Pop => instr::pop.run(self),
            BlockEnd => instr::block_end.run(self),

            Println => instr::println(self),

            Neg => instr::neg.run(self),
            Pos => instr::pos.run(self),
            Not => instr::not.run(self),

            Add => instr::add.run(self),
            Sub => instr::sub.run(self),
            Mul => instr::mul.run(self),
            Div => instr::div.run(self),
            Rem => instr::rem.run(self),

            EqualsEquals => instr::equals_equals.run(self),
            NotEquals => instr::not_equals.run(self),
            GreaterThan => instr::greater_than.run(self),
            GreaterThanEquals => instr::greater_than_equals.run(self),
            LessThan => instr::less_than.run(self),
            LessThanEquals => instr::less_than_equals.run(self),
        }
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
