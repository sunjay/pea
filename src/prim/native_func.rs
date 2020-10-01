use std::fmt;

use crate::{gc::{self, Gc}, interpreter::{Interpreter, RuntimeResult}, value::Value};

pub struct NativeFunc {
    pub name: Gc<str>,
    pub arity: u8,
    pub func: Box<dyn Fn(&mut Interpreter, &[Value]) -> RuntimeResult>,
}

impl fmt::Debug for NativeFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {name, arity, func} = self;

        f.debug_struct("NativeFunc")
            .field("name", name)
            .field("arity", arity)
            .field("func", &format_args!("{:p}", func.as_ref()))
            .finish()
    }
}

impl PartialEq for NativeFunc {
    fn eq(&self, other: &Self) -> bool {
        let Self {name, arity, func} = self;
        *name == other.name && *arity == other.arity
            && func.as_ref() as *const _ == other.func.as_ref() as *const _
    }
}

impl fmt::Display for NativeFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}

impl gc::Trace for NativeFunc {
    fn trace(&self) {
        let Self {name, arity: _, func: _} = self;
        name.trace();
    }
}
