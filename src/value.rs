use std::{fmt, mem};

use static_assertions::const_assert_eq;

use parking_lot::Mutex;

use crate::{prim, gc::Gc};

/// The representation of a value in bytecode
#[derive(Debug, Clone)]
pub enum Value {
    /// The `()` value
    Unit,
    I64(i64),
    Bytes(Gc<Mutex<prim::Bytes>>),
    Func(Gc<prim::Func>),
}

// Make sure value doesn't grow beyond what we expect it to be
const_assert_eq!(mem::size_of::<Value>(), 16);

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Value::*;
        match self {
            Unit => write!(f, "()"),
            I64(value) => write!(f, "{}", value),
            Bytes(value) => write!(f, "{}", *value.lock()),
            Func(value) => write!(f, "{}", **value),
        }
    }
}

impl Value {
    pub fn unwrap_func(&self) -> &Gc<prim::Func> {
        use Value::*;
        match self {
            Func(func) => func,
            _ => panic!("expected a function"),
        }
    }
}
