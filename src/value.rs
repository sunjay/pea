mod obj;

pub use obj::*;

use std::{fmt, mem};

use static_assertions::const_assert_eq;

use crate::gc::Gc;

/// The representation of a value in bytecode
#[derive(Debug, Clone)]
pub enum Value {
    I64(i64),
    Obj(Gc<Obj>),
}

// Make sure value doesn't grow beyond what we expect it to be
const_assert_eq!(mem::size_of::<Value>(), 16);

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Value::*;
        match self {
            I64(value) => write!(f, "{}", value),
            Obj(obj) => write!(f, "{}", *obj.lock()),
        }
    }
}
