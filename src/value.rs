use std::{fmt, mem};

use static_assertions::const_assert_eq;

use crate::{prim, gc::GcPrim};

/// The representation of a value in bytecode
#[derive(Debug, Clone)]
pub enum Value {
    /// The `()` value
    Unit,
    I64(i64),
    Prim(GcPrim),
}

// Make sure value doesn't grow beyond what we expect it to be
const_assert_eq!(mem::size_of::<Value>(), 16);

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Value::*;
        match self {
            Unit => write!(f, "()"),
            I64(value) => write!(f, "{}", value),
            Prim(value) => write!(f, "{}", **value),
        }
    }
}

impl Value {
    pub fn unwrap_func(&self) -> &prim::Func {
        use Value::*;
        use prim::Prim::*;
        match self {
            Prim(value) => match &**value {
                Func(func) => func,
                _ => panic!("expected a function"),
            },
            _ => panic!("expected a function"),
        }
    }
}
