use std::{fmt, mem};

use static_assertions::const_assert_eq;

use crate::{prim, gc::{Gc, Trace}};

/// The different supported types of values
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unit,
    Bool,
    I64,
    Bytes,
    Func,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Type::*;
        match self {
            Unit => write!(f, "()"),
            Bool => write!(f, "bool"),
            I64 => write!(f, "i64"),
            Bytes => write!(f, "[u8]"),
            Func => write!(f, "fn"),
        }
    }
}

/// The representation of a value in bytecode
#[derive(Debug, Clone)]
pub enum Value {
    /// The `()` value
    Unit,
    Bool(bool),
    I64(i64),
    Bytes(Gc<prim::Bytes>),
    Func(Gc<prim::Func>),
}

// Make sure value doesn't grow beyond what we expect it to be
const_assert_eq!(mem::size_of::<Value>(), 16);

impl Trace for Value {
    fn trace(&self) {
        use Value::*;
        match self {
            Unit |
            Bool(_) |
            I64(_) => {},
            Bytes(value) => value.trace(),
            Func(value) => value.trace(),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Value::*;
        match self {
            Unit => write!(f, "()"),
            Bool(value) => write!(f, "{}", value),
            I64(value) => write!(f, "{}", value),
            Bytes(value) => write!(f, "{}", *value),
            Func(value) => write!(f, "{}", **value),
        }
    }
}

impl Value {
    pub fn typ(&self) -> Type {
        use Value::*;
        match self {
            Unit => Type::Unit,
            Bool(_) => Type::Bool,
            I64(_) => Type::I64,
            Bytes(_) => Type::Bytes,
            Func(_) => Type::Func,
        }
    }

    pub fn unwrap_func(&self) -> &Gc<prim::Func> {
        use Value::*;
        match self {
            Func(func) => func,
            _ => panic!("expected a function"),
        }
    }

    /// Applies the unary `-` operator to this value or returns `None` if that operation is not
    /// supported for this type
    pub fn neg(self) -> Option<Self> {
        use Value::*;
        match self {
            I64(value) => Some(Value::I64(-value)),

            Unit |
            Bool(_) |
            Bytes(_) |
            Func(_) => None,
        }
    }

    /// Applies the unary `+` operator to this value or returns `None` if that operation is not
    /// supported for this type
    pub fn pos(self) -> Option<Self> {
        use Value::*;
        match self {
            // unary `+` has no effect on integers
            I64(_) => Some(self),

            Unit |
            Bool(_) |
            Bytes(_) |
            Func(_) => None,
        }
    }

    /// Applies the unary `!` operator to this value or returns `None` if that operation is not
    /// supported for this type
    pub fn not(self) -> Option<Self> {
        use Value::*;
        match self {
            Bool(value) => Some(Value::Bool(!value)),

            Unit |
            I64(_) |
            Bytes(_) |
            Func(_) => None,
        }
    }
}
