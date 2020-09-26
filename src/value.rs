mod deep_clone;

pub use deep_clone::*;

use std::{fmt, mem};

use static_assertions::const_assert_eq;

use crate::{prim, gc::{Gc, Trace}, interpreter::RuntimeError};

/// The different supported types of values
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unit,
    Bool,
    I64,
    List,
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
            List => write!(f, "list"),
            Bytes => write!(f, "[u8]"),
            Func => write!(f, "fn"),
        }
    }
}

/// The representation of a value in bytecode
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// The `()` value
    Unit,
    Bool(bool),
    I64(i64),
    List(Gc<prim::List>),
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
            List(value) => value.trace(),
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
            List(value) => write!(f, "{}", value),
            Bytes(value) => write!(f, "{}", value),
            Func(value) => write!(f, "{}", value),
        }
    }
}

impl DeepClone for Value {
    fn deep_clone(&self) -> Self {
        use Value::*;
        match self {
            Unit => Unit,
            Bool(value) => Bool(value.deep_clone()),
            I64(value) => I64(value.deep_clone()),
            List(value) => List(value.deep_clone()),
            Bytes(value) => Bytes(value.deep_clone()),
            // Functions are immutable, so we can short-circuit the deep clone here
            Func(value) => Func(value.clone()),
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
            List(_) => Type::List,
            Bytes(_) => Type::Bytes,
            Func(_) => Type::Func,
        }
    }

    pub fn to_bool(&self) -> Option<bool> {
        use Value::*;
        match self {
            &Bool(value) => Some(value),
            _ => None,
        }
    }

    pub fn to_i64(&self) -> Option<i64> {
        use Value::*;
        match self {
            &I64(value) => Some(value),
            _ => None,
        }
    }

    pub fn unwrap_i64(&self) -> i64 {
        use Value::*;
        match self {
            &I64(value) => value,
            _ => panic!("expected an `i64` value"),
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
            List(_) |
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
            List(_) |
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
            List(_) |
            Bytes(_) |
            Func(_) => None,
        }
    }

    /// Applies the binary `+` operator to this value and the given other value or returns `None` if
    /// that operation is not supported for the types of values provided
    pub fn add(self, other: Self) -> Option<Self> {
        use Value::*;
        match (self, other) {
            (I64(value1), I64(value2)) => Some(Value::I64(value1 + value2)),
            (Bytes(value1), Bytes(value2)) => Some(Value::Bytes(Gc::new(value1.add(&value2)))),

            _ => None,
        }
    }

    /// Applies the binary `-` operator to this value and the given other value or returns `None` if
    /// that operation is not supported for the types of values provided
    pub fn sub(self, other: Self) -> Option<Self> {
        use Value::*;
        match (self, other) {
            (I64(value1), I64(value2)) => Some(Value::I64(value1 - value2)),

            _ => None,
        }
    }

    /// Applies the binary `*` operator to this value and the given other value or returns `None` if
    /// that operation is not supported for the types of values provided
    pub fn mul(self, other: Self) -> Option<Self> {
        use Value::*;
        match (self, other) {
            (I64(value1), I64(value2)) => Some(Value::I64(value1 * value2)),

            _ => None,
        }
    }

    /// Applies the binary `/` operator to this value and the given other value or returns `None` if
    /// that operation is not supported for the types of values provided
    pub fn div(self, other: Self) -> Option<Result<Self, RuntimeError>> {
        use Value::*;
        match (self, other) {
            (I64(value1), I64(value2)) => Some(match value2 {
                0 => Err(RuntimeError::DivideByZero),
                _ => Ok(Value::I64(value1 / value2)),
            }),

            _ => None,
        }
    }

    /// Applies the binary `%` operator to this value and the given other value or returns `None` if
    /// that operation is not supported for the types of values provided
    pub fn rem(self, other: Self) -> Option<Result<Self, RuntimeError>> {
        use Value::*;
        match (self, other) {
            (I64(value1), I64(value2)) => Some(match value2 {
                0 => Err(RuntimeError::RemainderByZero),
                _ => Ok(Value::I64(value1 % value2)),
            }),

            _ => None,
        }
    }

    /// Applies the binary `==` operator to this value and the given other value or returns `None`
    /// if that operation is not supported for the types of values provided
    pub fn equals_equals(self, other: Self) -> Option<Self> {
        use Value::*;
        match (self, other) {
            (Unit, Unit) => Some(Value::Bool(true)),
            (Bool(value1), Bool(value2)) => Some(Value::Bool(value1 == value2)),
            (I64(value1), I64(value2)) => Some(Value::Bool(value1 == value2)),
            (Bytes(value1), Bytes(value2)) => Some(Value::Bool(value1 == value2)),
            (Func(value1), Func(value2)) => Some(Value::Bool(value1 == value2)),

            _ => None,
        }
    }

    /// Applies the binary `!=` operator to this value and the given other value or returns `None`
    /// if that operation is not supported for the types of values provided
    pub fn not_equals(self, other: Self) -> Option<Self> {
        use Value::*;
        match (self, other) {
            (Unit, Unit) => Some(Value::Bool(false)),
            (Bool(value1), Bool(value2)) => Some(Value::Bool(value1 != value2)),
            (I64(value1), I64(value2)) => Some(Value::Bool(value1 != value2)),
            (Bytes(value1), Bytes(value2)) => Some(Value::Bool(value1 != value2)),
            (Func(value1), Func(value2)) => Some(Value::Bool(value1 != value2)),

            _ => None,
        }
    }

    /// Applies the binary `>` operator to this value and the given other value or returns `None`
    /// if that operation is not supported for the types of values provided
    pub fn greater_than(self, other: Self) -> Option<Self> {
        use Value::*;
        match (self, other) {
            (I64(value1), I64(value2)) => Some(Value::Bool(value1 > value2)),

            _ => None,
        }
    }

    /// Applies the binary `>=` operator to this value and the given other value or returns `None`
    /// if that operation is not supported for the types of values provided
    pub fn greater_than_equals(self, other: Self) -> Option<Self> {
        use Value::*;
        match (self, other) {
            (I64(value1), I64(value2)) => Some(Value::Bool(value1 >= value2)),

            _ => None,
        }
    }

    /// Applies the binary `<` operator to this value and the given other value or returns `None`
    /// if that operation is not supported for the types of values provided
    pub fn less_than(self, other: Self) -> Option<Self> {
        use Value::*;
        match (self, other) {
            (I64(value1), I64(value2)) => Some(Value::Bool(value1 < value2)),

            _ => None,
        }
    }

    /// Applies the binary `<=` operator to this value and the given other value or returns `None`
    /// if that operation is not supported for the types of values provided
    pub fn less_than_equals(self, other: Self) -> Option<Self> {
        use Value::*;
        match (self, other) {
            (I64(value1), I64(value2)) => Some(Value::Bool(value1 <= value2)),

            _ => None,
        }
    }
}
