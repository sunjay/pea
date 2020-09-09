use std::fmt;

/// The representation of a value in bytecode
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    I64(i64),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Value::*;
        match self {
            I64(value) => write!(f, "{}", value),
        }
    }
}
