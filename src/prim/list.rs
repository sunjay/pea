use std::{fmt, iter};

use crate::{value::Value, gc};

/// A dynamically-sized list of values
#[derive(Debug, Default, PartialEq)]
pub struct List {
    items: Vec<Value>,
}

impl gc::Trace for List {
    fn trace(&self) {
        let Self {items} = self;
        items.trace();
    }
}

impl iter::FromIterator<Value> for List {
    fn from_iter<I: iter::IntoIterator<Item = Value>>(iter: I) -> Self {
        Self {
            items: Vec::from_iter(iter),
        }
    }
}

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;

        if let Some(first) = self.items.get(0) {
            fmt::Display::fmt(first, f)?;
            for value in &self.items[1..] {
                write!(f, ", ")?;
                fmt::Display::fmt(value, f)?;
            }
        }

        write!(f, "]")
    }
}
