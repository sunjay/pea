use std::fmt;

use crate::gc;

/// A growable ASCII byte string
#[derive(Debug, Default)]
pub struct Bytes(Vec<u8>);

impl From<&[u8]> for Bytes {
    fn from(value: &[u8]) -> Self {
        Bytes(Vec::from(value))
    }
}

impl fmt::Display for Bytes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // write out each byte as if it was ascii
        for &byte in &*self.0 {
            write!(f, "{}", byte as char)?;
        }
        Ok(())
    }
}

impl gc::Trace for Bytes {
    fn trace(&self) {
        // No need to trace a bunch of `u8` values
        let Bytes(_bytes) = self;
    }
}
