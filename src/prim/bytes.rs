use std::fmt;

use crate::gc::{self, Gc};

/// A growable ASCII byte string
#[derive(Debug)]
pub struct Bytes(Gc<[u8]>);

impl Default for Bytes {
    fn default() -> Self {
        Bytes(Gc::from(&[] as &[_]))
    }
}

impl From<&[u8]> for Bytes {
    fn from(value: &[u8]) -> Self {
        Bytes(Gc::from(value))
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
        let Bytes(bytes) = self;
        bytes.trace();
    }
}
