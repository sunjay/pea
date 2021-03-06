use std::fmt;

use crate::{gc::{self, Gc}, value::DeepClone};

/// A growable ASCII byte string
#[derive(Debug, PartialEq, Eq)]
pub struct Bytes(Gc<[u8]>);

impl Default for Bytes {
    fn default() -> Self {
        Bytes(Gc::from(&[][..]))
    }
}

impl From<&[u8]> for Bytes {
    fn from(value: &[u8]) -> Self {
        Bytes(value.into())
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

impl DeepClone for Bytes {
    fn deep_clone(&self) -> Self {
        Bytes(self.0.deep_clone())
    }
}

impl Bytes {
    pub fn add(&self, other: &Self) -> Self {
        Bytes(self.0.iter().copied().chain(other.0.iter().copied()).collect())
    }
}
