use std::fmt;

/// A growable ASCII byte string
#[derive(Debug)]
pub struct Bytes(Vec<u8>);

impl fmt::Display for Bytes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // write out each byte as if it was ascii
        for &byte in &self.0 {
            write!(f, "{}", byte as char)?;
        }
        Ok(())
    }
}
