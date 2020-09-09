use std::fmt;

#[derive(Debug, Clone)]
pub enum Obj {
    Bytes(Vec<u8>),
}

impl fmt::Display for Obj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Obj::*;
        match self {
            Bytes(bytes) => {
                // write out each byte as if it was ascii
                for &byte in bytes {
                    write!(f, "{}", byte as char)?;
                }
                Ok(())
            },
        }
    }
}
