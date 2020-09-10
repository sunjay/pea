use std::fmt;

use super::FuncObj;

#[derive(Debug, Clone)]
pub enum Obj {
    Bytes(Vec<u8>),
    Func(FuncObj),
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

            Func(func) => write!(f, "{}", func),
        }
    }
}

impl Obj {
    pub fn unwrap_func_mut(&mut self) -> &mut FuncObj {
        use Obj::*;
        match self {
            Func(func) => func,
            _ => panic!("expected a func"),
        }
    }
}
