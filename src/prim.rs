mod bytes;
mod func;

pub use bytes::*;
pub use func::*;

use std::fmt;

use parking_lot::Mutex;

#[derive(Debug)]
pub enum Prim {
    Bytes(Mutex<Bytes>),
    Func(Func),
}

impl fmt::Display for Prim {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Prim::*;
        match self {
            Bytes(prim) => fmt::Display::fmt(&*prim.lock(), f),
            Func(prim) => fmt::Display::fmt(prim, f),
        }
    }
}
