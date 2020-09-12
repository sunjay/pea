use std::fmt;
use std::ptr::NonNull;

use crate::prim;

/// A read-only shared pointer to a function designed for fast access from within the interpreter
/// without needing to go through multiple layers of indirection.
pub struct FuncPtr {
    ptr: NonNull<prim::Func>,
}

impl<'a> From<&'a prim::Func> for FuncPtr {
    fn from(obj: &'a prim::Func) -> Self {
        Self {
            ptr: obj.into(),
        }
    }
}

impl fmt::Debug for FuncPtr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<ptr>")
    }
}

impl FuncPtr {
    /// Dereferences and retrieves the stored `prim::Func`
    ///
    /// # Safety
    ///
    /// The calling code must guarantee that the garbage collector has not freed this function yet.
    ///
    /// See `Safety` section in: https://doc.rust-lang.org/beta/std/ptr/struct.NonNull.html#method.as_ref
    pub unsafe fn get_unchecked(&self) -> &prim::Func {
        self.ptr.as_ref()
    }
}
