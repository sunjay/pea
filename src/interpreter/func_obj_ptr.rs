use std::fmt;
use std::ptr::NonNull;

use crate::value::FuncObj;

/// A shared pointer to a function designed for fast access from within the interpreter without
/// needing to go through multiple layers of indirection or locking.
pub struct FuncObjPtr {
    ptr: NonNull<FuncObj>,
}

impl<'a> From<&'a mut FuncObj> for FuncObjPtr {
    fn from(obj: &'a mut FuncObj) -> Self {
        Self {
            ptr: obj.into(),
        }
    }
}

impl fmt::Debug for FuncObjPtr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<ptr>")
    }
}

impl FuncObjPtr {
    /// Dereferences and retrieves the stored `FuncObj`
    ///
    /// # Safety
    ///
    /// See `Safety` section in: https://doc.rust-lang.org/beta/std/ptr/struct.NonNull.html#method.as_ref
    pub unsafe fn get_unchecked(&self) -> &FuncObj {
        self.ptr.as_ref()
    }
}
