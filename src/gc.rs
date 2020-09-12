mod alloc;

pub use alloc::*;

use std::fmt;
use std::ptr::NonNull;
use std::ops::Deref;

use crate::prim::Prim;

/// Mark the given GC allocated primitive as still reachable. This will result in the allocation NOT
/// being collected during the next sweep. Any allocation that is not marked will be freed.
pub fn mark(prim: GcPrim) {
    // Safety: All GcPrim values are allocated by `alloc` so this pointer should be valid
    unsafe { alloc::mark(prim.ptr); }
}

/// A cloneable pointer into memory managed by the GC
///
/// The value will be freed at some point after the pointer is no longer being used
///
/// # Safety
///
/// Note that this value is only safe to use as long as the GC has not collected it. It is
/// unfortunately not possible to guarantee that this has not occurred. The user of this type needs
/// to ensure that the GC is always aware that this value is still being used.
pub struct GcPrim {
    ptr: NonNull<Prim>,
}

impl Deref for GcPrim {
    type Target = Prim;

    fn deref(&self) -> &Self::Target {
        // Safety: The garbage collector should not collect a pointer while it still exists
        unsafe { self.ptr.as_ref() }
    }
}

impl Clone for GcPrim {
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr,
        }
    }
}

impl fmt::Debug for GcPrim {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl GcPrim {
    pub fn new(value: Prim) -> Self {
        Self {
            ptr: alloc(value),
        }
    }
}
