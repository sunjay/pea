mod alloc;

pub use alloc::sweep;

use std::fmt;
use std::ptr::NonNull;
use std::ops::Deref;

/// Every type that can be allocated on the GC must implement this trait
pub trait Trace {
    /// Called to trace any inner GC values within this type
    ///
    /// Call `gc::mark` with each field that is a GC type and then `trace` any fields that implement
    /// the `Trace` trait.
    fn trace(&self);
}

impl<T: Copy> Trace for T {
    // Copy types can't contain `Gc` types
    fn trace(&self) {}
}

/// Mark the given GC allocated value as still reachable. This will result in the allocation NOT
/// being collected during the next sweep. Any allocation that is not marked will be freed.
pub fn mark<T>(value: &Gc<T>) {
    // Safety: All Gc<T> values are allocated by `alloc` so this pointer should be valid
    unsafe { alloc::mark(value.ptr); }
}

/// A cloneable pointer into memory managed by the GC
///
/// The value will be freed at some point after the pointer is no longer being used
///
/// # Safety
///
/// Note that this value is only safe to use as long as the GC has not collected it. It is
/// unfortunately not possible to statically guarantee that this has not occurred. The user of this
/// type needs to ensure that the GC is always aware that this value is still being used.
pub struct Gc<T> {
    ptr: NonNull<T>,
}

impl<T: Trace> Gc<T> {
    /// Allocates memory managed by the GC and initializes it with the given value
    pub fn new(value: T) -> Self {
        Self {
            ptr: alloc::allocate(value),
        }
    }
}

impl<T> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // Safety: The garbage collector should not collect a pointer while it still exists
        unsafe { self.ptr.as_ref() }
    }
}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr,
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl<T: fmt::Display> fmt::Display for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&**self, f)
    }
}
