use std::fmt;
use std::ptr::NonNull;
use std::ops::Deref;

/// A cloneable pointer into memory managed by the GC
///
/// The value will be freed at some point after the pointer is no longer being used
///
/// # Safety
///
/// Note that this value is only safe to use as long as the GC has not collected it. It is
/// unfortunately not possible to guarantee that this has not occurred. The user of this type needs
/// to ensure that the GC is always aware that this value is still being used.
pub struct Gc<T> {
    ptr: NonNull<T>,
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

impl<T> Gc<T> {
    pub fn new(value: T) -> Self {
        Self {
            //TODO: Use garbage collector
            ptr: Box::leak(Box::new(value)).into(),
        }
    }
}
