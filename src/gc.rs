use std::fmt;
use std::ptr::NonNull;
use std::ops::{Deref, DerefMut};

pub struct GcGuard<'a, T: ?Sized> {
    value: &'a mut T,
}

impl<'a, T: ?Sized> Deref for GcGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.value
    }
}

impl<'a, T: ?Sized> DerefMut for GcGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.value
    }
}

/// A cloneable pointer into memory managed by the GC
///
/// The value will be freed at some point after the pointer is no longer being used
pub struct Gc<T: ?Sized> {
    ptr: NonNull<T>,
}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr,
        }
    }
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.try_lock() {
            Some(guard) => f.debug_tuple("Gc").field(&&*guard).finish(),
            None => {
                struct LockedPlaceholder;
                impl fmt::Debug for LockedPlaceholder {
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                        f.write_str("<locked>")
                    }
                }

                f.debug_tuple("Gc")
                    .field(&LockedPlaceholder)
                    .finish()
            }
        }
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

impl<T: ?Sized> Gc<T> {
    /// Locks the value or blocks the thread if it is currently locked
    pub fn lock(&self) -> GcGuard<T> {
        GcGuard {
            //TODO: This is wildly unsound because we aren't actually guaranteeing the mutual
            // exclusion required to create a &mut T.
            value: unsafe { &mut *self.ptr.as_ptr() },
        }
    }

    /// Attempts to lock the value or returns None if the operation would block
    pub fn try_lock(&self) -> Option<GcGuard<T>> {
        Some(self.lock()) //TODO
    }
}
