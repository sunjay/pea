mod trace;
mod alloc;

pub use trace::*;
pub use alloc::{sweep, needs_collect};

use std::fmt;
use std::iter;
use std::ptr::NonNull;
use std::ops::Deref;

/// Mark the given GC allocated value as still reachable. This will result in the allocation NOT
/// being collected during the next sweep. Any allocation that is not marked will be freed.
///
/// Returns true if the value was already marked previously.
///
/// # Safety
///
/// This function should not be called concurrently with `sweep`.
pub fn mark<T: ?Sized>(value: &Gc<T>) -> bool {
    // Safety: All Gc<T> values are allocated by `alloc` so this pointer should be valid
    unsafe { alloc::mark(value.ptr) }
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
pub struct Gc<T: ?Sized> {
    ptr: NonNull<T>,
}

unsafe impl<T: ?Sized + Sync + Send> Send for Gc<T> {}
unsafe impl<T: ?Sized + Sync + Send> Sync for Gc<T> {}

impl<T: Trace> Gc<T> {
    /// Allocates memory managed by the GC and initializes it with the given value
    #[inline]
    pub fn new(value: T) -> Self {
        Self {
            ptr: alloc::allocate(value),
        }
    }
}

impl<T: ?Sized> Gc<T> {
    /// Returns `true` if the two `Gc` values point to the same allocation
    /// (in a vein similar to [`ptr::eq`]).
    ///
    /// [`ptr::eq`]: core::ptr::eq
    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
        this.ptr.as_ptr() == other.ptr.as_ptr()
    }
}

impl<T: Trace> iter::FromIterator<T> for Gc<[T]> {
    fn from_iter<I: iter::IntoIterator<Item = T>>(iter: I) -> Self {
        let items: Vec<T> = Vec::from_iter(iter);
        items.into()
    }
}

impl<T: Trace> From<T> for Gc<T> {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

impl<'a, T: Trace + Clone> From<&'a [T]> for Gc<[T]> {
    #[inline]
    fn from(slice: &'a [T]) -> Self {
        Self {
            ptr: alloc::allocate_array(slice.iter().cloned()),
        }
    }
}

impl<T: Trace> From<Vec<T>> for Gc<[T]> {
    #[inline]
    fn from(items: Vec<T>) -> Self {
        Self {
            ptr: alloc::allocate_array(items.into_iter()),
        }
    }
}

// This code is pretty much the same as the impl for Arc<str>
impl From<&str> for Gc<str> {
    #[inline]
    fn from(value: &str) -> Self {
        let Gc {ptr} = Gc::<[u8]>::from(value.as_bytes());
        let ptr = unsafe { NonNull::new_unchecked(ptr.as_ptr() as *mut str) };

        Self {ptr}
    }
}

// This code is pretty much the same as the impl for Arc<str>
impl From<String> for Gc<str> {
    #[inline]
    fn from(value: String) -> Self {
        Gc::from(&value[..])
    }
}

impl From<&std::sync::Arc<str>> for Gc<str> {
    #[inline]
    fn from(value: &std::sync::Arc<str>) -> Self {
        (&**value).into()
    }
}

impl From<std::sync::Arc<str>> for Gc<str> {
    #[inline]
    fn from(value: std::sync::Arc<str>) -> Self {
        (&*value).into()
    }
}

impl<T: ?Sized + Trace> Trace for Gc<T> {
    fn trace(&self) {
        // Avoid reference cycles by only tracing values that weren't previously marked
        if !mark(self) {
            (**self).trace();
        }
    }
}

impl<T: ?Sized> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // Safety: The garbage collector should not collect a pointer while it still exists
        unsafe { self.ptr.as_ref() }
    }
}

impl<T: ?Sized> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr,
        }
    }
}

impl<T: Trace + Default> Default for Gc<T> {
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl<T: ?Sized + fmt::Display> fmt::Display for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&**self, f)
    }
}

impl<T: ?Sized> fmt::Pointer for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Pointer::fmt(&self.ptr, f)
    }
}

impl<T: ?Sized + PartialEq> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        Self::ptr_eq(self, other) || (**self).eq(&**other)
    }
}

impl<T: ?Sized + Eq> Eq for Gc<T> {}

impl<T: ?Sized + PartialOrd> PartialOrd for Gc<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (**self).partial_cmp(&**other)
    }
}

impl<T: ?Sized + Ord> Ord for Gc<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (**self).cmp(&**other)
    }
}

impl<T: ?Sized + std::borrow::Borrow<T>> std::borrow::Borrow<T> for Gc<T> {
    fn borrow(&self) -> &T {
        &**self
    }
}

impl<T: ?Sized> AsRef<T> for Gc<T> {
    fn as_ref(&self) -> &T {
        &**self
    }
}

impl<T: ?Sized> Unpin for Gc<T> {}

impl<T: ?Sized + std::hash::Hash> std::hash::Hash for Gc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (**self).hash(state)
    }
}
