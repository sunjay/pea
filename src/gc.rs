mod trace;
mod alloc;

pub use trace::*;
pub use alloc::{sweep, needs_collect};

use std::fmt;
use std::iter;
use std::ptr::NonNull;
use std::ops::Deref;

/// This is used to allow GC tests to pretend they are the only thing using the GC at any given time
#[cfg(test)]
static GC_TEST_LOCK: parking_lot::Mutex<()> = parking_lot::const_mutex(());

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

#[cfg(test)]
mod tests {
    use super::*;

    use std::sync::{Arc, atomic::{AtomicU8, Ordering}};

    #[test]
    fn gc_str() {
        let _lock = GC_TEST_LOCK.lock();

        let value = Gc::from("abc123 woooo");
        assert_eq!(&*value, "abc123 woooo");

        // Should not clean up anything because we've traced
        value.trace();
        sweep();

        // Value should still be the same
        assert_eq!(&*value, "abc123 woooo");

        // Should clean up all memory at the end
        sweep();
    }

    #[test]
    fn gc_array() {
        let _lock = GC_TEST_LOCK.lock();

        let values1: &[Gc<u16>] = &[
            Gc::new(2),
            Gc::new(3),
            Gc::new(123),
            Gc::new(70),
            Gc::new(42),
        ];

        let values2: &[u16] = &[
            2,
            3,
            123,
            70,
            42,
        ];

        // These type annotations are unnecessary, but good for testing
        let array1: Gc<[Gc<u16>]> = Gc::from(values1);
        let array2: Gc<[u16]> = Gc::from(values2);

        // Check all values are as we expect
        assert_eq!(array1.len(), array2.len());
        for (a, b) in array1.iter().zip(array2.iter()) {
            assert_eq!(**a, *b);
        }

        array1.trace();
        array2.trace();

        // Should not clean up anything
        sweep();

        // Check all values are still as we expect
        assert_eq!(array1.len(), array2.len());
        for (a, b) in array1.iter().zip(array2.iter()) {
            assert_eq!(**a, *b);
        }

        // Should clean up all memory at the end
        sweep();
    }

    #[derive(Debug, Clone)]
    struct NeedsDrop {
        value: u8,
        counter: Arc<AtomicU8>,
    }

    impl Trace for NeedsDrop {
        fn trace(&self) {}
    }

    impl Drop for NeedsDrop {
        fn drop(&mut self) {
            self.counter.fetch_add(1, Ordering::SeqCst);
        }
    }

    #[test]
    fn gc_array_drop() {
        let _lock = GC_TEST_LOCK.lock();

        let counter = Arc::new(AtomicU8::new(0));

        let values1: &[NeedsDrop] = &[
            NeedsDrop {value: 1, counter: counter.clone()},
            NeedsDrop {value: 2, counter: counter.clone()},
            NeedsDrop {value: 3, counter: counter.clone()},
            NeedsDrop {value: 4, counter: counter.clone()},
            NeedsDrop {value: 5, counter: counter.clone()},
        ];

        let array1: Gc<[NeedsDrop]> = Gc::from(values1);

        assert_eq!(counter.load(Ordering::SeqCst), 0);
        assert_eq!(array1.len(), 5);
        for (a, b) in array1.iter().zip(1u8..) {
            assert_eq!(a.value, b);
        }

        // Mark the value to prevent it from being collected
        mark(&array1);

        // Should not clean up anything
        sweep();
        assert_eq!(counter.load(Ordering::SeqCst), 0);

        // Should clean up all the memory at the end and call drop
        sweep();
        assert_eq!(counter.load(Ordering::SeqCst), values1.len() as u8);
    }

    #[test]
    fn gc_zero_sized_types() {
        let _lock = GC_TEST_LOCK.lock();

        let value1 = Gc::new(());
        let value2 = Gc::from(&[] as &[i32]);

        // Should be able to access the value as normal
        assert_eq!(*value1, ());
        println!("{:?}", *value1);
        assert_eq!(*value2, []);
        println!("{:?}", &*value2);

        // Marking the value should work even though it is zero-sized
        mark(&value1);
        mark(&value2);

        // Should not clean up anything
        sweep();

        // Should be able to access the value as normal
        assert_eq!(*value1, ());
        println!("{:?}", *value1);
        assert_eq!(*value2, []);
        println!("{:?}", &*value2);

        // Should clean up all the memory at the end
        sweep();
    }

    #[test]
    fn gc_cycles() {
        let _lock = GC_TEST_LOCK.lock();

        // Source: https://doc.rust-lang.org/book/ch15-06-reference-cycles.html
        use parking_lot::Mutex;

        #[derive(Debug)]
        enum List {
            Cons(i32, Mutex<Gc<List>>),
            Nil,
        }

        use List::*;

        impl Trace for List {
            fn trace(&self) {
                use List::*;
                match self {
                    Cons(_, rest) => rest.lock().trace(),
                    Nil => {},
                }
            }
        }

        impl List {
            pub fn value(&self) -> Option<i32> {
                match *self {
                    Cons(value, _) => Some(value),
                    Nil => None,
                }
            }

            pub fn tail(&self) -> Option<&Mutex<Gc<List>>> {
                match self {
                    Cons(_, item) => Some(item),
                    Nil => None,
                }
            }
        }

        let a = Gc::new(Cons(5, Mutex::new(Gc::new(Nil))));
        let b = Gc::new(Cons(10, Mutex::new(Gc::clone(&a))));
        // Create reference cycle
        if let Some(link) = a.tail() {
            *link.lock() = Gc::clone(&b);
        }

        // Values should be as we initialized
        assert_eq!(a.value(), Some(5));
        assert_eq!(b.value(), Some(10));

        // Should not loop infinitely
        a.trace();

        // Values should still be the same
        assert_eq!(a.value(), Some(5));
        assert_eq!(b.value(), Some(10));

        // Should not clean up anything (since `trace` marks the values)
        sweep();

        // Values should still be the same
        assert_eq!(a.value(), Some(5));
        assert_eq!(b.value(), Some(10));

        // Should clean up all the memory at the end
        sweep();
    }
}
