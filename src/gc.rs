mod alloc;

pub use alloc::sweep;

use std::fmt;
use std::ptr::NonNull;
use std::ops::Deref;

#[cfg(test)]
static GC_TEST_LOCK: parking_lot::Mutex<()> = parking_lot::Mutex::const_new(<parking_lot::RawMutex as parking_lot::lock_api::RawMutex>::INIT, ());

/// Every type that can be allocated on the GC must implement this trait
pub trait Trace {
    /// Called to trace any inner GC values within this type
    ///
    /// Call `trace` any fields that implement the `Trace` trait. Calling `trace` on a `Gc<T>` type
    /// will call `gc::mark` on that value.
    fn trace(&self);
}

impl<T: Copy> Trace for T {
    // Copy types can't contain `Gc` types
    fn trace(&self) {}
}

impl<T: Trace> Trace for [T] {
    fn trace(&self) {
        for item in self {
            item.trace();
        }
    }
}

/// Mark the given GC allocated value as still reachable. This will result in the allocation NOT
/// being collected during the next sweep. Any allocation that is not marked will be freed.
pub fn mark<T: ?Sized>(value: &Gc<T>) {
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
pub struct Gc<T: ?Sized> {
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

impl<'a, T: Trace + Clone> From<&'a [T]> for Gc<[T]> {
    fn from(slice: &'a [T]) -> Self {
        Self {
            ptr: alloc::allocate_array(slice.iter().cloned()),
        }
    }
}

impl<T: ?Sized + Trace> Trace for Gc<T> {
    fn trace(&self) {
        mark(self);

        (**self).trace();
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

#[cfg(test)]
mod tests {
    use super::*;

    use std::sync::{Arc, atomic::{AtomicU8, Ordering}};

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
}
