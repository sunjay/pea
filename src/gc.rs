mod alloc;

pub use alloc::sweep;

use std::fmt;
use std::ptr::NonNull;
use std::ops::Deref;
use std::hint::unreachable_unchecked;

use parking_lot::Mutex;

use crate::prim;

use alloc::GcEntryPtr;

/// Declares a constructor for all types that can be allocated by the garbage collector
macro_rules! gc_types {
    (
        enum $name:ident {
            $( $variant:ident ($typ:ty) ),* $(,)?
        }
    ) => {
        /// All of the types that can be allocated by the garbage collector. Having this allows us
        /// to take a `Gc<T>` pointer and reconstruct a type that Rust can successfully drop and
        /// deallocate.
        enum $name {
            $( $variant (GcEntryPtr<$typ>) ),*
        }

        $(
            impl From<$typ> for Gc<$typ> {
                fn from(value: $typ) -> Self {
                    // Allocate a variant of the enum
                    let (entry, mut value) = alloc::allocate($name::$variant(value.into()));

                    // Safety: We just allocated this pointer, so it should be valid. Nothing else
                    // can be aliasing it since we just allocated it.
                    let ptr = match unsafe { value.as_mut() } {
                        // Return a pointer to the value of the variant we allocated
                        $name::$variant(variant) => {
                            variant.entry = entry;
                            (&variant.value).into()
                        },

                        // Safety: We just constructed this variant, so it should still be the same
                        // variant after the allocation
                        _ => unsafe { unreachable_unchecked() },
                    };

                    Self {ptr}
                }
            }
        )*
    };
}

gc_types! {
    enum GcValue {
        Bytes(Mutex<prim::Bytes>),
        Func(prim::Func),
    }
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
/// unfortunately not possible to guarantee that this has not occurred. The user of this type needs
/// to ensure that the GC is always aware that this value is still being used.
pub struct Gc<T> {
    ptr: NonNull<T>,
}

impl<T> Gc<T> {
    /// Create a new `Gc<T>` value from any of the supported types
    ///
    /// This method is equivalent to calling `Gc::from` with the given value.
    pub fn new(value: T) -> Self
        where Gc<T>: From<T>,
    {
        Gc::from(value)
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

#[cfg(test)]
mod tests {
    use super::*;

    use crate::prim;

    // unsafe test helper for getting the value from a pointer
    macro_rules! assert_value_eq {
        ($value:expr, $expected:expr) => {
            assert_eq!(&*$value.lock().0, $expected);
        };
    }

    fn bytes(value: &[u8]) -> Gc<Mutex<prim::Bytes>> {
        Gc::new(Mutex::new(prim::Bytes(value.to_vec())))
    }

    #[test]
    fn gc_alloc() {
        let b1 = bytes(b"a");
        let b2 = bytes(b"b");
        let b3 = bytes(b"c");
        let b4 = bytes(b"123901");
        let b5 = bytes(b"sdfaioh2109");

        // Check that all the values are as we expect
        assert_value_eq!(b1, b"a");
        assert_value_eq!(b2, b"b");
        assert_value_eq!(b3, b"c");
        assert_value_eq!(b4, b"123901");
        assert_value_eq!(b5, b"sdfaioh2109");

        // Make sure we don't free marked pointers
        mark(&b2);
        mark(&b3);

        sweep();

        // Unsweeped pointers should keep their values
        assert_value_eq!(b2, b"b");
        assert_value_eq!(b3, b"c");

        // Should clean up all memory at the end
        sweep();

        //TODO: assert_eq!(ALLOC_LIST.load(Ordering::SeqCst), ptr::null_mut());
    }
}
