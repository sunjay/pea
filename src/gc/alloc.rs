use std::ptr::{self, NonNull};
use std::sync::atomic::{AtomicPtr, Ordering};

use crate::prim::Prim;

/// A linked list of allocations managed by the GC
static ALLOC_LIST: AtomicPtr<GcHeader> = AtomicPtr::new(ptr::null_mut());

/// A single heap allocated entry managed by the GC
#[repr(C)]
struct GcEntry {
    /// A header containing extra metadata needed by the GC
    ///
    /// Due to the `repr(C)` attribute, the address of this header is the same as the address
    /// returned by the heap allocation. This allows headers to be operated on independently from
    /// the type T.
    header: GcHeader,
    /// The actual allocated value
    value: Prim,
}

#[derive(Debug)]
struct GcHeader {
    /// if true, the allocation is still reachable from a root
    is_reachable: bool,
    /// The address of the previous allocation (makes this an intrusive list node)
    next: *mut GcHeader,
}

impl Default for GcHeader {
    fn default() -> Self {
        Self {
            // Presume not reachable until proven otherwise
            is_reachable: false,
            next: ptr::null_mut(),
        }
    }
}

/// Allocate memory managed by the GC and initialize it to the given value
///
/// Returns a non-null pointer to the value
pub(in super) fn alloc(value: Prim) -> NonNull<Prim> {
    // To avoid the ABA problem, create the header with a null pointer first, then swap the next
    // ALLOC_LIST pointer with the pointer to this entry in a single operation.
    let mut entry = Box::new(GcEntry {
        header: GcHeader::default(),
        value,
    });
    let header_ptr = &mut entry.header as *mut GcHeader;
    let value_ptr = (&entry.value).into();

    // Append onto the list of all allocations
    let next = ALLOC_LIST.swap(header_ptr, Ordering::SeqCst);
    entry.header.next = next;

    // Make sure the entry is not dropped at the end of this function
    Box::leak(entry);

    value_ptr
}

/// Marks a value managed by the GC as reachable
///
/// # Safety
///
/// This function may only be used with pointers returned from `alloc`.
pub(in super) unsafe fn mark(ptr: NonNull<Prim>) {
    // Safety: Since `GcEntry` is #[repr(C)], the fields are laid out with `GcHeader` before `Prim`.
    // That means that we *should* be able to just subtract the size of `GcHeader` to get to that
    // field. Note: sub(1) == -(sizeof(Prim)*1).
    //
    //TODO: Not actually sure if this offset will work. Probably a safer way would be to construct a
    // `GcEntry` and then find out the actual offset between the `header` and `value` fields.
    let header_ptr = (ptr.as_ptr() as *mut GcHeader).sub(1);
    let mut header = &mut *header_ptr;
    header.is_reachable = true;
}

/// Frees the memory associated with the given allocation
///
/// # Safety
///
/// This function may only be used with pointers in `ALLOC_LIST`. Do not call this concurrently with
/// `alloc`.
unsafe fn free(ptr: NonNull<GcHeader>, prev: Option<NonNull<GcHeader>>) {
    // Reconstruct the box that this pointer came from and free the value at the end of this scope
    //
    // Safety: This cast only works because #[repr(C)] guarantees that GcHeader is the first field
    // of the struct and therefore a pointer to that field is the same the pointer to the entire
    // struct.
    let entry = Box::from_raw(ptr.as_ptr() as *mut GcEntry);
    if let Some(mut prev) = prev {
        prev.as_mut().next = entry.header.next;

    // No previous, must be the head of the list
    } else {
        // Set a new head for the list
        //
        // NOTE: This line is not robust and will not work if run concurrently with `alloc`.
        ALLOC_LIST.compare_and_swap(ptr.as_ptr(), entry.header.next, Ordering::SeqCst);
    }
}

struct Traverse {
    next: *mut GcHeader,
}

impl Iterator for Traverse {
    type Item = NonNull<GcHeader>;

    fn next(&mut self) -> Option<Self::Item> {
        match NonNull::new(self.next) {
            Some(ptr) => {
                // Safety: the pointers in ALLOC_LIST should be valid
                let header = unsafe { ptr.as_ref() };
                self.next = header.next;

                Some(ptr)
            },
            None => None,
        }
    }
}

/// Traverses all allocations managed by the GC
fn traverse() -> impl Iterator<Item=NonNull<GcHeader>> {
    Traverse {
        next: ALLOC_LIST.load(Ordering::SeqCst),
    }
}

/// Sweeps/collects all allocations that have been not been marked as reachable
pub fn sweep() {
    let mut prev = None;
    for mut header in traverse() {
        // We need to be careful to keep the lifetime of &GcHeader short so we don't break the
        // aliasing rules when `free` takes ownership of the data.
        let is_reachable = {
            // Safety: the header should be a valid pointer or it would not be in the list.
            let header = unsafe { header.as_mut() };
            let is_reachable = header.is_reachable;

            // Reset `is_reachable` for next collection
            header.is_reachable = false;

            is_reachable
        };

        if is_reachable {
            // Only update prev with the allocations we aren't freeing
            prev = Some(header);
            continue;
        }

        // Safety: pointers generated by `traverse` are guaranteed to be from ALLOC_LIST
        unsafe { free(header, prev); }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::prim;

    // unsafe test helper for getting the value from a pointer
    macro_rules! assert_value_eq {
        ($ptr:expr, $expected:expr) => {
            match unsafe { $ptr.as_ref() } {
                Prim::Bytes(bytes) => {
                    assert_eq!(&*bytes.lock().0, $expected);
                },
                _ => unsafe { std::hint::unreachable_unchecked() },
            }
        };
    }

    fn bytes(value: &[u8]) -> Prim {
        Prim::Bytes(prim::Bytes(value.to_vec()).into())
    }

    #[test]
    fn gc_alloc() {
        let ptr1 = alloc(bytes(b"a"));
        let ptr2 = alloc(bytes(b"b"));
        let ptr3 = alloc(bytes(b"c"));
        let ptr4 = alloc(bytes(b"123901"));
        let ptr5 = alloc(bytes(b"sdfaioh2109"));

        // Check that all the values are as we expect
        assert_value_eq!(ptr1, b"a");
        assert_value_eq!(ptr2, b"b");
        assert_value_eq!(ptr3, b"c");
        assert_value_eq!(ptr4, b"123901");
        assert_value_eq!(ptr5, b"sdfaioh2109");

        // Make sure we don't free marked pointers
        unsafe { mark(ptr2) };
        unsafe { mark(ptr3) };

        sweep();

        // Unsweeped pointers should keep their values
        assert_value_eq!(ptr2, b"b");
        assert_value_eq!(ptr3, b"c");

        // Should clean up all memory at the end
        sweep();

        assert_eq!(ALLOC_LIST.load(Ordering::SeqCst), ptr::null_mut());
    }
}
