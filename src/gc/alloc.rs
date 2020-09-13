//! Allocation, deallocation, and heap traversal functions.
//!
//! The garbage collector does not currently implement its own heap. We use the default allocator
//! provided by the standard library.

use std::ptr::{self, NonNull};
use std::sync::atomic::{AtomicPtr, Ordering};
use std::alloc::{alloc, dealloc, handle_alloc_error, Layout};

/// A linked list of allocations managed by the GC
static ALLOC_LIST: AtomicPtr<GcHeader> = AtomicPtr::new(ptr::null_mut());

/// A single heap allocated entry managed by the GC
#[repr(C)]
pub(in super) struct GcEntry<T> {
    /// A header containing extra metadata needed by the GC
    ///
    /// Due to the `repr(C)` attribute, the address of this header is the same as the address
    /// returned by the heap allocation. This allows headers to be operated on independently from
    /// the type T.
    header: GcHeader,
    /// The actual allocated value
    value: T,
}

#[derive(Debug)]
struct GcHeader {
    /// The layout used to allocate this memory (used to be able to free the memory even when the
    /// type info has been erased)
    layout: Layout,
    /// if true, the allocation is still reachable from a root
    is_reachable: bool,
    /// The address of the previous allocation (makes this an intrusive list node)
    next: *mut GcHeader,
}

impl GcHeader {
    fn new(layout: Layout) -> Self {
        Self {
            layout,
            // Presume not reachable until proven otherwise
            is_reachable: false,
            next: ptr::null_mut(),
        }
    }
}

/// Allocate memory managed by the GC and initialize it to the given value
///
/// Returns a non-null pointer to the value
pub(in super) fn allocate<T>(value: T) -> NonNull<T> {
    // Allocate memory for an entry holding a value of the given type
    let layout = Layout::new::<GcEntry<T>>();

    // Safety: alloc requires non-zero sized types. None of our GC'd types are zero-sized.
    // Using debug assert because this is performance critical code and this check is just in case.
    debug_assert_ne!(layout.size(), 0, "bug: GC does not support zero-sized types");
    let entry_ptr = unsafe { alloc(layout) } as *mut GcEntry<T>;

    // Check for allocation failure
    if entry_ptr.is_null() {
        handle_alloc_error(layout);
    }

    // Initialize entry memory without dereferencing uninitialized value
    let header = GcHeader::new(layout);
    unsafe { entry_ptr.write(GcEntry {header, value}); }

    // Safety: We just initialized this pointer, so it should be valid
    let mut entry = unsafe { &mut *entry_ptr };
    let header_ptr = &mut entry.header as *mut GcHeader;
    let value_ptr = (&entry.value).into();

    // Append onto the list of all allocations
    //
    // To avoid the ABA problem, we initially create the header with a null pointer for `next`, then
    // we swap the current ALLOC_LIST pointer with the pointer to this entry in a single operation.
    let next = ALLOC_LIST.swap(header_ptr, Ordering::SeqCst);
    entry.header.next = next;

    value_ptr
}

/// Marks a value managed by the GC as reachable
///
/// # Safety
///
/// This function may only be used with pointers returned from `allocate`.
pub(in super) unsafe fn mark<T>(ptr: NonNull<T>) {
    // Safety: Since `GcEntry` is #[repr(C)], the fields are laid out with `GcHeader` before `T`.
    // That means that we *should* be able to just subtract the size of `GcHeader` to get to the
    // `header` field from the `value` field. Note: sub(1) == -(sizeof(GcHeader)*1).
    //
    //TODO: Not actually sure if this offset will work. A more implementation independent way would
    // be to construct a fake `GcEntry` and then find out the actual offset between the `header` and
    // `value` fields.
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
    // Safety: We know that *mut GcHeader is the same as the address returned by the allocator
    // because #[repr(C)] on GcEntry guarantees that that field is at the start of the struct.
    let entry_ptr = ptr.as_ptr() as *mut u8;

    // Use the layout stored in the header to free using the correct size and alignment (avoids UB).
    let header = ptr.as_ref();
    let layout = header.layout;

    // Remove this entry from the ALLOC_LIST linked list
    if let Some(mut prev) = prev {
        prev.as_mut().next = header.next;

    // No previous, must be the head of the list
    } else {
        // Set a new head for the list
        //
        // NOTE: This line is not robust and will not work if run concurrently with `alloc`.
        ALLOC_LIST.compare_and_swap(ptr.as_ptr(), header.next, Ordering::SeqCst);
    }

    // Free the memory
    dealloc(entry_ptr, layout);
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
///
/// # Safety
///
/// This function currently assumes that the program is frozen while collection is running. That is,
/// no other calls to any GC APIs may take place while this function is running.
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

    // unsafe test helper for getting the value from a pointer
    fn get<T>(ptr: &NonNull<T>) -> &T {
        unsafe { ptr.as_ref() }
    }

    #[test]
    fn gc_alloc() {
        let ptr1 = allocate(2i8);
        let ptr2 = allocate(42i16);
        let ptr3 = allocate(-33i32);
        let ptr4 = allocate(54i64);
        let ptr5 = allocate(-12931i128);

        // Check that all the values are as we expect
        assert_eq!(*get(&ptr1), 2);
        assert_eq!(*get(&ptr2), 42);
        assert_eq!(*get(&ptr3), -33);
        assert_eq!(*get(&ptr4), 54);
        assert_eq!(*get(&ptr5), -12931);

        // Make sure we don't free marked pointers
        unsafe { mark(ptr2) };
        unsafe { mark(ptr3) };

        sweep();

        assert_eq!(*get(&ptr2), 42);
        assert_eq!(*get(&ptr3), -33);

        // Should clean up all memory at the end
        sweep();

        assert_eq!(ALLOC_LIST.load(Ordering::SeqCst), ptr::null_mut());
    }
}
