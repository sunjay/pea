//! Allocation, deallocation, and heap traversal functions.
//!
//! The garbage collector does not currently implement its own heap. We use the default allocator
//! provided by the standard library.
//!
//! NOTE: This implementation is crazy unsafe. It relies on the unstable layout of trait objects and
//! assumes that the data pointer of a trait object is the same as the pointer that would have been
//! returned from `Box::new`. This may completely break and explode in the future if those details
//! change.

use std::mem;
use std::iter;
use std::ptr::{self, NonNull};
use std::sync::atomic::{AtomicPtr, Ordering};
use std::alloc::{alloc, dealloc, Layout, handle_alloc_error};

use super::Trace;

/// A linked list of allocations managed by the GC
static ALLOC_LIST: AtomicPtr<GcHeader> = AtomicPtr::new(ptr::null_mut());

/// Copied from: https://doc.rust-lang.org/std/raw/struct.TraitObject.html
//TODO: Will need to be updated in the future when the representation changes.
//  See: https://github.com/rust-lang/rust/issues/27751
#[repr(C)]
#[derive(Copy, Clone)]
pub struct TraitObject {
    pub data: *mut (),
    pub vtable: *mut (),
}

fn extract_vtable<T: Trace>(value: &T) -> *mut () {
    // Source: https://github.com/withoutboats/shifgrethor/blob/ab1873fc3e76e6b9fd9a96b11746617e14bd7c1c/src/lib/gc/alloc.rs#L110-L115
    unsafe {
        let obj = value as &dyn Trace;
        // Safety: This will continue to work as long as the unstable trait object layout does not change
        let TraitObject {vtable, ..} = mem::transmute(obj);
        vtable
    }
}

/// A single heap allocated entry managed by the GC
#[repr(C)]
pub(in super) struct GcEntry<T: ?Sized> {
    /// A header containing extra metadata needed by the GC
    ///
    /// Due to the `repr(C)` attribute, the address of this header is the same as the address
    /// returned by the heap allocation. This allows headers to be operated on independently from
    /// the type T.
    header: GcHeader,
    /// The actual allocated value
    value: T,
}

impl<T: ?Sized + Trace> Trace for GcEntry<T> {
    // GcEntry types are not traced, but they need to implement Trace so we can create a trait object
    fn trace(&self) {}
}

#[derive(Debug)]
struct GcHeader {
    /// The number of values allocated
    len: usize,
    /// The size of each allocated value
    size: usize,
    /// The pointer to the vtable for the type T stored in this allocation
    ///
    /// Optimization: Set to null if `T` does not need to be dropped
    vtable: *mut (),
    /// The layout used to allocate the entire `GcEntry<T>`
    layout: Layout,
    /// The address of the previous allocation (makes this an intrusive list node)
    next: *mut GcHeader,
    /// if true, the allocation is still reachable from a root
    is_reachable: bool,
}

impl GcHeader {
    fn array<T>(len: usize, vtable: *mut (), layout: Layout) -> Self {
        Self {
            len,
            size: mem::size_of::<T>(),
            vtable,
            layout,
            next: ptr::null_mut(),
            // Presume not reachable until proven otherwise
            is_reachable: false,
        }
    }
}

/// Allocate memory managed by the GC and initialize it to the given value
///
/// Returns a non-null pointer to the value
pub(in super) fn allocate_array<T, I>(values: I) -> NonNull<[T]>
    where T: Trace,
          I: ExactSizeIterator<Item=T>,
{
    // Start by generating the layout for the array, essentially `GcEntry<[T]>`
    let len = values.len();

    // Safety: The layout being generated here must match the layout of `GcEntry`
    let header_layout = Layout::new::<GcHeader>();
    let array_layout = Layout::array::<T>(len).expect("bug: failed to create array layout");
    let (layout, array_start_bytes) = header_layout.extend(array_layout).expect("bug: failed to create `GcEntry` layout");
    // Always need to finish a layout with `pad_to_align`
    let layout = layout.pad_to_align();

    debug_assert_eq!(array_start_bytes, mem::size_of::<GcHeader>(),
        "bug: `gc::mark` depends on the allocated value starting right after the header with no padding");

    // Allocate the `GcEntry<[T]>`
    // Safety: Due to the header, the layout passed to `alloc` cannot be zero-sized
    let entry_ptr = unsafe { alloc(layout) };
    // Check for allocation failure
    if entry_ptr.is_null() {
        handle_alloc_error(layout);
    }

    // Extract the vtable only if we have at least one element and that element needs to be dropped
    let mut values = values.peekable();
    let vtable = if len == 0 || !mem::needs_drop::<T>() {
        ptr::null_mut()
    } else {
        // This unwrap is fine because we just checked the length
        extract_vtable(values.peek().unwrap())
    };

    // Initialize the header (`header` field of `GcEntry<[T]>`)
    // Safety: #[repr(C)] guarantees that a pointer to `GcEntry` is the same as a pointer to
    // `GcHeader` (since `header` is the first field)
    let header_ptr = entry_ptr as *mut GcHeader;
    let header = GcHeader::array::<T>(len, vtable, layout);
    unsafe { header_ptr.write(header); }

    // Initialize the array (`value` field of `GcEntry<[T]>`)
    // Safety: Any offsets we take here must match the layouts used to allocate above
    let array = unsafe { entry_ptr.add(array_start_bytes) } as *mut T;
    for (i, value) in values.enumerate() {
        // Safety: We just allocated and checked `entry_ptr`, so this should work
        unsafe { array.add(i).write(value); }
    }

    // Safety: We initialized the `array` pointer earlier so it should still allow us to make a
    // valid slice with the given length. We already checked if the original `entry_ptr` was null.
    let value_ptr = unsafe { NonNull::new_unchecked(ptr::slice_from_raw_parts_mut(array, len)) };

    // Append onto the list of all allocations
    //
    // To avoid the ABA problem, we initially create the header with a null pointer for `next`, then
    // we swap the current ALLOC_LIST pointer with the pointer to this entry in a single operation.
    let next = ALLOC_LIST.swap(header_ptr, Ordering::SeqCst);
    // Safety: The header was initialized above, so it should still be valid to assign to
    unsafe { (*header_ptr).next = next; }

    value_ptr
}

/// Allocate memory managed by the GC and initialize it to the given value
///
/// Returns a non-null pointer to the value
pub(in super) fn allocate<T: Trace>(value: T) -> NonNull<T> {
    // Allocate the value as an array of one value
    allocate_array(iter::once(value)).cast()
}

/// Marks a value managed by the GC as reachable
///
/// # Safety
///
/// This function may only be used with pointers returned from `allocate`.
pub(in super) unsafe fn mark<T: ?Sized>(ptr: NonNull<T>) {
    // Safety: Since `GcEntry` is #[repr(C)], the fields are laid out with `GcHeader` before `T`.
    // That means that we *should* be able to just subtract the size of `GcHeader` to get to the
    // `header` field from the `value` field. Note: sub(1) == -(sizeof(GcHeader)*1).
    //
    //TODO: Not actually sure if this offset will always work. A more implementation independent way
    //  would be to construct a fake `GcEntry` and then find out the actual offset between the
    //  `header` and `value` fields.
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
    // Extract info from the header
    // Safety: The pointer should still be valid if it is currently in `ALLOC_LIST`. Note that in
    // order to avoid aliasing issues we are careful to copy the values out (avoids references).
    let GcHeader {len, size, vtable, layout, next, ..} = *ptr.as_ref();

    // Remove this entry from the ALLOC_LIST linked list
    if let Some(mut prev) = prev {
        prev.as_mut().next = next;

    // No previous, must be the head of the list
    } else {
        // Set a new head for the list
        //
        // NOTE: This line is not robust and will not work if run concurrently with `alloc`.
        ALLOC_LIST.compare_and_swap(ptr.as_ptr(), next, Ordering::SeqCst);
    }

    // Only drop if a vtable was provided for that
    if !vtable.is_null() {
        // Free each item in the array by constructing a trait object for each item and calling
        // `drop_in_place`

        // Safety: Much like `mark`, this assumes that the `value` begins immediately after the
        // header field with no padding
        let array = (ptr.as_ptr() as *mut GcHeader).add(1) as *mut u8;

        for i in 0..len {
            let data = array.add(i*size) as *mut ();
            let obj: &mut dyn Trace = mem::transmute(TraitObject {data, vtable});
            ptr::drop_in_place(obj);
        }
    }

    // Free the memory
    dealloc(ptr.as_ptr() as *mut u8, layout);
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
        let _lock = super::super::GC_TEST_LOCK.lock();

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
