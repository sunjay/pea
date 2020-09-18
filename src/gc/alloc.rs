//! Allocation, deallocation, and heap traversal functions for memory managed by the GC.
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
use std::sync::atomic::{AtomicBool, Ordering};
use std::alloc::{alloc, dealloc, Layout, handle_alloc_error};

use parking_lot::{Mutex, const_mutex};

use crate::gc_debug;

use super::Trace;

#[derive(Debug)]
struct GcState {
    /// A linked list of allocations managed by the GC
    alloc_list: *mut GcHeader,
    /// The number of bytes needed to trigger a collection
    threshold: usize,
    /// The number of bytes currently allocated
    allocated: usize,
}

// Safety: This state is explicitly managed behind a Mutex to guarantee thread-safety
unsafe impl Send for GcState {}

static GC_STATE: Mutex<GcState> = const_mutex(GcState {
    alloc_list: ptr::null_mut(),
    // arbitrary -- goal is to not trigger the first few GCs too quickly but also to not wait too
    // long. Could be tuned with some real-world programs.
    threshold: 1024 * 1024,
    // Nothing has been allocated yet
    allocated: 0,
});

// arbitrary -- goal is to make it so that as the amount of memory the program uses grows, the
// threshold moves farther out to limit the total time spent re-traversing the larger live set.
// Could be tuned with some real-world programs.
const GC_HEAP_GROW_FACTOR: usize = 2;

/// true if the threshold has been reached for the next collection
static NEEDS_COLLECT: AtomicBool = AtomicBool::new(false);

/// Returns true if `sweep` should be called as soon as possible
pub fn needs_collect() -> bool {
    NEEDS_COLLECT.load(Ordering::SeqCst)
}

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
    // Note that this critical section is kept as small as possible to allow allocation and
    // initialiation to occur in parallel as much as possible
    let mut gc_state = GC_STATE.lock();
    let next = gc_state.alloc_list;
    gc_state.alloc_list = header_ptr;
    // Safety: The header was initialized above, so it should still be valid to assign to
    unsafe { (*header_ptr).next = next; }

    // Record the amount of memory that was allocated
    gc_state.allocated += layout.size();
    if gc_state.allocated > gc_state.threshold {
        // Notify that a collection should take place ASAP
        NEEDS_COLLECT.store(true, Ordering::SeqCst);
    }

    gc_debug!("{:p} allocate - size: {} bytes, type: {}, len: {}", value_ptr, array_layout.size(),
        std::any::type_name::<T>(), len);

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
/// Returns true if the value was already marked previously.
///
/// # Safety
///
/// This function may only be used with pointers returned from `allocate`. It should not be called
/// concurrently with `sweep`.
pub(in super) unsafe fn mark<T: ?Sized>(ptr: NonNull<T>) -> bool {
    gc_debug!("{:p} mark", ptr);

    // Safety: Since `GcEntry` is #[repr(C)], the fields are laid out with `GcHeader` before `T`.
    // That means that we *should* be able to just subtract the size of `GcHeader` to get to the
    // `header` field from the `value` field. Note: sub(1) == -(sizeof(GcHeader)*1).
    //
    //TODO: Not actually sure if this offset will always work. A more implementation independent way
    //  would be to construct a fake `GcEntry` and then find out the actual offset between the
    //  `header` and `value` fields.
    let header_ptr = (ptr.as_ptr() as *mut GcHeader).sub(1);
    let mut header = &mut *header_ptr;

    let prev_reachable = header.is_reachable;
    header.is_reachable = true;
    prev_reachable
}

/// Frees the memory associated with the given allocation
///
/// Returns the number of bytes that were deallocated as well as a pointer to the next allocation in
/// the allocation list after the one that was freed.
///
/// # Safety
///
/// This function may only be used with pointers from the allocation list in `GC_STATE`.
unsafe fn free(ptr: NonNull<GcHeader>) -> (usize, *mut GcHeader) {
    // Extract info from the header
    // Safety: The pointer should be valid because it is currently in the allocation list. Note that
    // in order to avoid aliasing issues we are careful to copy the values out (avoids references).
    let GcHeader {len, size, vtable, layout, next, ..} = *ptr.as_ref();

    gc_debug!("{:p} free - size: {} bytes, len: {}", ptr.as_ptr().add(1), size, len);

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

    (layout.size(), next)
}

/// Sweeps/collects all allocations that have been not been marked as reachable
///
/// # Safety
///
/// This function pauses all other GC functions while it is running. No calls to `mark` should take
/// place while this is running.
pub fn sweep() {
    // Keep the GC state locked so no allocations can be registered while this takes place
    let mut gc_state = GC_STATE.lock();

    // Register ASAP that we are currently collecting so no other thread starts a `sweep`
    NEEDS_COLLECT.store(false, Ordering::SeqCst);

    let mut prev = None;
    let mut current = gc_state.alloc_list;
    while let Some(mut header) = NonNull::new(current) {
        // We need to be careful to keep the lifetime of &GcHeader short so we don't break the
        // aliasing rules when `free` takes ownership of the data.
        let is_reachable = {
            // Safety: the header should be a valid pointer or it would not be in the list.
            let header = unsafe { header.as_mut() };
            // Advance the loop
            current = header.next;

            // Store the reachable status of this allocation
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

        // Safety: All the pointers we are going through are from `alloc_list`.
        let (bytes_freed, next) = unsafe { free(header) };
        gc_state.allocated -= bytes_freed;

        // Remove the freed allocation from the linked list
        match prev {
            // Assign the next pointer of the previous item to the list to the item just after the
            // item that was just freed
            Some(mut prev_header) => unsafe {
                prev_header.as_mut().next = next;
            },

            // No previous, must be the head of the list
            None => gc_state.alloc_list = next,
        }
    }

    // Adjust the threshold based on how much is still allocated
    gc_state.threshold = gc_state.allocated * GC_HEAP_GROW_FACTOR;
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

        assert_eq!(GC_STATE.lock().alloc_list, ptr::null_mut());
    }
}
