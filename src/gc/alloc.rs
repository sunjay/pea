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
use std::ptr::{self, NonNull};
use std::sync::atomic::{AtomicPtr, Ordering};
use std::alloc::{alloc, Layout, handle_alloc_error};

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
    /// The pointer to the vtable for the trait object that generated this type
    vtable: *mut (),
    /// if true, the allocation is still reachable from a root
    is_reachable: bool,
    /// The address of the previous allocation (makes this an intrusive list node)
    next: *mut GcHeader,
}

impl Default for GcHeader {
    fn default() -> Self {
        Self {
            vtable: ptr::null_mut(),
            // Presume not reachable until proven otherwise
            is_reachable: false,
            next: ptr::null_mut(),
        }
    }
}

/// Allocate memory managed by the GC and initialize it to the given value
///
/// Returns a non-null pointer to the value
pub(in super) fn allocate_array<T, I>(values: I) -> NonNull<[T]>
    where I: ExactSizeIterator<Item=T>,
{
    // Start by generating the layout for the array, essentially `GcEntry<[T]>`
    let size = values.len();

    // Safety: The layout being generated here must match the layout of `GcEntry`
    let header_layout = Layout::new::<GcHeader>();
    let array_layout = Layout::array::<T>(size).expect("bug: failed to create array layout");
    let (layout, array_start_bytes) = header_layout.extend(array_layout).expect("bug: failed to create `GcEntry` layout");
    // Always need to finish a layout with `pad_to_align`
    let layout = layout.pad_to_align();

    // Allocate the `GcEntry<[T]>`
    // Safety: Due to the header, the layout passed to `alloc` cannot be zero-sized
    let entry_ptr = unsafe { alloc(layout) };
    // Check for allocation failure
    if entry_ptr.is_null() {
        handle_alloc_error(layout);
    }

    // Initialize the header (`header` field of `GcEntry<[T]>`)
    // Safety: #[repr(C)] guarantees that a pointer to `GcEntry` is the same as a pointer to
    // `GcHeader` (since `header` is the first field)
    unsafe { (entry_ptr as *mut GcHeader).write(GcHeader::default()); }

    // Initialize the array (`value` field of `GcEntry<[T]>`)
    // Safety: Any offsets we take here must match the layouts used to allocate above
    let array = unsafe { entry_ptr.add(array_start_bytes) } as *mut T;
    for (i, value) in values.enumerate() {
        unsafe { array.add(i).write(value); }
    }

    // `GcEntry<[T]>` is now fully initialized so it is safe to use it
    //let entry: &mut dyn Trace = unsafe { &mut *(entry_ptr as *mut GcEntry<[T]>) };
    // Safety: This will continue to work as long as the unstable trait object layout does not change
    //let TraitObject {data, vtable} = unsafe { mem::transmute(entry) };

    // Safety: Due to #[repr(C)], a pointer to `GcEntry` is the same as a pointer to `GcHeader`
    //let header_ptr = data as *mut GcHeader;
    // Safety: We initialized the `array` pointer earlier so it should still allow us to make a
    // valid slice with the given length. We already checked if the original `entry_ptr` was null.
    //let value_ptr = unsafe { NonNull::new_unchecked(ptr::slice_from_raw_parts_mut(array, size)) };

    //unsafe { finish_header_setup(header_ptr, value_ptr, vtable) }
    todo!()
}

/// Allocate memory managed by the GC and initialize it to the given value
///
/// Returns a non-null pointer to the value
pub(in super) fn allocate<T: Trace>(value: T) -> NonNull<T> {
    // Allocate a trait object so we can erase the type info and just keep the stored value
    // The vtable will be used later to reconstruct the value in a way that can be dropped using the
    // dynamic type info stored in the vtable.
    let entry: Box<dyn Trace> = Box::new(GcEntry {
        header: GcHeader::default(),
        value,
    });

    // Safety: This will continue to work as long as the unstable trait object layout does not change
    let TraitObject {data, vtable} = unsafe { mem::transmute(entry) };
    let entry_ptr = data as *mut GcEntry<T>;

    // Safety: We just initialized this pointer, so it should be valid
    let entry = unsafe { &mut *entry_ptr };

    let header_ptr = &mut entry.header as *mut GcHeader;
    let value_ptr = (&entry.value).into();

    unsafe { finish_header_setup(header_ptr, value_ptr, vtable) }
}

/// Finishes setting up the `GcHeader` of an allocated value and appends it onto `ALLOC_LIST`.
///
/// # Safety
///
/// All provided pointers must be non-null, fully-initialized, and safe to dereference.
unsafe fn finish_header_setup<T: ?Sized>(
    header_ptr: *mut GcHeader,
    value_ptr: NonNull<T>,
    vtable: *mut (),
) -> NonNull<T> {
    (*header_ptr).vtable = vtable;

    // Append onto the list of all allocations
    //
    // To avoid the ABA problem, we initially create the header with a null pointer for `next`, then
    // we swap the current ALLOC_LIST pointer with the pointer to this entry in a single operation.
    let next = ALLOC_LIST.swap(header_ptr, Ordering::SeqCst);
    (*header_ptr).next = next;

    value_ptr
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
    // Remove this entry from the ALLOC_LIST linked list
    if let Some(mut prev) = prev {
        prev.as_mut().next = ptr.as_ref().next;

    // No previous, must be the head of the list
    } else {
        // Set a new head for the list
        //
        // NOTE: This line is not robust and will not work if run concurrently with `alloc`.
        ALLOC_LIST.compare_and_swap(ptr.as_ptr(), ptr.as_ref().next, Ordering::SeqCst);
    }

    // Reconstruct the box that this pointer came from and free the value at the end of this scope
    //
    // Safety: This cast only works because #[repr(C)] guarantees that `GcHeader` is the first field
    // of the struct and therefore a pointer to that field is the same the pointer to the entire
    // struct.
    let data = ptr.as_ptr() as *mut ();
    let vtable = ptr.as_ref().vtable;

    let _entry: Box<dyn Trace> = mem::transmute(TraitObject {data, vtable});
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
