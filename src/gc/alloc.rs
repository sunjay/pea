//! Allocation, deallocation, and heap traversal functions.
//!
//! The garbage collector does not currently implement its own heap. We use the `Box` type and the
//! system allocator.
//!
//! ## Implementation Notes
//!
//! The ideal interface for this garbage collector would be a `Gc<T>` type similar to `Arc<T>` that
//! frees its data after it is done being used. That interface is possible and actually very easy to
//! implement with C semantics. In C, there is no `Drop`, so you can just put a header at the start
//! of every allocation and operate as if the type `T` doesn't even exist. The C allocator only
//! needs you to return back the same pointer value it returned from `malloc`. In Rust, it isn't
//! that easy because you can't just free an arbitrary pointer, you need to provide a pointer with
//! the same size and alignment as the value you allocated. You also need to run any `Drop` impl
//! that the type `T` might have. With no runtime type info, this is very hard to implement.
//!
//! This GC implementation provides the exact `Gc<T>` interface you would want with only one small
//! caveat: `T` cannot be any type. Instead, you have to list all the types you support in the
//! `gc_types` macro. That macro will then generate a constructor for your type via the `From`
//! trait. The way this works is that we have an enum, `GcValue`, that has a variant for each
//! supported type listed in `gc_types`. When we allocate, we allocate a value of `GcValue` instead
//! of a value of type `T`. That makes all of allocations the same size and gives us a consistent
//! type `GcValue` to tell Rust to free when we are done with the memory. Since `GcValue` is just a
//! regular enum, Rust knows how to run its `Drop` implementation as well.
//!
//! The tricky thing about this method is that enum layouts are largely unspecified, so if we have a
//! pointer to a type `T`, it's difficult to derive the pointer to `GcValue`. We work around this by
//! storing a footer with the value of type `T` that points back to the `GcEntry` we originally
//! allocated. (`GcEntry` is just `GcValue` with an extra header.)
//!
//! So for each value of type `T`, we actually end up allocating an additional overhead of both the
//! enum variant for `GcValue` and the extra `*mut GcEntry` footer. Every variant of an enum
//! allocates the same amount of memory, so if the variants of `GcValue` vary wildly in size, we may
//! end up wasting a lot of space.
//!
//! Hopefully this is all worth it to have a simple `Gc<T>` interface that is almost exactly the
//! same as the equivalent C code that this was based on.

use std::ptr::{self, NonNull};
use std::sync::atomic::{AtomicPtr, Ordering};

use super::GcValue;

/// A linked list of allocations managed by the GC
static ALLOC_LIST: AtomicPtr<GcHeader> = AtomicPtr::new(ptr::null_mut());

/// Due to the `repr(C)` attribute, the address of the `value` field is the same as the address of
/// the `GcEntryPtr` struct itself. This allows the `entry` field to be accessed from a pointer to
/// the type T.
///
/// This is essentially the same as appending a pointer onto the tail of whatever value is being
/// allocated. We need this because the layout of enums is unspecified so we can't get to the
/// address just from the value of an enum variant itself.
#[repr(C)]
pub(in super) struct GcEntryPtr<T> {
    pub value: T,
    pub entry: NonNull<GcEntry>,
}

impl<T> From<T> for GcEntryPtr<T> {
    fn from(value: T) -> Self {
        Self {
            value,
            entry: NonNull::dangling(),
        }
    }
}

/// A single heap allocated entry managed by the GC
#[repr(C)]
pub(in super) struct GcEntry {
    /// A header containing extra metadata needed by the GC
    header: GcHeader,
    /// The actual allocated value
    value: GcValue,
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
pub(in super) fn allocate(value: GcValue) -> (NonNull<GcEntry>, NonNull<GcValue>) {
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
    let entry_ptr = Box::leak(entry).into();

    (entry_ptr, value_ptr)
}

/// Marks a value managed by the GC as reachable
///
/// # Safety
///
/// This function may only be used with pointers returned from `alloc`.
pub(in super) unsafe fn mark<T>(ptr: NonNull<T>) {
    // Safety: This cast is safe because each `T` allocated by the GC is guaranteed to be the
    // `value` field of `GcEntryPtr`. Since `GcEntryPtr` is #[repr(C)] and `value` is the first
    // field, a pointer to `value` is guaranteed to be a pointer to `GcEntryPtr`.
    let ptr: NonNull<GcEntryPtr<T>> = ptr.cast();
    mark_inner(ptr.as_ref().entry)
}

unsafe fn mark_inner(mut ptr: NonNull<GcEntry>) {
    let entry = ptr.as_mut();
    entry.header.is_reachable = true;
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
