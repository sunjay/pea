use pea::gc::{self, Gc, Trace};

/// This is used to allow GC tests to pretend they are the only thing using the GC at any given time
static GC_TEST_LOCK: parking_lot::Mutex<()> = parking_lot::const_mutex(());

#[test]
fn gc_alloc() {
    let _lock = GC_TEST_LOCK.lock();

    let value1 = Gc::new(2i8);
    let value2 = Gc::new(42i16);
    let value3 = Gc::new(-33i32);
    let value4 = Gc::new(54i64);
    let value5 = Gc::new(-12931i128);

    // Check that all the values are as we expect
    assert_eq!(*value1, 2);
    assert_eq!(*value2, 42);
    assert_eq!(*value3, -33);
    assert_eq!(*value4, 54);
    assert_eq!(*value5, -12931);

    // Make sure we don't free gc::marked pointers
    gc::mark(&value2);
    gc::mark(&value3);

    gc::sweep();

    assert_eq!(*value2, 42);
    assert_eq!(*value3, -33);

    // Should clean up all memory at the end
    gc::sweep();
}

use std::sync::{Arc, atomic::{AtomicU8, Ordering}};

#[test]
fn gc_str() {
    let _lock = GC_TEST_LOCK.lock();

    let value = Gc::from("abc123 woooo");
    assert_eq!(&*value, "abc123 woooo");

    // Should not clean up anything because we've traced
    value.trace();
    gc::sweep();

    // Value should still be the same
    assert_eq!(&*value, "abc123 woooo");

    // Should clean up all memory at the end
    gc::sweep();
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
    gc::sweep();

    // Check all values are still as we expect
    assert_eq!(array1.len(), array2.len());
    for (a, b) in array1.iter().zip(array2.iter()) {
        assert_eq!(**a, *b);
    }

    // Should clean up all memory at the end
    gc::sweep();
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

    // gc::mark the value to prevent it from being collected
    gc::mark(&array1);

    // Should not clean up anything
    gc::sweep();
    assert_eq!(counter.load(Ordering::SeqCst), 0);

    // Should clean up all the memory at the end and call drop
    gc::sweep();
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

    // gc::marking the value should work even though it is zero-sized
    gc::mark(&value1);
    gc::mark(&value2);

    // Should not clean up anything
    gc::sweep();

    // Should be able to access the value as normal
    assert_eq!(*value1, ());
    println!("{:?}", *value1);
    assert_eq!(*value2, []);
    println!("{:?}", &*value2);

    // Should clean up all the memory at the end
    gc::sweep();
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

    // Should not clean up anything (since `trace` gc::marks the values)
    gc::sweep();

    // Values should still be the same
    assert_eq!(a.value(), Some(5));
    assert_eq!(b.value(), Some(10));

    // Should clean up all the memory at the end
    gc::sweep();
}
