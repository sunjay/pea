/// Every type that can be allocated on the GC must implement this trait
pub trait Trace {
    /// Called to trace any inner GC values within this type
    ///
    /// Call `trace` any fields that implement the `Trace` trait. Calling `trace` on a `Gc<T>` type
    /// will call `gc::mark` on that value.
    fn trace(&self);
}

/// Implement `Trace` for `Copy` types
///
/// Note: `impl<T: Copy> Trace for T` causes problems because `&T` is `Copy`.
macro_rules! impl_trace_copy {
    ($($typ:ty),* $(,)?) => {
        $(
            impl Trace for $typ {
                // Copy types can't contain `Gc` types
                fn trace(&self) {}
            }
        )*
    };
}

impl_trace_copy!(
    (),

    bool,
    char,

    f32,
    f64,

    i8,
    i16,
    i32,
    i64,
    i128,
    isize,

    u8,
    u16,
    u32,
    u64,
    u128,
    usize,
);

impl<T: Trace> Trace for [T] {
    fn trace(&self) {
        for item in self {
            item.trace();
        }
    }
}

macro_rules! impl_trace_tuples {
    ($first:ident $(, $rest:ident)* $(,)?) => {
        impl_trace_tuples!($($rest),*);

        impl<$first: Trace, $($rest: Trace),*> Trace for ($first, $($rest,)*) {
            fn trace(&self) {
                #[allow(non_snake_case)]
                let ($first, $($rest,)*) = self;
                $first.trace();
                $($rest.trace();)*
            }
        }
    };

    () => ();
}

impl_trace_tuples!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z);
