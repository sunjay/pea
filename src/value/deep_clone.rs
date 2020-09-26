use crate::gc::{self, Gc};

/// Creates a deep copy of the value, only if the value is mutable
///
/// Immutable values may produce a shallow copy since they cannot be modified anyway
pub trait DeepClone {
    fn deep_clone(&self) -> Self;
}

macro_rules! impl_deep_clone_copy {
    ($($typ:ty),* $(,)?) => {
        $(
            impl DeepClone for $typ {
                fn deep_clone(&self) -> $typ {
                    *self
                }
            }
        )*
    };
}

impl_deep_clone_copy!(
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

impl<T: DeepClone> DeepClone for Vec<T> {
    fn deep_clone(&self) -> Self {
        self.iter().map(|item| item.deep_clone()).collect()
    }
}

macro_rules! impl_deep_clone_tuples {
    ($first:ident $(, $rest:ident)* $(,)?) => {
        impl_deep_clone_tuples!($($rest),*);

        impl<$first: DeepClone, $($rest: DeepClone),*> DeepClone for ($first, $($rest,)*) {
            fn deep_clone(&self) -> Self {
                #[allow(non_snake_case)]
                let ($first, $($rest,)*) = self;
                (
                    $first.deep_clone(),
                    $($rest.deep_clone(),)*
                )
            }
        }
    };

    () => ();
}

impl_deep_clone_tuples!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z);

impl<T: gc::Trace + DeepClone> DeepClone for Gc<T> {
    fn deep_clone(&self) -> Self {
        Gc::new((**self).deep_clone())
    }
}

impl<T> DeepClone for Gc<[T]> {
    fn deep_clone(&self) -> Self {
        // No need to deep clone since this is immutable anyway
        self.clone()
    }
}
