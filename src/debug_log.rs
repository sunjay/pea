#[macro_export]
macro_rules! gc_debug {
    ($($arg:tt)*) => {{
        #[cfg(feature = "gc_debug")]
        println!($($arg)*);
    }};
}
