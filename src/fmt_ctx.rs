use std::fmt;
use std::borrow::Cow;
use std::sync::Arc;

use crate::gc::Gc;

/// Similar to `write!` but allows some external context to be used during formatting
#[macro_export]
macro_rules! cwrite {
    ($dst:expr, $ctx:expr) => {
        {
            write!($dst)
        }
    };
    ($dst:expr, $ctx:expr, $fmtstr:literal) => {
        {
            write!($dst, $fmtstr)
        }
    };
    ($dst:expr, $ctx:expr, $fmtstr:literal, $($arg:expr),*) => {
        {
            let ctx = $ctx;
            write!($dst, $fmtstr, $($crate::fmt_ctx::DisplayWith {
                ctx,
                value: &$arg,
            }),*)
        }
    };
}

/// Similar to `writeln!` but allows some external context to be used during formatting
#[macro_export]
macro_rules! cwriteln {
    ($dst:expr, $ctx:expr) => {
        {
            writeln!($dst)
        }
    };
    ($dst:expr, $ctx:expr, $fmtstr:literal) => {
        {
            writeln!($dst, $fmtstr)
        }
    };
    ($dst:expr, $ctx:expr, $fmtstr:literal, $($arg:expr),*) => {
        {
            let ctx = $ctx;
            writeln!($dst, $fmtstr, $($crate::fmt_ctx::DisplayWith {
                ctx,
                value: &$arg,
            }),*)
        }
    };
}

/// Similar to `print!` but allows some external context to be used during formatting
#[macro_export]
macro_rules! cprint {
    ($ctx:expr) => {
        {
            print!()
        }
    };
    ($ctx:expr, $fmtstr:literal) => {
        {
            print!($fmtstr)
        }
    };
    ($ctx:expr, $fmtstr:literal, $($arg:expr),*) => {
        {
            let ctx = $ctx;
            print!($fmtstr, $($crate::fmt_ctx::DisplayWith {
                ctx,
                value: &$arg,
            }),*)
        }
    };
}

/// Similar to `println!` but allows some external context to be used during formatting
#[macro_export]
macro_rules! cprintln {
    ($ctx:expr) => {
        {
            println!()
        }
    };
    ($ctx:expr, $fmtstr:literal) => {
        {
            println!($fmtstr)
        }
    };
    ($ctx:expr, $fmtstr:literal, $($arg:expr),*) => {
        {
            let ctx = $ctx;
            println!($fmtstr, $($crate::fmt_ctx::DisplayWith {
                ctx,
                value: &$arg,
            }),*)
        }
    };
}

/// Similar to `eprint!` but allows some external context to be used during formatting
#[macro_export]
macro_rules! ceprint {
    ($ctx:expr) => {
        {
            eprint!()
        }
    };
    ($ctx:expr, $fmtstr:literal) => {
        {
            eprint!($fmtstr)
        }
    };
    ($ctx:expr, $fmtstr:literal, $($arg:expr),*) => {
        {
            let ctx = $ctx;
            eprint!($fmtstr, $($crate::fmt_ctx::DisplayWith {
                ctx,
                value: &$arg,
            }),*)
        }
    };
}

/// Similar to `eprintln!` but allows some external context to be used during formatting
#[macro_export]
macro_rules! ceprintln {
    ($ctx:expr) => {
        {
            eprintln!()
        }
    };
    ($ctx:expr, $fmtstr:literal) => {
        {
            eprintln!($fmtstr)
        }
    };
    ($ctx:expr, $fmtstr:literal, $($arg:expr),*) => {
        {
            let ctx = $ctx;
            eprintln!($fmtstr, $($crate::fmt_ctx::DisplayWith {
                ctx,
                value: &$arg,
            }),*)
        }
    };
}

/// Similar to `format!` but allows some external context to be used during formatting
#[macro_export]
macro_rules! cformat {
    ($ctx:expr) => {
        {
            format!()
        }
    };
    ($ctx:expr, $fmtstr:literal) => {
        {
            format!($fmtstr)
        }
    };
    ($ctx:expr, $fmtstr:literal, $($arg:expr),*) => {
        {
            let ctx = $ctx;
            format!($fmtstr, $($crate::fmt_ctx::DisplayWith {
                ctx,
                value: &$arg,
            }),*)
        }
    };
}

/// Similar to `std::fmt::Display` but allows some external context to be used during formatting
pub trait DisplayCtx<Ctx> {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &Ctx) -> fmt::Result;
}

impl<Ctx, T: ?Sized + DisplayCtx<Ctx>> DisplayCtx<Ctx> for &T {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &Ctx) -> fmt::Result {
        DisplayCtx::fmt_ctx(&**self, f, ctx)
    }
}

impl<'a, Ctx, T> DisplayCtx<Ctx> for Cow<'a, T>
    where T: DisplayCtx<Ctx> + ToOwned + ?Sized,
          <T as ToOwned>::Owned: DisplayCtx<Ctx>,
{
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &Ctx) -> fmt::Result {
        match self {
            Cow::Borrowed(value) => DisplayCtx::fmt_ctx(value, f, ctx),
            Cow::Owned(value) => DisplayCtx::fmt_ctx(value, f, ctx),
        }
    }
}

impl<Ctx, T> DisplayCtx<Ctx> for Box<T>
    where T: DisplayCtx<Ctx> + ?Sized,
{
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &Ctx) -> fmt::Result {
        DisplayCtx::fmt_ctx(&**self, f, ctx)
    }
}

impl<Ctx, T> DisplayCtx<Ctx> for Arc<T>
    where T: DisplayCtx<Ctx> + ?Sized,
{
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &Ctx) -> fmt::Result {
        DisplayCtx::fmt_ctx(&**self, f, ctx)
    }
}

impl<Ctx, T> DisplayCtx<Ctx> for Gc<T>
    where T: DisplayCtx<Ctx> + ?Sized,
{
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &Ctx) -> fmt::Result {
        DisplayCtx::fmt_ctx(&**self, f, ctx)
    }
}

impl<Ctx> DisplayCtx<Ctx> for str {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, _ctx: &Ctx) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl<Ctx> DisplayCtx<Ctx> for usize {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, _ctx: &Ctx) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl<Ctx> DisplayCtx<Ctx> for u64 {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, _ctx: &Ctx) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl<Ctx> DisplayCtx<Ctx> for i64 {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, _ctx: &Ctx) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl<Ctx> DisplayCtx<Ctx> for i32 {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, _ctx: &Ctx) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl<Ctx> DisplayCtx<Ctx> for f64 {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, _ctx: &Ctx) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl<Ctx> DisplayCtx<Ctx> for bool {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, _ctx: &Ctx) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

/// Displays a value with the given context
pub struct DisplayWith<'a, Ctx, T> {
    pub ctx: &'a Ctx,
    pub value: &'a T,
}

impl<'a, Ctx, T: DisplayCtx<Ctx>> fmt::Display for DisplayWith<'a, Ctx, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {ctx, value} = self;
        value.fmt_ctx(f, ctx)
    }
}
