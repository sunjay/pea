use std::{convert::TryInto, fmt};

use crate::{gc::{self, Gc}, ty::{FuncTy, PrimTy, PrimFuncTy}, interpreter::{Interpreter, RuntimeError}, value::Value};

pub struct NativeFunc {
    pub name: Gc<str>,
    pub ty: FuncTy,
    pub arity: u8,
    pub func: Box<dyn Fn(&mut Interpreter, Vec<Value>) -> Result<Value, RuntimeError>>,
}

impl fmt::Debug for NativeFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {name, ty, arity, func} = self;

        f.debug_struct("NativeFunc")
            .field("name", name)
            .field("ty", ty)
            .field("arity", arity)
            .field("func", &format_args!("{:p}", func.as_ref()))
            .finish()
    }
}

impl PartialEq for NativeFunc {
    fn eq(&self, other: &Self) -> bool {
        let Self {name, ty, arity, func} = self;
        *name == other.name && *ty == other.ty && *arity == other.arity
            && func.as_ref() as *const _ == other.func.as_ref() as *const _
    }
}

impl fmt::Display for NativeFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}

impl gc::Trace for NativeFunc {
    fn trace(&self) {
        let Self {name, ty: _, arity: _, func: _} = self;
        name.trace();
    }
}

pub trait IntoNativeFunc<A> {
    fn into_native_func<N: Into<Gc<str>>>(self, name: N) -> NativeFunc;
}

impl<F: 'static, Ret> IntoNativeFunc<()> for F
    where F: for<'a> Fn(&'a mut Interpreter) -> Result<Ret, RuntimeError>,
          Ret: PrimTy + Into<Value>,
{
    fn into_native_func<N: Into<Gc<str>>>(self, name: N) -> NativeFunc {
        const ARITY: usize = 0;

        NativeFunc {
            name: name.into(),
            ty: <fn () -> Ret>::func_ty(),
            arity: ARITY as u8,
            func: Box::new(move |interpreter, args| {
                debug_assert_eq!(args.len(), ARITY);

                self(interpreter).map(Into::into)
            }),
        }
    }
}

macro_rules! impl_into_native_func {
    ($arg0:ident, $($arg:ident),*) => {
        impl<$arg0, $($arg),*, Ret, FN: 'static> IntoNativeFunc<($arg0, $($arg),*)> for FN
            where FN: for<'a> Fn(&'a mut Interpreter, $arg0, $($arg),*) -> Result<Ret, RuntimeError>,
                  $arg0: PrimTy,
                  $($arg: PrimTy,)*
                  Ret: PrimTy + Into<Value>,
                  Value: TryInto<$arg0, Error=Value>,
                  $(Value: TryInto<$arg, Error=Value>),*
        {
            #[allow(non_snake_case)]
            fn into_native_func<NAME: Into<Gc<str>>>(self, name: NAME) -> NativeFunc {
                const ARITY: usize = impl_into_native_func!(@count $arg0, $($arg),*);

                NativeFunc {
                    name: name.into(),
                    ty: <fn($arg0, $($arg),*) -> Ret>::func_ty(),
                    arity: ARITY as u8,
                    func: Box::new(move |interpreter, args| {
                        debug_assert_eq!(args.len(), ARITY);

                        let args: Box<[_; ARITY]> = args.into_boxed_slice().try_into()
                            .expect("bug: did not get the expected number of args");

                        let [$arg0, $($arg),*] = *args;

                        let $arg0 = $arg0.try_into()
                            .expect("bug: type checker allowed wrong type of argument for native func");
                        $(let $arg = $arg.try_into()
                            .expect("bug: type checker allowed wrong type of argument for native func");)*

                        self(interpreter, $arg0, $($arg),*).map(Into::into)
                    }),
                }
            }
        }
    };

    () => {};

    (@count $arg0:ident, $($arg:ident),*) => (
        1 + impl_into_native_func!(@count $($arg),*)
    );

    (@count $arg0:ident) => (
        1
    );
}

impl_into_native_func!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z);
