use super::{RuntimeResult, Interpreter};

pub trait ReadArg {
    fn read_arg(interpreter: &mut Interpreter) -> Self;
}

impl ReadArg for u8 {
    #[inline(always)]
    fn read_arg(interpreter: &mut Interpreter) -> Self {
        // Safety: If this code is running, the interpreter must have at least one call frame
        let frame = unsafe { interpreter.call_stack.top_unchecked_mut() };
        // Safety: This relies on the bytecode being compiled correctly
        unsafe { frame.cursor.read_u8_unchecked(&frame.func.code) }
    }
}

impl ReadArg for u16 {
    #[inline(always)]
    fn read_arg(interpreter: &mut Interpreter) -> Self {
        // Safety: If this code is running, the interpreter must have at least one call frame
        let frame = unsafe { interpreter.call_stack.top_unchecked_mut() };
        // Safety: This relies on the bytecode being compiled correctly
        unsafe { frame.cursor.read_u16_unchecked(&frame.func.code) }
    }
}

pub trait Execute<Args> {
    fn run(self, interpreter: &mut Interpreter) -> RuntimeResult;
}

impl<FN> Execute<()> for FN
    where FN: FnMut(&mut Interpreter) -> RuntimeResult,
{
    #[inline(always)]
    fn run(mut self, interpreter: &mut Interpreter) -> RuntimeResult {
        self(interpreter)
    }
}

macro_rules! impl_execute {
    ($first:ident $(, $rest:ident)* $(,)?) => {
        impl_execute!($($rest),*);

        impl<FN, $first: ReadArg, $($rest: ReadArg),*> Execute<($first, $($rest),*)> for FN
            where FN: FnMut(&mut Interpreter, $first, $($rest,)*) -> RuntimeResult,
        {
            #[inline(always)]
            fn run(mut self, interpreter: &mut Interpreter) -> RuntimeResult {
                #![allow(non_snake_case)]

                let $first = $first::read_arg(interpreter);
                $(let $rest = $rest::read_arg(interpreter);)*
                self(interpreter, $first, $($rest),*)
            }
        }
    };

    () => ();
}

impl_execute!(A, B, C, D, E);
