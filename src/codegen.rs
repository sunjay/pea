mod function;

use std::sync::Arc;
use std::collections::HashMap;

use crate::{
    ast,
    bytecode,
    interpreter::Interpreter,
    diagnostics::Diagnostics,
    value::{Value, Obj, FuncObj},
    gc::Gc,
};

use function::FunctionCompiler;

pub struct Compiler<'a> {
    diag: &'a Diagnostics,

    consts: bytecode::Constants,
    /// Map of function name to constant index
    func_consts: HashMap<Arc<str>, bytecode::ConstId>,
}

impl<'a> Compiler<'a> {
    pub fn compile(program: &ast::Program, diag: &'a Diagnostics) -> Interpreter {
        let mut compiler = Compiler {
            diag,

            consts: Default::default(),
            func_consts: Default::default(),
        };

        compiler.walk_program(program);

        let mut interpreter = Interpreter::new(compiler.consts);

        // Call main function
        //
        // Note: `main` must be declared in the top scope, take zero arguments, and return nothing.
        match compiler.func_consts.get("main") {
            Some(&index) => {
                interpreter.call_main(index);
            },

            None => {
                diag.error("`main` function not found").emit();
            },
        }

        interpreter
    }

    fn walk_program(&mut self, program: &ast::Program) {
        let ast::Program {decls} = program;

        for decl in decls {
            self.walk_decl(decl);
        }
    }

    fn walk_decl(&mut self, decl: &ast::Decl) {
        use ast::Decl::*;
        match decl {
            Func(func) => {
                let name = &func.name;

                let func_value = Gc::new(Obj::Func(FuncObj::new(name.value.clone())));
                let func_const = self.consts.push(Value::Obj(func_value.clone()));

                if self.func_consts.contains_key(&name.value) {
                    self.diag.span_error(name.span, format!("the name `{}` is defined multiple times", name.value))
                        .emit();
                }
                self.func_consts.insert(name.value.clone(), func_const);

                let func_code = FunctionCompiler::compile(func, &mut self.consts);

                let mut obj = func_value.lock();
                match &mut *obj {
                    Obj::Func(func_obj) => func_obj.code = func_code,
                    _ => unreachable!("bug: function constant should remain a function"),
                }
            },
        }
    }
}
