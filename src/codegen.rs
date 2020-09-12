mod function;

use std::sync::Arc;
use std::collections::HashMap;

use crate::{
    ast,
    bytecode,
    prim::{self, Prim},
    interpreter::Interpreter,
    diagnostics::Diagnostics,
    value::Value,
    gc::GcPrim,
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

                let prim = GcPrim::new(Prim::Func(prim::Func::new(name.value.clone())));
                let func_const = self.consts.push(Value::Prim(prim.clone()));

                if self.func_consts.contains_key(&name.value) {
                    self.diag.span_error(name.span, format!("the name `{}` is defined multiple times", name.value))
                        .emit();
                }
                self.func_consts.insert(name.value.clone(), func_const);

                let func_code = FunctionCompiler::compile(func, &mut self.consts);

                let prim = GcPrim::new(Prim::Func(prim::Func::with_code(name.value.clone(), func_code)));
                self.consts.replace(func_const, Value::Prim(prim));
            },
        }
    }
}
