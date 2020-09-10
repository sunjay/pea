mod function;

use crate::{
    ast,
    bytecode,
    interpreter::Interpreter,
    diagnostics::Diagnostics,
};

use function::FunctionCompiler;

pub struct Compiler<'a> {
    diag: &'a Diagnostics,

    consts: bytecode::Constants,
}

impl<'a> Compiler<'a> {
    pub fn compile(program: &ast::Program, diag: &'a Diagnostics) -> Interpreter {
        let mut compiler = Compiler {
            diag,

            consts: Default::default(),
        };

        compiler.walk_program(program);

        //TODO: Should generate a call to `main` function here or compiler error if no such function
        // exists. `main` must be declared in the top scope, take zero arguments, and return nothing
        todo!()
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
                let func = FunctionCompiler::compile(func, &mut self.consts);
            },
        }
    }
}
