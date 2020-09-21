mod def_consts;
mod function;

use std::convert::TryInto;

use crate::{
    nir,
    bytecode,
    prim,
    interpreter::Interpreter,
    diagnostics::Diagnostics,
    value::Value,
    gc::Gc,
};

use def_consts::DefConsts;
use function::FunctionCompiler;

pub struct Compiler<'a> {
    def_table: &'a nir::DefTable,
    diag: &'a Diagnostics,

    consts: bytecode::Constants,
    const_ids: DefConsts,
}

impl<'a> Compiler<'a> {
    pub fn compile(
        program: &nir::Program,
        def_table: &'a nir::DefTable,
        diag: &'a Diagnostics,
    ) -> Interpreter {
        let mut compiler = Compiler {
            def_table,
            diag,

            consts: Default::default(),
            const_ids: Default::default(),
        };

        // Declare constants in the bytecode for every item in the program
        compiler.declare_consts_program(program);

        // Generate the code for the entire program
        compiler.walk_program(program);

        let Compiler {consts, const_ids, ..} = compiler;

        let mut interpreter = Interpreter::new(consts, diag.source_files().clone());

        // Call main function
        //
        // Note: `main` must be declared in the root scope, take zero arguments, and return nothing.
        let main_const_id = program.scope.lookup("main")
            .and_then(|def_id| const_ids.get(def_id));

        match main_const_id {
            Some(index) => {
                //TODO: Check that `main` is a function with zero arguments
                interpreter.call_main(index);
            },

            None => {
                diag.error("`main` function not found").emit();
            },
        }

        interpreter
    }

    fn declare_consts_program(&mut self, program: &nir::Program) {
        let nir::Program {decls, scope: _} = program;

        for decl in decls {
            self.declare_consts_decl(decl);
        }
    }

    fn declare_consts_decl(&mut self, decl: &nir::Decl) {
        use nir::Decl::*;
        match decl {
            Func(func) => self.declare_consts_func_decl(func),
        }
    }

    fn declare_consts_func_decl(&mut self, func: &nir::FuncDecl) {
        let def_id = func.name.id;
        let name = self.def_table.get(def_id);

        // Define and insert an empty function for now (it will be replaced later during compilation)
        let arity = func.params.len().try_into()
            .expect("bug: should have validated that functions cannot have more than 255 parameters");
        let func = Gc::new(prim::Func::new(name.value.clone(), arity));

        let const_id = self.consts.push(Value::Func(func));
        self.const_ids.insert(def_id, const_id);
    }

    fn walk_program(&mut self, program: &nir::Program) {
        let nir::Program {decls, scope: _} = program;

        for decl in decls {
            self.walk_decl(decl);
        }
    }

    fn walk_decl(&mut self, decl: &nir::Decl) {
        use nir::Decl::*;
        match decl {
            Func(func) => self.walk_func(func),
        }
    }

    fn walk_func(&mut self, func: &nir::FuncDecl) {
        let func_code = FunctionCompiler::compile(
            func,
            &mut self.consts,
            &self.const_ids,
            self.diag,
        );

        let def_id = func.name.id;
        let name = self.def_table.get(def_id);
        let const_id = self.const_ids.get(def_id)
            .expect("bug: function constant did not remain a constant");

        let arity = func.params.len().try_into()
            .expect("bug: should have validated that functions cannot have more than 255 parameters");

        // Replace the defined constant for this function with a version that has the compiled code
        let func = Gc::new(prim::Func::with_code(name.value.clone(), arity, func_code));
        self.consts.replace(const_id, Value::Func(func));
    }
}
