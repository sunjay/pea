mod function;

use std::collections::HashMap;

use crate::{
    nir,
    bytecode,
    prim,
    interpreter::Interpreter,
    diagnostics::Diagnostics,
    value::Value,
    gc::Gc,
};

use function::FunctionCompiler;

pub struct Compiler<'a> {
    def_table: &'a nir::DefTable,

    consts: bytecode::Constants,
    /// Map of function name to constant index
    func_consts: HashMap<nir::DefId, bytecode::ConstId>,
}

impl<'a> Compiler<'a> {
    pub fn compile(
        program: &nir::Program,
        def_table: &'a nir::DefTable,
        diag: &'a Diagnostics,
    ) -> Interpreter {
        let mut compiler = Compiler {
            def_table,

            consts: Default::default(),
            func_consts: Default::default(),
        };

        compiler.walk_program(program);

        let Compiler {consts, func_consts, ..} = compiler;

        let mut interpreter = Interpreter::new(consts);

        // Call main function
        //
        // Note: `main` must be declared in the root scope, take zero arguments, and return nothing.
        let main_const_id = program.scope.functions.get("main")
            .and_then(|def_id| func_consts.get(def_id));

        match main_const_id {
            Some(&index) => {
                interpreter.call_main(index);
            },

            None => {
                diag.error("`main` function not found").emit();
            },
        }

        interpreter
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
            Func(func) => {
                let def_id = func.name.id;
                let name = self.def_table.get(def_id);

                let prim = Gc::new(prim::Func::new(name.value.clone()));
                let func_const = self.consts.push(Value::Func(prim.clone()));

                debug_assert!(!self.func_consts.contains_key(&def_id), "bug: walked the same function more than once");
                self.func_consts.insert(def_id, func_const);

                let func_code = FunctionCompiler::compile(func, &mut self.consts);

                let prim = Gc::new(prim::Func::with_code(name.value.clone(), func_code));
                self.consts.replace(func_const, Value::Func(prim));
            },
        }
    }
}
