mod function;

use std::convert::TryInto;

use crate::{
    bytecode,
    cgenir,
    nir,
    prim,
    gc::Gc,
    diagnostics::Diagnostics,
    package::DefConsts,
    value::Value,
};

use function::FunctionCompiler;

pub struct Compiler<'a> {
    def_table: &'a nir::DefTable,
    diag: &'a Diagnostics,

    consts: &'a mut bytecode::Constants,
    def_consts: DefConsts,
}

impl<'a> Compiler<'a> {
    pub fn compile(
        program: &cgenir::Program,
        consts: &mut bytecode::Constants,
        diag: &'a Diagnostics,
    ) -> DefConsts {
        let mut compiler = Compiler {
            def_table: &program.def_table,
            diag,

            consts,
            def_consts: Default::default(),
        };

        compiler.walk_program(program);

        let Compiler {consts: _, def_consts, ..} = compiler;

        def_consts
    }

    fn walk_program(&mut self, program: &cgenir::Program) {
        let cgenir::Program {root_module, def_table: _} = program;

        // Declare constants in the bytecode for every item in the module
        self.declare_consts_module(root_module);

        // Generate the code for the root module
        self.walk_module(root_module);
    }

    fn declare_consts_module(&mut self, module: &cgenir::Module) {
        let cgenir::Module {name: _, decls, scope: _} = module;

        for decl in decls {
            self.declare_consts_decl(decl);
        }
    }

    fn declare_consts_decl(&mut self, decl: &cgenir::Decl) {
        use cgenir::Decl::*;
        match decl {
            Func(func) => self.declare_consts_func_decl(func),
        }
    }

    fn declare_consts_func_decl(&mut self, func: &cgenir::FuncDecl) {
        let def_id = func.name.id;
        let name = self.def_table.get(def_id);

        // Define and insert an empty function for now (it will be replaced later during compilation)
        let arity = func.params.len().try_into()
            .expect("bug: should have validated that functions cannot have more than 255 parameters");
        let func = Gc::new(prim::Func::new(name.value.clone(), arity));

        let const_id = self.consts.push(Value::Func(func));
        self.def_consts.insert(def_id, const_id);
    }

    fn walk_module(&mut self, module: &cgenir::Module) {
        let cgenir::Module {name: _, decls, scope: _} = module;

        for decl in decls {
            self.walk_decl(decl);
        }
    }

    fn walk_decl(&mut self, decl: &cgenir::Decl) {
        use cgenir::Decl::*;
        match decl {
            Func(func) => self.walk_func(func),
        }
    }

    fn walk_func(&mut self, func: &cgenir::FuncDecl) {
        let func_code = FunctionCompiler::compile(
            func,
            &mut self.consts,
            &self.def_consts,
            self.diag,
        );

        let def_id = func.name.id;
        let name = self.def_table.get(def_id);
        let const_id = self.def_consts.get(def_id)
            .expect("bug: function constant did not remain a constant");

        let arity = func.params.len().try_into()
            .expect("bug: should have validated that functions cannot have more than 255 parameters");

        // Replace the defined constant for this function with a version that has the compiled code
        let func = Gc::new(prim::Func::with_code(name.value.clone(), arity, func_code));
        self.consts.replace(const_id, Value::Func(func));
    }
}
