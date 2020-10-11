mod def_consts;
mod def_names;
mod decls;

pub use def_consts::*;
pub use def_names::*;
pub use decls::*;

use std::sync::Arc;

use crate::{bytecode::ConstId, nir::DefId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PkgId(usize);

#[derive(Debug, Default)]
pub struct Packages {
    next_pkg_id: usize,
}

impl Packages {
    pub fn add_package(&mut self) -> PkgId {
        let id = PkgId(self.next_pkg_id);
        self.next_pkg_id += 1;
        id
    }
}

#[derive(Debug)]
pub struct Package {
    pub id: PkgId,
    /// Provides a mapping from paths to `DefId`
    pub root_module: Module,
    /// Provides a mapping from `DefId` to name
    pub def_names: DefNames,
    /// Provides a mapping from `DefId` to `ConstId`
    pub def_consts: DefConsts,
}

impl Package {
    pub fn new(id: PkgId, name: impl Into<Arc<str>>) -> Self {
        Self {
            id,
            root_module: Module::new(name.into()),
            def_names: DefNames::new(id),
            def_consts: DefConsts::default(),
        }
    }

    /// Associates the given name with the given constant ID and returns the generated `DefId`.
    ///
    /// For the value to be usable, it must then be inserted into a module using the same name.
    pub fn insert(&mut self, name: Arc<str>, const_id: ConstId) -> DefId {
        let def_id = self.def_names.insert(name);
        self.def_consts.insert(def_id, const_id);
        def_id
    }
}

#[derive(Debug)]
pub struct Module {
    pub name: Arc<str>,
    //TODO: There could be a `submodules` field here with type `Vec<Module>`
    /// Provides a mapping from name to `DefId`
    pub decls: Decls,
}

impl Module {
    pub fn new(name: Arc<str>) -> Self {
        Self {
            name: name.into(),
            decls: Default::default(),
        }
    }

    /// Looks up a name in this module
    pub fn lookup(&self, name: &str) -> Option<DefId> {
        self.decls.get(name)
    }
}
