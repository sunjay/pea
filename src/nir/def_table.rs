use std::collections::HashMap;

use crate::{ast, gc, package::PkgId};

use super::{DefId, DefIdGen};

/// A table that maps `Ident`s to `DefId`s and back
///
/// Note that this only provides a mechanism for creating a `DefId` from an `Ident`. A separate
/// structure needs to be used to lookup the `DefId` for a given `Ident` value. This type is not
/// aware of scoping or name resolution.
#[derive(Debug)]
pub struct DefTable {
    def_ids: DefIdGen,
    defs: HashMap<DefId, ast::Ident>,
}

impl gc::Trace for DefTable {
    fn trace(&self) {
        let Self {def_ids: _, defs} = self;

        for (_, ident) in defs {
            ident.value.trace();
        }
    }
}

impl DefTable {
    pub fn new(pkg_id: PkgId) -> Self {
        Self {
            def_ids: DefIdGen::new(pkg_id),
            defs: HashMap::default(),
        }
    }

    /// Adds a new `Ident` to the table and returns its ID
    pub fn insert(&mut self, ident: ast::Ident) -> DefId {
        let id = self.def_ids.next_id();
        self.defs.insert(id, ident);
        id
    }

    pub fn get(&self, id: DefId) -> &ast::Ident {
        self.defs.get(&id)
            .expect("bug: def ID is not in this table")
    }
}
