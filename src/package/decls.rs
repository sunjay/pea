use std::collections::HashMap;

use crate::{nir::DefId, gc::Gc};

#[derive(Debug, Default)]
pub struct Decls {
    ids: HashMap<Gc<str>, DefId>,
}

impl Decls {
    pub fn insert(&mut self, name: Gc<str>, def_id: DefId) {
        assert!(self.ids.insert(name.into(), def_id).is_none(),
            "bug: overwrote decl name with a new `DefId`");
    }

    pub fn get(&self, name: &str) -> Option<DefId> {
        self.ids.get(name).copied()
    }
}
