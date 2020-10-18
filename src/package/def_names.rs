use std::collections::HashMap;

use crate::{gc::Gc, nir::{DefId, DefIdGen}};

use super::PkgId;

#[derive(Debug)]
pub struct DefNames {
    def_ids: DefIdGen,
    names: HashMap<DefId, Gc<str>>,
}

impl DefNames {
    pub fn new(pkg_id: PkgId) -> Self {
        Self {
            def_ids: DefIdGen::new(pkg_id),
            names: HashMap::default(),
        }
    }

    pub fn insert(&mut self, name: Gc<str>) -> DefId {
        let id = self.def_ids.next_id();
        self.names.insert(id, name.into());
        id
    }

    pub fn get(&self, id: DefId) -> Option<&Gc<str>> {
        self.names.get(&id)
    }
}
