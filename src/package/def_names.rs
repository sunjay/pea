use std::{collections::HashMap, sync::Arc};

use crate::nir::{DefId, DefIdGen};

use super::PkgId;

#[derive(Debug)]
pub struct DefNames {
    def_ids: DefIdGen,
    names: HashMap<DefId, Arc<str>>,
}

impl DefNames {
    pub fn new(pkg_id: PkgId) -> Self {
        Self {
            def_ids: DefIdGen::new(pkg_id),
            names: HashMap::default(),
        }
    }

    pub fn insert(&mut self, name: Arc<str>) -> DefId {
        let id = self.def_ids.next_id();
        self.names.insert(id, name.into());
        id
    }

    pub fn get(&self, id: DefId) -> Option<&Arc<str>> {
        self.names.get(&id)
    }
}
