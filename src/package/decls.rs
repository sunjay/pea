use std::{collections::HashMap, sync::Arc};

use crate::nir::DefId;

#[derive(Debug, Default)]
pub struct Decls {
    ids: HashMap<Arc<str>, DefId>,
}

impl Decls {
    pub fn insert(&mut self, name: Arc<str>, def_id: DefId) {
        assert!(self.ids.insert(name.into(), def_id).is_none(),
            "bug: overwrote decl name with a new `DefId`");
    }

    pub fn get(&self, name: &str) -> Option<DefId> {
        self.ids.get(name).copied()
    }
}
