use std::{collections::HashMap, sync::Arc};

use crate::nir;

#[derive(Debug, Default)]
pub struct PrimMethods {
    pub unit: Methods,
    pub bool: Methods,
    pub i64: Methods,
    pub u8: Methods,
    pub list: Methods,
}

#[derive(Debug, Default)]
pub struct Methods {
    methods: HashMap<Arc<str>, nir::DefId>,
}

impl Methods {
    pub fn insert(&mut self, name: Arc<str>, id: nir::DefId) {
        debug_assert!(!self.methods.contains_key(&name),
            "bug: inserted same method twice");
        self.methods.insert(name, id);
    }

    pub fn get(&self, name: &str) -> Option<nir::DefId> {
        self.methods.get(name).copied()
    }
}
