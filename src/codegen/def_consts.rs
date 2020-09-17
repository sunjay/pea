use std::collections::HashMap;

use crate::{nir, bytecode};

/// Map of `DefId` to `ConstId`
#[derive(Debug, Default)]
pub struct DefConsts {
    const_ids: HashMap<nir::DefId, bytecode::ConstId>,
}

impl DefConsts {
    pub fn insert(&mut self, def_id: nir::DefId, const_id: bytecode::ConstId) {
        assert!(!self.const_ids.contains_key(&def_id),
            "bug: defined constant for a `DefId` more than once");

        self.const_ids.insert(def_id, const_id);
    }

    pub fn get(&self, def_id: nir::DefId) -> bytecode::ConstId {
        self.const_ids.get(&def_id).copied()
            .expect("bug: no `ConstId` associated with the given `DefId`")
    }
}
