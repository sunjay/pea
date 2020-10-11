use std::collections::HashMap;

use crate::{nir, ty::Ty};

/// Map of `DefId` to `Ty`
#[derive(Debug, Default)]
pub struct DefTys {
    tys: HashMap<nir::DefId, Ty>,
}

impl DefTys {
    pub fn insert(&mut self, def_id: nir::DefId, ty: Ty) {
        assert!(!self.tys.contains_key(&def_id),
            "bug: defined type for a `DefId` more than once");

        self.tys.insert(def_id, ty);
    }

    pub fn get(&self, def_id: nir::DefId) -> Option<&Ty> {
        self.tys.get(&def_id)
    }
}
