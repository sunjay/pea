use std::sync::Arc;

use super::DefId;

/// A single scope and all the names defined in it
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Scope {
    /// The names defined in this scope, in the order that they were defined
    names: Vec<(Arc<str>, DefId)>,
}

impl Scope {
    pub fn define(&mut self, name: Arc<str>, def_id: DefId) {
        self.names.push((name, def_id));
    }

    pub fn lookup(&self, target: &str) -> Option<DefId> {
        self.names.iter().rev().find(|(name, _)| &**name == target).map(|&(_, id)| id)
    }

    pub fn iter(&self) -> impl Iterator<Item=&(Arc<str>, DefId)> {
        self.names.iter()
    }
}
