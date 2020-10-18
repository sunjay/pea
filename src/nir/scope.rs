use crate::gc::Gc;

use super::DefId;

/// A single scope and all the names defined in it
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Scope {
    /// The names defined in this scope, in the order that they were defined
    names: Vec<(Gc<str>, DefId)>,
}

impl Scope {
    pub fn define(&mut self, name: Gc<str>, def_id: DefId) {
        self.names.push((name, def_id));
    }

    pub fn lookup(&self, target: &str) -> Option<DefId> {
        self.names.iter().rev().find(|(name, _)| &**name == target).map(|&(_, id)| id)
    }

    pub fn iter(&self) -> impl Iterator<Item=&(Gc<str>, DefId)> {
        self.names.iter()
    }
}
