use crate::ast;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(usize);

/// A table that maps `Ident`s to `DefId`s and back
///
/// Note that this only provides a mechanism for creating a `DefId` from an `Ident`. A separate
/// structure needs to be used to lookup the `DefId` for a given `Ident` value. This type is not
/// aware of scoping or name resolution.
#[derive(Debug, Default)]
pub struct DefTable {
    defs: Vec<ast::Ident>,
}

impl DefTable {
    /// Adds a new `Ident` to the table and returns its ID
    pub fn insert(&mut self, ident: ast::Ident) -> DefId {
        let id = DefId(self.defs.len());
        self.defs.push(ident);
        id
    }

    pub fn get(&self, id: DefId) -> &ast::Ident {
        let DefId(index) = id;

        // Safety: DefId indexes are valid by construction
        unsafe { self.defs.get_unchecked(index) }
    }
}
