use crate::package::PkgId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId {
    pkg_id: PkgId,
    id: usize
}

impl DefId {
    pub fn pkg(self) -> PkgId {
        self.pkg_id
    }
}

#[derive(Debug)]
pub struct DefIdGen {
    pkg_id: PkgId,
    next_def_id: usize,
}

impl DefIdGen {
    pub fn new(pkg_id: PkgId) -> Self {
        Self {
            pkg_id,
            next_def_id: 0,
        }
    }

    pub fn next_id(&mut self) -> DefId {
        let id = self.next_def_id;
        self.next_def_id += 1;

        DefId {
            pkg_id: self.pkg_id,
            id,
        }
    }
}
