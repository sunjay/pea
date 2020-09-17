use std::{collections::HashMap, sync::Arc};

use super::DefId;

/// A single scope and all the variables defined in it
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Scope {
    pub functions: HashMap<Arc<str>, DefId>,
}
