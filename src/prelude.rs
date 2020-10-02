use std::sync::Arc;

use crate::{bytecode, package::{Package, PkgId}, value::Value};

pub fn populate(pkg_id: PkgId, consts: &mut bytecode::Constants) -> Package {
    let package = Package::new(pkg_id, "prelude");

    package
}

fn insert_into_root_module(
    package: &mut Package,
    consts: &mut bytecode::Constants,
    name: impl Into<Arc<str>>,
    value: impl Into<Value>,
) {
    let name = name.into();

    let const_id = consts.push(value.into());
    let def_id = package.insert(name.clone(), const_id);
    package.root_module.decls.insert(name, def_id);
}
