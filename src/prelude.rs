use std::{sync::Arc, time::{SystemTime, UNIX_EPOCH}};

use crate::{
    bytecode,
    ty::Ty,
    interpreter::{Interpreter, RuntimeError},
    package::{Package, PkgId},
    prim::IntoNativeFunc,
    value::Value,
};

pub fn populate(pkg_id: PkgId, consts: &mut bytecode::Constants) -> Package {
    let mut package = Package::new(pkg_id, "prelude");

    insert_native_func_into_root_module(&mut package, consts, "clock", clock);

    package
}

fn insert_native_func_into_root_module<A>(
    package: &mut Package,
    consts: &mut bytecode::Constants,
    name: impl Into<Arc<str>>,
    f: impl IntoNativeFunc<A>,
) {
    let name = name.into();
    let func = f.into_native_func(&name);
    let ty = Ty::Func(Box::new(func.ty.clone()));
    insert_into_root_module(package, consts, name, ty, func)
}

fn insert_into_root_module(
    package: &mut Package,
    consts: &mut bytecode::Constants,
    name: impl Into<Arc<str>>,
    ty: Ty,
    value: impl Into<Value>,
) {
    let name = name.into();

    let const_id = consts.push(value.into());
    let def_id = package.insert(name.clone(), ty, const_id);
    package.root_module.decls.insert(name, def_id);
}

/// Returns the time since epoch in milliseconds
//TODO: We probably want to replace this with a better time interface
fn clock(_: &mut Interpreter) -> Result<i64, RuntimeError> {
    let time = SystemTime::now();
    let since_the_epoch = time
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards")
        .as_millis() as i64;

    Ok(since_the_epoch)
}
