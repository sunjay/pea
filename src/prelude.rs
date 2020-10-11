mod prim_methods;

pub use prim_methods::*;

use std::{sync::Arc, time::{SystemTime, UNIX_EPOCH}};

use crate::{
    bytecode,
    ty::Ty,
    gc::Gc,
    interpreter::{Interpreter, RuntimeError},
    package::{Package, PkgId},
    prim::{self, IntoNativeFunc},
    value::Value,
};

pub fn populate(pkg_id: PkgId, consts: &mut bytecode::Constants) -> (Package, PrimMethods) {
    let mut package = Package::new(pkg_id, "prelude");

    insert_native_func_into_root_module(&mut package, consts, "clock", clock);

    let mut prim_methods = PrimMethods::default();
    populate_prim_methods(&mut package, consts, &mut prim_methods);

    (package, prim_methods)
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

fn populate_prim_methods(
    package: &mut Package,
    consts: &mut bytecode::Constants,
    prim_methods: &mut PrimMethods,
) {
    populate_unit_methods(package, consts, &mut prim_methods.unit);
    populate_bool_methods(package, consts, &mut prim_methods.bool);
    populate_i64_methods(package, consts, &mut prim_methods.i64);
    populate_u8_methods(package, consts, &mut prim_methods.u8);
    populate_list_methods(package, consts, &mut prim_methods.list);
}

fn populate_unit_methods(
    package: &mut Package,
    consts: &mut bytecode::Constants,
    methods: &mut Methods,
) {
    insert_method(package, consts, methods, "==", |_: &mut _, x: (), y: ()| Ok(x == y));
    insert_method(package, consts, methods, "!=", |_: &mut _, x: (), y: ()| Ok(x != y));
}

fn populate_bool_methods(
    package: &mut Package,
    consts: &mut bytecode::Constants,
    methods: &mut Methods,
) {
    insert_method(package, consts, methods, "not", |_: &mut _, x: bool| Ok(!x));

    insert_method(package, consts, methods, "==", |_: &mut _, x: bool, y: bool| Ok(x == y));
    insert_method(package, consts, methods, "!=", |_: &mut _, x: bool, y: bool| Ok(x != y));
}

fn populate_i64_methods(
    package: &mut Package,
    consts: &mut bytecode::Constants,
    methods: &mut Methods,
) {
    insert_method(package, consts, methods, "neg", |_: &mut _, x: i64| Ok(-x));
    // unary `+` has no effect on integers
    insert_method(package, consts, methods, "pos", |_: &mut _, x: i64| Ok(x));

    insert_method(package, consts, methods, "add", |_: &mut _, x: i64, y: i64| Ok(x + y));
    insert_method(package, consts, methods, "sub", |_: &mut _, x: i64, y: i64| Ok(x - y));
    insert_method(package, consts, methods, "mul", |_: &mut _, x: i64, y: i64| Ok(x * y));
    insert_method(package, consts, methods, "div", |_: &mut _, x: i64, y: i64| Ok(x / y));
    insert_method(package, consts, methods, "rem", |_: &mut _, x: i64, y: i64| Ok(x % y));

    insert_method(package, consts, methods, "==", |_: &mut _, x: i64, y: i64| Ok(x == y));
    insert_method(package, consts, methods, "!=", |_: &mut _, x: i64, y: i64| Ok(x != y));
    insert_method(package, consts, methods, ">", |_: &mut _, x: i64, y: i64| Ok(x > y));
    insert_method(package, consts, methods, ">=", |_: &mut _, x: i64, y: i64| Ok(x >= y));
    insert_method(package, consts, methods, "<", |_: &mut _, x: i64, y: i64| Ok(x < y));
    insert_method(package, consts, methods, "<=", |_: &mut _, x: i64, y: i64| Ok(x <= y));
}

fn populate_u8_methods(
    package: &mut Package,
    consts: &mut bytecode::Constants,
    methods: &mut Methods,
) {
    // unary `+` has no effect on integers
    insert_method(package, consts, methods, "pos", |_: &mut _, x: u8| Ok(x));

    insert_method(package, consts, methods, "add", |_: &mut _, x: u8, y: u8| Ok(x + y));
    insert_method(package, consts, methods, "sub", |_: &mut _, x: u8, y: u8| Ok(x - y));
    insert_method(package, consts, methods, "mul", |_: &mut _, x: u8, y: u8| Ok(x * y));
    insert_method(package, consts, methods, "div", |_: &mut _, x: u8, y: u8| Ok(x / y));
    insert_method(package, consts, methods, "rem", |_: &mut _, x: u8, y: u8| Ok(x % y));

    insert_method(package, consts, methods, "==", |_: &mut _, x: u8, y: u8| Ok(x == y));
    insert_method(package, consts, methods, "!=", |_: &mut _, x: u8, y: u8| Ok(x != y));
    insert_method(package, consts, methods, ">", |_: &mut _, x: u8, y: u8| Ok(x > y));
    insert_method(package, consts, methods, ">=", |_: &mut _, x: u8, y: u8| Ok(x >= y));
    insert_method(package, consts, methods, "<", |_: &mut _, x: u8, y: u8| Ok(x < y));
    insert_method(package, consts, methods, "<=", |_: &mut _, x: u8, y: u8| Ok(x <= y));
}

fn populate_list_methods(
    package: &mut Package,
    consts: &mut bytecode::Constants,
    methods: &mut Methods,
) {
    //TODO: Can't add any generic methods because the type system doesn't support polymorphism yet
    insert_method(package, consts, methods, "add",
        |_: &mut _, x: Gc<prim::Bytes>, y: Gc<prim::Bytes>| Ok(Gc::new(x.add(&y))));

    insert_method(package, consts, methods, "==", |_: &mut _, x: Gc<prim::Bytes>, y: Gc<prim::Bytes>| Ok(x == y));
    insert_method(package, consts, methods, "!=", |_: &mut _, x: Gc<prim::Bytes>, y: Gc<prim::Bytes>| Ok(x != y));
}

fn insert_method<A>(
    package: &mut Package,
    consts: &mut bytecode::Constants,
    methods: &mut Methods,
    name: impl Into<Arc<str>>,
    f: impl IntoNativeFunc<A>,
) {
    let name = name.into();

    let func = f.into_native_func(&name);
    let ty = Ty::Func(Box::new(func.ty.clone()));
    let const_id = consts.push(func.into());
    let def_id = package.insert(name.clone(), ty, const_id);
    methods.insert(name, def_id);
}
