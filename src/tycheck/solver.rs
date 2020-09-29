use std::collections::HashSet;

use ena::unify::InPlaceUnificationTable;

use super::{constraints::TyVar, ty::{Ty, TySpan}};

pub fn apply_defaults(
    ty_var_table: &mut InPlaceUnificationTable<TyVar>,
    ty_vars: impl Iterator<Item=TyVar>,
    default_unit: &HashSet<TyVar>,
) {
    for ty_var in ty_vars {
        if ty_var_table.probe_value(ty_var).is_none() {
            if default_unit.contains(&ty_var) {
                ty_var_table.union_value(ty_var, Some(TySpan {ty: Ty::Unit, span: None}));
            }
        }
    }
}
