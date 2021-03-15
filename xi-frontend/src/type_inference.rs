use std::collections::BTreeMap;

use crate::desugar::{Judg_ment, Judg_mentKind, Var};
use xi_core::judgment::Judgment;
use xi_uuid::VarUuid;

fn to_judgment(judg_ment: Judg_ment) -> Judgment<T, S> {
    todo!()
}

fn to_type_var(judg_ment: Judg_ment) -> BTreeMap<Var, Judg_mentKind> {
    let judg_ment_kind = *judg_ment.0;
    match judg_ment_kind {
        Judg_mentKind::Type => {}
        Judg_mentKind::VarUuid(_) => {}
        Judg_mentKind::Fun(_, _) => {}
        Judg_mentKind::Pi(_, _) => {}
        Judg_mentKind::Lam(_, _) => {}
        Judg_mentKind::App(_, _) => {}
        Judg_mentKind::Bind(_, _, _) => {}
        Judg_mentKind::IdBind(_, _, _) => {}
        Judg_mentKind::StringLit(_) => {}
        Judg_mentKind::Iota(_) => {}
        Judg_mentKind::TypeVarUuid(_) => {}
    }
}

fn create_type_var(var: Var, ctx: &mut BTreeMap<Var, Var>) {
    let new_index = VarUuid::new();
    let name = var.name.clone();
    name.push_str("'s");
    let type_var = Var {
        index: new_index,
        var_type: None,
        name: name,
        span: var.span,
    };
    (*ctx).insert(var, type_var);
}
