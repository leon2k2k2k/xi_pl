/*  */
use std::collections::BTreeMap;

use crate::desugar::{text_to_judg_ment, Judg_ment, Judg_mentKind, Var};
use xi_core::judgment::Judgment;
use xi_uuid::VarUuid;

fn to_judgment(judg_ment: Judg_ment) -> Judgment<(), ()> {
    todo!()
}

fn to_type_var(judg_ment: Judg_ment) -> BTreeMap<Var, Judg_mentKind> {
    let mut ctx = BTreeMap::new();

    fn to_type_var_rec(judg_ment: Judg_ment, mut ctx: BTreeMap<Var, Judg_mentKind>) {
        let judg_ment_kind = *judg_ment.0;
        match judg_ment_kind {
            Judg_mentKind::Type => {}
            Judg_mentKind::VarUuid(var) => create_type_var(var, &mut ctx),
            Judg_mentKind::Fun(source, target) => {
                create_type_var(source, ctx);
                create_type_var(target, ctx);
            }
            Judg_mentKind::Pi(_, _) => {}
            Judg_mentKind::Lam(_, _) => {}
            Judg_mentKind::App(_, _) => {}
            Judg_mentKind::Bind(_, _, _) => todo!(),
            Judg_mentKind::IdBind(_, _, _) => {}
            Judg_mentKind::StringLit(_) => {}
            Judg_mentKind::Iota(_) => {}
            Judg_mentKind::TypeVarUuid(_) => todo!("UI shouldn't access type variables"),
            Judg_mentKind::None => todo!("UI shouldn't access None"),
        }
    }
    to_type_var_rec(judg_ment, ctx);
    ctx
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
    ctx.insert(var, type_var);
}

fn front_end(text: &str) -> Judgment<(), ()> {
    let judg_ment = text_to_judg_ment(text);
    to_judgment(judg_ment)
}

mod test {

    #[test]
    fn test_to_judgment() {
        use super::*;
        let text1 = "fn foo |x : Nat| -> {val x}";
        let judgment1 = front_end(text1);
        dbg!(judgment1);

        let text2 = "fn foo |x| -> {val x} val foo \"hello world\" ";
        let judgment2 = front_end(text2);
        dbg!(judgment2);

        let text3 = "let y = \"hello world\"  let x = y";
        let judgment3 = front_end(text3);
        dbg!(judgment3);
    }
}
