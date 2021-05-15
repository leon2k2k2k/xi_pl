use std::collections::BTreeMap;

use swc_ecma_ast::Expr;
use xi_core::judgment::{Judgment, Primitive};
use xi_uuid::VarUuid;

use crate::output::{make_var_name, to_js_ident, to_js_num, to_js_str, JsMetadata};

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum JsPrim {
    StringType,
    NumberType,
    StringElem(String),
    NumberElem(String),
    Ffi(String, String),
    Var(VarUuid),
}

impl Primitive for JsPrim {
    fn maybe_prim_type<S: xi_core::judgment::Metadata>(
        &self,
    ) -> Option<xi_core::judgment::Judgment<Self, S>> {
        use JsPrim::*;

        match self {
            StringType => Some(Judgment::u(None)),
            NumberType => Some(Judgment::u(None)),
            StringElem(_) => Some(Judgment::prim_wo_prim_type(StringType, None)),
            NumberElem(_) => Some(Judgment::prim_wo_prim_type(NumberType, None)),
            Ffi(_, _) => None,
            Var(_) => None,
        }
    }
}

impl JsPrim {
    pub fn to_js_prim(&self, ffi: &mut BTreeMap<(String, String), VarUuid>) -> Expr {
        match self {
            JsPrim::StringType => to_js_ident("String".into()),
            JsPrim::NumberType => to_js_ident("Number".into()),
            JsPrim::StringElem(str) => to_js_str(str.clone()),
            JsPrim::NumberElem(num) => to_js_num(num.clone()),
            JsPrim::Ffi(filename, ffi_name) => {
                let var = match ffi.get(&(filename.clone(), ffi_name.clone())) {
                    Some(var) => *var,
                    None => {
                        let var = VarUuid::new();
                        ffi.insert((filename.clone(), ffi_name.clone()), var);
                        var
                    }
                };

                to_js_ident(format!("ffi{}", var.index()))
            }
            JsPrim::Var(index) => Expr::Ident(make_var_name(*index)),
        }
    }
}

#[derive(Clone, Debug)]
pub struct JsModule {
    pub str_to_index: BTreeMap<String, VarUuid>,
    pub module_items: BTreeMap<VarUuid, JsModuleItem>,
}

#[derive(Clone, Debug)]

pub struct JsModuleAndImports {
    pub module: JsModule,
    pub imports: BTreeMap<VarUuid, JsModuleAndImports>,
}

#[derive(Clone, Debug)]
pub enum JsModuleItem {
    Define(JsDefineItem),
    // Import(JsImportItem),
}

impl JsModuleItem {
    pub fn type_(&self) -> Judgment<JsPrim, JsMetadata> {
        match self {
            JsModuleItem::Define(define_item) => define_item.type_.clone(),
            // JsModuleItem::Import(import_item) => import_item.type_.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct JsDefineItem {
    pub name: String,
    pub backend: Option<String>,
    pub type_: Judgment<JsPrim, JsMetadata>,
    pub impl_: Judgment<JsPrim, JsMetadata>,
    // pub publicity
}
// #[derive(Clone, Debug)]

// pub struct ImportItem {
//     pub name: String,
//     pub type_: Judgment<UiPrim, UiMetadata>,
//     pub import_info: (VarUuid, VarUuid), // (file_name, func_index)
// }
