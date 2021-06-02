use std::collections::BTreeMap;

use xi_core::judgment::{Judgment, Primitive};
use xi_uuid::VarUuid;

use crate::py_backend::py_output::{
    make_var_name, promise_resolve, to_py_ident, to_py_num, to_py_str, Expr,
};

use super::py_output::to_py_ident1;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum PyPrim {
    StringType,
    NumberType,
    StringElem(String),
    NumberElem(String),
    Ffi(String, String),
    Var(VarUuid),
}

impl Primitive for PyPrim {
    fn maybe_prim_type(&self) -> Option<Judgment<Self>> {
        use PyPrim::*;

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

impl PyPrim {
    pub fn to_py_prim(&self, ffi: &mut BTreeMap<(String, String), VarUuid>) -> Expr {
        match self {
            PyPrim::StringType => promise_resolve(to_py_str("String")),
            PyPrim::NumberType => promise_resolve(to_py_str("Number")),
            PyPrim::StringElem(str) => promise_resolve(to_py_str(str.clone())),
            PyPrim::NumberElem(num) => promise_resolve(to_py_num(num.clone())),
            PyPrim::Ffi(filename, ffi_name) => {
                let var = match ffi.get(&(filename.clone(), ffi_name.clone())) {
                    Some(var) => *var,
                    None => {
                        let var = VarUuid::new();
                        ffi.insert((filename.clone(), ffi_name.clone()), var);
                        var
                    }
                };

                to_py_ident(format!("ffi{}", var.index()))
            }
            PyPrim::Var(index) => to_py_ident1(make_var_name(*index)),
        }
    }
}

#[derive(Clone, Debug)]
pub struct PyModule {
    pub str_to_index: BTreeMap<String, VarUuid>,
    pub module_items: BTreeMap<VarUuid, PyModuleItem>,
}

#[derive(Clone, Debug)]

pub struct PyModuleAndImports {
    pub module: PyModule,
    pub imports: BTreeMap<VarUuid, PyModuleAndImports>,
}

#[derive(Clone, Debug)]
pub enum PyModuleItem {
    Define(PyDefineItem),
    // Import(JsImportItem),
}

impl PyModuleItem {
    pub fn type_(&self) -> Judgment<PyPrim> {
        match self {
            PyModuleItem::Define(define_item) => define_item.type_.clone(),
            // JsModuleItem::Import(import_item) => import_item.type_.clone(),
        }
    }

    pub fn transport_info(&self) -> TransportInfo {
        match self {
            PyModuleItem::Define(define_item) => define_item.transport_info.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct PyDefineItem {
    pub name: String,
    pub transport_info: TransportInfo,
    pub type_: Judgment<PyPrim>,
    pub impl_: Judgment<PyPrim>,
}

#[derive(Clone, Debug)]
pub struct TransportInfo {
    pub origin: Option<String>,
    pub transport: Option<String>,
}

impl TransportInfo {
    pub fn none() -> TransportInfo {
        TransportInfo {
            origin: None,
            transport: None,
        }
    }

    pub fn only_origin(origin: String) -> TransportInfo {
        TransportInfo {
            origin: Some(origin),
            transport: None,
        }
    }

    pub fn origin_and_transport(origin: String, tranport: String) -> TransportInfo {
        TransportInfo {
            origin: Some(origin),
            transport: Some(tranport),
        }
    }
}
