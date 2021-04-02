use xi_core::judgment::{Judgment, Primitive};

use crate::output::{to_js_ident, to_js_num, to_js_str};

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum JsPrim {
    StringType,
    NumberType,
    StringElem(String),
    NumberElem(String),
    Ffi(String, String),
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
        }
    }
}

impl JsPrim {
    pub fn to_js_prim(&self, ffi: &mut Vec<(String, String)>) -> swc_ecma_ast::Expr {
        match self {
            JsPrim::StringType => to_js_ident("String".into()),
            JsPrim::NumberType => to_js_ident("Number".into()),
            JsPrim::StringElem(str) => to_js_str(str.clone()),
            JsPrim::NumberElem(num) => to_js_num(num.clone()),
            JsPrim::Ffi(filename, ffi_name) => {
                ffi.push((filename.clone(), ffi_name.clone()));
                to_js_ident(format!("ffi{}", ffi.len() - 1))
            }
        }
    }
}
