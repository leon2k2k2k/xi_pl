use xi_core::judgment::{Judgment, Primitive};

use crate::output::{to_js_ident, to_js_num, to_js_str, JsOutput};

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum JsPrim {
    StringType,
    NumberType,
    StringElem(String),
    NumberElem(String),
}

impl Primitive for JsPrim {
    fn type_of<S: xi_core::judgment::Metadata>(&self) -> xi_core::judgment::Judgment<Self, S> {
        use JsPrim::*;

        match self {
            StringType => Judgment::u(None),
            NumberType => Judgment::u(None),
            StringElem(_) => Judgment::prim(StringType, None),
            NumberElem(_) => Judgment::prim(NumberType, None),
        }
    }
}

impl JsOutput for JsPrim {
    fn to_js_prim(&self) -> swc_ecma_ast::Expr {
        match self {
            JsPrim::StringType => to_js_ident("String".into()),
            JsPrim::NumberType => to_js_ident("Number".into()),
            JsPrim::StringElem(str) => to_js_str(str.clone()),
            JsPrim::NumberElem(num) => to_js_num(num.clone()),
        }
    }
}
