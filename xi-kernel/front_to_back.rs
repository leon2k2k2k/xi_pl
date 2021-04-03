use std::rc::Rc;

use xi_backend::runtime::RUNTIME_FILE;
use xi_backend::{js_prim::JsPrim, output::JsMetadata};
use xi_core::judgment::{Judgment, Primitive};
use xi_frontend::type_inference::{UiMetadata, UiPrim};
use xi_uuid::VarUuid;

pub fn front_to_back(front: Judgment<UiPrim, UiMetadata>) -> Judgment<JsPrim, JsMetadata> {
    // In the backend, make some primitive function to FFIs.
    fn make_ffi(
        name: &str,
        var_type: Judgment<JsPrim, JsMetadata>,
    ) -> Judgment<JsPrim, JsMetadata> {
        let runtime = RUNTIME_FILE.into();
        let ffi = JsPrim::Ffi(runtime, name.into());

        Judgment::prim(ffi, var_type, None)
        // Judgment::free(
        //     VarUuid::new(),
        //     var_type,
        //     Some(JsMetadata {
        //         ffi: Some((runtime, name.into())),
        //     }),
        // )
    }

    use xi_backend::js_prim::JsPrim::*;
    use xi_frontend::type_inference::UiBinaryOp;
    use xi_proc_macro::term;

    let io_monad = make_ffi("IO", term!(U -> U));

    let changed_metadata = front.cast_metadata(Rc::new(|_: UiMetadata| JsMetadata()));

    changed_metadata.define_prim(Rc::new(
        move |s, prim_type, define_prim| -> Judgment<JsPrim, JsMetadata> {
            match s {
                UiPrim::IOMonad => io_monad.clone(),
                UiPrim::IOBind => make_ffi(
                    "io_bind",
                    term!(Pi |A : U, B : U| {io_monad} A -> (A -> {io_monad} B) -> {io_monad} B),
                ),
                UiPrim::IOPure => make_ffi("io_pure", term!(Pi |A : U| A -> {io_monad} A)),
                UiPrim::StringElem(str) => {
                    Judgment::prim_wo_prim_type(JsPrim::StringElem(str), None)
                }
                UiPrim::StringType => Judgment::prim_wo_prim_type(JsPrim::StringType, None),
                UiPrim::NumberElem(num) => {
                    Judgment::prim_wo_prim_type(JsPrim::NumberElem(num), None)
                }
                UiPrim::NumberType => Judgment::prim_wo_prim_type(JsPrim::NumberType, None),
                UiPrim::Binary(op) => match op {
                    UiBinaryOp::And => todo!(),
                    UiBinaryOp::Or => todo!(),
                    UiBinaryOp::Equal => todo!(),
                    UiBinaryOp::NotEqual => todo!(),
                    UiBinaryOp::LessThan => todo!(),
                    UiBinaryOp::LessThanEqual => todo!(),
                    UiBinaryOp::GreaterThan => todo!(),
                    UiBinaryOp::GreaterThanEqual => todo!(),
                    UiBinaryOp::Plus => {
                        make_ffi("plus", term!([NumberType] -> [NumberType] -> [NumberType]))
                    }
                    UiBinaryOp::Minus => {
                        make_ffi("minus", term!([NumberType] -> [NumberType] -> [NumberType]))
                    }
                    UiBinaryOp::Multiply => make_ffi(
                        "multiply",
                        term!([NumberType] -> [NumberType] -> [NumberType]),
                    ),
                    UiBinaryOp::Divide => todo!(),
                    UiBinaryOp::Modulo => make_ffi(
                        "modulo",
                        term!([NumberType] -> [NumberType] -> [NumberType]),
                    ),
                },
                UiPrim::Ffi(filename, ffi_name) => Judgment::prim(
                    JsPrim::Ffi(filename, ffi_name),
                    define_prim(prim_type),
                    None,
                ),
            }
        },
    ))
}
