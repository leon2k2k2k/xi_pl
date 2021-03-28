use std::rc::Rc;

use xi_backend::js_prim::JsPrim;
use xi_backend::runtime::RUNTIME_FILE;
use xi_core::judgment::Judgment;
use xi_frontend::type_inference::{UiMetadata, UiPrim};
use xi_uuid::VarUuid;

pub fn front_to_back(front: Judgment<UiPrim, UiMetadata>) -> Judgment<JsPrim, UiMetadata> {
    fn make_ffi(
        name: &str,
        var_type: Judgment<JsPrim, UiMetadata>,
    ) -> Judgment<JsPrim, UiMetadata> {
        let runtime = RUNTIME_FILE.into();
        Judgment::free(
            VarUuid::new(),
            var_type,
            Some(UiMetadata {
                ffi: Some((runtime, name.into())),
            }),
        )
    }

    use xi_backend::js_prim::JsPrim::*;
    use xi_frontend::type_inference::UiBinaryOp;
    use xi_proc_macro::term;

    let io_monad = make_ffi("IO", term!(U -> U));

    front.define_prim(Rc::new(move |s| match s {
        UiPrim::IOMonad => io_monad.clone(),
        UiPrim::IOBind => make_ffi(
            "io_bind",
            term!(Pi |A : U, B : U| {io_monad} A -> (A -> {io_monad} B) -> {io_monad} B),
        ),
        UiPrim::IOPure => make_ffi("io_pure", term!(Pi |A : U| A -> {io_monad} A)),
        UiPrim::StringElem(str) => Judgment::prim(JsPrim::StringElem(str), None),
        UiPrim::StringType => Judgment::prim(JsPrim::StringType, None),
        UiPrim::NumberElem(num) => Judgment::prim(JsPrim::NumberElem(num), None),
        UiPrim::NumberType => Judgment::prim(JsPrim::NumberType, None),
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
    }))
}
