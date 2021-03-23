use std::rc::Rc;

use xi_backend::jsprim::{JsIO, JsPrim, JsType};
use xi_core::judgment::Judgment;
use xi_frontend::type_inference::UiPrim;

pub fn front_to_back(front: Judgment<UiPrim, ()>) -> Judgment<JsPrim, ()> {
    use xi_proc_macro::term;
    use JsPrim::*;

    front.define_prim(Rc::new(|s| match s {
        UiPrim::IOMonad => {
            term!([IO(JsIO::IOMonad)])
        }
        UiPrim::IOBind => {
            term!([IO(JsIO::Bind)])
        }
        UiPrim::StringElem(str) => {
            term!([Type(JsType::Str(str))])
        }
        UiPrim::StringType => {
            term!([Type(JsType::StrType)])
        }
        UiPrim::UnitType => {
            term!([Type(JsType::UnitType)])
        }
        UiPrim::UnitElem => {
            term!([Type(JsType::Unit)])
        }
        UiPrim::ConsoleInput => {
            term!([IO(JsIO::ConsoleInput)])
        }
        UiPrim::ConsoleOutput => {
            term!([IO(JsIO::ConsoleOutput)])
        }
        UiPrim::IOPure => {
            term!([IO(JsIO::Pure)])
        }
    }))
}
