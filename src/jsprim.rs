use crate::judgment::{Judgment, Primitive};
use crate::output::{ JsOutput};
use free_var::FreeVar;
use swc_ecma_ast::{
    Expr
};
use term_macro::term;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum JsPrim {
    IO(JsIO),
    Promise(JsPromise),
    Type(JsType),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum JsType {
    Str(String),
    StrType,
    UnitType,
    Unit,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum JsIO {
    IOMonad,
    Bind,
    Pure,
    ConsoleOutput,
    ConsoleInput,
}
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum JsPromise {
    PromiseMonad,
    Bind,
    Pure,
}

impl Primitive for JsPrim {
    fn type_of(&self) -> Judgment<Self> {
        use JsPrim::*;

        match self {
            JsPrim::IO(iomonad) => {
                use JsIO::*;
                match iomonad {
                    ConsoleOutput => {
                        term!([IO(IOMonad)] ([Type(JsType::StrType)] -> ([Promise(JsPromise::PromiseMonad)] [Type(JsType::UnitType)])))
                    }
                    ConsoleInput => term!([IO(IOMonad)](
                        [Promise(JsPromise::PromiseMonad)][Type(JsType::StrType)]
                    )),
                    IOMonad => term!(U -> U),
                    Bind => {
                        term!(Pi | T : U, S : U| [IO(IOMonad)] T -> (T -> [IO(IOMonad)] S) -> [IO(IOMonad)] S)
                    }
                    Pure => term!(Pi |T : U| T -> [IO(IOMonad)] T),
                }
            }
            JsPrim::Promise(promise) => {
                use JsPromise::*;
                match promise {
                    JsPromise::PromiseMonad => term!(U -> U),
                    JsPromise::Bind => {
                        term!(Pi |T : U, S : U| [Promise(PromiseMonad)] T -> (T -> [Promise(PromiseMonad)] S) -> [Promise(PromiseMonad)] S)
                    }
                    JsPromise::Pure => term!(Pi |T : U| T -> [Promise(PromiseMonad)] T),
                }
            }
            JsPrim::Type(js_type) => {
                use JsType::*;
                match js_type {
                    JsType::Str(_str) => term!([Type(StrType)]),

                    JsType::StrType => term!(U),
                    JsType::UnitType => term!(U),
                    JsType::Unit => term!([Type(UnitType)]),
                }
            }
        }
    }
}

impl JsOutput for JsPrim {
    fn to_js_prim(&self) -> Expr {
        use crate::output::runtime_ident;
        use crate::output::to_js_str;

        use JsPrim::*;
        match self {
            IO(iomonad) => match iomonad {
                JsIO::IOMonad => runtime_ident("id".into()),
                JsIO::Bind => runtime_ident("io_bind".into()),
                JsIO::Pure => runtime_ident("io_pure".into()),
                JsIO::ConsoleOutput => runtime_ident("console_output".into()),
                
                JsIO::ConsoleInput => runtime_ident("console_input".into()),
            },
            Promise(promise) => match promise {
                JsPromise::PromiseMonad => runtime_ident("id".into()),
                JsPromise::Bind => runtime_ident("promise_bind".into()),
                JsPromise::Pure => runtime_ident("promise_pure".into()),
            },
            Type(js_type) => match js_type {
                JsType::Str(str) => to_js_str(str.clone()),
                JsType::StrType =>  to_js_str("StrType".into()),
                JsType::UnitType =>  to_js_str("UnitType".into()),
                JsType::Unit =>  to_js_str("Unit".into()),
            },
        }
    }
}

mod test {

    #[tokio::test]
    async fn to_js_test() {
        use super::JsPrim::*;
        use super::*;
        use crate::output::to_js_program;
        use term_macro::term;
        use crate::runtime::*;

        // // let hello_world = term!([IO(JsIO::ConsoleOutput)][Type(JsType::Str("hello world".into()))]);
        // let str: Judgment<JsPrim> = term!([Type(JsType::Str("hello world".into()))]);
        // let hello_world = term!([IO(JsIO::Bind)] ([Type(JsType::StrType)] -> ([Promise(JsPromise::PromiseMonad)] [Type(JsType::UnitType)]))
        //     ([Promise(JsPromise::PromiseMonad)] [Type(JsType::UnitType)]) [IO(JsIO::ConsoleOutput)] 
        //     (Lam |func : [Type(JsType::StrType)] -> ([Promise(JsPromise::PromiseMonad)] [Type(JsType::UnitType)])| ([IO(JsIO::Pure)] ([Promise(JsPromise::PromiseMonad)] [Type(JsType::UnitType)]) (func {str}))));
        // let s = to_js_program(hello_world);
        // run_js_from_string(s).await.unwrap();
        // // assert_eq!(s, "console.log(\"hello world\");\n");

        let id: Judgment<JsPrim> = term!(Lam | T: U, t: T | t);
        let hello = term!({ id }[Type(JsType::StrType)][Type(JsType::Str("hello".into()))]);
        println!("{:?}", hello);
        let s2 = to_js_program(hello);
        panic!(s2);
    }
}
