use xi_core::judgment::{Judgment, Metadata, Primitive};
use crate::output::{ JsOutput};
use xi_uuid::VarUuid;
use swc_ecma_ast::{
    Expr
};
use xi_proc_macro::term;

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
    fn type_of<S: Metadata>(&self) -> Judgment<Self, S> {
        use JsPrim::*;

        match self {
            JsPrim::IO(iomonad) => {
                use JsIO::*;
                match iomonad {
                    ConsoleOutput => {
                        term!([Type(JsType::StrType)] -> [IO(JsIO::IOMonad)] [Type(JsType::UnitType)])
                    }
                    ConsoleInput => term!([IO(IOMonad)](
                        [Type(JsType::StrType)]
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

impl JsPrim{
    pub fn promise_unit<S: Metadata>() -> Judgment<JsPrim, S> {
        term!([JsPrim::Promise(JsPromise::PromiseMonad)] [JsPrim::Type(JsType::UnitType)])
    }

    pub fn str_to_promise_unit<S: Metadata>() -> Judgment<JsPrim, S>{
        term!([JsPrim::Type(JsType::StrType)] -> ([JsPrim::Promise(JsPromise::PromiseMonad)] [JsPrim::Type(JsType::UnitType)]))
    }


    pub fn output_str(str: String) -> String {
        use JsPrim::*;
        use crate::output;
        let str_judgment = term!([Type(JsType::Str(str))]);

        let func = term!((Lam |func1 : {JsPrim::str_to_promise_unit()}| 
                ([IO(JsIO::Pure)] {JsPrim::promise_unit()} (func1 {str_judgment}))));

        let str_expression:Judgment<JsPrim, ()> = term!([IO(JsIO::Bind)]{JsPrim::str_to_promise_unit()} {JsPrim::promise_unit()} [IO(JsIO::ConsoleOutput)] {func});
        // println!("type of str_expression: {:?}", str_expression.type_of());
        output::to_js_program(str_expression)
        
    }

    // pub fn iobind<S:Metadata>(input : Judgment<JsPrim, S>, output : Judgment<JsPrim,S>) -> Judgment<JsPrim,S> {
    //     use JsPrim::*;
    //     use crate::output;
    //     let input_type =  match input.type_of(){
    //         Some(input_type) => {
    //             match input_type.tree{

    //                 xi_core::judgment::JudgmentKind::Application(monad, _type) => {
    //                     if let JudgmentKind::Prim(JsPrim::IO(IOMonad)) = monad.tree {
    //                         _type
    //                     } else{
    //                         panic!("the type of input should be iobind sometype")
    //                     }
    //                 }
    //                 _ => panic!("the type of inpy should be IOBind sometype")
    //             }
    //         }
    //         None => panic!("this shouldn't happen")
    //     };

    //     if let JudgmentKind::Pi(input_type, expr) = output.type_of().unwrap().tree{
    //         if let JudgmentKind::Application(monad, outputtype)
    //     }
        
    //     let ans: Judgment<JsPrim, S>  = term!([IO(JsIO::Bind)]);

    // }
    
    pub fn console_output1() -> Judgment<JsPrim, ()> {
        use JsPrim::*;
        term!(Lam | input : [Type(JsType:: StrType)] | [IO(JsIO::Bind)] 
        [Type(JsType::UnitType)] [Type(JsType::UnitType)]
                    ([IO(JsIO::ConsoleOutput)] input) 
                    (Lam | star : [Type(JsType::UnitType)]| [IO(JsIO::Pure)] [Type(JsType::UnitType)] [Type(JsType::Unit)]))
    }

    // pub fn output_inpt() -> String{
    //     use JsPrim::*;
    //     use crate::output;

    //     let func2 = term!(Lam |output : {JsPrim::str_to_promise_unit()}| [IO(JsIO::Pure)] {JsPrim::promise_unit()}(help1));


    //     let help1 = term!([IO(JsIO::Pure)] {JsPrim::promise_unit()} () );
    //     let func1 = term!(|output: {JsPrim::str_to_promise_unit()} | Lam |input : [Type(JsType::StrType)]| output input);




    //     output::to_js_program(answer);
    // }
}



        // let hello_world = term!([IO(JsIO::ConsoleOutput)][Type(JsType::Str("hello world".into()))]);
        // let str: Judgment<JsPrim> = term!([Type(JsType::Str("hello world".into()))]);
        // let hello_world = term!([IO(JsIO::Bind)] ([Type(JsType::StrType)] -> ([Promise(JsPromise::PromiseMonad)] [Type(JsType::UnitType)]))
        //     ([Promise(JsPromise::PromiseMonad)] [Type(JsType::UnitType)]) [IO(JsIO::ConsoleOutput)] 
        //     (Lam |func : [Type(JsType::StrType)] -> ([Promise(JsPromise::PromiseMonad)] [Type(JsType::UnitType)])| ([IO(JsIO::Pure)] ([Promise(JsPromise::PromiseMonad)] [Type(JsType::UnitType)]) (func {str}))));
        // let s = to_js_program(hello_world);
        // assert_eq!(s, "console.log(\"hello world\");\n");

// mod test {

//     #[tokio::test]
//     async fn to_js_test() {
//         use super::*;
//         use crate::runtime::run_js_from_string;
//         // use super::JsPrim::*;
//         // use super::*;
//         // use crate::output::to_js_program;
//         // use term_macro::term;
//         // use crate::runtime::*;

//         // // let hello_world = term!([IO(JsIO::ConsoleOutput)][Type(JsType::Str("hello world".into()))]);
//         // let str: Judgment<JsPrim> = term!([Type(JsType::Str("hello world".into()))]);
//         // let s = to_js_program(hello_world);
//         // run_js_from_string(s).await.unwrap();
//         // // assert_eq!(s, "console.log(\"hello world\");\n");


//         // let hello_world = term!([IO(JsIO::ConsoleOutput)][Type(JsType::Str("hello world".into()))]);
//         // let str: Judgment<JsPrim> = term!([Type(JsType::Str("hello world".into()))]);
//         // let hello_world = term!([IO(JsIO::Bind)] ([Type(JsType::StrType)] -> ([Promise(JsPromise::PromiseMonad)] [Type(JsType::UnitType)]))
//         //     ([Promise(JsPromise::PromiseMonad)] [Type(JsType::UnitType)]) [IO(JsIO::ConsoleOutput)] 
//         //     (Lam |func : [Type(JsType::StrType)] -> ([Promise(JsPromise::PromiseMonad)] [Type(JsType::UnitType)])| ([IO(JsIO::Pure)] ([Promise(JsPromise::PromiseMonad)] [Type(JsType::UnitType)]) (func {str}))));
//         // let s = to_js_program(hello_world);
//         let s = JsPrim::output_str("frank is stupid\n".into());
//         run_js_from_string(s).await.unwrap();
//         // assert_eq!(s, "console.log(\"hello world\");\n");

//         // let id: Judgment<JsPrim> = term!(Lam | T: U, t: T | t);
//         // let hello = term!({ id }[Type(JsType::StrType)][Type(JsType::Str("hello".into()))]);
//         // println!("{:?}", hello);
//         // let s2 = to_js_program(hello);
//         // panic!(s2);
//     }
// }

