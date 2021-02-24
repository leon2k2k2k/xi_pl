use crate::xi_syntax::{Judgment, Primitive};
use free_var::FreeVar;
use std::rc::Rc;
use swc_common::{FilePathMapping, SourceMap, DUMMY_SP};
use swc_ecma_ast::{
    ArrowExpr, BindingIdent, BlockStmtOrExpr, CallExpr, Expr, ExprOrSpread, ExprOrSuper, ExprStmt,
    Ident, Lit, MemberExpr, Pat, Script, Stmt, Str,
};
use swc_ecma_codegen::{text_writer::JsWriter, Config, Emitter};
use term_macro::term;

pub fn to_js_program<T: JsOutput>(judgment: Judgment<T>) -> String {
    let stmt = Stmt::Expr(ExprStmt {
        span: DUMMY_SP,
        expr: Box::new(to_js(judgment, vec![])),
    });

    let script: Script = Script {
        span: DUMMY_SP,
        body: vec![stmt],
        shebang: None,
    };

    let mut output_buf = vec![];
    {
        let cm = Rc::new(SourceMap::new(FilePathMapping::new(vec![])));
        let mut emitter = Emitter {
            cfg: Config { minify: false },
            cm: Rc::clone(&cm),
            comments: None,
            wr: Box::new(JsWriter::new(Rc::clone(&cm), "\n", &mut output_buf, None)),
        };

        emitter.emit_script(&script).unwrap();
    }

    std::str::from_utf8(output_buf.as_slice()).unwrap().into()
}

fn make_var_name(ctx: &Vec<Ident>) -> Ident {
    to_js_ident2(format!("var_{}", ctx.len()))
}
fn to_js<T: JsOutput>(judgment: Judgment<T>, ctx: Vec<Ident>) -> Expr {
    match judgment {
        Judgment::UInNone => to_js_str_u(),
        Judgment::Prim(t) => 
        {JsOutput::to_js_prim(&t)},
        Judgment::FreeVar(_, _) => panic!("Should not have a FreeVar in this expression"),
        Judgment::Pi(_, _) => to_js_str_pi(),
        Judgment::Lam(_var_type, body) => {
            let var_name = make_var_name(&ctx);
            to_js_lam(
                swc_ecma_ast::BlockStmtOrExpr::Expr(Box::new(to_js(
                    *body,
                    add_to_ctx(ctx, var_name.clone()),
                ))),
                vec![var_name],
            )
        }
        Judgment::BoundVar(index, _var_type) => {
            to_js_ident(ctx[ctx.len() - 1 - index as usize].clone())
        }
        Judgment::Application(func, arg) => {
            if let Judgment::Prim(prim) = *func {
                // if prim.app_special().is_none() {
                //     to_js_app(to_js(*func, ctx.clone()), to_js(*arg, ctx))
                // } else {
                //     todo!()
                // }
                todo!();
            } else {
                to_js_app(to_js(*func, ctx.clone()), vec![to_js(*arg, ctx)])
            }
        }
    }
}

fn add_to_ctx<T: Clone>(v: Vec<T>, t: T) -> Vec<T> {
    let mut v2 = v.clone();
    v2.push(t);
    v2
}

fn to_js_lam(body: BlockStmtOrExpr, var_names: Vec<Ident>) -> Expr {
    // let pat = Pat::Ident(BindingIdent {
    //     id: var_name,
    //     type_ann: None,
    // });
    let pats = var_names
        .iter()
        .map(|var_name| {
            Pat::Ident(BindingIdent {
                id: *var_name,
                type_ann: None,
            })
        })
        .collect();
    let lam = ArrowExpr {
        span: DUMMY_SP,
        params: pats,
        body: body,
        is_async: false,
        is_generator: false,
        type_params: None,
        return_type: None,
    };

    Expr::Arrow(lam)
}

fn to_js_ident2(name: String) -> Ident {
    Ident {
        span: DUMMY_SP,
        sym: name.into(),
        optional: false,
    }
}

fn to_js_ident(var_name: Ident) -> Expr {
    Expr::Ident(var_name)
}

fn to_js_app(func: Expr, args: Vec<Expr>) -> Expr {
    let app = CallExpr {
        span: DUMMY_SP,
        callee: ExprOrSuper::Expr(Box::new(func)),
        // args: vec![ExprOrSpread {
        //     spread: None,
        //     expr: Box::new(arg),
        // }],
        args: args
            .iter()
            .map(|arg| ExprOrSpread {
                spread: None,
                expr: Box::new(*arg),
            })
            .collect(),

        type_args: None,
    };

    Expr::Call(app)
}

///Take a rust string and returns a javascript string object
fn to_js_str(string: String) -> Expr {
    let str = Str {
        span: DUMMY_SP,
        value: string.into(),
        has_escape: false,
        kind: swc_ecma_ast::StrKind::Synthesized,
    };
    Expr::Lit(Lit::Str(str))
}

fn to_js_str_u() -> Expr {
    to_js_str("U".into())
}

fn to_js_str_pi() -> Expr {
    to_js_str("Pi".into())
}

pub trait JsOutput {
    fn to_js_prim(&self) -> (usize, Rc<dyn Fn(Vec<Expr>) -> Expr>);
}

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
    Await,
}

impl Primitive for JsPrim {
    fn type_of(&self) -> Judgment<Self> {
        use JsPrim::*;

        match self {
            JsPrim::IO(iomonad) => {
                use JsIO::*;
                match iomonad {
                    ConsoleOutput => {
                        term!([Type(JsType::StrType)] -> [IO(IOMonad)] [Promise(JsPromise::PromiseMonad)] [Type(JsType::UnitType)])
                    }
                    ConsoleInput => term!(
                        [IO(IOMonad)][Promise(JsPromise::PromiseMonad)][Type(JsType::StrType)]
                    ),
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
                    JsPromise::Await => {
                        term!(Pi |T : U| [Promise(PromiseMonad)] T -> [IO(JsIO::IOMonad)])
                    }
                }
            }
            JsPrim::Type(js_type) => {
                use JsType::*;
                match js_type {
                    JsType::Str(String) => term!([Type(StrType)]),

                    JsType::StrType => term!(U),
                    JsType::UnitType => term!(U),
                    JsType::Unit => term!([Type(UnitType)]),
                }
            }
        }
    }
}

impl JsOutput for JsPrim {
    fn to_js_prim(&self) -> Result<Expr, Rc<dyn Fn(Vec<Expr>) -> Expr>> {
        use JsPrim::*;
        // match self {
        // ConsoleOutput => {
        //     let console = Ident {
        //         span: DUMMY_SP,
        //         sym: "console".into(),
        //         optional: false,
        //     };

        //     let log = Ident {
        //         span: DUMMY_SP,
        //         sym: "log".into(),
        //         optional: false,
        //     };

        //     let memberexpr = MemberExpr {
        //         span: DUMMY_SP,
        //         obj: ExprOrSuper::Expr(Box::new(Expr::Ident(console))),
        //         prop: Box::new(Expr::Ident(log)),
        //         computed: false,
        //     };

        //     Expr::Member(memberexpr)
        // }
        //     Str(str) => to_js_str(str.clone()),
        //     StrType => to_js_str("StrType".into()),
        //     ConsoleInput => todo!(),
        //     JsIO => to_js_str("JsIO".into()),
        //     JsIOBind => todo!(),
        //     JsIOPure => todo!(),
        //     UnitType => to_js_str("UnitType".into()),
        //     Unit => todo!(),
        // }

        // I  changed the type signature of this funtion to return an Option
        match self {
            IO(iomonad) => match iomonad {
                JsIO::IOMonad => Ok(to_js_str("IOMonad".into())),
                JsIO::Bind => to_js_bind(),
                JsIO::Pure => to_js_io_pure(),
                JsIO::ConsoleOutput => {
                    // let console = Ident {
                    //     span: DUMMY_SP,
                    //     sym: "console".into(),
                    //     optional: false,
                    // };

                    // let log = Ident {
                    //     span: DUMMY_SP,
                    //     sym: "log".into(),
                    //     optional: false,
                    // };

                    // let memberexpr = MemberExpr {
                    //     span: DUMMY_SP,
                    //     obj: ExprOrSuper::Expr(Box::new(Expr::Ident(console))),
                    //     prop: Box::new(Expr::Ident(log)),
                    //     computed: false,
                    // };

                    // Ok(Expr::Member(memberexpr))
                }
                JsIO::ConsoleInput => {}
            },
            Promise(promise) => match promise {
                JsPromise::PromiseMonad => Ok(to_js_str("IOMonad".into())),
                JsPromise::Bind => {}
                JsPromise::Pure => {}
                JsPromise::Await => todo!{},
            },
            Type(js_type) => match js_type {
                JsType::Str(str) => Ok(to_js_str(str.clone())),
                JsType::StrType => Ok(to_js_str("StrType".into())),
                JsType::UnitType => Ok(to_js_str("UnitType".into())),
                JsType::Unit => Ok(to_js_str("Unit".into())),
            },
        }
    }

    fn to_js_io_pure() => 
}

mod test {

    #[test]
    fn to_js_test() {
        use super::*;
        use term_macro::term;
        use JsPrim::*;
        let hello_world = term!([ConsoleOutput][Str("hello world".into())]);
        let s = to_js_program(hello_world);
        assert_eq!(s, "console.log(\"hello world\");\n");

        let id: Judgment<JsPrim> = term!(Lam | T: U, t: T | t);
        let hello = term!({ id }[StrType][Str("hello".into())]);
        println!("{:?}", hello);
        let s2 = to_js_program(hello);
        panic!(s2);
    }

    // #[test]
    // fn swc_test() {
    //     let str = Str {
    //         span: DUMMY_SP,
    //         value: "hello world".into(),
    //         has_escape: false,
    //         kind: swc_ecma_ast::StrKind::Synthesized,
    //     };

    //     let hello_world = ExprOrSpread {
    //         spread: None,
    //         expr: Box::new(Expr::Lit(Lit::Str(str))),
    //     };

    //     let console = Ident {
    //         span: DUMMY_SP,
    //         sym: "console".into(),
    //         optional: false,
    //     };

    //     let log = Ident {
    //         span: DUMMY_SP,
    //         sym: "log".into(),
    //         optional: false,
    //     };

    //     let memberexpr = MemberExpr {
    //         span: DUMMY_SP,
    //         obj: ExprOrSuper::Expr(Box::new(Expr::Ident(console))),
    //         prop: Box::new(Expr::Ident(log)),
    //         computed: false,
    //     };

    //     let callexpr = CallExpr {
    //         span: DUMMY_SP,
    //         callee: ExprOrSuper::Expr(Box::new(Expr::Member(memberexpr))),
    //         args: vec![hello_world],
    //         type_args: None,
    //     };
    //     let exprstmt = ExprStmt {
    //         span: DUMMY_SP,
    //         expr: Box::new(Call(callexpr)),
    //     };
    //     let stmt = Stmt::Expr(exprstmt);

    //     let script: Script = Script {
    //         span: DUMMY_SP,
    //         body: vec![stmt],
    //         shebang: None,
    //     };

    //     let cm = Rc::new(SourceMap::new(FilePathMapping::new(vec![])));
    //     let mut emitter = Emitter {
    //         cfg: Config { minify: false },
    //         cm: Rc::clone(&cm),
    //         comments: None,
    //         wr: Box::new(JsWriter::new(Rc::clone(&cm), "\n", stdout(), None)),
    //     };

    //     emitter.emit_script(&script)?;

    //     Ok(())
    // }
}
