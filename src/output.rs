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

fn to_js<T: JsOutput>(judgment: Judgment<T>, ctx: Vec<String>) -> Expr {
    match judgment {
        Judgment::UInNone => to_js_str_u(),
        Judgment::Prim(t) => JsOutput::to_js_prim(&t),
        Judgment::FreeVar(_, _) => panic!("Should not have a FreeVar in this expression"),
        Judgment::Pi(_, _) => to_js_str_pi(),
        Judgment::Lam(_var_type, body) => {
            let var_name = format!("var_{}", ctx.len());
            to_js_lam(
                to_js(*body, add_to_ctx(ctx, var_name.clone())),
                to_js_ident2(var_name),
            )
        }
        Judgment::BoundVar(index, _var_type) => {
            to_js_ident(ctx[ctx.len() - 1 - index as usize].clone())
        }
        Judgment::Application(func, arg) => to_js_app(to_js(*func, ctx.clone()), to_js(*arg, ctx)),
    }
}

fn add_to_ctx<T: Clone>(v: Vec<T>, t: T) -> Vec<T> {
    let mut v2 = v.clone();
    v2.push(t);
    v2
}

fn to_js_lam(body: Expr, var_name: Ident) -> Expr {
    let pat = Pat::Ident(BindingIdent {
        id: var_name,
        type_ann: None,
    });
    let lam = ArrowExpr {
        span: DUMMY_SP,
        params: vec![pat],
        body: BlockStmtOrExpr::Expr(Box::new(body)),
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

fn to_js_ident(name: String) -> Expr {
    Expr::Ident(to_js_ident2(name))
}

fn to_js_app(func: Expr, arg: Expr) -> Expr {
    let app = CallExpr {
        span: DUMMY_SP,
        callee: ExprOrSuper::Expr(Box::new(func)),
        args: vec![ExprOrSpread {
            spread: None,
            expr: Box::new(arg),
        }],
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
    fn to_js_prim(&self) -> Expr;
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum JsPrim {
    ConsoleOutput,
    ConsoleInput,
    Str(String),
    StrType,
    JsIO,
    JsIOBind,
    JsIOPure,
    UnitType,
    Unit,
}

impl Primitive for JsPrim {
    fn type_of(&self) -> Judgment<Self> {
        use JsPrim::*;
        match self {
            Str(_) => term!([StrType]),
            StrType => term!(U),
            ConsoleInput => term!([JsIO][StrType]),
            JsIO => term!(U -> U),
            JsIOBind => term!(Pi | T : U, S : U| [JsIO] T -> (T -> [JsIO] S) -> [JsIO] S),
            JsIOPure => term!(Pi |T : U| T -> [JsIO] T),
            ConsoleOutput => term!([StrType] -> [JsIO] [UnitType]),
            UnitType => term!(U),
            Unit => term!([UnitType]),
            UnitType => term!(U),
        }
    }
}

impl JsOutput for JsPrim {
    fn to_js_prim(&self) -> Expr {
        use JsPrim::*;
        match self {
            ConsoleOutput => {
                let console = Ident {
                    span: DUMMY_SP,
                    sym: "console".into(),
                    optional: false,
                };

                let log = Ident {
                    span: DUMMY_SP,
                    sym: "log".into(),
                    optional: false,
                };

                let memberexpr = MemberExpr {
                    span: DUMMY_SP,
                    obj: ExprOrSuper::Expr(Box::new(Expr::Ident(console))),
                    prop: Box::new(Expr::Ident(log)),
                    computed: false,
                };

                Expr::Member(memberexpr)
            }
            Str(str) => to_js_str(str.clone()),
            StrType => to_js_str("StrType".into()),
            ConsoleInput => todo!(),
            JsIO => to_js_str("JsIO".into()),
            JsIOBind => todo!(),
            JsIOPure => todo!(),
            UnitType => to_js_str("UnitType".into()),
            Unit => todo!(),
        }
    }
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
