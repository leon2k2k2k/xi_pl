use std::rc::Rc;
use swc_common::{FilePathMapping, SourceMap, DUMMY_SP};
use swc_ecma_ast::{
    ArrowExpr, BindingIdent, BlockStmtOrExpr, CallExpr, Expr, ExprOrSpread, ExprOrSuper, ExprStmt,
    Ident, ImportDecl, ImportNamedSpecifier, ImportSpecifier, ImportStarAsSpecifier, Lit,
    MemberExpr, Module, ModuleDecl, ModuleItem, ParenExpr, Pat, Stmt, Str,
};
use swc_ecma_codegen::{text_writer::JsWriter, Config, Emitter};
use xi_core::judgment::{Judgment, JudgmentKind};
use xi_frontend::type_inference::UiMetadata;
use xi_uuid::VarUuid;

pub fn to_js_program<T: JsOutput>(judgment: Judgment<T, UiMetadata>) -> String {
    let mut ffi_functions = vec![];
    let stmt = Stmt::Expr(ExprStmt {
        span: DUMMY_SP,
        expr: Box::new(to_js(judgment, vec![], &mut ffi_functions)),
    });

    dbg!(&stmt);
    let mut ffi_imports = vec![];
    for (var_index, (file_name, function_name)) in ffi_functions {
        let module_import = ModuleDecl::Import(ImportDecl {
            span: DUMMY_SP,
            specifiers: vec![ImportSpecifier::Named(ImportNamedSpecifier {
                span: DUMMY_SP,
                local: to_js_ident2(format!("ffi{}", var_index.index())),
                imported: Some(to_js_ident2(function_name)),
            })],
            src: Str {
                span: DUMMY_SP,
                value: file_name.into(),
                has_escape: false,
                kind: swc_ecma_ast::StrKind::Synthesized,
            },
            type_only: false,
            asserts: None,
        });
        ffi_imports.push(module_import);
    }
    let module_import = ModuleDecl::Import(ImportDecl {
        span: DUMMY_SP,
        specifiers: vec![ImportSpecifier::Namespace(ImportStarAsSpecifier {
            span: DUMMY_SP,
            local: to_js_ident2("runtime".into()),
        })],
        src: Str {
            span: DUMMY_SP,
            value: "./$deno$runtime.ts".into(),
            has_escape: false,
            kind: swc_ecma_ast::StrKind::Synthesized,
        },
        type_only: false,
        asserts: None,
    });

    let mut module_body: Vec<ModuleItem> = ffi_imports
        .iter()
        .map(|module| ModuleItem::ModuleDecl(module.clone()))
        .collect();
    module_body.push(ModuleItem::ModuleDecl(module_import));
    module_body.push(ModuleItem::Stmt(stmt));

    let module: Module = Module {
        span: DUMMY_SP,
        body: module_body,
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

        emitter.emit_module(&module).unwrap();
    }

    std::str::from_utf8(output_buf.as_slice()).unwrap().into()
}

fn make_var_name(ctx: &Vec<Ident>) -> Ident {
    to_js_ident2(format!("var_{}", ctx.len()))
}

fn to_js<T: JsOutput>(
    judgment: Judgment<T, UiMetadata>,
    ctx: Vec<Ident>,
    ffi: &mut Vec<(VarUuid, (String, String))>,
) -> Expr {
    match judgment.tree {
        JudgmentKind::UInNone => to_js_str_u(),
        JudgmentKind::Prim(t) => T::to_js_prim(&t),
        JudgmentKind::FreeVar(var_index, _var_type) => {
            let metadata = judgment.metadata.ffi.unwrap();
            ffi.push((var_index, metadata));
            to_js_ident(format!("ffi{}", var_index.index()))
        }
        JudgmentKind::Pi(_, _) => to_js_str_pi(),
        JudgmentKind::Lam(_var_type, body) => {
            let var_name = make_var_name(&ctx);
            to_js_lam(
                swc_ecma_ast::BlockStmtOrExpr::Expr(Box::new(to_js(
                    *body,
                    add_to_ctx(ctx, var_name.clone()),
                    ffi,
                ))),
                vec![var_name],
            )
        }
        JudgmentKind::BoundVar(index, _var_type) => {
            to_js_ident1(ctx[ctx.len() - 1 - index as usize].clone())
        }
        JudgmentKind::Application(func, arg) => {
            to_js_app(to_js(*func, ctx.clone(), ffi), vec![to_js(*arg, ctx, ffi)])
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
                id: var_name.clone(),
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

    let arrow_expr = Expr::Arrow(lam);

    // Now we add a parenthesis around it.
    let paren_expr = ParenExpr {
        span: DUMMY_SP,
        expr: Box::new(arrow_expr),
    };

    Expr::Paren(paren_expr)
}

pub fn to_js_ident(name: String) -> Expr {
    to_js_ident1(to_js_ident2(name))
}
pub fn to_js_ident2(name: String) -> Ident {
    Ident {
        span: DUMMY_SP,
        sym: name.into(),
        optional: false,
    }
}

pub fn runtime_ident(name: &str) -> Expr {
    Expr::Member(MemberExpr {
        span: DUMMY_SP,
        obj: ExprOrSuper::Expr(Box::new(to_js_ident("runtime".into()))),
        prop: Box::new(to_js_ident(name.into())),
        computed: false,
    })
}

fn to_js_ident1(var_name: Ident) -> Expr {
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
                expr: Box::new(arg.clone()),
            })
            .collect(),

        type_args: None,
    };

    Expr::Call(app)
}

///Take a rust string and returns a javascript string object
pub fn to_js_str(string: String) -> Expr {
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

mod test {

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
