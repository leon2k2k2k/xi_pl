use std::{collections::BTreeMap, rc::Rc};
use swc_common::{FilePathMapping, SourceMap, DUMMY_SP};
use swc_ecma_ast::{
    ArrowExpr, AwaitExpr, BigInt, BindingIdent, BlockStmtOrExpr, CallExpr, Decl, ExportDecl, Expr,
    ExprOrSpread, ExprOrSuper, ExprStmt, Ident, ImportDecl, ImportNamedSpecifier, ImportSpecifier,
    Lit, MemberExpr, Module, ModuleDecl, ModuleItem, Number, ParenExpr, Pat, Stmt, Str, VarDecl,
    VarDeclKind, VarDeclarator,
};
use swc_ecma_codegen::{text_writer::JsWriter, Config, Emitter};
use xi_core::judgment::{Judgment, JudgmentKind};
use xi_uuid::VarUuid;

use crate::js_backend::js_prim::{JsModule, JsModuleItem, JsPrim};

pub fn judgment_to_swc_expr(
    ffi_functions: &mut BTreeMap<(String, String), VarUuid>,
    judgment: &Judgment<JsPrim>,
) -> Expr {
    to_js(&judgment, BTreeMap::new(), ffi_functions, false)
}

// a module_item should go to a swc module_item
pub fn module_item_to_swc_module_item(
    ffi_functions: &mut BTreeMap<(String, String), VarUuid>,
    module_item: &JsModuleItem,
    var_index: &VarUuid,
) -> ModuleItem {
    let pat = Pat::Ident(BindingIdent {
        id: make_var_name(var_index),
        type_ann: None,
    });
    match module_item {
        JsModuleItem::Define(define_item) => {
            let expr = judgment_to_swc_expr(ffi_functions, &define_item.impl_);
            let var_declarator = VarDeclarator {
                span: DUMMY_SP,
                name: pat,
                init: Some(Box::new(expr)),
                definite: false,
            };

            let var_decl = VarDecl {
                span: DUMMY_SP,
                kind: VarDeclKind::Const,
                declare: false,
                decls: vec![var_declarator],
            };
            let decl = Decl::Var(var_decl);
            let module = ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
                span: DUMMY_SP,
                decl: decl,
            }));

            module
        }
    }
}

// a js_module go to a swc module
pub fn module_to_js_module(module: JsModule, main_id: Option<VarUuid>) -> Module {
    let mut body = vec![];
    let mut ffi_functions = BTreeMap::new();

    // we need to import pi, prim, u free, this is how we are representing types in js.
    // this is really not needed for run-no-server, but it is needed for with server.

    let import_names = vec!["pi", "prim", "u", "freevar"];
    body.push(std_import_from_server(import_names));
    for (var_index, module_item) in &module.module_items {
        let module_item =
            module_item_to_swc_module_item(&mut ffi_functions, module_item, var_index);
        body.push(module_item);
    }

    let mut ffi_imports = vec![];
    for ((file_name, function_name), index) in ffi_functions {
        let module_import = ModuleDecl::Import(ImportDecl {
            span: DUMMY_SP,
            specifiers: vec![ImportSpecifier::Named(ImportNamedSpecifier {
                span: DUMMY_SP,
                local: to_js_ident2(format!("ffi{}", index.index())),
                imported: Some(to_js_ident2(&function_name)),
            })],
            src: Str {
                span: DUMMY_SP,
                value: file_name.clone().into(),
                has_escape: false,
                kind: swc_ecma_ast::StrKind::Synthesized,
            },
            type_only: false,
            asserts: None,
        });
        ffi_imports.push(ModuleItem::ModuleDecl(module_import));
    }

    let body_with_imports = {
        let mut t = ffi_imports;
        t.extend(body);
        // if there is a main_id, run the function.

        if let Some(main_id) = &main_id {
            let run_main = ModuleItem::Stmt(Stmt::Expr(ExprStmt {
                span: DUMMY_SP,
                expr: Box::new(run_io(Expr::Ident(make_var_name(main_id)))),
            }));
            t.push(run_main);
        }
        t
    };

    Module {
        span: DUMMY_SP,
        body: body_with_imports,
        shebang: None,
    }
}

pub fn swc_module_to_string(module: Module) -> String {
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

pub fn module_to_js_string(module: JsModule, main_id: Option<VarUuid>) -> String {
    let module = module_to_js_module(module, main_id);
    swc_module_to_string(module)
}

pub fn make_var_name(index: &VarUuid) -> Ident {
    to_js_ident2(format!("var_{}", index.index()))
}

pub fn to_js(
    judgment: &Judgment<JsPrim>,
    ctx: BTreeMap<&VarUuid, Ident>,
    ffi: &mut BTreeMap<(String, String), VarUuid>,
    in_type: bool,
) -> Expr {
    match &*judgment.tree {
        JudgmentKind::Type => to_js_app_wo_await(to_js_ident("u"), vec![]),
        JudgmentKind::Prim(t, _prim_type) => JsPrim::to_js_prim(&t, ffi),
        JudgmentKind::FreeVar(var_index, var_type) => {
            if in_type == true {
                to_js_app_wo_await(
                    to_js_ident("freevar"),
                    vec![
                        to_js_num(format!("{}", var_index.index())),
                        to_js(var_type, ctx, ffi, false),
                    ],
                )
            } else {
                match ctx.get(var_index) {
                    Some(ident) => promise_resolve(to_js_ident1(ident.clone())),
                    None => {
                        unreachable!(format!(
                            "this means that we have a loose free variable, judgment is {:?}",
                            judgment
                        ))
                    }
                }
            }
        }
        JudgmentKind::Pi(arg_type, return_type) => {
            let (index, return_type) = return_type.clone().unbind();
            let var_name = make_var_name(&index);
            let args = vec![
                to_js(arg_type, ctx.clone(), ffi, true),
                to_js(
                    &return_type,
                    add_to_ctx(ctx, &index, &var_name.clone()),
                    ffi,
                    true,
                ),
                to_js_num(format!("{}", index.index())),
            ];
            to_js_app_wo_await(to_js_ident("pi"), args)
        }
        JudgmentKind::Lam(_var_type, sexpr) => {
            let (index, expr) = &sexpr.clone().unbind();
            let var_name = make_var_name(index);

            to_js_lam(
                swc_ecma_ast::BlockStmtOrExpr::Expr(Box::new(to_js(
                    &expr,
                    add_to_ctx(ctx, index, &var_name.clone()),
                    ffi,
                    false,
                ))),
                vec![var_name],
            )
        }
        JudgmentKind::BoundVar(_, _var_type) => {
            // to_js_ident1(ctx[ctx.len() - 1 - *index as usize].clone())
            unreachable!("we use unbind so should never see a BoundVar")
        }
        JudgmentKind::App(func, arg) => to_js_app(
            to_js(&*func, ctx.clone(), ffi, false),
            vec![to_js(&*arg, ctx, ffi, false)],
        ),
    }
}

fn add_to_ctx<T: Ord, U: Clone>(v: BTreeMap<T, U>, index: T, x: &U) -> BTreeMap<T, U> {
    let mut v = v;
    v.insert(index, x.clone());
    v
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
        is_async: true,
        is_generator: false,
        type_params: None,
        return_type: None,
    };

    let arrow_expr = Expr::Arrow(lam);

    promise_resolve(arrow_expr)
}

pub fn to_js_ident(name: impl Into<String>) -> Expr {
    to_js_ident1(to_js_ident2(name))
}

pub fn to_js_ident2(name: impl Into<String>) -> Ident {
    Ident {
        span: DUMMY_SP,
        sym: name.into().into(),
        optional: false,
    }
}

pub fn to_js_member(obj: Expr, prop: Expr) -> Expr {
    Expr::Member(MemberExpr {
        span: DUMMY_SP,
        obj: ExprOrSuper::Expr(Box::new(obj)),
        prop: Box::new(prop),
        computed: false,
    })
}

fn to_js_ident1(var_name: Ident) -> Expr {
    Expr::Ident(var_name)
}

pub fn to_js_app(func: Expr, args: Vec<Expr>) -> Expr {
    let app = CallExpr {
        span: DUMMY_SP,
        callee: ExprOrSuper::Expr(Box::new(parenthesize(to_js_await(func)))),
        args: args
            .iter()
            .map(|arg| ExprOrSpread {
                spread: None,
                expr: Box::new(to_js_await(arg.clone())),
            })
            .collect(),

        type_args: None,
    };

    Expr::Call(app)
}

pub fn to_js_app_wo_await(func: Expr, args: Vec<Expr>) -> Expr {
    Expr::Call(CallExpr {
        span: DUMMY_SP,
        callee: ExprOrSuper::Expr(Box::new(func)),
        args: args
            .iter()
            .map(|arg| ExprOrSpread {
                spread: None,
                expr: Box::new(arg.clone()),
            })
            .collect(),
        type_args: None,
    })
}

pub fn to_js_await(expr: Expr) -> Expr {
    // Transforms await (Promise.resolve(x)) => x
    if let Expr::Call(CallExpr {
        callee: ExprOrSuper::Expr(call_expr),
        args: awaited_expr,
        ..
    }) = &expr
    {
        if **call_expr == (to_js_member(to_js_ident("Promise"), to_js_ident("resolve"))) {
            return *awaited_expr[0].expr.clone();
        }
    }

    Expr::Await(AwaitExpr {
        span: DUMMY_SP,
        arg: Box::new(expr),
    })
}

pub fn parenthesize(expr: Expr) -> Expr {
    let paren_expr = ParenExpr {
        span: DUMMY_SP,
        expr: Box::new(expr),
    };

    Expr::Paren(paren_expr)
}

///Take a rust string and returns a javascript string object
pub fn to_js_str(string: impl Into<String>) -> Expr {
    let str = Str {
        span: DUMMY_SP,
        value: string.into().into(),
        has_escape: false,
        kind: swc_ecma_ast::StrKind::Synthesized,
    };
    Expr::Lit(Lit::Str(str))
}
// goes to BigInt
pub fn to_js_num(num: String) -> Expr {
    let bigint_oops = num_bigint::BigInt::parse_bytes(&num.into_bytes(), 10).unwrap();
    let bigint = BigInt {
        span: DUMMY_SP,
        value: bigint_oops,
    };
    Expr::Lit(Lit::BigInt(bigint))
}

pub fn to_js_sm_int(int: u32) -> Expr {
    Expr::Lit(Lit::Num(Number {
        span: DUMMY_SP,
        value: int as f64,
    }))
}

pub fn promise_resolve(expr: Expr) -> Expr {
    Expr::Call(CallExpr {
        span: DUMMY_SP,
        callee: ExprOrSuper::Expr(Box::new(to_js_member(
            to_js_ident("Promise"),
            to_js_ident("resolve"),
        ))),
        args: vec![ExprOrSpread {
            spread: None,
            expr: Box::new(expr),
        }],
        type_args: None,
    })
}

pub fn run_io(expr: Expr) -> Expr {
    to_js_app(expr, vec![])
}

pub fn string_to_import_specifier(name: String) -> ImportSpecifier {
    ImportSpecifier::Named(ImportNamedSpecifier {
        span: DUMMY_SP,
        local: to_js_ident2(name),
        imported: None,
    })
}

pub fn std_import_from_server(strs: Vec<&str>) -> ModuleItem {
    let import_names = vec!["Server", "pi", "prim", "u", "freevar"];
    let specifiers: Vec<ImportSpecifier> = strs
        .iter()
        .map(|str| string_to_import_specifier(str.clone().into()))
        .collect();

    ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
        span: DUMMY_SP,
        specifiers: specifiers,
        src: Str {
            span: DUMMY_SP,
            value: "./js_server.ts".into(),
            has_escape: false,
            kind: swc_ecma_ast::StrKind::Synthesized,
        },
        type_only: false,
        asserts: None,
    }))
}
