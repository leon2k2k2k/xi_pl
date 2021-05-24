use std::{collections::BTreeMap, rc::Rc};
use swc_common::{FilePathMapping, SourceMap, DUMMY_SP};
use swc_ecma_ast::{
    ArrowExpr, BigInt, BindingIdent, BlockStmtOrExpr, CallExpr, Decl, ExportDecl, Expr,
    ExprOrSpread, ExprOrSuper, ExprStmt, Ident, ImportDecl, ImportNamedSpecifier, ImportSpecifier,
    Lit, MemberExpr, Module, ModuleDecl, ModuleItem, ParenExpr, Pat, Stmt, Str, VarDecl,
    VarDeclKind, VarDeclarator,
};
use swc_ecma_codegen::{text_writer::JsWriter, Config, Emitter};
use xi_core::judgment::{Judgment, JudgmentKind};
use xi_uuid::VarUuid;

use crate::js_prim::{JsModule, JsModuleItem, JsPrim};

pub fn judgment_to_swc_expr(
    judgment: Judgment<JsPrim>,
) -> (BTreeMap<(String, String), VarUuid>, Expr) {
    let mut ffi_functions = BTreeMap::new();

    let main_js = to_js(&judgment, BTreeMap::new(), &mut ffi_functions);

    // let main_js2 = Expr::Await(AwaitExpr {
    //     span: DUMMY_SP,
    //     arg: Box::new(to_js_app(main_js, vec![])),
    // });

    (ffi_functions, main_js)
}

// a module_item should go to a swc module_item
pub fn module_item_to_swc_module_item(
    module_item: JsModuleItem,
    var_index: VarUuid,
) -> (BTreeMap<(String, String), VarUuid>, ModuleItem) {
    let pat = Pat::Ident(BindingIdent {
        id: make_var_name(var_index),
        type_ann: None,
    });
    match module_item {
        JsModuleItem::Define(define_item) => {
            let (ffi_functions, expr) = judgment_to_swc_expr(define_item.impl_);
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

            (ffi_functions, module)
        }
    }
}

// a js_module go to a swc module
pub fn module_to_swc_module(module: JsModule, main_id: VarUuid) -> Module {
    let mut body = vec![];
    let mut ffi_functions = BTreeMap::new();

    for (var_index, module_item) in &module.module_items {
        let (new_ffi_functions, module_item) =
            module_item_to_swc_module_item(module_item.clone(), *var_index);
        ffi_functions.extend(new_ffi_functions);
        body.push(module_item);
    }

    let mut ffi_imports = vec![];
    for ((file_name, function_name), index) in ffi_functions {
        let module_import = ModuleDecl::Import(ImportDecl {
            span: DUMMY_SP,
            specifiers: vec![ImportSpecifier::Named(ImportNamedSpecifier {
                span: DUMMY_SP,
                local: to_js_ident2(format!("ffi{}", index.index())),
                imported: Some(to_js_ident2(function_name.clone())),
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

    let run_main = ModuleItem::Stmt(Stmt::Expr(ExprStmt {
        span: DUMMY_SP,
        expr: Box::new(run_io(Expr::Ident(make_var_name(main_id)))),
    }));

    let body_with_imports = {
        let mut t = ffi_imports;
        t.extend(body);
        t.push(run_main);
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

pub fn module_to_js_string(module: JsModule, main_id: VarUuid) -> String {
    let module = module_to_swc_module(module, main_id);
    swc_module_to_string(module)
}

pub fn make_var_name(index: VarUuid) -> Ident {
    to_js_ident2(format!("var_{}", index.index()))
}

fn to_js(
    judgment: &Judgment<JsPrim>,
    ctx: BTreeMap<VarUuid, Ident>,
    ffi: &mut BTreeMap<(String, String), VarUuid>,
) -> Expr {
    match &*judgment.tree {
        JudgmentKind::Type => to_js_str_u(),
        JudgmentKind::Prim(t, _prim_type) => JsPrim::to_js_prim(&t, ffi),
        JudgmentKind::FreeVar(var_index, _var_type) => {
            match ctx.get(var_index) {
                Some(ident) => to_js_ident1(ident.clone()),
                None => {
                    unreachable!("this means that we have a loose free variable")
                }
            }
            // let metadata = judgment.metadata.ffi.clone().unwrap();
            // ffi.push((*var_index, metadata));
            // to_js_ident(format!("ffi{}", var_index.index()))
        }
        JudgmentKind::Pi(_, _) => to_js_str_pi(),
        JudgmentKind::Lam(_var_type, sexpr) => {
            let (index, expr) = sexpr.clone().unbind();
            let var_name = make_var_name(index);

            to_js_lam(
                swc_ecma_ast::BlockStmtOrExpr::Expr(Box::new(to_js(
                    &expr,
                    add_to_ctx(ctx, index, &var_name.clone()),
                    ffi,
                ))),
                vec![var_name],
            )
        }
        JudgmentKind::BoundVar(_, _var_type) => {
            // to_js_ident1(ctx[ctx.len() - 1 - *index as usize].clone())
            unreachable!("we use unbind so should never see a BoundVar")
        }
        JudgmentKind::App(func, arg) => to_js_app(
            to_js(&*func, ctx.clone(), ffi),
            vec![to_js(&*arg, ctx, ffi)],
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

pub fn to_js_app(func: Expr, args: Vec<Expr>) -> Expr {
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

pub fn to_js_num(num: String) -> Expr {
    let bigint_oops = num_bigint::BigInt::parse_bytes(&num.into_bytes(), 10).unwrap();
    let bigint = BigInt {
        span: DUMMY_SP,
        value: bigint_oops,
    };
    Expr::Lit(Lit::BigInt(bigint))
}

fn to_js_str_u() -> Expr {
    to_js_str("U".into())
}

fn to_js_str_pi() -> Expr {
    to_js_str("Pi".into())
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
