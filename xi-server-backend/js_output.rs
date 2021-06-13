use std::collections::BTreeMap;
use swc_common::DUMMY_SP;
use swc_ecma_ast::{
    AwaitExpr, BindingIdent, Decl, ExportDecl, Expr, ExprOrSpread, ExprOrSuper, ExprStmt,
    ImportDecl, ImportNamedSpecifier, ImportSpecifier, MemberExpr, Module, ModuleDecl, ModuleItem,
    NewExpr, Pat, Stmt, Str, VarDecl, VarDeclKind, VarDeclarator,
};

use xi_backends::js_backend::{
    js_output::{
        make_var_name, module_item_to_swc_module_item, promise_resolve, run_io,
        std_import_from_server, swc_module_to_string, to_js, to_js_app_wo_await, to_js_await,
        to_js_ident, to_js_ident2, to_js_sm_int, to_js_str,
    },
    js_prim::{JsModule, JsPrim},
};

use xi_core::judgment::Judgment;
use xi_uuid::VarUuid;

// let's try to write the modules again.

pub fn module_to_js_module(
    module: JsModule,
    main_id: Option<VarUuid>,
    port: u32,
    other_port: u32,
) -> Module {
    // first the standard server kind of stuff:
    // import {Server, pi, prim, u , freevar} from "./server.ts";

    // let server = new Server("js");

    // then for every moduleitem, we look at its transport info,
    // if its origin is js, then we write it down the normal way,
    // and serialize it and stuff. If its transport is js, then we call the server to
    // deserialize it.

    // the same is absolutely the same for python.

    let mut body = vec![];

    // first we add in the import server stuff code:
    let import_names = vec!["Server", "pi", "prim", "u", "freevar"];

    body.push(std_import_from_server(import_names));
    // then run server.
    body.push(let_server_code(port, other_port));

    // keep track of all the ffi functions.
    let mut ffi_functions = BTreeMap::new();

    // for every single module_item, look at their transport_info,
    // if their origin == server_name, then module_item_to... it, and serialize it,
    // else if trasnport == server_name, then deseraliaze it.
    // else just ignore it.
    dbg!(&module.module_items);
    for (var_index, module_item) in &module.module_items {
        if Some("js".into()) == module_item.transport_info().origin {
            let swc_module_item =
                module_item_to_swc_module_item(&mut ffi_functions, module_item, var_index);
            // we add everything to the var_names now:
            // first the ffi functions, which are ffi{var_index}

            // export const var_1 = ....
            body.push(swc_module_item);
            // do below only if it has a transport
            // server.register(var_1, serialized_var_1);

            if Some("py".into()) == module_item.transport_info().transport {
                let serialized_var_module_item = register_top_level(var_index, module_item.type_());
                body.push(serialized_var_module_item);
            }
        } else if Some("js".into()) == module_item.transport_info().transport {
            let deregister_top_level = Expr::Member(MemberExpr {
                span: DUMMY_SP,
                obj: ExprOrSuper::Expr(Box::new(to_js_ident("server"))),
                prop: Box::new(to_js_ident("deregister_top_level")),
                computed: false,
            });
            let call_expr = to_js_app_wo_await(
                deregister_top_level,
                vec![
                    to_js_str(format!("var_{}", var_index.index())),
                    to_js(
                        &module_item.type_(),
                        BTreeMap::new(),
                        &mut BTreeMap::new(),
                        false,
                    ),
                ],
            );
            // Promise.resolve(await server.deserialize(thing(value), module_item.type_))
            let promise_resolve_expr = promise_resolve(Expr::Await(AwaitExpr {
                span: DUMMY_SP,
                arg: Box::new(call_expr),
            }));

            let var_decl = VarDeclarator {
                span: DUMMY_SP,
                name: Pat::Ident(BindingIdent {
                    id: to_js_ident2(format!("var_{}", var_index.index())),
                    type_ann: None,
                }),
                init: Some(Box::new(promise_resolve_expr)),
                definite: false,
            };
            let module_item = ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
                span: DUMMY_SP,
                decl: Decl::Var(VarDecl {
                    span: DUMMY_SP,
                    kind: VarDeclKind::Let,
                    declare: false,
                    decls: vec![var_decl],
                }),
            }));
            body.push(module_item);
        }
    }
    // importing ffi stuff.
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

    let body_with_imports = {
        let mut t = ffi_imports;
        t.extend(body);
        if let Some(main_id) = &main_id {
            let run_main = ModuleItem::Stmt(Stmt::Expr(ExprStmt {
                span: DUMMY_SP,
                expr: Box::new(run_io(Expr::Ident(make_var_name(main_id)))),
            }));
            t.push(run_main)
        }
        // now I want to apply server_registration to every var_name!
        t
    };

    Module {
        span: DUMMY_SP,
        body: body_with_imports,
        shebang: None,
    }
}

pub fn get_vars(type_: Judgment<JsPrim>, vars: &mut Vec<Judgment<JsPrim>>) {
    match *type_.tree {
        xi_core::judgment::JudgmentKind::Pi(var_type, expr) => {
            vars.push(var_type);
            get_vars(expr.unbind().1, vars);
        }
        _ => {}
    }
}

pub fn module_with_server_to_js_string(
    module: JsModule,
    main_id: Option<VarUuid>,
    port: u32,
    other_port: u32,
) -> String {
    let module = module_to_js_module(module, main_id, port, other_port);
    swc_module_to_string(module)
}

// pub fn js_module_to_py_string(module: JsModule) -> String {
//     let py_module = module_to_js_module(module, None, "py".into());
//     swc_module_to_string(py_module)
// }

// pub fn js_module_to_py_string_with_run(module: JsModule, main_id: VarUuid) -> String {
//     let py_main_module = module_to_js_module(module, Some(main_id), "py".into());
//     swc_module_to_string(py_main_module)
// }

pub fn register_top_level(index: &VarUuid, type_: Judgment<JsPrim>) -> ModuleItem {
    // first let's make the json object associated to var_a.type_:
    // server.register_var()
    let json_var_type_ = to_js(&type_, BTreeMap::new(), &mut BTreeMap::new(), false);
    let server_register_top_level = Expr::Member(MemberExpr {
        span: DUMMY_SP,
        obj: ExprOrSuper::Expr(Box::new(to_js_ident("server"))),
        prop: Box::new(to_js_ident("register_top_level")),
        computed: false,
    });

    let call_expr = to_js_app_wo_await(
        server_register_top_level,
        vec![
            to_js_await(to_js_ident(format!("var_{}", index.index()))),
            to_js_str(format!("var_{}", index.index())),
            json_var_type_,
        ],
    );
    ModuleItem::Stmt(Stmt::Expr(ExprStmt {
        span: DUMMY_SP,
        expr: Box::new(call_expr),
    }))
}

// // this function takes an Aplite type and encode it as a JSON object of binary tree of Pi's.
// // right now we can do primitives and dependent pi types.
// pub fn type_to_json(type_: Judgment<JsPrim>) -> Expr {
//     match *type_.tree {
//         xi_core::judgment::JudgmentKind::Pi(arg_type, return_type) => {
//             let (id, return_type) = return_type.unbind();

//             let args = vec![
//                 type_to_json(arg_type),
//                 type_to_json(return_type),
//                 to_js_num(format!("{}", id.index())),
//             ];
//             to_js_app_wo_await(to_js_ident("pi"), args)
//         }
//         xi_core::judgment::JudgmentKind::Type => to_js_app_wo_await(to_js_ident("u"), vec![]),
//         xi_core::judgment::JudgmentKind::Prim(prim, _) => match prim {
//             JsPrim::StringType => to_js_app_wo_await(to_js_ident("prim"), vec![to_js_str("Str")]),
//             JsPrim::NumberType => to_js_app_wo_await(to_js_ident("prim"), vec![to_js_str("Int")]),
//             JsPrim::StringElem(_) => unreachable!("nope"),
//             JsPrim::NumberElem(_) => unreachable!("nope"),
//             JsPrim::Ffi(_, _) => unreachable!("nope"),
//             JsPrim::Var(_) => unreachable!("nope"),
//         },
//         xi_core::judgment::JudgmentKind::FreeVar(index, var_type) => to_js_app_wo_await(
//             to_js_ident("freevar"),
//             vec![
//                 to_js_num(format!("{}", index.index())),
//                 type_to_json(var_type),
//             ],
//         ),
//         xi_core::judgment::JudgmentKind::Lam(_, _) => {
//             unreachable!("we shouldn't see this here")
//         }
//         xi_core::judgment::JudgmentKind::BoundVar(_, _) => {
//             unreachable!("we shouldn't see this here")
//         }
//         xi_core::judgment::JudgmentKind::App(_, _) => {
//             panic!("idk")
//         }
//     }
// }

// takes [str] to {kind: [str]}
pub fn json_kind(str: String) -> Expr {
    to_js_app_wo_await(to_js_ident("json_kind"), vec![to_js_str(str)])
}

pub fn let_server_code(port: u32, other_port: u32) -> ModuleItem {
    let var_declarator = VarDeclarator {
        span: DUMMY_SP,
        name: Pat::Ident(BindingIdent {
            id: to_js_ident2("server"),
            type_ann: None,
        }),
        init: Some(Box::new(Expr::New(NewExpr {
            span: DUMMY_SP,
            callee: Box::new(to_js_ident("Server")),
            args: Some(vec![
                ExprOrSpread {
                    spread: None,
                    expr: Box::new(to_js_sm_int(port)),
                },
                ExprOrSpread {
                    spread: None,
                    expr: Box::new(to_js_sm_int(other_port)),
                },
            ]),
            type_args: None,
        }))),
        definite: false,
    };

    ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
        span: DUMMY_SP,
        decl: Decl::Var(VarDecl {
            span: DUMMY_SP,
            kind: VarDeclKind::Let,
            declare: false,
            decls: vec![var_declarator],
        }),
    }))
}
