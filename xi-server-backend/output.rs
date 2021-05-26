use std::collections::BTreeMap;
use swc_common::DUMMY_SP;
use swc_ecma_ast::{
    AwaitExpr, BindingIdent, Decl, ExportDecl, Expr, ExprOrSpread, ExprOrSuper, ExprStmt,
    ImportDecl, ImportNamedSpecifier, ImportSpecifier, MemberExpr, Module, ModuleDecl, ModuleItem,
    NewExpr, Pat, Stmt, Str, VarDecl, VarDeclKind, VarDeclarator,
};

use xi_backend::{
    js_prim::{JsModule, JsPrim},
    output::{
        module_item_to_swc_module_item, string_to_import_specifier, swc_module_to_string,
        to_js_app, to_js_ident, to_js_ident2, to_js_num, to_js_str,
    },
};

use xi_core::judgment::Judgment;
use xi_uuid::VarUuid;

pub struct JsPyModules {
    js_backend: Module,
    py: Module,
}

// let's try to write the modules again.

pub fn module_to_swc_js_module(module: JsModule) -> Module {
    // first the standard server kind of stuff:
    // import {Server, pi_to_json, json_kind} from "./server.ts";

    // let server = new Server("js");

    // then for every moduleitem, we look at its transport info,
    // if its origin is js, then we write it down the normal way,
    // and serialize it and stuff. If its transport is js, then we call the server to
    // deserialize it.

    // the same is absolutely the same for python.

    todo!()
}

// this output is different as we need to import server and update_server,
// and run the server in the beginning of the file, and stuff.

// on the js side, the js.js file looks liek:
// import {Server, pi_to_json, json_kind} from "./server.ts";

// let server = new Server("js");

// export const var_1 = ....
// server.serialize(var_1, [json(var_1.type_)])

// export const var_2 = ....
// server.serialize(var_2, [json(var_2.type_)])
// ...

// import { server, pi_to_json, json_kind } from "./new_server.js";

pub fn std_import_from_server() -> ModuleItem {
    let server = string_to_import_specifier("Server".into());
    let pi_to_json = string_to_import_specifier("pi_to_json".into());
    let json_kind = string_to_import_specifier("json_kind".into());
    let thing = string_to_import_specifier("thing".into());
    ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
        span: DUMMY_SP,
        specifiers: vec![server, pi_to_json, json_kind, thing],
        src: Str {
            span: DUMMY_SP,
            value: "./server.ts".into(),
            has_escape: false,
            kind: swc_ecma_ast::StrKind::Synthesized,
        },
        type_only: false,
        asserts: None,
    }))
}

pub fn module_to_js_module(module: JsModule) -> Module {
    let mut body = vec![];
    // first we want to include the module item :
    // import { server, pi_to_json, json_kind } from "./new_server.js";

    body.push(std_import_from_server());

    // after importing we immediately run the server:
    // let server = new Server("js");
    // the constructor runs the server.

    body.push(let_server_code("js".into()));

    let mut ffi_functions = BTreeMap::new();

    // the following code should generate this:
    // export const var_1 = ....
    // server.serialize(var_1, [json(var_1.type_)])

    // as well as getting tracking all the ffi functions.

    for (var_index, module_item) in &module.module_items {
        let (new_ffi_functions, swc_module_item) =
            module_item_to_swc_module_item(module_item.clone(), *var_index);
        ffi_functions.extend(new_ffi_functions);
        // we add everything to the var_names now:
        // first the ffi functions, which are ffi{var_index}

        // export const var_1 = ....
        body.push(swc_module_item);

        // server.register(var_1, serialized_var_1);
        let serialized_var_module_item = serialize(var_index, module_item.type_());
        body.push(serialized_var_module_item)
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

    let body_with_imports = {
        let mut t = ffi_imports;
        t.extend(body);
        // now I want to apply server_registration to every var_name!
        t
    };

    Module {
        span: DUMMY_SP,
        body: body_with_imports,
        shebang: None,
    }
}

// given an Aplite module_item, we should be able to generate the serialized version(for python to use) and put it onto the server.

// this is corresponding python file, transporting all the javascript to python.
// the file goes as:
// import {Server, pi_to_json, json_kind} from "./server.ts";
// let server = new Server("py");
// let js_var_1 = ....
pub fn module_to_py_module(module: JsModule) -> Module {
    let mut body = vec![];
    // import {Server, pi_to_json, json_kind} from "./server.ts";
    body.push(std_import_from_server());

    // let server = new Server("py");
    body.push(let_server_code("py".into()));

    // // I think the only thing I need to know to transport a file over is its types.
    let mut value = 0;

    for (index, module_item) in module.module_items {
        // let js_var_[index] = await server.deserialize(thing(value), module_item.type_)

        let server_deserialize = Expr::Member(MemberExpr {
            span: DUMMY_SP,
            obj: ExprOrSuper::Expr(Box::new(to_js_ident("server".into()))),
            prop: Box::new(to_js_ident("deserialize".into())),
            computed: false,
        });
        let call_expr = to_js_app(
            server_deserialize,
            vec![
                to_js_app(
                    to_js_ident("thing".into()),
                    vec![to_js_num(value.to_string())],
                ),
                type_to_json(module_item.type_()),
            ],
        );
        // await server.deserialize(thing(value), module_item.type_)
        let await_expr = Expr::Await(AwaitExpr {
            span: DUMMY_SP,
            arg: Box::new(call_expr),
        });
        let var_decl = VarDeclarator {
            span: DUMMY_SP,
            name: Pat::Ident(BindingIdent {
                id: to_js_ident2(format!("js_var_{}", index.index())),
                type_ann: None,
            }),
            init: Some(Box::new(await_expr)),
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
        value += 1;
    }

    Module {
        span: DUMMY_SP,
        body: body,
        shebang: None,
    }
}

// we are constructing something like this:
// export async function py_[var_index] (local[var_index],local[var_index..],...) {
// update_server(local[var_index]);
// update_server(b);
// ...
// now we make the body:
// }

pub fn get_vars(type_: Judgment<JsPrim>, vars: &mut Vec<Judgment<JsPrim>>) {
    match *type_.tree {
        xi_core::judgment::JudgmentKind::Pi(var_type, expr) => {
            vars.push(var_type);
            get_vars(expr.unbind().1, vars);
        }
        _ => {}
    }
}

pub fn js_module_to_string(module: JsModule) -> String {
    let module = module_to_js_module(module);
    swc_module_to_string(module)
}

pub fn js_module_to_py_string(module: JsModule) -> String {
    let py_module = module_to_py_module(module);
    swc_module_to_string(py_module)
}
// this function generate the serialized var from the var_index and the Aplite type of the module_item:
// server.serialize(var_1, [json(var_1.type_)])

pub fn serialize(index: &VarUuid, type_: Judgment<JsPrim>) -> ModuleItem {
    // first let's make the json object associated to var_a.type_:
    let json_var_type_ = type_to_json(type_);
    let server_serialize = Expr::Member(MemberExpr {
        span: DUMMY_SP,
        obj: ExprOrSuper::Expr(Box::new(to_js_ident("server".into()))),
        prop: Box::new(to_js_ident("serialize".into())),
        computed: false,
    });

    let call_expr = to_js_app(
        server_serialize,
        vec![
            to_js_ident(format!("var_{}", index.index())),
            json_var_type_,
        ],
    );
    ModuleItem::Stmt(Stmt::Expr(ExprStmt {
        span: DUMMY_SP,
        expr: Box::new(call_expr),
    }))
}

// this function takes an Aplite type and encode it as a JSON object of binary tree of Pi's.

pub fn type_to_json(type_: Judgment<JsPrim>) -> Expr {
    match *type_.tree {
        xi_core::judgment::JudgmentKind::Pi(arg_type, return_type) => {
            //use pi_to_json:
            // ahhh what about free vars
            let args = vec![type_to_json(arg_type), type_to_json(return_type.unbind().1)];
            to_js_app(to_js_ident("pi_to_json".into()), args)
        }
        xi_core::judgment::JudgmentKind::Type => {
            unreachable!("we shouldn't see this here")
        }
        xi_core::judgment::JudgmentKind::Prim(prim, _) => match prim {
            JsPrim::StringType => json_kind("Str".into()),
            JsPrim::NumberType => json_kind("Int".into()),
            JsPrim::StringElem(_) => unreachable!("nope"),
            JsPrim::NumberElem(_) => unreachable!("nope"),
            JsPrim::Ffi(_, _) => unreachable!("nope"),
            JsPrim::Var(_) => unreachable!("nope"),
        },
        xi_core::judgment::JudgmentKind::FreeVar(_, _) => {
            panic!("ahhhh")
        }
        xi_core::judgment::JudgmentKind::Lam(_, _) => {
            unreachable!("we shouldn't see this here")
        }
        xi_core::judgment::JudgmentKind::BoundVar(_, _) => {
            unreachable!("we shouldn't see this here")
        }
        xi_core::judgment::JudgmentKind::App(_, _) => {
            panic!("idk")
        }
    }
}

// takes [str] to {kind: [str]}
pub fn json_kind(str: String) -> Expr {
    to_js_app(to_js_ident("json_kind".into()), vec![to_js_str(str)])
}

pub fn let_server_code(server_name: String) -> ModuleItem {
    let var_declarator = VarDeclarator {
        span: DUMMY_SP,
        name: Pat::Ident(BindingIdent {
            id: to_js_ident2("server".into()),
            type_ann: None,
        }),
        init: Some(Box::new(Expr::New(NewExpr {
            span: DUMMY_SP,
            callee: Box::new(to_js_ident("Server".into())),
            args: Some(vec![ExprOrSpread {
                spread: None,
                expr: Box::new(to_js_str(server_name)),
            }]),
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

// this backend takes an Aplite file and gets a Python and Js module:
fn module_to_js_py_modules(module: JsModule) -> JsPyModules {
    JsPyModules {
        js_backend: module_to_js_module(module.clone()),
        py: module_to_py_module(module),
    }
}
