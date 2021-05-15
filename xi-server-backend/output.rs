use std::collections::BTreeMap;
use swc_common::DUMMY_SP;
use swc_ecma_ast::{
    ExprStmt, ImportDecl, ImportNamedSpecifier, ImportSpecifier, Module, ModuleDecl, ModuleItem,
    Stmt, Str,
};

use xi_backend::{js_prim::JsModule, output::to_js_ident};
use xi_backend::{
    js_prim::JsPrim,
    output::{
        module_item_to_swc_module_item, swc_module_to_string, to_js_app, to_js_ident2, to_js_str,
        JsMetadata,
    },
};
use xi_core::judgment::Judgment;
use xi_uuid::VarUuid;

pub struct JsPyModules {
    js_backend: Module,
    py: Module,
}

// this output is different as we need to import server and update_server,
// and run the server in the beginning of the file, and stuff.

pub fn module_to_js_backend_module(module: JsModule) -> Module {
    let mut body = vec![];
    // first we want to include the module item :
    // import { new_server, update_server } from "./new_server.js";

    let new_server = ImportSpecifier::Named(ImportNamedSpecifier {
        span: DUMMY_SP,
        local: to_js_ident2("run_server".into()),
        imported: Some(to_js_ident2("run_server".into())),
    });
    let update_server = ImportSpecifier::Named(ImportNamedSpecifier {
        span: DUMMY_SP,
        local: to_js_ident2("update_server".into()),
        imported: Some(to_js_ident2("update_server".into())),
    });
    let module_item = ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
        span: DUMMY_SP,
        specifiers: vec![new_server, update_server],
        src: Str {
            span: DUMMY_SP,
            value: "./server.js".into(),
            has_escape: false,
            kind: swc_ecma_ast::StrKind::Synthesized,
        },
        type_only: false,
        asserts: None,
    }));

    body.push(module_item);

    // after importing we immediately run the server:
    let module_item = ModuleItem::Stmt(Stmt::Expr(ExprStmt {
        span: DUMMY_SP,
        expr: Box::new(to_js_app(to_js_ident("run_server".into()), vec![])),
    }));

    body.push(module_item);

    // for everything that we introduced, we also need to call update server on it.
    // if we have a var name var122, then we need update_server("var122", var122).
    let mut var_names = vec![];

    let mut ffi_functions = BTreeMap::new();

    for (var_index, module_item) in &module.module_items {
        let (new_ffi_functions, module_item) =
            module_item_to_swc_module_item(module_item.clone(), *var_index);
        ffi_functions.extend(new_ffi_functions);
        // we add everything to the var_names now:
        // first the ffi functions, which are ffi{var_index}
        for (_, index) in ffi_functions.clone() {
            var_names.push(format!("ffi{}", index.index()));
        }
        // now the module_item itself:
        var_names.push(format!("var_{}", var_index.clone().index()).into());

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
    // // I don't think we need run_main
    // let run_main = ModuleItem::Stmt(Stmt::Expr(ExprStmt {
    //     span: DUMMY_SP,
    //     expr: Box::new(run_io(Expr::Ident(make_var_name(main_id)))),
    // }));

    let body_with_imports = {
        let mut t = ffi_imports;
        t.extend(body);
        // t.push(run_main);
        // now I want to apply server_registration to every var_name!
        // server_registration("var_name", var_name)
        for var_name in var_names {
            let expr = to_js_app(
                to_js_ident("update_server".into()),
                vec![to_js_str(var_name.clone()), to_js_ident(var_name)],
            );
            let module_item = ModuleItem::Stmt(Stmt::Expr(ExprStmt {
                span: DUMMY_SP,
                expr: Box::new(expr),
            }));
            t.push(module_item)
        }
        t
    };

    Module {
        span: DUMMY_SP,
        body: body_with_imports,
        shebang: None,
    }
}

// this is corresponding python file, transporting all the javascript to python.
pub fn module_to_py_module(module: JsModule) -> Module {
    // I think the only thing I need to know to transport a file over is its types.

    // same as js, we need to run a server and update stuff.
    let mut body = vec![];
    // importing {run_server, update_server} from "./py_server.js"
    let new_server = ImportSpecifier::Named(ImportNamedSpecifier {
        span: DUMMY_SP,
        local: to_js_ident2("run_server".into()),
        imported: Some(to_js_ident2("run_server".into())),
    });
    let update_server = ImportSpecifier::Named(ImportNamedSpecifier {
        span: DUMMY_SP,
        local: to_js_ident2("update_server".into()),
        imported: Some(to_js_ident2("update_server".into())),
    });
    let module_item = ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
        span: DUMMY_SP,
        specifiers: vec![new_server, update_server],
        src: Str {
            span: DUMMY_SP,
            value: "./py_server.js".into(),
            has_escape: false,
            kind: swc_ecma_ast::StrKind::Synthesized,
        },
        type_only: false,
        asserts: None,
    }));

    body.push(module_item);
    // after importing we immediately run the server:
    let module_item = ModuleItem::Stmt(Stmt::Expr(ExprStmt {
        span: DUMMY_SP,
        expr: Box::new(to_js_app(to_js_ident("run_server".into()), vec![])),
    }));

    body.push(module_item);

    // now for every single module, item, we want to transport it over:
    for (var_index, module_item) in &module.module_items {
        let type_ = module_item.type_();
        let module_item = js_to_python(*var_index, type_);
        body.push(module_item)
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

pub fn js_to_python(var_index: VarUuid, type_: Judgment<JsPrim, JsMetadata>) -> ModuleItem {
    // first we get the list of variables.
    let mut var_types: Vec<Judgment<JsPrim, JsMetadata>> = vec![];
    get_vars(type_, &mut var_types);
    // then we construct the body of the function, which is a blockstmt
    let mut stmts = vec![];
    // first thing is the update_server stuff:
    let var_indexes = var_indexes;
    for var_index in var_indexes {
        let var_name = format!("local{}", var_index.index());
        let expr = to_js_app(
            to_js_ident("update_server".into()),
            vec![to_js_str(var_name), to_js_ident(var_name)],
        );
        let stmt = Stmt::Expr(ExprStmt {
            span: DUMMY_SP,
            expr: Box::new(expr),
        });
        stmts.push(stmt);
    }

    // now we add the body and stuff. I think I can automate it away.

    todo!();
}

pub fn get_vars(type_: Judgment<JsPrim, JsMetadata>, vars: &mut Vec<Judgment<JsPrim, JsMetadata>>) {
    match *type_.tree {
        xi_core::judgment::JudgmentKind::Pi(var_type, expr) => {
            vars.push(var_type);
            get_vars(expr.unbind().1, vars);
        }
        _ => {}
    }
}

// pub fn js_module_to_string(module: JsModule) -> String {
//     let module = module_to_swc_module(module);
//     let string = swc_module_to_string(module);
//     string
// }
