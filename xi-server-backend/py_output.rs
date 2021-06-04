use std::collections::BTreeMap;

use deno_core::serde_json::{json, Value};
use xi_backends::py_backend::{
    py_output::{
        module_item_to_stmt, py_expr_to_stmt, python_module_to_string, to_py_app, to_py_arguments,
        to_py_assign, to_py_await, to_py_ident, to_py_ident2, to_py_member, to_py_num, to_py_str,
        Expr, Mod, Stmt,
    },
    py_prim::{PyModule, PyPrim},
};
use xi_core::judgment::Judgment;
use xi_uuid::VarUuid;

pub fn module_to_py_mod(
    module: PyModule,
    _main_id: Option<VarUuid>,
    port: u32,
    other_port: u32,
) -> Mod {
    let mut body = vec![];
    let mut ffi_functions = BTreeMap::new();

    for (var_index, module_item) in &module.module_items {
        dbg!(&module_item.transport_info());
        if Some("py".into()) == module_item.transport_info().origin {
            dbg!("hello!!!");
            let module_items = module_item_to_stmt(&mut ffi_functions, module_item, var_index);
            body.extend(module_items);

            // check that if it needs to be transported somewhere:
            // server.register(var_[], "var_[]", var_type)
            if let Some(_server_name) = module_item.transport_info().transport {
                let register_stmt = register_top_level(var_index.clone(), module_item.type_());
                body.push(register_stmt);
            }
        } else if Some("py".into()) == module_item.transport_info().transport {
            // we need to deregister here.
            let deregister_stmt = deregister_top_level(var_index.clone(), module_item.type_());
            body.push(deregister_stmt);
        }
    }

    let mut ffi_imports = vec![];

    let import_asyncio = Stmt(json!({
        "ast_type": "Import",
        "names": [{"ast_type": "alias", "name": to_py_ident2("asyncio").0}],
    }));
    ffi_imports.push(import_asyncio);

    let asyncio_loop_stmt = asyncio_loop_stmt();
    let start_server = start_server(port, other_port);
    let mut vec = vec![asyncio_loop_stmt, start_server];
    ffi_imports.append(&mut vec);

    // let promise_resolve_decl = Stmt(json!({
    //     "ast_type": "FunctionDef",
    //     "name": to_py_ident2("promise_resolve").0,
    //     "args": to_py_arguments(vec![to_py_ident2("x")]).0,
    //     "body": [{
    //         "ast_type": "AsyncFunctionDef",
    //         "name": to_py_ident2("helper").0,
    //         "args": to_py_arguments(vec![]).0,
    //         "body": [{
    //             "ast_type": "Return",
    //             "value": to_py_ident("x").0,

    //         }],
    //         "decorator_list": [],
    //     }, {
    //         "ast_type": "Return",
    //         "value": to_py_ident("helper").0,
    //     }],
    //     "decorator_list": [],
    // }));

    // ffi_imports.push(promise_resolve_decl);

    for ((file_name, function_name), index) in ffi_functions {
        let file_no_extension = &file_name[..file_name.len() - 3];
        let module_import = Stmt(json!({
            "ast_type": "ImportFrom",
            "module": to_py_ident2(file_no_extension).0,
            "names": [{
                "ast_type": "alias",
                "name": to_py_ident2(function_name).0,
                "asname": to_py_ident2(format!("ffi{}", index.index())).0,
            }],
            "level": 0,
        }));

        ffi_imports.push(module_import);
    }

    // let run_main = Stmt(json!({
    //     "ast_type": "Expr",
    //     "value": to_py_await2(to_py_await2(to_py_ident1(make_var_name(main_id)))).0,
    // }));
    // body.push(run_main);

    let main_fn = Stmt(json!({
        "ast_type": "AsyncFunctionDef",
        "name": to_py_ident2("main").0,
        "args": to_py_arguments(vec![]).0,
        "body": Value::Array(body.into_iter().map(|x| x.0).collect()),
        "decorator_list": [],
    }));

    // let run_main2 = Stmt(json!({
    //     "ast_type": "Expr",
    //     "value": to_py_app(to_py_member(to_py_ident("asyncio"), to_py_ident("run")), vec![to_py_app(to_py_ident("main"), vec![])]).0,
    // }));

    let body_with_imports = {
        let mut t = ffi_imports;
        t.push(main_fn);
        // t.push(run_main2);
        t.append(&mut loop_run());
        t.into_iter().map(|x| x.0).collect()
    };

    Mod(json!({
        "ast_type": "Module",
        "body": Value::Array(body_with_imports),
    }))
}

// this function adds
// loop = asyncio.get_event_loop()
pub fn asyncio_loop_stmt() -> Stmt {
    let loop_ = to_py_ident("loop");
    let func = to_py_member(to_py_ident("asyncio"), to_py_ident("get_event_loop"));
    let value = to_py_app(func, vec![]);
    Stmt(json!({"ast_type": "Assign", "targets": [loop_.0], "value": value.0}))
}

// this starts the server
// server = Server(port, other_port, loop)
pub fn start_server(port: u32, other_port: u32) -> Stmt {
    let server = to_py_ident("server");
    let value = to_py_app(
        to_py_ident("Server"),
        vec![
            to_py_num(port.to_string()),
            to_py_num(other_port.to_string()),
            to_py_ident("loop"),
        ],
    );
    Stmt(json!({"ast_type": "Assign", "targets": [server.0], "value": value.0}))
}

// this produces stmts:
// loop.run_until_complete(main())
// loop.run_forever()

pub fn loop_run() -> Vec<Stmt> {
    let func = to_py_member(to_py_ident("loop"), to_py_ident("run_until_complete"));
    let run_until_expr = to_py_app(func, vec![to_py_app(to_py_ident("main"), vec![])]);
    let run_until_comp_stmt = Stmt(json!({
        "ast_type": "Expr",
        "value": run_until_expr.0,
    }));
    let run_forever_expr = to_py_app(
        to_py_member(to_py_ident("loop"), to_py_ident("run_forever")),
        vec![],
    );
    let run_forever = Stmt(json!({"ast_type": "Expr",
    "value": run_forever_expr.0,}));
    vec![run_until_comp_stmt, run_forever]
}

pub fn module_with_server_to_py_string(
    module: PyModule,
    main_id: Option<VarUuid>,
    port: u32,
    other_port: u32,
) -> String {
    let py_mod = module_to_py_mod(module, main_id, port, other_port);
    python_module_to_string(py_mod)
}

// write out the generated registration code:
// server.register_top_level(var_[], "var_[]", json_kind(type_))
pub fn register_top_level(index: VarUuid, type_: Judgment<PyPrim>) -> Stmt {
    let json_var_type_ = type_to_json(type_);
    let server_reg_top_level =
        to_py_member(to_py_ident("server"), to_py_ident("register_top_level"));
    let expr = to_py_app(
        server_reg_top_level,
        vec![
            to_py_ident(format!("var_{}", index.index())),
            to_py_str(format!("var_{}", index.index())),
            json_var_type_,
        ],
    );
    py_expr_to_stmt(expr)
}

// this is the deregister code:
// var_0 = promise_resolve(await server.deregister_top_level("var_0", int_type))

pub fn deregister_top_level(index: VarUuid, type_: Judgment<PyPrim>) -> Stmt {
    let await_expr = to_py_await(to_py_app(
        to_py_member(to_py_ident("server"), to_py_ident("deregister_top_level")),
        vec![
            to_py_str(format!("var_{}", index.index())),
            type_to_json(type_),
        ],
    ));
    let value = to_py_app(to_py_ident("promise_resolve"), vec![await_expr]);
    to_py_assign(vec![to_py_ident(format!("var_{}", index.index()))], value)
}
pub fn type_to_json(type_: Judgment<PyPrim>) -> Expr {
    match *type_.tree {
        xi_core::judgment::JudgmentKind::Pi(arg_type, return_type) => {
            let args = vec![type_to_json(arg_type), type_to_json(return_type.unbind().1)];
            to_py_app(to_py_ident("pi_to_json"), args)
        }
        xi_core::judgment::JudgmentKind::Prim(prim, _) => match prim {
            PyPrim::StringType => json_kind("Str".into()),
            PyPrim::NumberType => json_kind("Int".into()),
            PyPrim::StringElem(_) => todo!(),
            PyPrim::NumberElem(_) => todo!(),
            PyPrim::Ffi(_, _) => todo!(),
            PyPrim::Var(_) => todo!(),
        },

        xi_core::judgment::JudgmentKind::Type => unreachable!("we shoudn't see this here"),
        xi_core::judgment::JudgmentKind::FreeVar(_, _) => panic!("ahhhh"),
        xi_core::judgment::JudgmentKind::Lam(_, _) => unreachable!("we shouldn't see this here"),
        xi_core::judgment::JudgmentKind::BoundVar(_, _) => {
            unreachable!("we shouldn't see this here")
        }
        xi_core::judgment::JudgmentKind::App(_, _) => panic!("idk"),
    }
}

pub fn json_kind(str: String) -> Expr {
    to_py_app(to_py_ident("json_kind"), vec![to_py_str(str)])
}
