use serde_json::{json, Value};
use std::{
    collections::BTreeMap,
    io::Write,
    process::{Command, Stdio},
    str::FromStr,
};
use xi_core::judgment::{Judgment, JudgmentKind};
use xi_uuid::VarUuid;

use crate::py_backend::py_prim::{PyModule, PyModuleItem, PyPrim};

#[derive(Clone, Debug)]
pub struct Mod(pub Value);
#[derive(Clone, Debug)]
pub struct Stmt(pub Value);
#[derive(Clone, Debug)]
pub struct Expr(pub Value);
#[derive(Clone, Debug)]
pub struct Identifier(pub Value);

#[derive(Clone, Debug)]
pub struct Arguments(pub Value);

pub fn judgment_to_mod(
    ffi_functions: &mut BTreeMap<(String, String), VarUuid>,
    judgment: Judgment<PyPrim>,
) -> (Vec<Stmt>, Expr) {
    let (func_defs, main_py) = to_py(&judgment, BTreeMap::new(), ffi_functions);

    (func_defs, main_py)
}

pub fn module_item_to_stmt(
    ffi_functions: &mut BTreeMap<(String, String), VarUuid>,
    module_item: PyModuleItem,
    var_index: VarUuid,
) -> (Vec<Stmt>) {
    match module_item {
        PyModuleItem::Define(define_item) => {
            let (func_defs, expr) = judgment_to_mod(ffi_functions, define_item.impl_);

            let var_name = to_py_ident1(make_var_name(var_index));

            let stmt =
                Stmt(json!({"ast_type": "Assign", "targets": [var_name.0], "value": expr.0}));

            let mut result = func_defs;
            result.push(stmt);

            result
        }
    }
}

pub fn module_to_python_module(module: PyModule, main_id: VarUuid) -> Mod {
    let mut body = vec![];
    let mut ffi_functions = BTreeMap::new();

    for (var_index, module_item) in &module.module_items {
        let module_items = module_item_to_stmt(&mut ffi_functions, module_item.clone(), *var_index);
        body.extend(module_items);
    }

    let mut ffi_imports = vec![];

    let import_asyncio = Stmt(json!({
        "ast_type": "Import",
        "names": [{"ast_type": "alias", "name": to_py_ident2("asyncio").0}],
    }));

    ffi_imports.push(import_asyncio);

    let promise_resolve_decl = Stmt(json!({
        "ast_type": "FunctionDef",
        "name": to_py_ident2("promise_resolve").0,
        "args": to_py_arguments(vec![to_py_ident2("x")]).0,
        "body": [{
            "ast_type": "AsyncFunctionDef",
            "name": to_py_ident2("helper").0,
            "args": to_py_arguments(vec![]).0,
            "body": [{
                "ast_type": "Return",
                "value": to_py_ident("x").0,

            }],
            "decorator_list": [],
        }, {
            "ast_type": "Return",
            "value": to_py_ident("helper").0,
        }],
        "decorator_list": [],
    }));

    ffi_imports.push(promise_resolve_decl);

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

    let run_main = Stmt(json!({
        "ast_type": "Expr",
        "value": to_py_await2(to_py_await2(to_py_ident1(make_var_name(main_id)))).0,
    }));
    body.push(run_main);

    let main_fn = Stmt(json!({
        "ast_type": "AsyncFunctionDef",
        "name": to_py_ident2("main").0,
        "args": to_py_arguments(vec![]).0,
        "body": Value::Array(body.into_iter().map(|x| x.0).collect()),
        "decorator_list": [],
    }));

    let run_main2 = Stmt(json!({
        "ast_type": "Expr",
        "value": to_py_app(to_py_member(to_py_ident("asyncio"), to_py_ident("run")), vec![to_py_app(to_py_ident("main"), vec![])]).0,
    }));

    let body_with_imports = {
        let mut t = ffi_imports;
        t.push(main_fn);
        t.push(run_main2);
        t.into_iter().map(|x| x.0).collect()
    };

    Mod(json!({
        "ast_type": "Module",
        "body": Value::Array(body_with_imports),
    }))
}

pub fn python_module_to_string(module: Mod) -> String {
    eprintln!("{}", &serde_json::to_string_pretty(&module.0).unwrap());

    let json = module.0.to_string();
    let python_file = include_str!("json_to_ast.py");
    let mut python_process = Command::new("python3")
        .arg("-c")
        .arg(python_file)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .ok()
        .expect("Failed to spawn python");

    python_process
        .stdin
        .as_mut()
        .unwrap()
        .write(json.as_bytes())
        .unwrap();

    let output = python_process
        .wait_with_output()
        .expect("Failed to execute python");

    if !output.status.success() {
        dbg!(output.stderr);
        panic!("Failed to execute python");
    }

    std::str::from_utf8(&output.stdout).unwrap().into()
}

pub fn module_to_py_string(module: PyModule, main_id: VarUuid) -> String {
    let module = module_to_python_module(module, main_id);
    python_module_to_string(module)
}

pub fn make_var_name(index: VarUuid) -> Identifier {
    to_py_ident2(format!("var_{}", index.index()))
}

pub fn to_py(
    judgment: &Judgment<PyPrim>,
    ctx: BTreeMap<VarUuid, Identifier>,
    ffi: &mut BTreeMap<(String, String), VarUuid>,
) -> (Vec<Stmt>, Expr) {
    match &*judgment.tree {
        JudgmentKind::Type => (vec![], to_py_str("Type")),
        JudgmentKind::Prim(t, _prim_type) => (vec![], PyPrim::to_py_prim(&t, ffi)),
        JudgmentKind::FreeVar(var_index, _var_type) => match ctx.get(var_index) {
            Some(ident) => (vec![], promise_resolve(to_py_ident1(ident.clone()))),
            None => {
                unreachable!("this means that we have a loose free variable")
            }
        },
        JudgmentKind::Pi(_, _) => (vec![], to_py_str("pi")),
        JudgmentKind::Lam(_var_type, sexpr) => {
            let (index, expr) = sexpr.clone().unbind();
            let var_name = make_var_name(index);

            let (sub_func_defs, sub_expr) =
                to_py(&expr, add_to_ctx(ctx, index, &var_name.clone()), ffi);

            let mut func_body = sub_func_defs.into_iter().map(|x| x.0).collect::<Vec<_>>();
            func_body.push(json!({"ast_type": "Return", "value": sub_expr.0}));

            let func_def = Stmt(json!({
                "ast_type": "AsyncFunctionDef",
                "decorator_list": [],
                "name": var_name.0.clone(),
                "args": to_py_arguments(vec![var_name.clone()]).0,
                "body": Value::Array(func_body),
            }));

            (vec![func_def], promise_resolve(to_py_ident1(var_name)))
        }
        JudgmentKind::BoundVar(_, _var_type) => {
            unreachable!("we use unbind so should never see a BoundVar")
        }
        JudgmentKind::App(func, arg) => {
            let (sub_func_defs_func, sub_expr_func) = to_py(&*func, ctx.clone(), ffi);
            let (sub_func_defs_arg, sub_expr_arg) = to_py(&*arg, ctx.clone(), ffi);

            let mut result_func_defs = sub_func_defs_func;
            result_func_defs.extend(sub_func_defs_arg);

            (
                result_func_defs,
                to_py_await(to_py_app(
                    to_py_await2(sub_expr_func),
                    vec![to_py_await2(sub_expr_arg)],
                )),
            )
        }
    }
}

pub fn add_to_ctx<T: Ord, U: Clone>(v: BTreeMap<T, U>, index: T, x: &U) -> BTreeMap<T, U> {
    let mut v = v;
    v.insert(index, x.clone());
    v
}

pub fn to_py_arguments(args: Vec<Identifier>) -> Arguments {
    let args2 = args
        .iter()
        .map(|arg| json!({"ast_type": "arg", "arg": arg.0}))
        .collect();

    Arguments(json!({
        "ast_type": "arguments",
        "args": Value::Array(args2),
        "defaults": [],
    }))
}

pub fn to_py_ident(name: impl Into<String>) -> Expr {
    to_py_ident1(to_py_ident2(name))
}

pub fn to_py_ident1(var_name: Identifier) -> Expr {
    Expr(json!({"ast_type": "Name", "id": var_name.0}))
}

pub fn to_py_ident2(name: impl Into<String>) -> Identifier {
    Identifier(json!(name.into()))
}

pub fn to_py_member(obj: Expr, prop: Expr) -> Expr {
    Expr(json!({"ast_type": "Attribute", "value": obj.0, "attr": prop.0}))
}

pub fn to_py_app(func: Expr, args: Vec<Expr>) -> Expr {
    let real_args = Value::Array(args.into_iter().map(|x| x.0).collect());
    Expr(json!({"ast_type": "Call", "func": func.0, "args": real_args, "keywords":[]}))
}

pub fn to_py_await(expr: Expr) -> Expr {
    Expr(json!({"ast_type": "Await", "value": expr.0}))
}

pub fn to_py_await2(expr: Expr) -> Expr {
    // Transforms await (promise_resolve(x)) => x
    if let Some(map) = expr.0.as_object() {
        if map["ast_type"] == "Call" {
            if map["func"] == to_py_ident("promise_resolve").0 {
                return Expr(map["args"].as_array().unwrap()[0].clone());
            }
        }
    }

    Expr(json!({"ast_type": "Await", "value": to_py_app(expr, vec![]).0}))
}

///Take a rust string and returns a javascript string object
pub fn to_py_str(string: impl Into<String>) -> Expr {
    Expr(json!({"ast_type": "Constant", "value": string.into()}))
}

pub fn to_py_num(num: String) -> Expr {
    let num = Value::from_str(&num).unwrap();
    Expr(json!({"ast_type": "Constant", "value": num}))
}

pub fn promise_resolve(expr: Expr) -> Expr {
    to_py_app(to_py_ident("promise_resolve"), vec![expr])
}

pub fn run_io(expr: Expr) -> Expr {
    to_py_app(
        to_py_member(to_py_ident("asyncio"), to_py_ident("run")),
        vec![to_py_app(expr, vec![])],
    )
}

pub fn py_expr_to_stmt(expr: Expr) -> Stmt {
    Stmt(json!({"ast_type": "Expr", "value": expr.0}))
}

pub fn to_py_assign(targets: Vec<Expr>, value: Expr) -> Stmt {
    let exprs: Vec<Value> = targets.into_iter().map(|x| x.0).collect();
    Stmt(json!({"ast_type": "Assign", "targets": exprs, "value": value.0,}))
}
