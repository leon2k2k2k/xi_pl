use std::{collections::BTreeMap, rc::Rc};

use xi_backends::{
    js_backend::js_prim::{self, JsDefineItem, JsModule, JsModuleItem, JsPrim},
    py_backend::py_prim::{PyDefineItem, PyModule, PyModuleItem, PyPrim, TransportInfo},
};
use xi_core::judgment::{Judgment, Primitive};
use xi_frontend::{type_inference::UiPrim, Module, ModuleAndImports, ModuleItem};
use xi_runtimes::js_runtime::js_runtime::JS_RUNTIME_FILE;
// use xi_runtimes::js_runtime::js_runtime::RUNTIME_FILE;
use xi_uuid::VarUuid;

pub fn add_to_module(module: &mut Module, modules: ModuleAndImports) {
    for (_index, item) in modules.imports {
        add_to_module(module, item)
    }

    for (index, item) in modules.module.module_items {
        match item {
            ModuleItem::Define(_) => module.add(index, item),
            ModuleItem::Import(_) => {}
        }
    }
}

pub fn combine_module_and_imports(modules: ModuleAndImports) -> Module {
    let mut module = Module::new();
    add_to_module(&mut module, modules);
    // dbg!("returning module", &module);
    return module;
}
// takes a module and get back a PyModule, which is exactly what is needed to produce a Py file.
pub fn front_to_py_back(modules: ModuleAndImports) -> PyModule {
    let module = combine_module_and_imports(modules);
    let mut py_module_items = BTreeMap::new();
    for (var_index, module_item) in module.module_items {
        let py_module_item = match module_item {
            ModuleItem::Define(define_item) => PyModuleItem::Define(PyDefineItem {
                name: define_item.name,
                transport_info: transport_info_front_to_py_back(define_item.backend),
                type_: ui_to_py_judgment(define_item.type_),
                impl_: ui_to_py_judgment(define_item.impl_),
            }),
            ModuleItem::Import(_) => panic!("don't support compiling module items"),
        };
        py_module_items.insert(var_index, py_module_item);
    }

    PyModule {
        str_to_index: module.str_to_index,
        module_items: py_module_items,
    }
}
pub fn front_to_js_back(modules: ModuleAndImports) -> JsModule {
    let module = combine_module_and_imports(modules);
    let mut js_module_items = BTreeMap::new();
    for (var_index, module_item) in module.module_items {
        let js_module_item = match module_item {
            ModuleItem::Define(define_item) => JsModuleItem::Define(JsDefineItem {
                name: define_item.name,
                transport_info: transport_info_front_to_js_back(define_item.backend),
                type_: ui_to_js_judgment(define_item.type_),
                impl_: ui_to_js_judgment(define_item.impl_),
            }),
            ModuleItem::Import(_) => panic!("don't support compiling module items"),
        };
        js_module_items.insert(var_index, js_module_item);
    }

    JsModule {
        str_to_index: module.str_to_index,
        module_items: js_module_items,
    }
}
pub fn transport_info_front_to_py_back(
    transport_info: xi_frontend::resolve::TransportInfo,
) -> xi_backends::py_backend::py_prim::TransportInfo {
    TransportInfo {
        origin: transport_info.origin,
        transport: transport_info.transport,
    }
}

pub fn transport_info_front_to_js_back(
    transport_info: xi_frontend::resolve::TransportInfo,
) -> xi_backends::js_backend::js_prim::TransportInfo {
    js_prim::TransportInfo {
        origin: transport_info.origin,
        transport: transport_info.transport,
    }
}

pub fn ui_to_js_judgment(front: Judgment<UiPrim>) -> Judgment<JsPrim> {
    // In the backend, make some primitive function to FFIs.
    fn make_ffi(name: &str, var_type: Judgment<JsPrim>) -> Judgment<JsPrim> {
        let runtime = JS_RUNTIME_FILE.into();
        let ffi = JsPrim::Ffi(runtime, name.into());

        Judgment::prim(ffi, var_type, None)
    }

    use xi_backends::js_backend::js_prim::JsPrim::*;
    use xi_frontend::type_inference::UiBinaryOp;
    use xi_proc_macro::term;

    let io_monad = make_ffi("IO", term!(U -> U));
    let UnitType = make_ffi("UnitType", term!(U));
    front.define_prim_unchecked(Rc::new(
        move |s, prim_type, define_prim| -> Judgment<JsPrim> {
            match s {
                UiPrim::IOMonad => io_monad.clone(),
                UiPrim::IOBind => make_ffi(
                    "io_bind",
                    term!(Pi |A : U, B : U| {io_monad} A -> (A -> {io_monad} B) -> {io_monad} B),
                ),
                UiPrim::IOPure => make_ffi("io_pure", term!(Pi |A : U| A -> {io_monad} A)),
                UiPrim::StringElem(str) => {
                    Judgment::prim_wo_prim_type(JsPrim::StringElem(str), None)
                }
                UiPrim::StringType => Judgment::prim_wo_prim_type(JsPrim::StringType, None),
                UiPrim::NumberElem(num) => {
                    Judgment::prim_wo_prim_type(JsPrim::NumberElem(num), None)
                }
                UiPrim::NumberType => Judgment::prim_wo_prim_type(JsPrim::NumberType, None),
                UiPrim::Binary(op) => match op {
                    UiBinaryOp::And => todo!(),
                    UiBinaryOp::Or => todo!(),
                    UiBinaryOp::Equal => todo!(),
                    UiBinaryOp::NotEqual => todo!(),
                    UiBinaryOp::LessThan => todo!(),
                    UiBinaryOp::LessThanEqual => todo!(),
                    UiBinaryOp::GreaterThan => todo!(),
                    UiBinaryOp::GreaterThanEqual => todo!(),
                    UiBinaryOp::Plus => {
                        make_ffi("plus", term!([NumberType] -> [NumberType] -> [NumberType]))
                    }
                    UiBinaryOp::Minus => {
                        make_ffi("minus", term!([NumberType] -> [NumberType] -> [NumberType]))
                    }
                    UiBinaryOp::Multiply => make_ffi(
                        "multiply",
                        term!([NumberType] -> [NumberType] -> [NumberType]),
                    ),
                    UiBinaryOp::Divide => make_ffi(
                        "divide",
                        term!([NumberType] -> [NumberType] -> [NumberType]),
                    ),
                    UiBinaryOp::Modulo => make_ffi(
                        "modulo",
                        term!([NumberType] -> [NumberType] -> [NumberType]),
                    ),
                },
                UiPrim::ConsoleOutput => make_ffi(
                    "console_output",
                    term!([StringType] -> {io_monad}{UnitType}),
                ),
                UiPrim::ConsoleInput => make_ffi("console_input", term!({ io_monad }[StringType])),
                UiPrim::UnitType => UnitType.clone(),
                UiPrim::Unit => make_ffi("unit", UnitType.clone()),
                UiPrim::Ffi(filename, ffi_name) => Judgment::prim(
                    JsPrim::Ffi(filename, ffi_name),
                    define_prim(prim_type),
                    None,
                ),
                UiPrim::Global(index) => {
                    Judgment::prim(JsPrim::Var(index), define_prim(prim_type), None)
                }
            }
        },
    ))
}

pub fn ui_to_py_judgment(front: Judgment<UiPrim>) -> Judgment<PyPrim> {
    // In the backend, make some primitive function to FFIs.
    fn make_ffi(name: &str, var_type: Judgment<PyPrim>) -> Judgment<PyPrim> {
        let runtime = "runtime.py";
        let ffi = PyPrim::Ffi(runtime.into(), name.into());

        Judgment::prim(ffi, var_type, None)
    }

    use xi_backends::py_backend::py_prim::PyPrim::*;
    use xi_frontend::type_inference::UiBinaryOp;
    use xi_proc_macro::term;

    let io_monad = make_ffi("IO", term!(U -> U));
    let UnitType = make_ffi("UnitType", term!(U));
    front.define_prim_unchecked(Rc::new(
        move |s, prim_type, define_prim| -> Judgment<PyPrim> {
            match s {
                UiPrim::IOMonad => io_monad.clone(),
                UiPrim::IOBind => make_ffi(
                    "io_bind",
                    term!(Pi |A : U, B : U| {io_monad} A -> (A -> {io_monad} B) -> {io_monad} B),
                ),
                UiPrim::IOPure => make_ffi("io_pure", term!(Pi |A : U| A -> {io_monad} A)),
                UiPrim::ConsoleOutput => make_ffi(
                    "console_output",
                    term!([StringType] -> {io_monad}{UnitType}),
                ),
                UiPrim::ConsoleInput => make_ffi("console_input", term!({io_monad }{UnitType})),
                UiPrim::UnitType => UnitType.clone(),
                UiPrim::Unit => make_ffi("unit", UnitType.clone()),
                UiPrim::StringElem(str) => {
                    Judgment::prim_wo_prim_type(PyPrim::StringElem(str), None)
                }
                UiPrim::StringType => Judgment::prim_wo_prim_type(PyPrim::StringType, None),
                UiPrim::NumberElem(num) => {
                    Judgment::prim_wo_prim_type(PyPrim::NumberElem(num), None)
                }
                UiPrim::NumberType => Judgment::prim_wo_prim_type(PyPrim::NumberType, None),
                UiPrim::Binary(op) => match op {
                    UiBinaryOp::And => todo!(),
                    UiBinaryOp::Or => todo!(),
                    UiBinaryOp::Equal => todo!(),
                    UiBinaryOp::NotEqual => todo!(),
                    UiBinaryOp::LessThan => todo!(),
                    UiBinaryOp::LessThanEqual => todo!(),
                    UiBinaryOp::GreaterThan => todo!(),
                    UiBinaryOp::GreaterThanEqual => todo!(),
                    UiBinaryOp::Plus => {
                        make_ffi("plus", term!([NumberType] -> [NumberType] -> [NumberType]))
                    }
                    UiBinaryOp::Minus => {
                        make_ffi("minus", term!([NumberType] -> [NumberType] -> [NumberType]))
                    }
                    UiBinaryOp::Multiply => make_ffi(
                        "multiply",
                        term!([NumberType] -> [NumberType] -> [NumberType]),
                    ),
                    UiBinaryOp::Divide => make_ffi(
                        "divide",
                        term!([NumberType] -> [NumberType] -> [NumberType]),
                    ),
                    UiBinaryOp::Modulo => make_ffi(
                        "modulo",
                        term!([NumberType] -> [NumberType] -> [NumberType]),
                    ),
                },
                UiPrim::Ffi(filename, ffi_name) => Judgment::prim(
                    PyPrim::Ffi(filename, ffi_name),
                    define_prim(prim_type),
                    None,
                ),
                UiPrim::Global(index) => {
                    Judgment::prim(PyPrim::Var(index), define_prim(prim_type), None)
                }
            }
        },
    ))
}
