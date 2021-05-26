use desugar::desugar_module_stmt;
use resolve::{parse_module_stmt, StmtKind, TransportInfo};
use rowan::SyntaxNode;
use rowan_ast::{nonextra_children, string_to_syntax, Lang};
use std::rc::Rc;
use std::{cell::RefCell, collections::BTreeMap};
use type_inference::{type_infer_mod_ule_item, UiPrim};
use xi_core::judgment::Judgment;
use xi_uuid::VarUuid;
pub mod desugar;
pub mod resolve;
pub mod rowan_ast;
pub mod type_inference;
#[derive(Clone, Debug)]

pub struct Module {
    pub str_to_index: BTreeMap<String, VarUuid>,
    pub module_items: BTreeMap<VarUuid, ModuleItem>,
}
#[derive(Clone, Debug)]

pub struct ModuleAndImports {
    pub module: Module,
    pub imports: BTreeMap<VarUuid, ModuleAndImports>,
}

impl ModuleAndImports {
    fn new() -> ModuleAndImports {
        ModuleAndImports {
            module: Module::new(),
            imports: BTreeMap::new(),
        }
    }

    // takes a syntaxnode child, parse it with the module (the imports), and then add it on at the end.
    pub fn add_stmt_to_module_item(&mut self, child: &SyntaxNode<Lang>) {
        let (module_stmt, resolve_var) = parse_module_stmt(&mut self.module, child);
        if let StmtKind::Import(file_name, imports) = module_stmt.0 {
            // we need to parse the file and get their module_and_imports out.
            let file_contents = std::fs::read_to_string(file_name.clone()).unwrap();
            let imported_module = ui_to_module(&file_contents);
            let file_index = VarUuid::new();
            self.imports.insert(file_index, imported_module.clone());
            // now we need to extrat the imports from there, together with their type:
            for import in imports {
                let import_index = imported_module
                    .module
                    .str_to_index
                    .get(&import.name)
                    .unwrap();
                let import_type = imported_module
                    .module
                    .module_items
                    .get(import_index)
                    .unwrap()
                    .type_();
                let import_item = ImportItem {
                    name: import.name,
                    type_: import_type,
                    import_info: (file_index, *import_index),
                };
                let module_item = ModuleItem::Import(import_item);
                self.module.add(*import_index, module_item);
            }
        } else {
            let mod_ule_items = desugar_module_stmt(module_stmt, resolve_var);
            for mod_ule_item in mod_ule_items {
                let (index, module_item) =
                    type_infer_mod_ule_item(&self.module, &mod_ule_item).unwrap();
                self.module.add(index, module_item);
            }
        }
    }
}

impl Module {
    pub fn new() -> Module {
        Module {
            str_to_index: BTreeMap::new(),
            module_items: BTreeMap::new(),
        }
    }

    pub fn add(&mut self, index: VarUuid, module_item: ModuleItem) {
        let name = match module_item.clone() {
            ModuleItem::Define(define_item) => define_item.name,
            ModuleItem::Import(import_item) => import_item.name,
        };
        self.str_to_index.insert(name, index);
        self.module_items.insert(index, module_item);
    }
}

#[derive(Clone, Debug)]
pub enum ModuleItem {
    Define(DefineItem),
    Import(ImportItem),
}

impl ModuleItem {
    fn type_(&self) -> Judgment<UiPrim> {
        match self {
            ModuleItem::Define(define_item) => define_item.type_.clone(),
            ModuleItem::Import(import_item) => import_item.type_.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct DefineItem {
    pub name: String,
    pub backend: TransportInfo,
    pub type_: Judgment<UiPrim>,
    pub impl_: Judgment<UiPrim>,
    // pub publicity
}
#[derive(Clone, Debug)]

pub struct ImportItem {
    pub name: String,
    pub type_: Judgment<UiPrim>,
    pub import_info: (VarUuid, VarUuid), // (file_name, func_index)
}
// #[derive(Clone, Debug)]
// pub struct ImportItem {
//     pub name : String,
//     pub type_: Judgment<UiPrim>,
//     pub

// }

// #[derive(Clone, Debug)]

// pub enum Publicity {
//     Private,
//     Public,
// }

pub fn ui_to_module(text: &str) -> ModuleAndImports {
    let syntax_node = string_to_syntax(text);
    // dbg!(&syntax_node);
    // syntax_node is the rowan tree level
    let mut module_and_imports = ModuleAndImports::new();

    for child in nonextra_children(&syntax_node) {
        module_and_imports.add_stmt_to_module_item(&child);
    }
    module_and_imports
}

pub fn compile_module_item(
    module_and_imports: ModuleAndImports,
    func_name: &str,
) -> Judgment<UiPrim> {
    let index = *module_and_imports
        .module
        .str_to_index
        .get(func_name)
        .unwrap();
    let empty_cache = Rc::new(RefCell::new(BTreeMap::new()));
    compile_module_item_from_index(module_and_imports, index, empty_cache).clone()
}

pub fn compile_module_item_from_index(
    module_and_imports: ModuleAndImports,
    index: VarUuid,
    cache: Rc<RefCell<BTreeMap<VarUuid, Judgment<UiPrim>>>>,
) -> Judgment<UiPrim> {
    let maybe_cached_body = cache.clone().borrow().get(&index).cloned();
    if let Some(cached_result) = maybe_cached_body {
        return cached_result;
    }

    let cache_clone = cache.clone();
    let module_item = module_and_imports.module.module_items.get(&index).unwrap();

    let result = match module_item {
        ModuleItem::Define(define_item) => {
            let impl_ = define_item.clone().impl_;
            let run_impl_: Judgment<UiPrim> = impl_.define_prim_unchecked(Rc::new(
                move |prim: UiPrim, prim_type, define_prim_unchecked| match prim {
                    UiPrim::Global(index1) => compile_module_item_from_index(
                        module_and_imports.clone(),
                        index1,
                        cache_clone.clone(),
                    ),
                    _ => Judgment::prim(prim, define_prim_unchecked(prim_type), None),
                },
            ));
            run_impl_
        }
        ModuleItem::Import(import_item) => {
            let (file_index, import_index) = import_item.import_info;
            let mod_and_imp = module_and_imports.imports.get(&file_index).unwrap().clone();
            compile_module_item_from_index(mod_and_imp, import_index, cache.clone())
        }
    };

    cache.borrow_mut().insert(index, result.clone());
    result
}

pub fn get_impl_(module: Module, index: VarUuid) -> Option<Judgment<UiPrim>> {
    let module_item = module.module_items.get(&index)?;
    if let ModuleItem::Define(define_item) = module_item {
        Some(define_item.impl_.clone())
    } else {
        todo!()
    }
}
