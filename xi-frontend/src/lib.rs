use desugar::desugar_module_stmt;
use resolve::{parse_module_stmt, Var};
use rowan::SyntaxNode;
use rowan_ast::{nonextra_children, string_to_syntax, Lang};
use std::collections::BTreeMap;
use type_inference::{type_infer_mod_ule_item, TypeError, UiMetadata, UiPrim};
use xi_core::judgment::{Judgment, Metadata, Primitive};
use xi_uuid::VarUuid;

mod desugar;
mod resolve;
mod rowan_ast;
pub mod type_inference;
#[derive(Clone, Debug)]

pub struct Module {
    pub str_to_index: BTreeMap<String, VarUuid>,
    pub module_items: BTreeMap<VarUuid, ModuleItem>,
}

impl Module {
    pub fn new() -> Module {
        Module {
            str_to_index: BTreeMap::new(),
            module_items: BTreeMap::new(),
        }
    }
    // takes a syntaxnode child, parse it with the module (the imports), and then add it on at the end.
    pub fn add_stmt_to_module_item(&mut self, child: &SyntaxNode<Lang>) {
        let (name, module_stmt) = parse_module_stmt(self, child);
        let mod_ule_item = desugar_module_stmt(self, module_stmt);
        let module_item = type_infer_mod_ule_item(self, mod_ule_item);
        self.add(name, module_item)
    }

    pub fn add(&mut self, name: String, module_item: ModuleItem) {
        let index = VarUuid::new();
        self.str_to_index.insert(name, index);
        self.module_items.insert(index, module_item);
    }
}

#[derive(Clone, Debug)]
pub enum ModuleItem {
    Define(DefineItem),
    // Import(ImportItem),
    // impl_: Judgment<UiPrim, UiMetadata>,
    // pub type_: Judgment<UiPrim, UiMetadata>,
    // pub name: String,
    // pub index: VarUuid,
    // // pub public_or_private: Publicity,
    // pub dependencies: Vec<VarUuid>
}

#[derive(Clone, Debug)]
pub struct DefineItem {
    pub type_: Judgment<UiPrim, UiMetadata>,
    pub impl_: Judgment<UiPrim, UiMetadata>,
    pub type_dependencies: Vec<VarUuid>,
    // pub publicity
}
// #[derive(Clone, Debug)]
// pub struct ImportItem {
//     pub name : String,
//     pub type_: Judgment<UiPrim, UiMetadata>,
//     pub

// }

// #[derive(Clone, Debug)]

// pub enum Publicity {
//     Private,
//     Public,
// }

pub fn frontend(text: &str) -> Module {
    let syntax_node = string_to_syntax(text);
    // dbg!(&syntax_node);
    // syntax_node is the rowan tree level
    let mut module = Module::new();
    for child in nonextra_children(&syntax_node) {
        module.add_stmt_to_module_item(&child);
    }
    module
}

//     // #[test]
//     // fn test_ffi() {
//     //     use super::frontend;
//     //     // let ffi_text = "ffi \"some_file.js\"{
//     //     //     Int : Type,
//     //     //     five : Int,
//     //     //     six : Int,
//     //     //     add : Int -> Int -> Int,
//     //     //     int_to_string : Int -> String
//     //     // }

//     //     // let ans = add five six
//     //     // let better_ans = add ans six
//     //     // let even_better_ans = int_to_string(better_ans)

//     //     // do console_output(even_better_ans)!
//     //     // val unit!";
//     //     let ffi_text = "ffi \"some_file.js\" {
//     //         concat_hello: String -> String
//     //        }

//     //        let ans = concat_hello (\"world\")
//     //        do console_output(ans)!
//     //        val unit!";
//     //     let judgment = frontend(ffi_text);
//     //     dbg!(judgment).unwrap();
//     // }

//     // #[test]
//     // fn test_add() {
//     //     let text = "

//     //     val 3 + 5";

//     //     frontend(text);
//     // }
// }
