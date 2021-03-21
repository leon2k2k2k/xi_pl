mod desugar;
mod resolve;
mod rowan_ast;
pub mod type_inference;
use desugar::text_to_judg_ment;
use tree_sitter::{Language, Parser, Tree};
use type_inference::{to_judgment, TypeError, UiPrim};
use xi_core::judgment::Judgment;

extern "C" {
    fn tree_sitter_aplite() -> Language;
}

pub fn to_tree(source_code: &str) -> Tree {
    let mut parser = Parser::new();
    let language = unsafe { tree_sitter_aplite() };
    parser.set_language(language).unwrap();
    parser.parse(source_code, None).unwrap()
}

pub fn frontend(text: &str) -> Result<Judgment<UiPrim, ()>, TypeError> {
    let judg_ment = text_to_judg_ment(text);
    to_judgment(judg_ment.0, judg_ment.1)
}

#[cfg(test)]
mod tests {
    #[test]
    fn parser_test() {
        use super::*;
        let source_code = "5";
        let tree = to_tree(source_code.into());
        let root_node = tree.root_node();

        // println!("{:?}", root_node.kind());
        println!("{:?}", root_node.child(0).unwrap().kind());
        let word = root_node.child(0).unwrap().child(0).unwrap();
        println!("{:?}", word);
        let text = &source_code[root_node.child(0).unwrap().child(0).unwrap().byte_range()];
        println!("{}", text);
    }

    mod test {
        use std::task::Context;

        #[test]
        fn test_to_judgment() {
            use super::super::*;
            let text1 = "fn foo |x : Type| -> Type {val x}
            val foo";
            use crate::desugar::*;
            let judgment1 = frontend(text1);
            dbg!(judgment1);

            let text2 = "fn foo |x| {val x} 
             val foo (Pi |y: Type| y)";
            let judgment2 = frontend(text2);

            dbg!(judgment2);
        }

        //     let text2 = "fn foo |x| -> {val x} val foo \"hello world\" ";
        //     let judgment2 = front_end(text2);
        //     dbg!(judgment2);

        //     let text3 = "let y = \"hello world\"  let x = y";
        //     let judgment3 = front_end(text3);
        //     dbg!(judgment3);
        // }
    }
}
