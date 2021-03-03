mod rowan;
use tree_sitter::{Language, Parser, Tree};

extern "C" {
    fn tree_sitter_aplite() -> Language;
}

pub fn to_tree(source_code: &str) -> Tree {
    let mut parser = Parser::new();
    let language = unsafe { tree_sitter_aplite() };
    parser.set_language(language).unwrap();
    parser.parse(source_code, None).unwrap()
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
}
