use tree_sitter::{Language, Parser};

extern "C" {
    fn tree_sitter_aplite() -> Language;
}

#[cfg(test)]
mod tests {
    #[test]
    fn parser_test() {
        use super::*;

        let mut parser = Parser::new();
        let language = unsafe { tree_sitter_aplite() };
        parser.set_language(language).unwrap();

        let source_code = "let x = 5";
        let tree = parser.parse(source_code, None).unwrap();
        let root_node = tree.root_node();

        println!("{:?}", tree)
        
    }
}
