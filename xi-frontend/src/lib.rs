use desugar::source_file_to_judg_ment;
use resolve::parse_source_file;
use rowan_ast::{string_to_syntax, to_tree};
use type_inference::{to_judgment, TypeError, UiMetadata, UiPrim};
use xi_core::judgment::Judgment;

mod desugar;
mod resolve;
mod rowan_ast;
pub mod type_inference;

pub fn frontend(text: &str) -> Result<Judgment<UiPrim, UiMetadata>, TypeError> {
    let syntax_node = string_to_syntax(text);
    dbg!(&syntax_node);
    // syntax_node is the rowan tree level
    let source_file = parse_source_file(&syntax_node);
    dbg!(&source_file);
    // source_file is at the name resolution level
    let judg_ment = source_file_to_judg_ment(source_file);
    // judg_ment is at the desugar level.
    dbg!(&judg_ment);
    let judgment = to_judgment(judg_ment);
    dbg!(judgment)
    // this lands in Judgment<UiPrim, Metadata>
}

mod test {
    use crate::frontend;

    // #[test]
    // fn test_ffi() {
    //     use super::frontend;
    //     // let ffi_text = "ffi \"some_file.js\"{
    //     //     Int : Type,
    //     //     five : Int,
    //     //     six : Int,
    //     //     add : Int -> Int -> Int,
    //     //     int_to_string : Int -> String
    //     // }

    //     // let ans = add five six
    //     // let better_ans = add ans six
    //     // let even_better_ans = int_to_string(better_ans)

    //     // do console_output(even_better_ans)!
    //     // val unit!";
    //     let ffi_text = "ffi \"some_file.js\" {
    //         concat_hello: String -> String
    //        }

    //        let ans = concat_hello (\"world\")
    //        do console_output(ans)!
    //        val unit!";
    //     let judgment = frontend(ffi_text);
    //     dbg!(judgment).unwrap();
    // }

    // #[test]
    // fn test_add() {
    //     let text = "

    //     val 3 + 5";

    //     frontend(text);
    // }
}
