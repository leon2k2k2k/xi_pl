use desugar::text_to_judg_ment;
use type_inference::{to_judgment, TypeError, UiMetadata, UiPrim};
use xi_core::judgment::Judgment;

mod desugar;
mod resolve;
mod rowan_ast;
pub mod type_inference;

pub fn frontend(text: &str) -> Result<Judgment<UiPrim, UiMetadata>, TypeError> {
    let judg_ment = text_to_judg_ment(text);
    dbg!(&judg_ment);
    to_judgment(judg_ment)
}

mod test {
    #[test]
    fn test_ffi() {
        use super::frontend;
        // let ffi_text = "ffi \"some_file.js\"{
        //     Int : Type,
        //     five : Int,
        //     six : Int,
        //     add : Int -> Int -> Int,
        //     int_to_string : Int -> String
        // }

        // let ans = add five six
        // let better_ans = add ans six
        // let even_better_ans = int_to_string(better_ans)

        // do console_output(even_better_ans)!
        // val unit!";
        let ffi_text = "ffi \"some_file.js\" {
            concat_hello: String -> String   
           }
           
           let ans = concat_hello (\"world\")
           do console_output(ans)!
           val unit!";
        let judgment = frontend(ffi_text);
        dbg!(judgment).unwrap();
    }
}
