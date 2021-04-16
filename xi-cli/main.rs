// #![allow(dead_code)]
// // Take an Aplite UI &str and returns a string of JavaScript.
fn ui_to_js(text: &str, func_name: &str) -> String {
    use xi_backend::output::to_js_program;
    use xi_frontend::{compile_module_item, ui_to_module};
    use xi_kernel::front_to_back::front_to_back;
    let module = ui_to_module(text);
    dbg!(&module);
    let compiled_judgment = compile_module_item(module, func_name);
    let backend_judgment = front_to_back(compiled_judgment);
    to_js_program(backend_judgment)
}

// fn ap_lib_to_judg(text: &str) -> Vec<ModuleItem> {
//     unimplemented!("please implement this")
// }

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    use xi_backend::runtime;

    // let str = "fn foo |x| {val x}
    // val foo (Pi|y: Type| y)";

    // runtime::run_js_from_string(ui_to_js(str)).await;
    // println!("hi");
    // Ok(());

    // output(in) >= pure()
    // use xi_backend::jsprim::{
    //     JsIO,
    //     JsPrim::{self, *},
    //     JsType,
    // };
    // use xi_core::judgment::Judgment;
    // use xi_proc_macro::term;

    // let string = "let in = console_input!
    // let y = console_output(in)!
    // val unit!";

    // runtime::run_js_from_string(ui_to_js(string)).await?;
    // println!("hi");

    // let string2 = "let str = \" hello \"
    // let y = console_output(str)!
    // val unit!";
    let input = std::env::args().collect::<Vec<_>>();
    let file_name = std::fs::read_to_string(input[1].clone())?;
    let func_name = input[2].clone();

    let js = ui_to_js(&file_name, &func_name);

    println!("{}", js);
    runtime::run_js_from_string(js).await?;
    Ok(())
    // // dbg!(JsPrim::console_output1());
    // let judgment: Judgment<JsPrim, ()> = term!([IO(JsIO::Bind)] [Type(JsType:: StrType)] [Type(JsType::UnitType)]
    //     [IO(JsIO::ConsoleInput)] {JsPrim::console_output1()});
    // let str = output::to_js_program(judgment);
    // runtime::run_js_from_string(str).await;
    // Ok(())
}
