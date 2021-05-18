// #[cfg(feature = "deno")]
// #[tokio::main]
// async fn main() -> Result<(), Box<dyn std::error::Error>> {
//     use xi_backend::output::module_to_js_string;
//     use xi_backend::runtime;
//     use xi_frontend::compile_module_item;
//     use xi_frontend::ui_to_module;
//     use xi_kernel::front_to_back::front_to_back;

//     let input = std::env::args().collect::<Vec<_>>();
//     let file_contents = std::fs::read_to_string(input[1].clone())?;
//     let func_name = input[2].clone();

//     let module_and_imports = ui_to_module(&file_contents);
//     let module = module_and_imports.module;
//     let jsmodule = front_to_back(module);
//     let index = *jsmodule
//         .str_to_index
//         .get(&func_name)
//         .expect("func not found");
//     let js = module_to_js_string(jsmodule, index);
//     println!("{}", js);
//     runtime::run_js_from_string(js).await?;
//     Ok(())
// }

#[cfg(feature = "deno")]
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    use xi_backend::runtime;
    use xi_frontend::compile_module_item;
    use xi_frontend::ui_to_module;
    use xi_kernel::front_to_back::front_to_back;
    use xi_server_backend::output::{js_module_to_py_string, js_module_to_string};

    let input = std::env::args().collect::<Vec<_>>();
    let file_contents = std::fs::read_to_string(input[1].clone())?;

    let module_and_imports = ui_to_module(&file_contents);
    let module = module_and_imports.module;
    let jsmodule = front_to_back(module);

    let js_or_py = input[2].clone();
    if js_or_py == "js" {
        let js = js_module_to_string(jsmodule.clone());
        println!("{}", js);
        runtime::run_js_from_string(js).await?;
    } else {
        let py = js_module_to_py_string(jsmodule);
        println!("{}", py);
        runtime::run_js_from_string(py).await?;
    }

    Ok(())
}

// we are going to compile this all the way down to javascript
#[cfg(not(feature = "deno"))]
fn main() {
    use xi_frontend::ui_to_module;

    let input = std::env::args().collect::<Vec<_>>();
    let file_contents = std::fs::read_to_string(input[1].clone()).unwrap();

    let module = ui_to_module(&file_contents);
    dbg!(&module);
}
