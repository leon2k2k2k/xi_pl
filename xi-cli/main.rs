use xi_backends::py_backend::py_output::module_to_py_string;
use xi_kernel::front_to_back::front_to_py_back;

#[cfg(feature = "deno-no-server")]
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    use xi_backend::output::module_to_js_string;
    use xi_frontend::compile_module_item;
    use xi_frontend::ui_to_module;
    use xi_kernel::front_to_back::front_to_back;
    use xi_runtime::runtime;

    let input = std::env::args().collect::<Vec<_>>();
    let file_contents = std::fs::read_to_string(input[1].clone())?;
    let func_name = input[2].clone();

    let module_and_imports = ui_to_module(&file_contents);
    let module = module_and_imports.module;
    let jsmodule = front_to_back(module);
    let index = *jsmodule
        .str_to_index
        .get(&func_name)
        .expect("func not found");
    let js = module_to_js_string(jsmodule, index);
    println!("{}", js);
    runtime::run_js_from_string(js).await?;
    Ok(())
}

#[cfg(feature = "deno-with-server")]
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    use xi_frontend::compile_module_item;
    use xi_frontend::ui_to_module;
    use xi_kernel::front_to_back::{front_to_js_back, front_to_py_back};
    use xi_runtimes::js_runtime::js_runtime;
    use xi_runtimes::py_runtime::py_runtime;
    use xi_server_backend::js_output::js_module_to_string;
    use xi_server_backend::py_output::module_to_py_string;
    let input = std::env::args().collect::<Vec<_>>();
    let file_contents = std::fs::read_to_string(input[1].clone())?;

    let module_and_imports = ui_to_module(&file_contents);
    let module = module_and_imports.module;
    dbg!(&module);

    let js_or_py = input[2].clone();
    if js_or_py == "js" {
        let jsmodule = front_to_js_back(module);

        let js = js_module_to_string(jsmodule.clone());
        println!("{}", js);
        js_runtime::run_js_from_string(js).await?;
    } else {
        let py_module = front_to_py_back(module);
        let py = module_to_py_string(py_module, None, 8080, 5000);
        println!("{}", py);
        py_runtime::run_py_from_string(&py);
    }

    Ok(())
}

// we are going to compile this all the way down to javascript
#[cfg(all(not(feature = "deno-with-server"), not(feature = "deno-no-server")))]
fn main() {
    use xi_backends::js_backend::js_output::module_to_js_string;
    use xi_frontend::ui_to_module;
    use xi_kernel::front_to_back::front_to_js_back;

    let input = std::env::args().collect::<Vec<_>>();
    let file_contents = std::fs::read_to_string(input[1].clone()).unwrap();

    let module_and_imports = ui_to_module(&file_contents);

    let py_module = front_to_py_back(module_and_imports);

    let func_name = input[2].clone();

    let index = *py_module
        .str_to_index
        .get(&func_name)
        .expect("func not found");

    let py = module_to_py_string(py_module, index);
    println!("{}", py);
    // use xi_runtimes::py_runtime::py_runtime::run_py_from_string;
    // run_py_from_string(&py);
}
