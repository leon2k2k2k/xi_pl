#[cfg(feature = "deno")]
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    use xi_backend::output::to_js_program;
    use xi_backend::runtime;
    use xi_frontend::compile_module_item;
    use xi_frontend::ui_to_module;
    use xi_kernel::front_to_back::front_to_back;

    let input = std::env::args().collect::<Vec<_>>();
    let file_contents = std::fs::read_to_string(input[1].clone())?;
    let func_name = input[2].clone();

    let module = ui_to_module(&file_contents);
    let compiled_judgment = compile_module_item(module, &func_name);

    let backend_judgment = front_to_back(compiled_judgment);
    let js = to_js_program(backend_judgment);

    println!("{}", js);
    runtime::run_js_from_string(js).await?;
    Ok(())
}

#[cfg(not(feature = "deno"))]
fn main() {
    use xi_frontend::ui_to_module;

    let input = std::env::args().collect::<Vec<_>>();
    let file_contents = std::fs::read_to_string(input[1].clone()).unwrap();

    let module = ui_to_module(&file_contents);
    dbg!(module);
}
