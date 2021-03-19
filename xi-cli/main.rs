#![allow(dead_code)]
#[tokio::main]
// Take an Aplite UI &str and returns a string of JavaScript.
async fn ui_to_js(text: &str) -> String {
    use xi_backend::output::to_js_program;
    use xi_frontend::frontend;
    let frontend_judgment = frontend(text);
    let backend_judgment = frontend_to_backend(frontend_judgment);
    to_js_program(backend_judgment)
}

#[tokio::main]
async fn main() {
    use xi_backend::runtime;

    let str = "fn foo |x| {val x} 
    val foo (Pi|y: Type| y)"
        .into();

    runtime::run_js_from_string(str).await;
    println!("hi");
}
