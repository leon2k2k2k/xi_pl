use xi_backends::{
    js_backend::js_output::module_to_js_string, py_backend::py_output::module_to_py_string,
};
use xi_frontend::ui_to_module;
use xi_kernel::front_to_back::{front_to_js_back, front_to_py_back};
use xi_server_backend::js_output::module_with_server_to_js_string;
use xi_server_backend::py_output::module_with_server_to_py_string;

// it goes like
// cargo run --features [Options] {aplite file} {backend compiler} {Option(main_func_name)}

#[cfg(feature = "run-no-server")]
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = std::env::args().collect::<Vec<_>>();
    let file_contents = std::fs::read_to_string(input[1].clone()).unwrap();
    let back_end = BackEnd::parse_back_end(input[2].clone());
    let main_func = if input.len() == 4 {
        Some(input[3].clone())
    } else {
        None
    };
    let str = aplite_to_backend_source_code(file_contents, main_func, back_end.clone(), false);

    match back_end {
        BackEnd::Js => xi_runtimes::js_runtime::js_runtime::run_js_from_string(str).await?,
        BackEnd::Py => {
            println!("helloooo");
            xi_runtimes::py_runtime::py_runtime::run_py_from_string(&str);
        }
    }

    Ok(())
}

// this one runs the code with server
#[cfg(all(feature = "run-with-server", not(feature = "run-no-server")))]
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = std::env::args().collect::<Vec<_>>();
    let file_contents = std::fs::read_to_string(input[1].clone()).unwrap();
    let back_end = BackEnd::parse_back_end(input[2].clone());
    let main_func = if input.len() == 4 {
        Some(input[3].clone())
    } else {
        None
    };
    let str = aplite_to_backend_source_code(file_contents, main_func, back_end.clone(), true);

    match back_end {
        BackEnd::Js => xi_runtimes::js_runtime::js_runtime::run_js_from_string(str).await?,
        BackEnd::Py => {
            println!("helloooo");
            xi_runtimes::py_runtime::py_runtime::run_py_from_string(&str);
        }
    }

    Ok(())
}

// this one prints the output js/py source code.

#[cfg(all(not(feature = "run-with-server"), not(feature = "run-no-server")))]
fn main() {
    let input = std::env::args().collect::<Vec<_>>();
    let file_contents = std::fs::read_to_string(input[1].clone()).unwrap();
    let back_end = BackEnd::parse_back_end(input[2].clone());
    let main_func = if input.len() == 4 {
        Some(input[3].clone())
    } else {
        None
    };
    let str = aplite_to_backend_source_code(file_contents, main_func, back_end, false);
    println!("{}", str);
}
#[derive(Clone, Debug)]
pub enum BackEnd {
    Js,
    Py,
    //go,
    //haskell,
}

impl BackEnd {
    pub fn parse_back_end(str: String) -> BackEnd {
        if str == "js" {
            BackEnd::Js
        } else if str == "py" {
            BackEnd::Py
        } else {
            panic!("No such backend (for now ;))")
        }
    }
}

pub fn aplite_to_backend_source_code(
    source_code: String,
    main_func: Option<String>,
    backend: BackEnd,
    with_server: bool,
) -> String {
    let module_and_imports = ui_to_module(&source_code);
    let main_id = match main_func {
        Some(func_name) => Some(
            *module_and_imports
                .module
                .str_to_index
                .get(&func_name)
                .expect("func not found"),
        ),
        None => None,
    };
    match backend {
        BackEnd::Js => {
            let js_module = front_to_js_back(module_and_imports);
            match with_server {
                true => {
                    let js_string = module_with_server_to_js_string(js_module, main_id, 5000, 8080);
                    println!("{}", &js_string);
                    js_string
                }
                false => {
                    let js_string = module_to_js_string(js_module, main_id);
                    println!("{}", &js_string);
                    js_string
                }
            }
        }
        BackEnd::Py => {
            let py_module = front_to_py_back(module_and_imports);
            match with_server {
                true => {
                    let py_string = module_with_server_to_py_string(py_module, main_id, 8080, 5000);
                    println!("{}", &py_string);
                    py_string
                }
                false => {
                    let py_string = module_to_py_string(py_module, main_id);
                    println!("{}", &py_string);
                    py_string
                }
            }
        }
    }
}
