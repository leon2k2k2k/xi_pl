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
    let cli_info = CliInfo::get_inputs(false);
    let str = CliInfo::aplite_to_backend_source_code(&cli_info);
    println!("{}", str);
    match &cli_info.back_end {
        BackEnd::Js => xi_runtimes::js_runtime::js_runtime::run_js_from_string(str).await?,
        BackEnd::Py => {
            xi_runtimes::py_runtime::py_runtime::run_py_from_string(&str);
        }
    }

    Ok(())
}

// this one runs the code with server
#[cfg(all(feature = "run-with-server", not(feature = "run-no-server")))]
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli_info = CliInfo::get_inputs(true);
    let str = CliInfo::aplite_to_backend_source_code(&cli_info);
    // println!("{}", str);
    match &cli_info.back_end {
        BackEnd::Js => xi_runtimes::js_runtime::js_runtime::run_js_from_string(str).await?,
        BackEnd::Py => {
            xi_runtimes::py_runtime::py_runtime::run_py_from_string(&str);
        }
    }

    Ok(())
}

// this one prints the output js/py source code.

#[cfg(all(not(feature = "run-with-server"), not(feature = "run-no-server")))]
fn main() {
    let cli_info = CliInfo::get_inputs(false);
    let _str = CliInfo::aplite_to_backend_source_code(&cli_info);
    // println!("{}", str);
}
#[derive(Clone, Debug)]
pub enum BackEnd {
    Js,
    Py,
    //Go,
    //Haskell,
}

impl BackEnd {
    pub fn parse_back_end(str: &str) -> BackEnd {
        if str == "js" {
            BackEnd::Js
        } else if str == "py" {
            BackEnd::Py
        } else {
            panic!("No such backend (for now ;))")
        }
    }
}
#[derive(Clone, Debug)]
pub struct CliInfo {
    pub source_code: String,
    pub main_func: Option<String>,
    pub back_end: BackEnd,
    pub with_server: bool,
}
impl CliInfo {
    pub fn get_inputs(with_server: bool) -> CliInfo {
        let input = std::env::args().collect::<Vec<_>>();
        let file_contents = std::fs::read_to_string(&input[1]).unwrap();
        let back_end = BackEnd::parse_back_end(&input[2]);
        let main_func = if input.len() == 4 {
            let main_func = &input[3];
            Some(main_func.clone())
        } else {
            None
        };
        CliInfo {
            source_code: file_contents,
            main_func: main_func,
            back_end: back_end,
            with_server: with_server,
        }
    }

    pub fn aplite_to_backend_source_code(cli_info: &CliInfo) -> String {
        let module_and_imports = ui_to_module(&cli_info.source_code);
        let main_id = match &cli_info.main_func {
            Some(func_name) => Some(
                *module_and_imports
                    .module
                    .str_to_index
                    .get(func_name)
                    .expect("func not found"),
            ),
            None => None,
        };
        match &cli_info.back_end {
            BackEnd::Js => {
                let js_module = front_to_js_back(module_and_imports);
                match &cli_info.with_server {
                    true => {
                        let js_string =
                            module_with_server_to_js_string(js_module, main_id, 5000, 8080);
                        // println!("{}", &js_string);
                        js_string
                    }
                    false => {
                        let js_string = module_to_js_string(js_module, main_id);
                        // println!("{}", &js_string);
                        js_string
                    }
                }
            }
            BackEnd::Py => {
                let py_module = front_to_py_back(module_and_imports);
                match &cli_info.with_server {
                    true => {
                        let py_string =
                            module_with_server_to_py_string(py_module, main_id, 8080, 5000);
                        // println!("{}", &py_string);
                        py_string
                    }
                    false => {
                        let py_string = module_to_py_string(py_module, main_id);
                        // println!("{}", &py_string);
                        py_string
                    }
                }
            }
        }
    }
}

// integration tests!
mod test {
    use std::{process::Command, str::from_utf8};

    use crate::CliInfo;

    // #[tokio::test]
    // async fn py_test() {
    //     let source_code = include_str!("./tests/std_tests/arithmetic_test.ap");
    //     let main_func = "main";
    //     println!("{}", source_code);
    //     let js_cli_info = CliInfo {
    //         source_code: source_code.into(),
    //         main_func: Some(main_func.into()),
    //         back_end: crate::BackEnd::Py,
    //         with_server: false,
    //     };

    //     let py_source_code = CliInfo::aplite_to_backend_source_code(&js_cli_info);
    //     println!("{}", &py_source_code);

    //     // xi_runtimes::py_runtime::py_runtime::run_py_from_string(source_code);
    //     // panic!("laksjdflja");
    //     let output = xi_runtimes::py_runtime::py_runtime::run_py_to_stdout(&py_source_code)
    //         .expect("we got this");
    //     eprintln!("the output is {}", &output);
    //     assert_eq!("11\n", output);
    // }

    // #[tokio::test]
    // async fn js_test() {
    //     // this case we are going to run Aplite with Js backend as a subprocess, and
    //     // take its stdout and assert_eq it.
    //     let run_aplite = Command::new("cargo")
    //         .args(&[
    //             "run",
    //             "--features",
    //             "run-no-server",
    //             "./tests/std_tests/arithmetic_test.ap",
    //             "js",
    //             "main",
    //         ])
    //         .output()
    //         .expect("failed to execute Aplite");
    //     let stdout_str: String = from_utf8(&run_aplite.stdout).expect("str").into();
    //     assert_eq!("11\n", stdout_str);
    // }
    #[tokio::test]
    async fn py_test() {
        let run_aplite = Command::new("cargo")
            .args(&[
                "run",
                "--features",
                "run-no-server",
                "./tests/std_tests/arithmetic_test.ap",
                "py",
                "main",
            ])
            .output()
            .expect("failed to execute Aplite");
        let stdout_str: String = from_utf8(&run_aplite.stdout).expect("str").into();
        assert_eq!("11\n", stdout_str);
    }
}
