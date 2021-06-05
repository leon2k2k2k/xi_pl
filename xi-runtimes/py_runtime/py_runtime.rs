use std::process::{Command, Stdio};

pub const PY_RUNTIME_FILE: &str = "_aplite_python_runtime";

pub fn run_py_from_string(source_code: &str) -> Result<(), std::io::Error> {
    let runtime_code = include_str!("runtime.py");

    let actual_source_code = format!("{}\n\n{}", runtime_code, source_code);
    // eprintln!("{}", &actual_source_code);

    let _output = Command::new("python3")
        .args(&["-c", &actual_source_code])
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .stdin(Stdio::inherit())
        .output()
        .expect("failed to execute process");
    Ok(())
}

mod test {
    #[test]
    fn test1() {
        use super::run_py_from_string;
        let source_code = "print('hello world')";
        run_py_from_string(source_code);
    }
}
