use std::{
    process::{Command, Stdio},
    str::from_utf8,
};

pub const PY_RUNTIME_FILE: &str = "_aplite_python_runtime";

pub fn run_py_from_string(source_code: &str) -> Result<(), std::io::Error> {
    let runtime_code = include_str!("runtime.py");

    let actual_source_code = format!("{}\n\n{}", runtime_code, source_code);
    eprintln!("{}", &actual_source_code);

    let _output = Command::new("python3.9")
        .args(&["-c", &actual_source_code])
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .stdin(Stdio::inherit())
        .output()
        .expect("failed to execute process");
    Ok(())
}

pub fn run_py_to_stdout(source_code: &str) -> Result<String, std::io::Error> {
    let runtime_code = include_str!("runtime.py");

    let actual_source_code = format!("{}\n\n{}", runtime_code, source_code);

    let ans = Command::new("python3.9")
        .args(&["-c", &actual_source_code])
        .stderr(Stdio::inherit())
        .stdin(Stdio::inherit())
        .output()
        .expect("Failed to execute process")
        .stdout;
    let str = from_utf8(&ans).expect("idk");
    Ok(str.into())
}

mod test {
    #[test]
    fn test1() {
        use super::run_py_from_string;
        let source_code = "print('hello world')";
        run_py_from_string(source_code).unwrap();
    }
}
