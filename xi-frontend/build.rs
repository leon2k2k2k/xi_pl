use std::path::PathBuf;
use std::process::Command;

fn main() {
    let dir: PathBuf = ["tree-sitter-aplite", "src"].iter().collect();

    println!("cargo:rerun-if-changed=tree-sitter-aplite/package.json");
    println!("cargo:rerun-if-changed=tree-sitter-aplite/grammar.js");

    let err = Command::new("npm")
        .arg("install")
        .current_dir("tree-sitter-aplite")
        .status()
        .expect("npm install failed")
        .success();

    if err != true {
        panic!("npm install failed")
    }

    let err = Command::new("npx")
        .args(&["tree-sitter", "generate"])
        .current_dir("tree-sitter-aplite")
        .status()
        .expect("tree-sitter generate failed")
        .success();

    if err != true {
        panic!("tree-sitter generate failed")
    }

    cc::Build::new()
        .include(&dir)
        .file(dir.join("parser.c"))
        .compile("tree-sitter-aplite");
}
