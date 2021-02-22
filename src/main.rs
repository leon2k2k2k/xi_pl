#![allow(dead_code)]
mod output;
mod runtime;
mod xi_semantics;
mod xi_syntax;
#[tokio::main]
async fn main() {
    use runtime;
    let str = "console.log('🤔🤔🤔🤔🤔🤔🤔🤔🤔🤔🤔🤔🤔🤔🤔');".into();
    runtime::run_js_from_string(str).await;
}
