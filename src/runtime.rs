use deno_core::error::AnyError;
use deno_runtime::worker::MainWorker;
use deno_runtime::{permissions, worker};
use std::rc::Rc;
use std::sync::Arc;
use url::Url;
use worker::WorkerOptions;

use crate::{
    output::{self, JsOutput},
    xi_syntax,
};

pub async fn run_js_from_string(str: String) -> Result<(), AnyError> {
    let mut worker = std_worker();

    let result = worker.execute(&str)?;
    worker.run_event_loop().await?;
    Ok(())
}

fn std_worker() -> MainWorker {
    let main_module = Url::parse("https://example.net").unwrap();
    let permissions = permissions::Permissions::allow_all();
    let module_free_loader = Rc::new(deno_core::NoopModuleLoader);
    let create_web = Arc::new(|x| panic!("asdfasdfsad"));

    let options = worker::WorkerOptions {
        apply_source_maps: false,
        args: vec![],
        debug_flag: false,
        unstable: false,
        ca_data: None,
        user_agent: "".into(),
        seed: None,
        module_loader: module_free_loader,
        create_web_worker_cb: create_web,
        js_error_create_fn: None,
        attach_inspector: false,
        maybe_inspector_server: None,
        should_break_on_first_statement: false,
        runtime_version: "frank sucks".into(),
        ts_version: "leon sucks".into(),
        no_color: false,
        get_error_class_fn: None,
        location: None,
    };

    let mut worker = MainWorker::from_options(main_module, permissions, &options);
    worker.bootstrap(&options);
    worker
}

fn run_js_from_judgment<T: JsOutput>(judgment: xi_syntax::Judgment<T>) -> Result<(), AnyError> {
    let str = output::to_js_program(judgment);
    run_js_from_string(str);
    Ok(())
}

mod test {
    use super::*;
    use output::JsTest::*;
    use term_macro::term;
    #[tokio::test]
    async fn run_js_from_string_test() {
        let str = "console.log('hello_world');".into();
        run_js_from_string(str).await;
    }

    // #[test]
    // fn run_js_from_judgment_test(){
    //     let id = term!(Lam |T : U, t : T| t);

    // }
}
