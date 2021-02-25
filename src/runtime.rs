use deno::create_main_worker;
use deno::file_fetcher::File;
use deno::media_type::MediaType;
use deno::program_state::ProgramState;
use deno_core::error::AnyError;
use deno_core::resolve_url_or_path;
use deno_runtime::permissions::Permissions;

pub async fn run_js_from_string(source_code: String) -> Result<(), AnyError> {
    let flags = deno::flags::flags_from_vec(vec!["eval".into()])?;

    let main_module = resolve_url_or_path("./$deno$eval.ts").unwrap();
    let permissions = Permissions::from_options(&flags.clone().into());
    let program_state = ProgramState::new(flags)?;
    let mut worker = create_main_worker(&program_state, main_module.clone(), permissions);

    let main_module_file = File {
        local: main_module.clone().to_file_path().unwrap(),
        maybe_types: None,
        media_type: MediaType::TypeScript,
        source: source_code,
        specifier: main_module.clone(),
    };

    program_state.file_fetcher.insert_cached(main_module_file);

    let runtime_module = resolve_url_or_path("./$deno$runtime.ts").unwrap();
    let runtime_code = include_str!("runtime.ts");

    let runtime_module_file = File {
        local: runtime_module.clone().to_file_path().unwrap(),
        maybe_types: None,
        media_type: MediaType::TypeScript,
        source: runtime_code.into(),
        specifier: runtime_module.clone(),
    };

    program_state
        .file_fetcher
        .insert_cached(runtime_module_file);

    worker.execute_module(&main_module).await?;
    worker.execute("window.dispatchEvent(new Event('load'))")?;
    worker.run_event_loop().await?;
    worker.execute("window.dispatchEvent(new Event('unload'))")?;
    Ok(())
}
mod test {

    #[tokio::test]
    async fn run_js_from_string_test() {
        use super::*;

        let str = "console.log('hello_world');".into();
        run_js_from_string(str).await.unwrap();
    }

    // #[tokio::test]
    // async fn run_js_from_judgment_test() {
    //     let id: Judgment<JsPrim> = term!([IO(JsIO::ConsoleOutput)][Str("hello world".into())]);
    //     run_js_from_judgment(id).await;
    // }
}
