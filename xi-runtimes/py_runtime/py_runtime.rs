use deno_core::error::AnyError;

const PY_RUNTIME_FILE: &str = "_aplite_python_runtime";

pub async fn run_py_from_string(source_code: String) -> Result<(), AnyError> {
    let runtime_text = include_str!("runtime.py");

    let flags = deno::flags::flags_from_vec(vec!["eval".into(), "--unstable".into()])?;

    let main_module = resolve_url_or_path("./$deno$eval.js").unwrap();
    let permissions = Permissions::from_options(&flags.clone().into());
    let program_state = ProgramState::build(flags.clone()).await?;
    let mut worker = create_main_worker(&program_state, main_module.clone(), permissions);

    let main_module_file = File {
        local: main_module.clone().to_file_path().unwrap(),
        maybe_types: None,
        media_type: MediaType::JavaScript,
        source: source_code,
        specifier: main_module.clone(),
    };

    program_state.file_fetcher.insert_cached(main_module_file);

    let runtime_module = resolve_url_or_path(RUNTIME_FILE).unwrap();
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
