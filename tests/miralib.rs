use randa::vm::VM;
use std::panic::{self, AssertUnwindSafe};
use std::path::{Path, PathBuf};

fn miralib_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("miralib")
}

fn panic_message(payload: Box<dyn std::any::Any + Send>) -> String {
    match payload.downcast::<String>() {
        Ok(message) => *message,
        Err(payload) => match payload.downcast::<&'static str>() {
            Ok(message) => (*message).to_string(),
            Err(_) => String::from("non-string panic payload"),
        },
    }
}

fn assert_loads_without_error(source_name: &str) {
    let path = miralib_dir().join(source_name);
    let mut vm = VM::new();
    let prior_hook = panic::take_hook();
    panic::set_hook(Box::new(|_| {}));
    let result =
        panic::catch_unwind(AssertUnwindSafe(|| vm.load_source_file(&path.to_string_lossy())));
    panic::set_hook(prior_hook);

    match result {
        Ok(Ok(())) => {}
        Ok(Err(err)) => panic!("expected {source_name} to load successfully: {err}"),
        Err(payload) => panic!(
            "expected {source_name} to load successfully, but it panicked: {}",
            panic_message(payload)
        ),
    }
}

#[test]
fn miralib_core_sources_load_successfully() {
    for source_name in ["stdenv.m", "prelude.m"] {
        assert_loads_without_error(source_name);
    }
}
