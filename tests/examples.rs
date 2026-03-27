use randa::vm::VM;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::panic::{self, AssertUnwindSafe};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, PartialEq, Eq)]
enum ExampleExpectation {
    Pass,
    Panic { detail: String },
    Skip { reason: String },
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ExampleOutcome {
    Pass,
    LoadError { message: String },
    Panic { message: String },
    Skip,
}

fn examples_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("examples")
}

fn expectation_manifest_path() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("example_expectations.txt")
}

fn parse_expectations() -> BTreeMap<String, ExampleExpectation> {
    let manifest = fs::read_to_string(expectation_manifest_path())
        .expect("failed to read example expectation manifest");
    let mut expectations = BTreeMap::new();

    for (line_number, line) in manifest.lines().enumerate() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }

        let mut fields = trimmed.splitn(3, '\t');
        let file_name = fields
            .next()
            .expect("manifest line must contain file name")
            .to_string();
        let status = fields
            .next()
            .unwrap_or_else(|| panic!("missing status on manifest line {}", line_number + 1));
        let detail = fields.next().unwrap_or("").to_string();

        let expectation = match status {
            "pass" => ExampleExpectation::Pass,
            "panic" => ExampleExpectation::Panic { detail },
            "skip" => ExampleExpectation::Skip { reason: detail },
            _ => panic!(
                "unknown manifest status `{status}` on line {}",
                line_number + 1
            ),
        };

        let replaced = expectations.insert(file_name.clone(), expectation);
        assert!(
            replaced.is_none(),
            "duplicate expectation entry for {file_name}"
        );
    }

    expectations
}

fn example_file_names() -> BTreeSet<String> {
    let mut names = BTreeSet::new();
    for entry in fs::read_dir(examples_dir()).expect("failed to read examples directory") {
        let entry = entry.expect("failed to read examples directory entry");
        let file_type = entry.file_type().expect("failed to read example file type");
        if file_type.is_file() {
            let file_name = entry.file_name().to_string_lossy().to_string();
            if !file_name.ends_with(".x") {
                names.insert(file_name);
            }
        }
    }
    names
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

fn run_example(example_name: &str, expectation: &ExampleExpectation) -> ExampleOutcome {
    if matches!(expectation, ExampleExpectation::Skip { .. }) {
        return ExampleOutcome::Skip;
    }

    let path = examples_dir().join(example_name);
    let mut vm = VM::new();
    let prior_hook = panic::take_hook();
    panic::set_hook(Box::new(|_| {}));
    let result = panic::catch_unwind(AssertUnwindSafe(|| vm.load_source_file(&path.to_string_lossy())));
    panic::set_hook(prior_hook);
    match result {
        Ok(Ok(())) => ExampleOutcome::Pass,
        Ok(Err(err)) => ExampleOutcome::LoadError {
            message: err.to_string(),
        },
        Err(payload) => ExampleOutcome::Panic {
            message: panic_message(payload),
        },
    }
}

#[test]
fn examples_manifest_is_complete() {
    let expectations = parse_expectations();
    let example_file_names = example_file_names();
    let manifest_names = expectations.keys().cloned().collect::<BTreeSet<_>>();

    let missing = example_file_names
        .difference(&manifest_names)
        .cloned()
        .collect::<Vec<_>>();
    let extra = manifest_names
        .difference(&example_file_names)
        .cloned()
        .collect::<Vec<_>>();

    assert!(missing.is_empty(), "missing example expectation entries: {missing:?}");
    assert!(extra.is_empty(), "stale example expectation entries: {extra:?}");
}

#[test]
fn examples_match_expected_outcomes() {
    let expectations = parse_expectations();

    for (example_name, expectation) in expectations {
        let outcome = run_example(&example_name, &expectation);
        match expectation {
            ExampleExpectation::Pass => assert_eq!(
                outcome,
                ExampleOutcome::Pass,
                "expected {example_name} to load successfully"
            ),
            ExampleExpectation::Panic { detail } => match outcome {
                ExampleOutcome::Panic { message } => assert!(
                    message.contains(&detail),
                    "expected panic for {example_name} to contain `{detail}`, got `{message}`"
                ),
                other => panic!("expected panic for {example_name}, got {other:?}"),
            },
            ExampleExpectation::Skip { .. } => assert_eq!(outcome, ExampleOutcome::Skip),
        }
    }
}
