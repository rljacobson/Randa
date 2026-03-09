use super::*;
use std::path::PathBuf;

fn unique_test_path(file_name: &str) -> PathBuf {
    let mut base = std::env::temp_dir();
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    base.push(format!("randa-vm-tests-{}-{}", std::process::id(), nanos));
    std::fs::create_dir_all(&base).expect("failed to create temp test directory");
    base.push(file_name);
    base
}

#[test]
fn load_file_missing_script_is_allowed_outside_initialization() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;
    let source_path = unique_test_path("missing_script.m");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok());
    assert!(!vm.loading);
    assert!(!vm.files.is_empty());
    assert!(!vm.old_files.is_empty());
}

#[test]
fn load_file_missing_script_errors_during_initialization() {
    let mut vm = VM::new_for_tests();
    vm.initializing = true;
    let source_path = unique_test_path("missing_prelude.m");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(BytecodeError::MissingSourceFile { path }) if path == source_path_str
    ));
    assert!(!vm.loading);
}

#[test]
fn undump_propagates_load_file_error_after_bad_dump() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("script.m");
    std::fs::write(&source_path, "-- test source\n").expect("failed to write source test file");

    let binary_path = source_path.with_extension("x");
    std::fs::write(&binary_path, [0_u8, 0_u8, 0_u8, 0_u8])
        .expect("failed to write binary test file");

    let source_path_str = source_path.to_string_lossy().to_string();
    let result = vm.undump(&source_path_str);

    assert!(matches!(
        result,
        Err(BytecodeError::ParserIntegrationDeferred { path }) if path == source_path_str
    ));
    assert!(!vm.loading);
}

#[test]
fn undump_falls_back_to_load_file_after_wrong_bytecode_version() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("script_version_mismatch.m");
    std::fs::write(&source_path, "-- test source\n").expect("failed to write source test file");

    let binary_path = source_path.with_extension("x");
    let mut bad_dump = vec![0_u8; 16];
    bad_dump[0] = WORD_SIZE as u8;
    bad_dump[1] = (XVERSION as u8).wrapping_add(1);
    std::fs::write(&binary_path, bad_dump).expect("failed to write binary test file");

    let source_path_str = source_path.to_string_lossy().to_string();
    let result = vm.undump(&source_path_str);

    assert!(matches!(
        result,
        Err(BytecodeError::ParserIntegrationDeferred { path }) if path == source_path_str
    ));
    assert!(!vm.loading);
}

#[test]
fn load_file_parser_deferred_keeps_attempted_source_in_old_files() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    vm.files = vm.empty_environment_for_source("prior_source.m", UNIX_EPOCH);

    let source_path = unique_test_path("parser_deferred.m");
    std::fs::write(&source_path, "-- no parse marker\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(BytecodeError::ParserIntegrationDeferred { path }) if path == source_path_str
    ));
    assert_eq!(vm.last_load_phase_trace, vec!["begin", "parse"]);
    assert!(!vm.loading);
    assert!(vm.files.is_empty());
    assert!(!vm.old_files.is_empty());

    let old_anchor = vm
        .old_files
        .head(&vm.heap)
        .expect("missing old_files anchor");
    assert_eq!(old_anchor.get_file_name(&vm.heap), source_path_str);
}

#[test]
fn load_file_missing_script_errors_during_make_mode() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;
    vm.making = true;
    let source_path = unique_test_path("missing_make_target.m");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(BytecodeError::MissingSourceFile { path }) if path == source_path_str
    ));
    assert_eq!(
        vm.last_load_phase_trace,
        vec!["begin", "missing-source-error"]
    );
    assert!(!vm.loading);
}

#[test]
fn load_file_normalizes_non_m_source_paths() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path_no_ext = unique_test_path("normalize_target");
    let source_path_with_m = source_path_no_ext.with_extension("m");
    std::fs::write(&source_path_with_m, "-- test source\n")
        .expect("failed to write source test file");

    let requested_path = source_path_no_ext.to_string_lossy().to_string();
    let normalized_path = source_path_with_m.to_string_lossy().to_string();
    let result = vm.load_file(&requested_path);

    assert!(matches!(
        result,
        Err(BytecodeError::ParserIntegrationDeferred { path }) if path == normalized_path
    ));
    assert!(!vm.loading);
}

#[test]
fn load_file_runs_placeholder_phase_pipeline_when_parse_is_marked_ok() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("phase_pipeline.m");
    std::fs::write(&source_path, "RANDA_PARSE_OK\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok());
    assert_eq!(
        vm.last_load_phase_trace,
        vec![
            "begin",
            "parse",
            "exportfile-checks",
            "include-expansion",
            "typecheck",
            "export-closure",
            "bereaved-warnings",
            "unused-diagnostics",
            "codegen",
            "dump-visibility",
            "dump-fixexports",
            "dump-write",
            "dump-unfixexports",
            "success-postlude",
        ]
    );
    assert!(!vm.loading);
    assert!(vm.sorted);
}

#[test]
fn load_file_uses_syntax_fallback_when_parse_marks_syntax_error() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("syntax_fallback.m");
    std::fs::write(&source_path, "RANDA_PARSE_SYNTAX_ERROR\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok());
    assert_eq!(
        vm.last_load_phase_trace,
        vec![
            "begin",
            "parse",
            "syntax-fallback",
            "syntax-unload",
            "syntax-dump-decision",
            "dump-write",
            "syntax-state-reset",
        ]
    );
    assert!(vm.files.is_empty());
    assert!(!vm.old_files.is_empty());
    assert!(!vm.loading);
}

#[test]
fn load_file_reports_syntax_error_during_initialization() {
    let mut vm = VM::new_for_tests();
    vm.initializing = true;

    let source_path = unique_test_path("init_syntax.m");
    std::fs::write(&source_path, "RANDA_PARSE_SYNTAX_ERROR\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(BytecodeError::SyntaxErrorInSource { path }) if path == source_path_str
    ));
    assert_eq!(
        vm.last_load_phase_trace,
        vec!["begin", "parse", "syntax-fallback"]
    );
    assert!(!vm.loading);
}

#[test]
fn success_postlude_phase_resets_syntax_editor_state() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;
    vm.sorted = false;
    vm.error_line = 42;
    vm.errs = vec![Combinator::True.into()];

    let source_path = unique_test_path("success_postlude_reset.m");
    std::fs::write(&source_path, "RANDA_PARSE_OK\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok());
    assert!(vm.sorted);
    assert_eq!(vm.error_line, 0);
    assert!(vm.errs.is_empty());
    assert!(!vm.old_files.is_empty());
}

#[test]
fn syntax_fallback_resets_syntax_editor_state_outside_initialization() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;
    vm.error_line = 17;
    vm.errs = vec![Combinator::False.into()];
    vm.includees = vm.empty_environment_for_source("stale_includee.m", UNIX_EPOCH);

    let source_path = unique_test_path("syntax_reset.m");
    std::fs::write(&source_path, "RANDA_PARSE_SYNTAX_ERROR\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok());
    assert_eq!(vm.error_line, 0);
    assert!(vm.errs.is_empty());
    assert!(vm.files.is_empty());
    assert!(vm.includees.is_empty());
    assert!(!vm.old_files.is_empty());
}

#[test]
fn exportfile_phase_errors_when_path_is_not_in_includees() {
    let mut vm = VM::new_for_tests();

    let path = unique_test_path("exports_only.m")
        .to_string_lossy()
        .to_string();
    let path_ref = vm.heap.string(path.clone());
    vm.exportfiles = vm.heap.cons_ref(Value::Reference(path_ref), NIL).into();
    vm.includees = ConsList::EMPTY;

    let result = vm.validate_exportfile_bindings_partial();

    assert!(matches!(
        result,
        Err(BytecodeError::ExportFileNotIncludedInScript { path: p }) if p == path
    ));
}

#[test]
fn exportfile_phase_errors_when_path_binding_is_ambiguous() {
    let mut vm = VM::new_for_tests();

    let path = unique_test_path("ambiguous.m")
        .to_string_lossy()
        .to_string();
    let path_ref = vm.heap.string(path.clone());
    vm.exportfiles = vm.heap.cons_ref(Value::Reference(path_ref), NIL).into();

    let first = FileRecord::new(
        &mut vm.heap,
        path.clone(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    let second = FileRecord::new(
        &mut vm.heap,
        path.clone(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.includees = ConsList::new(&mut vm.heap, first);
    vm.includees.push(&mut vm.heap, second);

    let result = vm.validate_exportfile_bindings_partial();

    assert!(matches!(
        result,
        Err(BytecodeError::ExportFileAmbiguous { path: p }) if p == path
    ));
}

#[test]
fn exportfile_phase_accepts_unique_included_binding() {
    let mut vm = VM::new_for_tests();

    let path = unique_test_path("included_once.m")
        .to_string_lossy()
        .to_string();
    let path_ref = vm.heap.string(path.clone());
    vm.exportfiles = vm.heap.cons_ref(Value::Reference(path_ref), NIL).into();

    let includee = FileRecord::new(&mut vm.heap, path, UNIX_EPOCH, false, ConsList::EMPTY);
    vm.includees = ConsList::new(&mut vm.heap, includee);

    let result = vm.validate_exportfile_bindings_partial();

    assert!(result.is_ok());
}

#[test]
fn include_expansion_phase_appends_includees_and_clears_bookkeeping() {
    let mut vm = VM::new_for_tests();

    let main_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("main.m").to_string_lossy().to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, main_file);

    let include_a = FileRecord::new(
        &mut vm.heap,
        unique_test_path("include_a.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    let include_b = FileRecord::new(
        &mut vm.heap,
        unique_test_path("include_b.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.includees = ConsList::new(&mut vm.heap, include_a);
    vm.includees.append(&mut vm.heap, include_b);
    vm.mkinclude_files = ConsList::new(&mut vm.heap, vm.includees);

    let result = vm.run_mkincludes_phase();

    assert!(result.is_ok());
    assert!(vm.includees.is_empty());
    assert!(vm.mkinclude_files.is_empty());
    assert_eq!(vm.files.len(&vm.heap), 3);
}

#[test]
fn include_expansion_phase_is_noop_when_includees_empty() {
    let mut vm = VM::new_for_tests();
    vm.files = ConsList::EMPTY;
    vm.includees = ConsList::EMPTY;
    let include_stub = FileRecord::new(
        &mut vm.heap,
        unique_test_path("ld_stuff.m").to_string_lossy().to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    let include_list = ConsList::new(&mut vm.heap, include_stub);
    vm.mkinclude_files = ConsList::new(&mut vm.heap, include_list);

    let result = vm.run_mkincludes_phase();

    assert!(result.is_ok());
    assert!(vm.files.is_empty());
    assert!(vm.includees.is_empty());
    assert!(vm.mkinclude_files.is_empty());
}

#[test]
fn typecheck_phase_succeeds_when_no_undefined_names() {
    let mut vm = VM::new_for_tests();
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_checktypes_phase();

    assert!(result.is_ok());
}

#[test]
fn typecheck_phase_fails_when_undefined_names_present() {
    let mut vm = VM::new_for_tests();
    let missing_id = vm.heap.make_empty_identifier("missing_name");
    vm.undefined_names = ConsList::new(&mut vm.heap, missing_id);

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(BytecodeError::TypecheckUndefinedNames { count: 1 })
    ));
}

#[test]
fn export_closure_phase_is_noop_without_exports() {
    let mut vm = VM::new_for_tests();
    vm.exports = NIL;

    let result = vm.run_export_closure_phase_partial();

    assert!(result.is_ok());
}

#[test]
fn export_closure_phase_clears_exports_when_undefined_names_present() {
    let mut vm = VM::new_for_tests();
    let export_id = vm.heap.make_empty_identifier("exported_name");
    vm.exports = vm.heap.cons_ref(export_id.into(), NIL).into();
    let missing_id = vm.heap.make_empty_identifier("missing_name");
    vm.undefined_names = ConsList::new(&mut vm.heap, missing_id);

    let result = vm.run_export_closure_phase_partial();

    assert!(matches!(
        result,
        Err(BytecodeError::ExportClosureBlockedByUndefinedNames)
    ));
    assert_eq!(vm.exports, NIL);
}

#[test]
fn export_closure_phase_keeps_exports_when_no_undefined_names() {
    let mut vm = VM::new_for_tests();
    let export_id = vm.heap.make_empty_identifier("exported_name");
    vm.exports = vm.heap.cons_ref(export_id.into(), NIL).into();
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_export_closure_phase_partial();

    assert!(result.is_ok());
    assert_ne!(vm.exports, NIL);
}

#[test]
fn bereaved_warning_phase_is_noop_without_risk_flag() {
    let mut vm = VM::new_for_tests();
    vm.exports = NIL;
    vm.unused_types = false;

    vm.emit_bereaved_warnings_partial();
    assert!(!vm.unused_types);
}

#[test]
fn bereaved_warning_phase_returns_ok_when_risk_flagged() {
    let mut vm = VM::new_for_tests();
    let export_id = vm.heap.make_empty_identifier("exported_name");
    vm.exports = vm.heap.cons_ref(export_id.into(), NIL).into();
    vm.unused_types = true;

    vm.emit_bereaved_warnings_partial();
    assert!(vm.unused_types);
    assert_ne!(vm.exports, NIL);
}

#[test]
fn unused_diagnostics_phase_clears_deferred_marker() {
    let mut vm = VM::new_for_tests();
    vm.eprodnts = Combinator::True.into();

    vm.emit_unused_definition_diagnostics_partial();
    assert_eq!(vm.eprodnts, Value::from(NIL));
}

#[test]
fn unused_diagnostics_phase_is_stable_when_marker_absent() {
    let mut vm = VM::new_for_tests();
    vm.eprodnts = Value::from(NIL);

    vm.emit_unused_definition_diagnostics_partial();
    assert_eq!(vm.eprodnts, Value::from(NIL));
}

#[test]
fn codegen_phase_fails_without_loaded_files() {
    let mut vm = VM::new_for_tests();
    vm.files = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(matches!(
        result,
        Err(BytecodeError::CodegenWithoutLoadedFiles)
    ));
}

#[test]
fn codegen_phase_fails_during_initialization_with_unresolved_names() {
    let mut vm = VM::new_for_tests();
    vm.initializing = true;
    let source_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_init.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, source_file);
    let missing_id = vm.heap.make_empty_identifier("missing_name");
    vm.undefined_names = ConsList::new(&mut vm.heap, missing_id);

    let result = vm.run_codegen_phase();

    assert!(matches!(
        result,
        Err(BytecodeError::InitializationLoadContainsErrors)
    ));
}

#[test]
fn codegen_phase_succeeds_with_loaded_files_and_no_init_errors() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;
    let source_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_ok.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, source_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
}

#[test]
fn dump_visibility_phase_runs_for_normal_m_source() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;
    let internal = vm.heap.make_empty_identifier("internal_name");
    vm.internals = ConsList::new(&mut vm.heap, internal);

    let source_path = unique_test_path("normal_source.m")
        .to_string_lossy()
        .to_string();
    let result = vm.run_dump_visibility_phase(&source_path);

    assert!(result.is_ok());
    assert!(vm.internals.is_empty());
}

#[test]
fn dump_visibility_phase_skips_non_m_source_when_not_initializing() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;
    let internal = vm.heap.make_empty_identifier("internal_name");
    vm.internals = ConsList::new(&mut vm.heap, internal);

    let source_path = unique_test_path("binary_source.x")
        .to_string_lossy()
        .to_string();
    let result = vm.run_dump_visibility_phase(&source_path);

    assert!(result.is_ok());
    assert!(!vm.internals.is_empty());
}

#[test]
fn dump_visibility_phase_runs_during_initialization_even_without_m_extension() {
    let mut vm = VM::new_for_tests();
    vm.initializing = true;
    let internal = vm.heap.make_empty_identifier("internal_name");
    vm.internals = ConsList::new(&mut vm.heap, internal);

    let source_path = unique_test_path("prelude_source")
        .to_string_lossy()
        .to_string();
    let result = vm.run_dump_visibility_phase(&source_path);

    assert!(result.is_ok());
    assert!(vm.internals.is_empty());
}

#[test]
fn syntax_dump_records_source_anchor_for_m_scripts() {
    let mut vm = VM::new_for_tests();
    vm.old_files = ConsList::EMPTY;
    vm.last_load_phase_trace.clear();

    let source_path = unique_test_path("syntax_dump_anchor.m")
        .to_string_lossy()
        .to_string();
    let result = vm.maybe_write_syntax_dump(&source_path);

    assert!(result.is_ok());
    assert!(!vm.old_files.is_empty());
    assert_eq!(vm.last_load_phase_trace, vec!["dump-write"]);
}

#[test]
fn syntax_dump_is_noop_for_non_m_sources() {
    let mut vm = VM::new_for_tests();
    vm.old_files = ConsList::EMPTY;
    vm.last_load_phase_trace.clear();

    let source_path = unique_test_path("syntax_dump_anchor.x")
        .to_string_lossy()
        .to_string();
    let result = vm.maybe_write_syntax_dump(&source_path);

    assert!(result.is_ok());
    assert!(vm.old_files.is_empty());
    assert!(vm.last_load_phase_trace.is_empty());
}

#[test]
fn alfasort_is_deterministic_for_diagnostic_identifier_lists() {
    let mut vm = VM::new_for_tests();

    let zeta = IdentifierRecordRef::new(
        &mut vm.heap,
        "zeta".to_string(),
        IdentifierDefinitionRef::undefined(),
        Type::Undefined.into(),
        None,
    );
    let alpha = IdentifierRecordRef::new(
        &mut vm.heap,
        "alpha".to_string(),
        IdentifierDefinitionRef::undefined(),
        Type::Undefined.into(),
        None,
    );
    let mu = IdentifierRecordRef::new(
        &mut vm.heap,
        "mu".to_string(),
        IdentifierDefinitionRef::undefined(),
        Type::Undefined.into(),
        None,
    );

    let mut unsorted = ConsList::new(&mut vm.heap, zeta);
    unsorted.push(&mut vm.heap, alpha);
    unsorted.push(&mut vm.heap, mu);

    let sorted_once = super::diagnostics::alfasort(&mut vm.heap, unsorted);
    let sorted_twice = super::diagnostics::alfasort(&mut vm.heap, unsorted);
    let rendered_once = super::diagnostics::printlist(&vm.heap, sorted_once);
    let rendered_twice = super::diagnostics::printlist(&vm.heap, sorted_twice);

    assert_eq!(rendered_once, rendered_twice);
    assert_eq!(rendered_once.split(", ").count(), 3);
}

#[test]
fn printlist_formats_identifier_names_for_diagnostics() {
    let mut vm = VM::new_for_tests();

    let alpha = IdentifierRecordRef::new(
        &mut vm.heap,
        "alpha".to_string(),
        IdentifierDefinitionRef::undefined(),
        Type::Undefined.into(),
        None,
    );
    let beta = IdentifierRecordRef::new(
        &mut vm.heap,
        "beta".to_string(),
        IdentifierDefinitionRef::undefined(),
        Type::Undefined.into(),
        None,
    );
    let gamma = IdentifierRecordRef::new(
        &mut vm.heap,
        "gamma".to_string(),
        IdentifierDefinitionRef::undefined(),
        Type::Undefined.into(),
        None,
    );

    let mut names = ConsList::new(&mut vm.heap, alpha);
    names.append(&mut vm.heap, beta);
    names.append(&mut vm.heap, gamma);

    let rendered = super::diagnostics::printlist(&vm.heap, names);

    let rendered_parts: Vec<&str> = rendered.split(", ").collect();
    assert_eq!(rendered_parts.len(), 3);
    assert!(rendered_parts.iter().all(|part| !part.is_empty()));
}

#[test]
fn source_update_check_detects_when_loaded_source_is_newer_than_dump_timestamp() {
    let mut vm = VM::new_for_tests();

    let source_path = unique_test_path("source_update_check_newer.m");
    std::fs::write(&source_path, "-- source\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let stale_dump_timestamp = UNIX_EPOCH;
    let file_record = FileRecord::new(
        &mut vm.heap,
        source_path_str,
        stale_dump_timestamp,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, file_record);

    assert!(super::diagnostics::source_update_check(&vm.heap, vm.files));
}

#[test]
fn source_update_check_is_false_when_dump_timestamp_is_not_older_than_source() {
    let mut vm = VM::new_for_tests();

    let source_path = unique_test_path("source_update_check_current.m");
    std::fs::write(&source_path, "-- source\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let future_dump_timestamp = SystemTime::now() + std::time::Duration::from_secs(60);
    let file_record = FileRecord::new(
        &mut vm.heap,
        source_path_str,
        future_dump_timestamp,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, file_record);

    assert!(!super::diagnostics::source_update_check(&vm.heap, vm.files));
}

#[test]
fn unfix_exports_partial_clears_export_processing_state() {
    let mut vm = VM::new_for_tests();

    vm.in_export_list = true;
    let internal = vm.heap.make_empty_identifier("internal_name");
    vm.internals = ConsList::new(&mut vm.heap, internal);

    vm.unfix_exports_partial();

    assert!(!vm.in_export_list);
    assert!(vm.internals.is_empty());
}

#[test]
fn hdsort_orders_free_binding_pairs_by_identifier_name() {
    let mut vm = VM::new_for_tests();

    let gamma = vm.heap.make_empty_identifier("gamma");
    let alpha = vm.heap.make_empty_identifier("alpha");
    let beta = vm.heap.make_empty_identifier("beta");

    let gamma_payload = vm.heap.integer_ref(1);
    let alpha_payload = vm.heap.integer_ref(2);
    let beta_payload = vm.heap.integer_ref(3);
    let gamma_pair: RawValue = vm.heap.cons_ref(gamma.into(), gamma_payload).into();
    let alpha_pair: RawValue = vm.heap.cons_ref(alpha.into(), alpha_payload).into();
    let beta_pair: RawValue = vm.heap.cons_ref(beta.into(), beta_payload).into();

    let mut unsorted: ConsList = ConsList::EMPTY;
    unsorted.append(&mut vm.heap, gamma_pair);
    unsorted.append(&mut vm.heap, alpha_pair);
    unsorted.append(&mut vm.heap, beta_pair);

    let sorted_ref = super::bytecode::hdsort_binding_list_ref(&mut vm.heap, unsorted.get_ref());
    let mut sorted: ConsList = ConsList::from_ref(sorted_ref);

    let mut names: Vec<String> = vec![];
    while let Some(binding_pair_ref) = sorted.pop_raw(&vm.heap) {
        let id_ref = IdentifierRecordRef::from_ref(vm.heap[binding_pair_ref].head);
        names.push(id_ref.get_name(&vm.heap));
    }

    assert_eq!(names, vec!["alpha", "beta", "gamma"]);
}

#[test]
fn bindparams_records_missing_and_extra_bindings_and_writes_matches() {
    let mut vm = VM::new_for_tests();

    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let z = vm.heap.make_empty_identifier("z");

    let x_original =
        IdentifierDefinitionRef::alias_metadata_from_source_identifier(&mut vm.heap, x);
    let y_original =
        IdentifierDefinitionRef::alias_metadata_from_source_identifier(&mut vm.heap, y);

    let x_formal_payload = vm
        .heap
        .cons_ref(x_original.get_ref().into(), Type::Undefined.into());
    let y_formal_payload = vm
        .heap
        .cons_ref(y_original.get_ref().into(), Type::Undefined.into());
    let x_formal_binding: RawValue = vm.heap.cons_ref(x.into(), x_formal_payload).into();
    let y_formal_binding: RawValue = vm.heap.cons_ref(y.into(), y_formal_payload).into();

    let mut formal_list: ConsList = ConsList::EMPTY;
    formal_list.append(&mut vm.heap, x_formal_binding);
    formal_list.append(&mut vm.heap, y_formal_binding);

    let x_payload = vm.heap.integer_ref(42);
    let z_payload = vm.heap.integer_ref(7);
    let x_actual_binding: RawValue = vm.heap.cons_ref(x.into(), x_payload).into();
    let z_actual_binding: RawValue = vm.heap.cons_ref(z.into(), z_payload).into();

    let mut actual_list: ConsList = ConsList::EMPTY;
    actual_list.append(&mut vm.heap, x_actual_binding);
    actual_list.append(&mut vm.heap, z_actual_binding);

    vm.bindparams(formal_list.get_ref().into(), actual_list.get_ref().into());

    assert_eq!(vm.heap[x.get_ref()].tail, RawValue::from(x_payload));

    assert_eq!(vm.missing_parameter_bindings.len(&vm.heap), 1);
    assert_eq!(
        vm.missing_parameter_bindings.raw_head(&vm.heap),
        Some(y_original.get_ref())
    );

    assert_eq!(vm.detritus_parameter_bindings.len(&vm.heap), 1);
    assert_eq!(
        vm.detritus_parameter_bindings.raw_head(&vm.heap),
        Some(z.get_ref())
    );

    assert_eq!(vm.free_binding_sets.len(&vm.heap), 1);
    assert_eq!(
        vm.free_binding_sets.raw_head(&vm.heap),
        Some(formal_list.get_ref())
    );
}

#[test]
fn bindparams_records_wrong_arity_in_detritus_and_still_writes_formal_value() {
    let mut vm = VM::new_for_tests();

    let t = vm.heap.make_empty_identifier("t");
    let t_original =
        IdentifierDefinitionRef::alias_metadata_from_source_identifier(&mut vm.heap, t);

    let formal_value_type =
        IdentifierValueTypeRef::new(&mut vm.heap, IdentifierValueTypeData::Free);
    t.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Typed {
            arity: 2,
            show_function: Combinator::Nil.into(),
            value_type: formal_value_type,
        },
    );

    let formal_payload = vm
        .heap
        .cons_ref(t_original.get_ref().into(), Type::Type.into());
    let formal_binding: RawValue = vm.heap.cons_ref(t.into(), formal_payload).into();
    let formal_list: ConsList = ConsList::new(&mut vm.heap, formal_binding);

    let actual_value_type =
        IdentifierValueTypeRef::new(&mut vm.heap, IdentifierValueTypeData::Free);
    let actual_type_payload = IdentifierValueRef::new(
        &mut vm.heap,
        IdentifierValueData::Typed {
            arity: 3,
            show_function: Combinator::Nil.into(),
            value_type: actual_value_type,
        },
    );

    let actual_binding_ref: RawValue = vm
        .heap
        .apply_ref(t.into(), Value::from(actual_type_payload.get_ref()))
        .into();
    let actual_list: ConsList = ConsList::new(&mut vm.heap, actual_binding_ref);

    vm.bindparams(formal_list.get_ref().into(), actual_list.get_ref().into());

    assert_eq!(vm.detritus_parameter_bindings.len(&vm.heap), 1);
    let detritus_entry = vm
        .detritus_parameter_bindings
        .raw_head(&vm.heap)
        .expect("expected one detritus entry");
    assert_eq!(vm.heap[detritus_entry].tag, Tag::Cons);
    assert_eq!(vm.heap[detritus_entry].head, t.get_ref());

    let arity_pair_ref = vm.heap[detritus_entry].tail;
    assert_eq!(vm.heap[arity_pair_ref].tag, Tag::DataPair);
    assert_eq!(vm.heap[arity_pair_ref].head, 2);
    assert_eq!(vm.heap[arity_pair_ref].tail, 3);

    assert_eq!(vm.heap[t.get_ref()].tail, vm.heap[actual_binding_ref].tail);
}
