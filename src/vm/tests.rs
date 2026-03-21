use super::*;
use crate::big_num::IntegerRef;
use crate::compiler::{
    HereInfo, ParserExportDirectivePayload, ParserIncludeDirectivePayload, ParserSupportError,
    ParserTopLevelDirectivePayload, Token,
};
use crate::data::api::{
    ApNodeRef, DataPair, FreeFormalBindingRef, HeapObjectProxy, IdentifierValueTypeData,
    IdentifierValueTypeKind, IdentifierValueTypeRef, TypeExprRef,
};
use crate::vm::load::LoadScriptForm;
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

fn test_anchor(vm: &mut VM, source_path: &str, line_number: isize) -> RawValue {
    Value::Reference(
        FileInfoRef::from_script_file(&mut vm.heap, source_path.to_string(), line_number).get_ref(),
    )
    .into()
}

fn include_request_payload(
    vm: &mut VM,
    source_path: &str,
    line_number: isize,
    target_path: &str,
) -> ParserIncludeDirectivePayload {
    ParserIncludeDirectivePayload {
        anchor: test_anchor(vm, source_path, line_number),
        target_path: Value::Reference(vm.heap.string(target_path)).into(),
        modifiers: NIL.into(),
        bindings: NIL.into(),
    }
}

fn export_payload(
    vm: &mut VM,
    source_path: &str,
    line_number: isize,
    exported_ids: &[&str],
    pathname_requests: &[&str],
    include_current_script: bool,
    export_embargoes: &[&str],
) -> ParserTopLevelDirectivePayload {
    let exported_ids = exported_ids.iter().rev().fold(NIL, |list, name| {
        let id = vm.intern_identifier(name);
        vm.heap.cons_ref(id.into(), list)
    });
    let pathname_requests = pathname_requests.iter().rev().fold(
        if include_current_script {
            vm.heap.cons_ref(Combinator::Plus.into(), NIL)
        } else {
            NIL
        },
        |list, path| {
            let path_ref = vm.heap.string(*path);
            vm.heap.cons_ref(Value::Reference(path_ref), list)
        },
    );
    let export_embargoes = export_embargoes.iter().rev().fold(NIL, |list, name| {
        let id = vm.intern_identifier(name);
        vm.heap.cons_ref(id.into(), list)
    });

    ParserTopLevelDirectivePayload {
        include_requests: Vec::new(),
        export: Some(ParserExportDirectivePayload {
            anchor: test_anchor(vm, source_path, line_number),
            exported_ids: exported_ids.into(),
            pathname_requests: pathname_requests.into(),
            embargoes: export_embargoes.into(),
        }),
    }
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
        Err(LoadFileError::SourceInput(SourceInputError::MissingFile { path })) if path == source_path_str
    ));
    assert!(!vm.loading);
}

#[test]
fn undump_propagates_load_file_error_after_bad_dump() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("script.m");
    std::fs::write(&source_path, "entry = 1\n").expect("failed to write source test file");

    let binary_path = source_path.with_extension("x");
    std::fs::write(&binary_path, [0_u8, 0_u8, 0_u8, 0_u8])
        .expect("failed to write binary test file");

    let source_path_str = source_path.to_string_lossy().to_string();
    let result = vm.undump(&source_path_str);

    assert!(result.is_ok());
    assert!(!vm.loading);
    assert_eq!(vm.files.len(&vm.heap), 1);
}

#[test]
fn undump_falls_back_to_load_file_after_wrong_bytecode_version() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("script_version_mismatch.m");
    std::fs::write(&source_path, "entry = 1\n").expect("failed to write source test file");

    let binary_path = source_path.with_extension("x");
    let mut bad_dump = vec![0_u8; 16];
    bad_dump[0] = WORD_SIZE as u8;
    bad_dump[1] = (XVERSION as u8).wrapping_add(1);
    std::fs::write(&binary_path, bad_dump).expect("failed to write binary test file");

    let source_path_str = source_path.to_string_lossy().to_string();
    let result = vm.undump(&source_path_str);

    assert!(result.is_ok());
    assert!(!vm.loading);
    assert_eq!(vm.files.len(&vm.heap), 1);
}

#[test]
fn load_file_parser_deferred_keeps_attempted_source_in_old_files() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    vm.files = vm.empty_environment_for_source("prior_source.m", UNIX_EPOCH);

    let source_path = unique_test_path("parser_deferred.m");
    std::fs::write(&source_path, "entry = 1\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);
    assert!(
        result.is_ok(),
        "result={result:?} diagnostics={:?}",
        vm.parser_diagnostics
    );
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
    assert_eq!(vm.files.len(&vm.heap), 1);
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
        Err(LoadFileError::SourceInput(SourceInputError::MissingFile { path })) if path == source_path_str
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
    std::fs::write(&source_path_with_m, "entry = 1\n").expect("failed to write source test file");

    let requested_path = source_path_no_ext.to_string_lossy().to_string();
    let normalized_path = source_path_with_m.to_string_lossy().to_string();
    let result = vm.load_file(&requested_path);

    assert!(result.is_ok());
    assert!(!vm.loading);
    assert_eq!(vm.files.len(&vm.heap), 1);

    let current_anchor = vm
        .files
        .head(&vm.heap)
        .expect("missing current file anchor");
    assert_eq!(current_anchor.get_file_name(&vm.heap), normalized_path);
}

#[test]
fn classify_load_script_form_distinguishes_supported_top_level_forms() {
    let mut vm = VM::new_for_tests();

    assert_eq!(
        vm.classify_load_script_form("slice.m", "+")
            .expect("expression input should classify"),
        LoadScriptForm::Expression
    );
    assert_eq!(
        vm.classify_load_script_form("slice.m", "%export foo")
            .expect("%export should classify"),
        LoadScriptForm::TopLevelScript
    );
    assert_eq!(
        vm.classify_load_script_form("slice.m", "%include target")
            .expect("%include should classify"),
        LoadScriptForm::TopLevelScript
    );
    assert_eq!(
        vm.classify_load_script_form("slice.m", "entry = 1")
            .expect("top-level definition should classify"),
        LoadScriptForm::TopLevelScript
    );
    assert_eq!(
        vm.classify_load_script_form("slice.m", "id x = x")
            .expect("top-level function definition should classify"),
        LoadScriptForm::TopLevelScript
    );
    assert_eq!(
        vm.classify_load_script_form("slice.m", "head (x:xs) = x")
            .expect("top-level cons-pattern definition should classify"),
        LoadScriptForm::TopLevelScript
    );
    assert_eq!(
        vm.classify_load_script_form("slice.m", "fromJust (Just x) = x")
            .expect("top-level constructor-pattern definition should classify"),
        LoadScriptForm::TopLevelScript
    );
    assert_eq!(
        vm.classify_load_script_form("slice.m", "second (x,y) = y")
            .expect("top-level tuple-pattern definition should classify"),
        LoadScriptForm::TopLevelScript
    );
    assert_eq!(
        vm.classify_load_script_form("slice.m", "entry :: type")
            .expect("top-level specification should classify"),
        LoadScriptForm::TopLevelScript
    );
    assert_eq!(
        vm.classify_load_script_form("slice.m", "thing == type")
            .expect("top-level synonym should classify"),
        LoadScriptForm::TopLevelScript
    );
    assert_eq!(
        vm.classify_load_script_form("slice.m", "maybe ::= Just | Nothing")
            .expect("top-level algebraic type should classify"),
        LoadScriptForm::TopLevelScript
    );
    assert_eq!(
        vm.classify_load_script_form("slice.m", "%free { x :: num }")
            .expect("%free should classify"),
        LoadScriptForm::TopLevelScript
    );
}

#[test]
fn load_file_keeps_cons_pattern_tail_name_bound_after_constructor_formal_checks() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("cons_pattern_tail_binding.m");
    std::fs::write(&source_path, "tail (x:xs) = xs\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(
        result.is_ok(),
        "result={result:?} diagnostics={:?}",
        vm.parser_diagnostics
    );
    assert!(vm.undefined_names.is_empty());

    let tail = vm
        .heap
        .get_identifier("tail")
        .expect("expected tail identifier to exist");
    let tail_value = tail
        .get_value(&vm.heap)
        .expect("expected tail definition value to exist");
    let IdentifierValueData::Arbitrary(Value::Reference(lambda_ref)) =
        tail_value.get_data(&vm.heap)
    else {
        panic!("expected top-level cons pattern form to lower into a lambda body");
    };
    let lambda_raw: RawValue = Value::Reference(lambda_ref).into();
    let body_identifier = IdentifierRecordRef::from_ref(vm.heap[lambda_raw].tail);
    assert_eq!(vm.identifier_name(body_identifier), "xs");
}

#[test]
fn load_file_accepts_negative_integer_literal_pattern_in_formal() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("negative_integer_literal_formal.m");
    std::fs::write(&source_path, "sign (-1) = 0\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(
        result.is_ok(),
        "result={result:?} diagnostics={:?}",
        vm.parser_diagnostics
    );
    assert!(vm.undefined_names.is_empty());
}

#[test]
fn load_file_accepts_n_plus_k_pattern_and_binds_inner_name() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("n_plus_k_formal.m");
    std::fs::write(&source_path, "pred (x+1) = x\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(
        result.is_ok(),
        "result={result:?} diagnostics={:?}",
        vm.parser_diagnostics
    );
    assert!(vm.undefined_names.is_empty());
}

#[test]
fn load_file_rejects_undeclared_constructor_atom_in_formal() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("undeclared_constructor_atom_formal.m");
    std::fs::write(&source_path, "isNope Nope = True\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::UndeclaredConstructorsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_rejects_undeclared_constructor_application_in_formal() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("undeclared_constructor_application_formal.m");
    std::fs::write(&source_path, "fromNope (Nope x) = x\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::UndeclaredConstructorsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_rejects_declared_constructor_application_for_wrong_arity_in_formal() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("constructor_application_wrong_arity_formal.m");
    std::fs::write(
        &source_path,
        "maybe ::= Just | Nothing\nfromJust (Just x) = x\n",
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::ConstructorArityMismatchInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_accepts_declared_unary_constructor_application_in_formal() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("constructor_application_matching_arity_formal.m");
    std::fs::write(
        &source_path,
        "maybe * ::= Nothing | Just *\nfromJust (Just x) = x\n",
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(
        result.is_ok(),
        "result={result:?} diagnostics={:?}",
        vm.parser_diagnostics
    );
    assert!(vm.undefined_names.is_empty());

    let maybe = vm
        .heap
        .get_identifier("maybe")
        .expect("expected maybe type identifier");
    let just = vm
        .heap
        .get_identifier("Just")
        .expect("expected Just constructor");
    let maybe_value = maybe
        .get_value(&vm.heap)
        .expect("expected typed algebraic value");
    let IdentifierValueData::Typed { value_type, .. } = maybe_value.get_data(&vm.heap) else {
        panic!("expected typed algebraic value")
    };
    let mut constructors = value_type
        .algebraic_constructor_metadata(&vm.heap)
        .expect("expected algebraic constructor metadata");
    let nothing_metadata = constructors
        .pop(&vm.heap)
        .expect("expected Nothing metadata");
    let just_metadata = constructors.pop(&vm.heap).expect("expected Just metadata");
    assert_eq!(nothing_metadata.arity(&vm.heap), 0);
    assert_eq!(just_metadata.arity(&vm.heap), 1);

    let just_type = just.get_type_expr(&vm.heap);
    assert!(vm.heap.is_arrow_type(just_type.value()));
    let outer_arrow = ApNodeRef::from_ref(just_type.value().into());
    let operator_application = outer_arrow
        .function_application(&vm.heap)
        .expect("expected binary arrow application");
    let left = Value::from(operator_application.argument_raw(&vm.heap));
    let right = Value::from(outer_arrow.argument_raw(&vm.heap));
    assert!(vm.heap.is_type_variable(left));
    let parent_application = ApNodeRef::from_ref(right.into());
    assert_eq!(
        Value::from(parent_application.function_raw(&vm.heap)),
        maybe.into()
    );
    assert!(vm
        .heap
        .is_type_variable(Value::from(parent_application.argument_raw(&vm.heap))));
}

#[test]
fn load_file_accepts_declared_infix_constructor_application_in_formal() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("infix_constructor_application_formal.m");
    std::fs::write(
        &source_path,
        "pair * ** ::= * $Pair **\nfirst (x $Pair y) = x\n",
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(
        result.is_ok(),
        "result={result:?} diagnostics={:?}",
        vm.parser_diagnostics
    );
    assert!(vm.undefined_names.is_empty());
}

#[test]
fn load_file_rejects_infix_name_application_in_formal() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("infix_name_application_formal.m");
    std::fs::write(&source_path, "bad (x $merge y) = x\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::ValueHeadApplicationsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_reports_infix_name_formal_before_generic_undefined_name() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("infix_name_formal_precedes_undefined_name.m");
    std::fs::write(&source_path, "bad (x $merge y) = missing\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::ValueHeadApplicationsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_rejects_value_head_application_in_formal() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("value_head_application_formal.m");
    std::fs::write(&source_path, "bad (g x) = x\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::ValueHeadApplicationsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_rejects_non_identifier_head_application_in_formal() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("non_identifier_head_application_formal.m");
    std::fs::write(&source_path, "bad ((x:xs) y) = y\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::NonIdentifierApplicationHeadsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_repeated_name_application_head_still_reaches_value_head_bucket() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("repeated_name_application_head_formal.m");
    std::fs::write(&source_path, "bad (x, (x y)) = y\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::ValueHeadApplicationsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_arithmetic_head_application_still_reaches_unsupported_arithmetic_bucket() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("arithmetic_head_application_formal.m");
    std::fs::write(&source_path, "bad (((x+1)) y) = y\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::UnsupportedArithmeticPatternsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_commits_strict_constructor_field_metadata() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("strict_constructor_field_metadata.m");
    std::fs::write(&source_path, "strictpair * ** ::= Pair *! **!\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(
        result.is_ok(),
        "result={result:?} diagnostics={:?}",
        vm.parser_diagnostics
    );

    let strictpair = vm
        .heap
        .get_identifier("strictpair")
        .expect("expected strictpair type identifier");
    let strictpair_value = strictpair
        .get_value(&vm.heap)
        .expect("expected typed algebraic value");
    let IdentifierValueData::Typed { value_type, .. } = strictpair_value.get_data(&vm.heap) else {
        panic!("expected typed algebraic value")
    };
    let mut constructors = value_type
        .algebraic_constructor_metadata(&vm.heap)
        .expect("expected algebraic constructor metadata");
    let pair_metadata = constructors.pop(&vm.heap).expect("expected Pair metadata");
    assert_eq!(pair_metadata.arity(&vm.heap), 2);

    let mut fields = pair_metadata.fields(&vm.heap);
    let first_field = fields.pop(&vm.heap).expect("expected first field metadata");
    let second_field = fields
        .pop(&vm.heap)
        .expect("expected second field metadata");
    assert!(first_field.is_strict(&vm.heap));
    assert!(second_field.is_strict(&vm.heap));

    let first_field_type = first_field.type_expr(&vm.heap);
    let second_field_type = second_field.type_expr(&vm.heap);
    assert_eq!(first_field_type.type_variable_ordinal(&vm.heap), Some(1));
    assert_eq!(second_field_type.type_variable_ordinal(&vm.heap), Some(2));
}

#[test]
fn load_file_rejects_declared_unary_constructor_used_nullary_in_formal() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("constructor_application_nullary_formal_wrong_arity.m");
    std::fs::write(
        &source_path,
        "maybe * ::= Nothing | Just *\nbad Just = True\n",
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::ConstructorArityMismatchInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_rejects_declared_unary_constructor_used_binary_in_formal() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("constructor_application_binary_formal_wrong_arity.m");
    std::fs::write(
        &source_path,
        "maybe * ::= Nothing | Just *\nbad (Just x y) = x\n",
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::ConstructorArityMismatchInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_rejects_undeclared_constructor_inside_tuple_formal() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("undeclared_constructor_tuple_formal.m");
    std::fs::write(&source_path, "isPair (Nope,x) = x\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::UndeclaredConstructorsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_rejects_undeclared_constructor_inside_list_formal() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("undeclared_constructor_list_formal.m");
    std::fs::write(&source_path, "isList [Nope] = True\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::UndeclaredConstructorsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_rejects_declared_constructor_application_inside_tuple_formal_for_wrong_arity() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("constructor_application_tuple_formal_wrong_arity.m");
    std::fs::write(
        &source_path,
        "maybe ::= Just | Nothing\nfromTuple (Just x,y) = x\n",
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::ConstructorArityMismatchInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_reports_tuple_contained_constructor_formal_before_generic_undefined_name() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("tuple_constructor_precedes_undefined_name.m");
    std::fs::write(&source_path, "isPair (Nope,x) = missing\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::UndeclaredConstructorsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_reports_undeclared_constructor_formal_before_generic_undefined_name() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("undeclared_constructor_precedes_undefined_name.m");
    std::fs::write(&source_path, "isNope Nope = missing\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::UndeclaredConstructorsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_reports_constructor_formal_arity_before_generic_undefined_name() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("constructor_arity_precedes_undefined_name.m");
    std::fs::write(
        &source_path,
        "maybe ::= Just | Nothing\nfromJust (Just x) = missing\n",
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::ConstructorArityMismatchInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_commits_narrow_spec_and_type_substrate_into_current_file() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("narrow_type_substrate.m");
    std::fs::write(
        &source_path,
        "entry :: type\nthing == type\nmaybe ::= Just | Nothing\n",
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok());

    let entry = vm
        .heap
        .get_identifier("entry")
        .expect("expected entry identifier to exist");
    let thing = vm
        .heap
        .get_identifier("thing")
        .expect("expected thing identifier to exist");
    let maybe = vm
        .heap
        .get_identifier("maybe")
        .expect("expected maybe identifier to exist");
    let just = vm
        .heap
        .get_identifier("Just")
        .expect("expected Just constructor to exist");
    let nothing = vm
        .heap
        .get_identifier("Nothing")
        .expect("expected Nothing constructor to exist");

    assert!(entry.get_type_expr(&vm.heap).is_builtin_type(Type::Type));
    assert_eq!(
        thing
            .get_value(&vm.heap)
            .expect("expected typed synonym value")
            .typed_kind(&vm.heap),
        Some(IdentifierValueTypeKind::Synonym)
    );
    assert_eq!(
        maybe
            .get_value(&vm.heap)
            .expect("expected typed algebraic value")
            .typed_kind(&vm.heap),
        Some(IdentifierValueTypeKind::Algebraic)
    );
    assert_eq!(just.get_type_expr(&vm.heap), TypeExprRef::new(maybe.into()));
    assert_eq!(
        nothing.get_type_expr(&vm.heap),
        TypeExprRef::new(maybe.into())
    );

    let current_file = vm.files.head(&vm.heap).expect("expected current file");
    assert_eq!(current_file.get_definienda(&vm.heap).len(&vm.heap), 5);
}

#[test]
fn load_file_commits_multi_name_specification_payloads() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("multi_name_spec.m");
    std::fs::write(&source_path, "f, g :: [num] -> num\nf = 1\ng = 2\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok());

    let f = vm.heap.get_identifier("f").expect("expected f identifier");
    let g = vm.heap.get_identifier("g").expect("expected g identifier");
    assert!(vm.heap.is_arrow_type(f.get_type_expr(&vm.heap).value()));
    assert!(vm.heap.is_arrow_type(g.get_type_expr(&vm.heap).value()));

    let current_file = vm.files.head(&vm.heap).expect("expected current file");
    assert_eq!(current_file.get_definienda(&vm.heap).len(&vm.heap), 2);
}

#[test]
fn load_file_commits_synonym_type_lhs_arity() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("synonym_type_lhs_arity.m");
    std::fs::write(&source_path, "pair * == (*, [*])\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok());

    let pair = vm
        .heap
        .get_identifier("pair")
        .expect("expected pair type identifier");
    let pair_value = pair
        .get_value(&vm.heap)
        .expect("expected typed synonym value");
    let IdentifierValueData::Typed {
        arity, value_type, ..
    } = pair_value.get_data(&vm.heap)
    else {
        panic!("expected typed identifier value")
    };
    assert_eq!(arity, 1);
    assert_eq!(
        value_type.get_identifier_value_type_kind(&vm.heap),
        IdentifierValueTypeKind::Synonym
    );
}

#[test]
fn load_file_commits_algebraic_type_lhs_arity_with_nullary_constructors() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("algebraic_type_lhs_arity.m");
    std::fs::write(&source_path, "option * ::= None | NilOpt\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok());

    let option = vm
        .heap
        .get_identifier("option")
        .expect("expected option type identifier");
    let none = vm
        .heap
        .get_identifier("None")
        .expect("expected None constructor");
    let nil_opt = vm
        .heap
        .get_identifier("NilOpt")
        .expect("expected NilOpt constructor");

    let option_value = option
        .get_value(&vm.heap)
        .expect("expected typed algebraic value");
    let IdentifierValueData::Typed {
        arity, value_type, ..
    } = option_value.get_data(&vm.heap)
    else {
        panic!("expected typed identifier value")
    };
    assert_eq!(arity, 1);
    assert_eq!(
        value_type.get_identifier_value_type_kind(&vm.heap),
        IdentifierValueTypeKind::Algebraic
    );

    let none_type = none.get_type_expr(&vm.heap);
    let nil_opt_type = nil_opt.get_type_expr(&vm.heap);
    for constructor_type in [none_type, nil_opt_type] {
        let applied = ApNodeRef::from_ref(constructor_type.value().into());
        assert_eq!(Value::from(applied.function_raw(&vm.heap)), option.into());
        assert!(vm
            .heap
            .is_type_variable(Value::from(applied.argument_raw(&vm.heap))));
    }
}

#[test]
fn parse_source_text_commits_narrow_free_substrate_and_marks_file_unshareable() {
    let mut vm = VM::new_for_tests();

    let source_path = unique_test_path("narrow_free_substrate.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let source_text = "%free { x :: num }\n";

    let result = vm.parse_source_text(&source_path_str, source_text, UNIX_EPOCH, false);

    assert!(result.is_ok());

    let x = vm
        .heap
        .get_identifier("x")
        .expect("expected free identifier to exist");
    assert!(x.get_type_expr(&vm.heap).is_builtin_type(Type::Number));

    assert_eq!(vm.free_identifiers.len(&vm.heap), 1);
    let free_binding = vm
        .free_identifiers
        .head(&vm.heap)
        .expect("expected one free formal binding");
    assert_eq!(free_binding.identifier(&vm.heap), x);
    assert!(free_binding
        .type_expr(&vm.heap)
        .is_builtin_type(Type::Number));
    let original_name = free_binding.original_name_metadata(&vm.heap);
    assert_eq!(original_name.right_value(&vm.heap), 0.into());
    let expected_original_name_ref = vm.heap.string(x.get_name(&vm.heap));
    assert_eq!(
        vm.heap[original_name.get_ref()].head,
        expected_original_name_ref
    );

    let current_file = result
        .expect("expected parse outcome")
        .files
        .head(&vm.heap)
        .expect("expected current file");
    assert!(!current_file.is_shareable(&vm.heap));
}

#[test]
fn parse_source_text_commits_rich_specification_type_expr() {
    let mut vm = VM::new_for_tests();

    let source_path = unique_test_path("rich_spec_type_expr.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let source_text = "map :: [*] -> [*]\n";

    let result = vm.parse_source_text(&source_path_str, source_text, UNIX_EPOCH, false);

    assert!(result.is_ok());

    let map = vm
        .heap
        .get_identifier("map")
        .expect("expected specification identifier");
    let map_type = map.get_type_expr(&vm.heap);
    assert!(vm.heap.is_arrow_type(map_type.value()));

    let outer_arrow = ApNodeRef::from_ref(map_type.value().into());
    let operator_application = outer_arrow
        .function_application(&vm.heap)
        .expect("expected binary arrow application");
    let left = Value::from(operator_application.argument_raw(&vm.heap));
    let right = Value::from(outer_arrow.argument_raw(&vm.heap));
    assert!(vm.heap.is_list_type(left));
    assert!(vm.heap.is_list_type(right));

    let left_arg = Value::from(ApNodeRef::from_ref(left.into()).argument_raw(&vm.heap));
    let right_arg = Value::from(ApNodeRef::from_ref(right.into()).argument_raw(&vm.heap));
    assert!(vm.heap.is_type_variable(left_arg));
    assert!(vm.heap.is_type_variable(right_arg));
}

#[test]
fn parse_source_text_commits_rich_synonym_rhs_type_expr() {
    let mut vm = VM::new_for_tests();

    let source_path = unique_test_path("rich_synonym_rhs.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let source_text = "pair == (*, [num])\n";

    let result = vm.parse_source_text(&source_path_str, source_text, UNIX_EPOCH, false);

    assert!(result.is_ok());

    let pair = vm
        .heap
        .get_identifier("pair")
        .expect("expected synonym identifier");
    let pair_value = pair
        .get_value(&vm.heap)
        .expect("expected synonym value payload");
    let IdentifierValueData::Typed { value_type, .. } = pair_value.get_data(&vm.heap) else {
        panic!("expected typed synonym payload");
    };
    assert_eq!(
        value_type.get_identifier_value_type_kind(&vm.heap),
        IdentifierValueTypeKind::Synonym
    );

    let synonym_rhs = value_type.synonym_rhs_type_expr(&vm.heap).value();
    assert!(vm.heap.is_comma_type(synonym_rhs));

    let outer_pair = ApNodeRef::from_ref(synonym_rhs.into());
    let operator_application = outer_pair
        .function_application(&vm.heap)
        .expect("expected binary comma application");
    let left = Value::from(operator_application.argument_raw(&vm.heap));
    let right = Value::from(outer_pair.argument_raw(&vm.heap));
    assert!(vm.heap.is_type_variable(left));
    assert!(vm.heap.is_comma_type(right));

    let right_pair = ApNodeRef::from_ref(right.into());
    let right_operator_application = right_pair
        .function_application(&vm.heap)
        .expect("expected tuple tail comma application");
    let tuple_second = Value::from(right_operator_application.argument_raw(&vm.heap));
    let tuple_terminator = Value::from(right_pair.argument_raw(&vm.heap));
    assert!(vm.heap.is_list_type(tuple_second));
    assert_eq!(RawValue::from(tuple_terminator), RawValue::from(Type::Void));

    let list_arg = Value::from(ApNodeRef::from_ref(tuple_second.into()).argument_raw(&vm.heap));
    assert_eq!(RawValue::from(list_arg), RawValue::from(Type::Number));
}

#[test]
fn parse_source_text_commits_rich_free_binding_type_expr() {
    let mut vm = VM::new_for_tests();

    let source_path = unique_test_path("rich_free_type_expr.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let source_text = "%free { xs :: [char] }\n";

    let result = vm.parse_source_text(&source_path_str, source_text, UNIX_EPOCH, false);

    assert!(result.is_ok());

    let xs = vm
        .heap
        .get_identifier("xs")
        .expect("expected free identifier to exist");
    let xs_type = xs.get_type_expr(&vm.heap);
    assert!(vm.heap.is_list_type(xs_type.value()));
    let list_arg = Value::from(ApNodeRef::from_ref(xs_type.value().into()).argument_raw(&vm.heap));
    assert_eq!(RawValue::from(list_arg), RawValue::from(Type::Char));

    let current_file = result
        .expect("expected parse outcome")
        .files
        .head(&vm.heap)
        .expect("expected current file");
    assert!(!current_file.is_shareable(&vm.heap));
}

#[test]
fn load_file_reports_undefined_typename_in_rich_synonym_rhs() {
    let mut vm = VM::new_for_tests();

    let source_path = unique_test_path("undefined_typename_rich_synonym.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    std::fs::write(&source_path, "alias == missing num\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::UndefinedTypeNames { count: 1 }
        ))
    ));
}

#[test]
fn load_file_reports_non_type_identifier_in_rich_free_binding_type_expr() {
    let mut vm = VM::new_for_tests();

    let source_path = unique_test_path("non_type_rich_free_expr.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    std::fs::write(
        &source_path,
        "value_alias = 1\n%free { x :: value_alias num }\n",
    )
    .expect("failed to write source test file");

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::NonTypeIdentifiersInTypeExpr { count: 1 }
        ))
    ));
}

#[test]
fn load_file_reports_type_arity_mismatch_in_rich_spec_type_expr() {
    let mut vm = VM::new_for_tests();

    let source_path = unique_test_path("arity_mismatch_rich_spec_expr.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    std::fs::write(
        &source_path,
        "maybe ::= Just | Nothing\nentry :: maybe num\nentry = 1\n",
    )
    .expect("failed to write source test file");

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::TypeArityMismatch { count: 1 }
        ))
    ));
}

#[test]
fn load_file_runs_include_export_directive_pipeline_and_commits_exports() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("include_export_directive_export_only.m");
    std::fs::write(&source_path, "%export foo\n").expect("failed to write source test file");
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
    assert_ne!(vm.exported_identifiers, NIL);
    assert!(!vm.loading);
}

#[test]
fn load_file_exportfile_validation_failure_leaves_no_authoritative_commit() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let missing_include_path = unique_test_path("not_included.m");
    let missing_include_path_str = missing_include_path.to_string_lossy().to_string();
    let source_path = unique_test_path("exportfile_validation_failure.m");
    std::fs::write(
        &source_path,
        format!("%export foo \"{}\"\n", missing_include_path_str),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::ExportValidation(ExportValidationError::PathNotIncludedInScript { path })) if path == missing_include_path_str
    ));
    assert_eq!(
        vm.last_load_phase_trace,
        vec!["begin", "parse", "exportfile-checks"]
    );
    assert_eq!(vm.exported_identifiers, NIL);
    assert_eq!(vm.export_paths, NIL);
    assert_eq!(vm.export_embargoes, NIL);
    assert!(!vm.loading);
}

#[test]
fn load_file_include_materialization_failure_leaves_no_authoritative_commit() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let missing_include_path = unique_test_path("missing_include_target.m");
    let missing_include_path_str = missing_include_path.to_string_lossy().to_string();
    let source_path = unique_test_path("include_materialization_failure.m");
    std::fs::write(
        &source_path,
        format!("%include \"{}\"\n%export foo\n", missing_include_path_str),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::SourceInput(SourceInputError::MissingFile { path })) if path == missing_include_path_str
    ));
    assert_eq!(
        vm.last_load_phase_trace,
        vec!["begin", "parse", "exportfile-checks", "include-expansion"]
    );
    assert_eq!(vm.exported_identifiers, NIL);
    assert_eq!(vm.export_paths, NIL);
    assert_eq!(vm.export_embargoes, NIL);
    assert_eq!(vm.files.len(&vm.heap), 1);
    assert!(!vm.loading);
}

#[test]
fn load_file_include_and_exportfile_success_commits_after_validation() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let include_path = unique_test_path("committed_include_target.m");
    std::fs::write(&include_path, "+\n").expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("directive_commit_success.m");
    std::fs::write(
        &source_path,
        format!(
            "%include \"{}\"\n%export foo \"{}\"\n",
            include_path_str, include_path_str
        ),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok());
    assert_eq!(vm.files.len(&vm.heap), 2);
    assert_eq!(vm.included_files, ConsList::EMPTY);
    assert_ne!(vm.exported_identifiers, NIL);
    assert_ne!(vm.export_paths, NIL);
    assert_eq!(
        vm.identifier_name(
            ConsList::<IdentifierRecordRef>::from_ref(vm.exported_identifiers.into())
                .head(&vm.heap)
                .expect("expected committed export id")
        ),
        "foo"
    );
    assert_eq!(
        vm.heap
            .resolve_string(
                ConsList::<Value>::from_ref(vm.export_paths.into())
                    .value_head(&vm.heap)
                    .expect("expected committed exportfile path")
            )
            .expect("committed exportfile should be a heap string"),
        include_path_str
    );
    assert!(!vm.loading);
}

#[test]
fn load_file_runs_phase_pipeline_when_expression_parse_succeeds() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("phase_pipeline.m");
    std::fs::write(&source_path, "+").expect("failed to write source test file");
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
fn load_file_uses_syntax_fallback_when_expression_parse_fails() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("syntax_fallback.m");
    std::fs::write(&source_path, "1 +\n").expect("failed to write source test file");
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
    std::fs::write(&source_path, "1 +\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Parse(SourceParseError::SyntaxErrorsPresent { path })) if path == source_path_str
    ));
    assert_eq!(
        vm.last_load_phase_trace,
        vec!["begin", "parse", "syntax-fallback"]
    );
    assert!(!vm.loading);
}

#[test]
fn parse_source_text_rejects_uppercase_type_lhs_in_synonym() {
    let mut vm = VM::new_for_tests();

    let source_path = unique_test_path("uppercase_type_lhs_synonym.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let result = vm.parse_source_text(&source_path_str, "Maybe * == *\n", UNIX_EPOCH, false);

    let outcome = result.expect("expected parse outcome");
    assert_eq!(outcome.status, ParsePhaseStatus::SyntaxError);
    assert!(vm.parser_diagnostics.iter().any(|diagnostic| diagnostic
        .message
        .contains("upper case identifier out of context")));
}

#[test]
fn parse_source_text_rejects_repeated_type_variable_on_type_lhs() {
    let mut vm = VM::new_for_tests();

    let source_path = unique_test_path("repeated_typevar_lhs_type_def.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let result = vm.parse_source_text(&source_path_str, "pair * * == *\n", UNIX_EPOCH, false);

    let outcome = result.expect("expected parse outcome");
    assert_eq!(outcome.status, ParsePhaseStatus::SyntaxError);
    assert!(vm.parser_diagnostics.iter().any(|diagnostic| {
        diagnostic
            .message
            .contains("repeated type variable on lhs of type def")
    }));
}

#[test]
fn parse_source_text_accepts_distinct_type_variables_on_algebraic_lhs() {
    let mut vm = VM::new_for_tests();

    let source_path = unique_test_path("distinct_typevars_algebraic_lhs.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let result = vm.parse_source_text(
        &source_path_str,
        "pair * ** ::= Pair * **\n",
        UNIX_EPOCH,
        false,
    );

    let outcome = result.expect("expected parse outcome");
    assert_eq!(
        outcome.status,
        ParsePhaseStatus::Parsed,
        "diagnostics={:?}",
        vm.parser_diagnostics
    );
}

#[test]
fn parse_source_script_reads_file_and_delegates_to_text_entry() {
    let mut vm = VM::new_for_tests();
    let source_path = unique_test_path("parse_source_script_wrapper.m");
    std::fs::write(&source_path, "%free { x :: num }\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();
    let source_file = File::open(&source_path).expect("failed to open source test file");

    let outcome = vm
        .parse_source_script(&source_file, &source_path_str, UNIX_EPOCH, false)
        .expect("expected parse outcome");

    assert_eq!(outcome.status, ParsePhaseStatus::Parsed);
    assert_eq!(vm.free_identifiers.len(&vm.heap), 1);
    let current_file = outcome.files.head(&vm.heap).expect("expected current file");
    assert!(!current_file.is_shareable(&vm.heap));
}

#[test]
fn success_postlude_phase_resets_syntax_editor_state() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;
    vm.sorted = false;
    vm.error_line = 42;
    vm.error_locations = vec![Combinator::True.into()];

    let source_path = unique_test_path("success_postlude_reset.m");
    std::fs::write(&source_path, "+").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok());
    assert!(vm.sorted);
    assert_eq!(vm.error_line, 0);
    assert!(vm.error_locations.is_empty());
    assert!(vm.parser_diagnostics.is_empty());
    assert!(!vm.old_files.is_empty());
}

#[test]
fn syntax_fallback_resets_syntax_editor_state_outside_initialization() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;
    vm.error_line = 17;
    vm.error_locations = vec![Combinator::False.into()];
    vm.included_files = vm.empty_environment_for_source("stale_includee.m", UNIX_EPOCH);

    let source_path = unique_test_path("syntax_reset.m");
    std::fs::write(&source_path, "1 +\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok());
    assert_eq!(vm.error_line, 0);
    assert!(vm.error_locations.is_empty());
    assert!(vm.parser_diagnostics.is_empty());
    assert!(vm.files.is_empty());
    assert!(vm.included_files.is_empty());
    assert!(!vm.old_files.is_empty());
}

#[test]
fn parser_vm_api_interns_identifiers_and_builds_alias_metadata() {
    let mut vm = VM::new_for_tests();

    let first = vm.intern_identifier("widget");
    let second = vm.intern_identifier("widget");

    assert_eq!(first, second);
    assert_eq!(vm.identifier_name(first), "widget");

    let metadata = vm.source_name_metadata("widget");
    assert_eq!(
        vm.heap
            .resolve_string(metadata.left_value(&vm.heap))
            .unwrap(),
        "widget"
    );
    assert_eq!(RawValue::from(metadata.right_value(&vm.heap)), 0);
}

#[test]
fn parser_vm_api_supports_type_and_runtime_helpers() {
    let mut vm = VM::new_for_tests();

    let bool_id = vm.intern_identifier("bool");
    assert_eq!(vm.translate_type_identifier(bool_id), Type::Bool.into());

    let custom_type_value = IdentifierValueTypeRef::new(
        &mut vm.heap,
        IdentifierValueTypeData::Abstract { basis: NIL },
    );
    let custom_type_id_value = IdentifierValueRef::new(
        &mut vm.heap,
        IdentifierValueData::Typed {
            arity: 0,
            show_function: Value::None,
            value_type: custom_type_value,
        },
    );
    let custom_type = IdentifierRecordRef::new(
        &mut vm.heap,
        "Thing".to_string(),
        IdentifierDefinitionRef::undefined(),
        Type::Type.into(),
        Some(custom_type_id_value),
    );

    let show_fn_name = format!("show{}", vm.identifier_name(custom_type));
    let show_fn = vm.intern_identifier(show_fn_name.as_str());
    assert_eq!(vm.identifier_name(show_fn), "showThing");
    vm.attach_type_show_function(custom_type, show_fn)
        .expect("expected typed show function attachment to succeed");

    let IdentifierValueData::Typed { show_function, .. } =
        custom_type.get_value(&vm.heap).unwrap().get_data(&vm.heap)
    else {
        panic!("expected typed identifier value");
    };
    assert_eq!(show_function, show_fn.into());

    assert_eq!(vm.listdiff_function(), vm.listdiff_fn.into());
    assert_eq!(vm.numeric_one(), Value::Data(1));
    assert_eq!(
        vm.void_tuple(),
        vm.void_.get_value(&vm.heap).unwrap().into()
    );
    assert_eq!(vm.indent_function().unwrap(), vm.indent_fn.into());
    assert_eq!(vm.outdent_function().unwrap(), vm.outdent_fn.into());

    let private_name = vm.private_name(Combinator::Undef.into());
    assert_ne!(private_name, Combinator::Undef.into());
}

#[test]
fn parser_vm_api_exposes_explicit_deferred_mutation_stubs() {
    let mut vm = VM::new_for_tests();

    assert_eq!(
        vm.record_deferred_exports(NIL, NIL, NIL),
        Err(ParserSupportError::DeferredMutation {
            operation: "record_deferred_exports"
        })
    );
    assert_eq!(
        vm.record_deferred_includees(NIL),
        Err(ParserSupportError::DeferredMutation {
            operation: "record_deferred_includees"
        })
    );
    assert_eq!(
        vm.record_deferred_freeids(NIL),
        Err(ParserSupportError::DeferredMutation {
            operation: "record_deferred_freeids"
        })
    );
    assert_eq!(
        vm.declare_definition(NIL),
        Err(ParserSupportError::DeferredMutation {
            operation: "declare_definition"
        })
    );
    assert_eq!(
        vm.apply_specification(NIL, NIL, NIL),
        Err(ParserSupportError::DeferredMutation {
            operation: "apply_specification"
        })
    );
    assert_eq!(
        vm.declare_type(NIL, IdentifierValueTypeKind::Abstract, NIL, NIL),
        Err(ParserSupportError::DeferredMutation {
            operation: "declare_type"
        })
    );
    assert_eq!(
        vm.declare_constructor(NIL, NIL, NIL, NIL),
        Err(ParserSupportError::DeferredMutation {
            operation: "declare_constructor"
        })
    );
    assert_eq!(
        vm.free_value(),
        Err(ParserSupportError::DeferredMutation {
            operation: "free_value"
        })
    );
}

#[test]
fn exportfile_phase_errors_when_path_is_not_in_includees() {
    let mut vm = VM::new_for_tests();

    let path = unique_test_path("exports_only.m")
        .to_string_lossy()
        .to_string();
    let mut payload = export_payload(
        &mut vm,
        "exports_only.m",
        1,
        &["foo"],
        &[path.as_str()],
        false,
        &[],
    );
    payload.include_requests = Vec::new();

    let result = vm.validate_exportfile_bindings_partial(Some(&payload));

    assert!(matches!(
        result,
        Err(ExportValidationError::PathNotIncludedInScript { path: p }) if p == path
    ));
}

#[test]
fn exportfile_phase_errors_when_path_binding_is_ambiguous() {
    let mut vm = VM::new_for_tests();

    let path = unique_test_path("ambiguous.m")
        .to_string_lossy()
        .to_string();
    let payload = ParserTopLevelDirectivePayload {
        include_requests: vec![
            include_request_payload(&mut vm, "ambiguous.m", 1, &path),
            include_request_payload(&mut vm, "ambiguous.m", 2, &path),
        ],
        export: export_payload(
            &mut vm,
            "ambiguous.m",
            1,
            &["foo"],
            &[path.as_str()],
            false,
            &[],
        )
        .export,
    };

    let result = vm.validate_exportfile_bindings_partial(Some(&payload));

    assert!(matches!(
        result,
        Err(ExportValidationError::AmbiguousPathRequest { path: p }) if p == path
    ));
}

#[test]
fn exportfile_phase_accepts_unique_included_binding() {
    let mut vm = VM::new_for_tests();

    let path = unique_test_path("included_once.m")
        .to_string_lossy()
        .to_string();
    let payload = ParserTopLevelDirectivePayload {
        include_requests: vec![include_request_payload(
            &mut vm,
            "included_once.m",
            1,
            &path,
        )],
        export: export_payload(
            &mut vm,
            "included_once.m",
            1,
            &["foo"],
            &[path.as_str()],
            false,
            &[],
        )
        .export,
    };

    let result = vm.validate_exportfile_bindings_partial(Some(&payload));

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

    let include_a_path = unique_test_path("include_a.m");
    std::fs::write(&include_a_path, "+\n").expect("failed to write include_a test file");
    let include_a_path = include_a_path.to_string_lossy().to_string();
    let include_b_path = unique_test_path("include_b.m");
    std::fs::write(&include_b_path, "+\n").expect("failed to write include_b test file");
    let include_b_path = include_b_path.to_string_lossy().to_string();
    let payload = ParserTopLevelDirectivePayload {
        include_requests: vec![
            include_request_payload(&mut vm, "main.m", 1, &include_a_path),
            include_request_payload(&mut vm, "main.m", 2, &include_b_path),
        ],
        export: None,
    };

    let result = vm.run_mkincludes_phase(Some(&payload));

    assert!(result.is_ok());
    assert!(vm.included_files.is_empty());
    assert!(vm.include_rollback_files.is_empty());
    assert_eq!(vm.files.len(&vm.heap), 3);
}

#[test]
fn include_expansion_phase_is_noop_when_includees_empty() {
    let mut vm = VM::new_for_tests();
    vm.files = ConsList::EMPTY;
    vm.included_files = ConsList::EMPTY;
    let include_stub = FileRecord::new(
        &mut vm.heap,
        unique_test_path("ld_stuff.m").to_string_lossy().to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    let include_list = ConsList::new(&mut vm.heap, include_stub);
    vm.include_rollback_files = ConsList::new(&mut vm.heap, include_list);

    let result = vm.run_mkincludes_phase(None);

    assert!(result.is_ok());
    assert!(vm.files.is_empty());
    assert!(vm.included_files.is_empty());
    assert!(vm.include_rollback_files.is_empty());
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
        Err(TypecheckError::UndefinedNames { count: 1 })
    ));
}

#[test]
fn typecheck_phase_does_not_bind_repeated_name_leaf_in_formal() {
    let mut vm = VM::new_for_tests();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("repeated_name_leaf_formal.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let x = vm.heap.make_empty_identifier("x");
    let repeated_name_pattern = vm.heap.cons_ref(Token::Constant.into(), x.into());
    let missing = vm.heap.make_empty_identifier("missing");
    let lambda_body = vm
        .heap
        .lambda_ref(repeated_name_pattern.into(), missing.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body.into()),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::UndefinedNames { count: 1 })
    ));
}

#[test]
fn typecheck_phase_does_not_bind_wrapped_constant_leaf_in_formal() {
    let mut vm = VM::new_for_tests();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("wrapped_constant_leaf_formal.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let wrapped_constant_pattern = vm.heap.cons_ref(Token::Constant.into(), Value::Data(0));
    let missing = vm.heap.make_empty_identifier("missing");
    let lambda_body = vm
        .heap
        .lambda_ref(wrapped_constant_pattern.into(), missing.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body.into()),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::UndefinedNames { count: 1 })
    ));
}

#[test]
fn typecheck_phase_rejects_value_head_application_in_formal() {
    let mut vm = VM::new_for_tests();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("non_constructor_formal_head.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let g = vm.heap.make_empty_identifier("g");
    let x = vm.heap.make_empty_identifier("x");
    let malformed_pattern = vm.heap.apply_ref(g.into(), x.into());
    let lambda_body = vm.heap.lambda_ref(malformed_pattern.into(), x.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body.into()),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::ValueHeadApplicationsInFormals { count: 1 })
    ));
}

#[test]
fn typecheck_phase_rejects_non_identifier_application_head_in_formal() {
    let mut vm = VM::new_for_tests();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("non_identifier_formal_head.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let x = vm.heap.make_empty_identifier("x");
    let xs = vm.heap.make_empty_identifier("xs");
    let y = vm.heap.make_empty_identifier("y");
    let malformed_head = vm.heap.cons_ref(x.into(), xs.into());
    let malformed_pattern = vm.heap.apply_ref(malformed_head.into(), y.into());
    let lambda_body = vm.heap.lambda_ref(malformed_pattern.into(), y.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body.into()),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::NonIdentifierApplicationHeadsInFormals { count: 1 })
    ));
}

#[test]
fn typecheck_phase_invalid_application_formal_does_not_bind_interior_names() {
    let mut vm = VM::new_for_tests();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("invalid_application_nonbinding.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let g = vm.heap.make_empty_identifier("g");
    let x = vm.heap.make_empty_identifier("x");
    let malformed_pattern = vm.heap.apply_ref(g.into(), x.into());
    let lambda_body = vm.heap.lambda_ref(malformed_pattern.into(), x.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body.into()),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::ValueHeadApplicationsInFormals { count: 1 })
    ));
    let mut undefined_names = result.undefined_names;
    assert_eq!(undefined_names.len(&vm.heap), 1);
    let identifier = undefined_names
        .pop(&mut vm.heap)
        .expect("expected one undefined name from invalid formal interior");
    assert_eq!(vm.identifier_name(identifier), "x");
}

#[test]
fn typecheck_phase_successor_pattern_binds_inner_name() {
    let mut vm = VM::new_for_tests();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("successor_pattern_binding.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let x = vm.heap.make_empty_identifier("x");
    let one = IntegerRef::from_i64(&mut vm.heap, 1);
    let successor_pattern = vm
        .heap
        .apply2(Combinator::Plus.into(), one.into(), x.into());
    let lambda_body = vm.heap.lambda_ref(successor_pattern.into(), x.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body.into()),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(result.failure.is_none());
    assert!(result.undefined_names.is_empty());
}

#[test]
fn typecheck_phase_rejects_undeclared_constructor_inside_successor_pattern() {
    let mut vm = VM::new_for_tests();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("successor_pattern_undeclared_constructor.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let nope = vm.heap.make_empty_identifier("Nope");
    let x = vm.heap.make_empty_identifier("x");
    let one = IntegerRef::from_i64(&mut vm.heap, 1);
    let inner_pattern = vm.heap.apply_ref(nope.into(), x.into());
    let successor_pattern =
        vm.heap
            .apply2(Combinator::Plus.into(), one.into(), inner_pattern.into());
    let lambda_body = vm.heap.lambda_ref(successor_pattern.into(), x.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body.into()),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::UndeclaredConstructorsInFormals { count: 1 })
    ));
}

#[test]
fn typecheck_phase_rejects_unsupported_arithmetic_pattern_in_formal() {
    let mut vm = VM::new_for_tests();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("unsupported_arithmetic_formal.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let x = vm.heap.make_empty_identifier("x");
    let one = IntegerRef::from_i64(&mut vm.heap, 1);
    let arithmetic_pattern = vm
        .heap
        .apply2(Combinator::Plus.into(), x.into(), one.into());
    let lambda_body = vm.heap.lambda_ref(arithmetic_pattern.into(), x.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body.into()),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::UnsupportedArithmeticPatternsInFormals { count: 1 })
    ));
}

#[test]
fn typecheck_phase_unsupported_arithmetic_pattern_does_not_bind_interior_names() {
    let mut vm = VM::new_for_tests();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("unsupported_arithmetic_nonbinding.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let x = vm.heap.make_empty_identifier("x");
    let one = IntegerRef::from_i64(&mut vm.heap, 1);
    let arithmetic_pattern = vm
        .heap
        .apply2(Combinator::Plus.into(), x.into(), one.into());
    let lambda_body = vm.heap.lambda_ref(arithmetic_pattern.into(), x.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body.into()),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::UnsupportedArithmeticPatternsInFormals { count: 1 })
    ));
    let mut undefined_names = result.undefined_names;
    assert_eq!(undefined_names.len(&vm.heap), 1);
    let identifier = undefined_names
        .pop(&mut vm.heap)
        .expect("expected one undefined name from unsupported arithmetic formal interior");
    assert_eq!(vm.identifier_name(identifier), "x");
}

#[test]
fn export_closure_phase_is_noop_without_exports() {
    let mut vm = VM::new_for_tests();

    let result = vm.run_export_closure_phase_partial(
        Some(&ParserTopLevelDirectivePayload::default()),
        ConsList::EMPTY,
    );

    assert!(result.is_ok());
}

#[test]
fn export_closure_phase_clears_exports_when_undefined_names_present() {
    let mut vm = VM::new_for_tests();
    let missing_id = vm.heap.make_empty_identifier("missing_name");
    vm.undefined_names = ConsList::new(&mut vm.heap, missing_id);
    let payload = export_payload(
        &mut vm,
        "exported_name.m",
        1,
        &["exported_name"],
        &[],
        false,
        &[],
    );

    let result = vm.run_export_closure_phase_partial(Some(&payload), ConsList::EMPTY);

    assert!(matches!(
        result,
        Err(ExportValidationError::BlockedByUndefinedNames)
    ));
    assert_eq!(vm.exported_identifiers, NIL);
}

#[test]
fn export_closure_phase_keeps_exports_when_no_undefined_names() {
    let mut vm = VM::new_for_tests();
    vm.undefined_names = ConsList::EMPTY;
    let payload = export_payload(
        &mut vm,
        "exported_name.m",
        1,
        &["exported_name"],
        &[],
        false,
        &[],
    );

    let result = vm.run_export_closure_phase_partial(Some(&payload), ConsList::EMPTY);

    assert!(result.is_ok());
    let committed = result.expect("expected committed export payload");
    assert_ne!(committed.exported_identifiers, NIL);
}

#[test]
fn bereaved_warning_phase_is_noop_without_risk_flag() {
    let mut vm = VM::new_for_tests();
    vm.exported_identifiers = NIL;
    vm.unused_types = false;

    vm.emit_bereaved_warnings_partial();
    assert!(!vm.unused_types);
}

#[test]
fn bereaved_warning_phase_returns_ok_when_risk_flagged() {
    let mut vm = VM::new_for_tests();
    let export_id = vm.heap.make_empty_identifier("exported_name");
    vm.exported_identifiers = vm.heap.cons_ref(export_id.into(), NIL);
    vm.unused_types = true;

    vm.emit_bereaved_warnings_partial();
    assert!(vm.unused_types);
    assert_ne!(vm.exported_identifiers, NIL);
}

#[test]
fn unused_diagnostics_phase_clears_deferred_marker() {
    let mut vm = VM::new_for_tests();
    vm.empty_production_nonterminals = Combinator::True.into();

    vm.emit_unused_definition_diagnostics_partial();
    assert_eq!(vm.empty_production_nonterminals, NIL);
}

#[test]
fn unused_diagnostics_phase_is_stable_when_marker_absent() {
    let mut vm = VM::new_for_tests();
    vm.empty_production_nonterminals = NIL;

    vm.emit_unused_definition_diagnostics_partial();
    assert_eq!(vm.empty_production_nonterminals, NIL);
}

#[test]
fn codegen_phase_fails_without_loaded_files() {
    let mut vm = VM::new_for_tests();
    vm.files = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(matches!(result, Err(CodegenError::NoLoadedFiles)));
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
        Err(CodegenError::InitializationBlockedByUnresolvedNames)
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
fn dump_visibility_phase_writes_dump_file_for_normal_m_source() {
    let mut vm = VM::new_for_tests();
    vm.initializing = false;

    let source_path = unique_test_path("dump_write_success.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    vm.files = vm.empty_environment_for_source(&source_path_str, UNIX_EPOCH);

    let result = vm.run_dump_visibility_phase(&source_path_str);

    assert!(result.is_ok());

    let dump_path = source_path.with_extension("x");
    let dump_bytes = std::fs::read(dump_path).expect("expected dump file to be written");
    assert!(dump_bytes.len() > 2);
    assert_eq!(dump_bytes[0], WORD_SIZE as u8);
    assert_eq!(dump_bytes[1], XVERSION as u8);
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
fn syntax_dump_writes_dump_file_for_m_scripts() {
    let mut vm = VM::new_for_tests();
    vm.old_files = ConsList::EMPTY;
    vm.error_line = 17;

    let source_path = unique_test_path("syntax_dump_write.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let result = vm.maybe_write_syntax_dump(&source_path_str);

    assert!(result.is_ok());

    let dump_path = source_path.with_extension("x");
    let dump_bytes = std::fs::read(dump_path).expect("expected syntax dump file to be written");
    assert!(dump_bytes.len() > 2);
    assert_eq!(dump_bytes[0], WORD_SIZE as u8);
    assert_eq!(dump_bytes[1], XVERSION as u8);
    assert_eq!(dump_bytes[2], 0);
}

#[test]
fn syntax_dump_is_noop_for_non_m_sources() {
    let mut vm = VM::new_for_tests();
    vm.old_files = ConsList::EMPTY;
    vm.last_load_phase_trace.clear();

    let source_path = unique_test_path("syntax_dump_anchor.x");
    let source_path_str = source_path.to_string_lossy().to_string();
    let result = vm.maybe_write_syntax_dump(&source_path_str);

    assert!(result.is_ok());
    assert!(vm.old_files.is_empty());
    assert!(vm.last_load_phase_trace.is_empty());
    assert!(!source_path.with_extension("x").exists());
}

#[test]
fn load_script_consumes_dump_shape_written_by_dump_visibility_phase() {
    let mut writer_vm = VM::new_for_tests();
    writer_vm.initializing = false;

    let source_path = unique_test_path("dump_decode_roundtrip.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    writer_vm.files = writer_vm.empty_environment_for_source(&source_path_str, UNIX_EPOCH);

    let write_result = writer_vm.run_dump_visibility_phase(&source_path_str);
    assert!(write_result.is_ok());

    let mut reader_vm = VM::new_for_tests();
    reader_vm.initializing = false;
    let dump_path = source_path.with_extension("x");
    let dump_file = File::open(dump_path).expect("expected generated dump file");
    let load_result =
        reader_vm.load_script(&dump_file, source_path_str, ConsList::EMPTY, NIL, true);

    let decode_compatible = match load_result {
        Ok(files) => !files.is_empty(),
        Err(LoadScriptError::Decode(BytecodeDecodeError::WrongSourceFile)) => true,
        _ => false,
    };
    assert!(decode_compatible, "{:?}", load_result);
}

#[test]
fn load_defs_decodes_short_integer_through_integerref_boundary() {
    let mut vm = VM::new_for_tests();
    let bytes = vec![Bytecode::Short.code(), 0x80, Bytecode::Definition.code()];
    let value = vm
        .load_defs(&mut bytes.into_iter())
        .expect("expected short integer payload to decode");
    let integer = IntegerRef::from_ref(value.into());

    assert_eq!(integer.to_i64_lossy(&vm.heap), -128);
}

#[test]
fn load_defs_decodes_int_x_multi_cell_chain_through_integerref_boundary() {
    let mut vm = VM::new_for_tests();
    let mut bytes = vec![Bytecode::Integer.code()];
    for word in [1isize, 2, 3, -1] {
        bytes.extend_from_slice(&word.to_le_bytes());
    }
    bytes.push(Bytecode::Definition.code());

    let value = vm
        .load_defs(&mut bytes.into_iter())
        .expect("expected INT_X payload to decode");
    let integer = IntegerRef::from_ref(value.into());

    assert_eq!(
        integer.encode_for_dump_bytecode(&vm.heap),
        vec![1, 2, 3, -1]
    );
}

#[test]
fn load_defs_preserves_int_x_first_word_negative_one() {
    let mut vm = VM::new_for_tests();
    let mut bytes = vec![Bytecode::Integer.code()];
    for word in [-1isize, -1] {
        bytes.extend_from_slice(&word.to_le_bytes());
    }
    bytes.push(Bytecode::Definition.code());

    let value = vm
        .load_defs(&mut bytes.into_iter())
        .expect("expected INT_X payload with first-word -1 to decode");
    let integer = IntegerRef::from_ref(value.into());

    assert_eq!(integer.encode_for_dump_bytecode(&vm.heap), vec![-1, -1]);
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
fn unfix_exports_clears_export_processing_state() {
    let mut vm = VM::new_for_tests();

    vm.export_list_is_active = true;
    let internal = vm.heap.make_empty_identifier("internal_name");
    vm.internals = ConsList::new(&mut vm.heap, internal);

    vm.unfix_exports();

    assert!(!vm.export_list_is_active);
    assert!(vm.internals.is_empty());
}

#[test]
fn unfix_exports_preserves_internals_in_exports_mode() {
    let mut vm = VM::new_for_tests();

    vm.export_list_is_active = true;
    vm.options.make_exports.push("script.m".to_string());
    let internal = vm.heap.make_empty_identifier("internal_name");
    vm.internals = ConsList::new(&mut vm.heap, internal);

    vm.unfix_exports();

    assert!(!vm.export_list_is_active);
    assert!(!vm.internals.is_empty());
}

#[test]
fn fixexports_internalizes_nested_free_id_bindings() {
    let mut vm = VM::new_for_tests();

    let source_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("fixexports_freeids_shape.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, source_file);

    let free_id = vm.heap.make_empty_identifier("free_param");
    let free_name = free_id.get_name(&vm.heap);
    let original_name_ref = vm.heap.string(free_name);
    let original_name = DataPair::new(&mut vm.heap, original_name_ref.into(), 0.into());
    let free_binding = FreeFormalBindingRef::new(
        &mut vm.heap,
        free_id,
        original_name,
        TypeExprRef::new(Type::Undefined.into()),
    );
    vm.free_identifiers = ConsList::new(&mut vm.heap, free_binding);

    vm.exported_identifiers = NIL;
    vm.export_paths = NIL;
    vm.export_embargoes = NIL;

    vm.fix_exports();

    assert_eq!(vm.heap[free_id.get_ref()].tag, Tag::StrCons);
    assert!(!vm.internals.is_empty());
}

#[test]
fn fixexports_internalize_undef_writes_fallback_application_payload() {
    let mut vm = VM::new_for_tests();

    vm.export_paths = Combinator::True.into();

    let source_path = unique_test_path("fixexports_undef_payload.m")
        .to_string_lossy()
        .to_string();
    let definition = IdentifierDefinitionRef::new(
        &mut vm.heap,
        HereInfo {
            script_file: source_path.clone(),
            line_number: 7,
        },
        None,
    );
    let undef_id = IdentifierRecordRef::new(
        &mut vm.heap,
        "missing_name".to_string(),
        definition,
        Type::Undefined.into(),
        Some(IdentifierValueRef::from_ref(Combinator::Undef.into())),
    );

    let defs = ConsList::new(&mut vm.heap, undef_id);
    let file_record = FileRecord::new(&mut vm.heap, source_path, UNIX_EPOCH, false, defs);
    vm.files = ConsList::new(&mut vm.heap, file_record);

    vm.fix_exports();

    assert_eq!(vm.heap[undef_id.get_ref()].tag, Tag::StrCons);
    let fallback_ref = vm.heap[undef_id.get_ref()].tail;
    assert_eq!(vm.heap[fallback_ref].tag, Tag::Ap);
    let fallback_head_ref = vm.heap[fallback_ref].head;
    assert_eq!(vm.heap[fallback_head_ref].tag, Tag::DataPair);
}

#[test]
fn unload_unsets_identifiers_reached_through_free_formal_bindings() {
    let mut vm = VM::new_for_tests();

    let free_id = vm.heap.make_empty_identifier("free_param");
    free_id.set_type_expr(&mut vm.heap, TypeExprRef::new(Type::Type.into()));
    let free_name = free_id.get_name(&vm.heap);
    let original_name_ref = vm.heap.string(free_name);
    let original_name = DataPair::new(&mut vm.heap, original_name_ref.into(), 0.into());
    let free_binding = FreeFormalBindingRef::new(
        &mut vm.heap,
        free_id,
        original_name,
        TypeExprRef::new(Type::Type.into()),
    );
    vm.free_identifiers = ConsList::new(&mut vm.heap, free_binding);

    vm.unload();

    assert!(vm.free_identifiers.is_empty());
    assert_eq!(
        RawValue::from(free_id.get_datatype(&vm.heap)),
        Type::Undefined.into()
    );
    assert_eq!(
        free_id
            .get_value(&vm.heap)
            .expect("expected unset identifier value field")
            .get_ref(),
        Combinator::Undef.into()
    );
}

#[test]
fn fixexports_internalize_type_name_wraps_definition_with_alias_metadata() {
    let mut vm = VM::new_for_tests();

    vm.export_paths = Combinator::True.into();

    let source_path = unique_test_path("fixexports_type_metadata.m")
        .to_string_lossy()
        .to_string();
    let definition = IdentifierDefinitionRef::new(
        &mut vm.heap,
        HereInfo {
            script_file: source_path.clone(),
            line_number: 11,
        },
        None,
    );
    let type_name = IdentifierRecordRef::new(
        &mut vm.heap,
        "TypeName".to_string(),
        definition,
        Type::Type.into(),
        Some(IdentifierValueRef::from_ref(Combinator::Undef.into())),
    );

    let defs = ConsList::new(&mut vm.heap, type_name);
    let file_record = FileRecord::new(&mut vm.heap, source_path, UNIX_EPOCH, false, defs);
    vm.files = ConsList::new(&mut vm.heap, file_record);

    vm.fix_exports();

    let internal_id = vm
        .internals
        .head(&vm.heap)
        .expect("expected internalized type-name entry");
    let wrapped_definition = internal_id.get_definition(&vm.heap);
    assert!(wrapped_definition.alias_metadata_pair(&vm.heap).is_some());
}

#[test]
fn hdsort_orders_free_binding_pairs_by_identifier_name() {
    let mut vm = VM::new_for_tests();

    let gamma = vm.heap.make_empty_identifier("gamma");
    let alpha = vm.heap.make_empty_identifier("alpha");
    let beta = vm.heap.make_empty_identifier("beta");

    let gamma_payload: Value = IntegerRef::from_i64(&mut vm.heap, 1).into();
    let alpha_payload: Value = IntegerRef::from_i64(&mut vm.heap, 2).into();
    let beta_payload: Value = IntegerRef::from_i64(&mut vm.heap, 3).into();
    let gamma_pair: RawValue = vm.heap.cons_ref(gamma.into(), gamma_payload).into();
    let alpha_pair: RawValue = vm.heap.cons_ref(alpha.into(), alpha_payload).into();
    let beta_pair: RawValue = vm.heap.cons_ref(beta.into(), beta_payload).into();

    let mut unsorted: ConsList = ConsList::EMPTY;
    unsorted.append(&mut vm.heap, gamma_pair);
    unsorted.append(&mut vm.heap, alpha_pair);
    unsorted.append(&mut vm.heap, beta_pair);

    let sorted_ref = super::bytecode::hdsort_binding_list_ref(&mut vm.heap, unsorted.get_ref());
    let mut sorted: ConsList<Value> = ConsList::from_ref(sorted_ref);

    let mut names: Vec<String> = vec![];
    while let Some(binding_pair_ref) = sorted.pop_value(&vm.heap) {
        let binding_pair_ref: RawValue = binding_pair_ref.into();
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

    let x_formal_binding = FreeFormalBindingRef::new(
        &mut vm.heap,
        x,
        DataPair::from_ref(x_original.get_ref()),
        TypeExprRef::new(Type::Undefined.into()),
    );
    let y_formal_binding = FreeFormalBindingRef::new(
        &mut vm.heap,
        y,
        DataPair::from_ref(y_original.get_ref()),
        TypeExprRef::new(Type::Undefined.into()),
    );

    let mut formal_list: ConsList<FreeFormalBindingRef> = ConsList::EMPTY;
    formal_list.push(&mut vm.heap, y_formal_binding);
    formal_list.push(&mut vm.heap, x_formal_binding);

    let x_payload: Value = IntegerRef::from_i64(&mut vm.heap, 42).into();
    let z_payload: Value = IntegerRef::from_i64(&mut vm.heap, 7).into();
    let x_actual_binding: RawValue = vm.heap.cons_ref(x.into(), x_payload).into();
    let z_actual_binding: RawValue = vm.heap.cons_ref(z.into(), z_payload).into();

    let mut actual_list: ConsList = ConsList::EMPTY;
    actual_list.append(&mut vm.heap, x_actual_binding);
    actual_list.append(&mut vm.heap, z_actual_binding);

    vm.bindparams(formal_list.get_ref().into(), actual_list.get_ref().into());

    assert_eq!(vm.heap[x.get_ref()].tail, RawValue::from(x_payload));

    assert_eq!(vm.missing_parameter_bindings.len(&vm.heap), 1);
    assert_eq!(
        vm.missing_parameter_bindings.value_head(&vm.heap),
        Some(y_original.get_ref().into())
    );

    assert_eq!(vm.detritus_parameter_bindings.len(&vm.heap), 1);
    assert_eq!(
        vm.detritus_parameter_bindings.value_head(&vm.heap),
        Some(z.get_ref().into())
    );

    assert_eq!(vm.free_binding_sets.len(&vm.heap), 1);
    assert_eq!(
        vm.free_binding_sets.value_head(&vm.heap),
        Some(formal_list.get_ref().into())
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

    let formal_binding = FreeFormalBindingRef::new(
        &mut vm.heap,
        t,
        DataPair::from_ref(t_original.get_ref()),
        TypeExprRef::new(Type::Type.into()),
    );
    let formal_list: ConsList<FreeFormalBindingRef> = ConsList::new(&mut vm.heap, formal_binding);

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
    let detritus_entry: RawValue = vm
        .detritus_parameter_bindings
        .value_head(&vm.heap)
        .expect("expected one detritus entry")
        .into();
    assert_eq!(vm.heap[detritus_entry].tag, Tag::Cons);
    assert_eq!(vm.heap[detritus_entry].head, t.get_ref());

    let arity_pair_ref = vm.heap[detritus_entry].tail;
    assert_eq!(vm.heap[arity_pair_ref].tag, Tag::DataPair);
    assert_eq!(vm.heap[arity_pair_ref].head, 2);
    assert_eq!(vm.heap[arity_pair_ref].tail, 3);

    assert_eq!(vm.heap[t.get_ref()].tail, vm.heap[actual_binding_ref].tail);
}
