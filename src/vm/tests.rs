use super::*;
use crate::big_num::IntegerRef;
use crate::compiler::{
    HereInfo, ParserExportDirectivePayload, ParserIncludeDirectivePayload,
    ParserPatternDefinitionPayload, ParserSupportError, ParserTopLevelDirectivePayload,
    ParserTopLevelScriptPayload, Token,
};
use crate::data::api::{
    ApNodeRef, ConstructorRef, DataPair, DefinitionRef, FreeFormalBindingRef, HeapObjectProxy,
    IdentifierDefinitionRef, IdentifierValueRef, IdentifierValueTypeData,
    IdentifierValueTypeKind, IdentifierValueTypeRef, TypeExprRef,
};
use crate::data::ATOM_LIMIT;
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

fn test_anchor(vm: &mut VM, source_path: &str, line_number: isize) -> FileInfoRef {
    FileInfoRef::from_script_file(&mut vm.heap, source_path.to_string(), line_number)
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
        modifiers: Vec::new(),
        bindings: Vec::new(),
    }
}

fn collect_subscript_indices(heap: &Heap, value: Value, indices: &mut Vec<RawValue>) {
    let raw: RawValue = value.into();
    if raw < ATOM_LIMIT {
        return;
    }

    match heap[raw].tag {
        Tag::Ap => {
            let application = ApNodeRef::from_ref(raw);
            if let Some(function_application) = application.function_application(heap) {
                if function_application.function_raw(heap) == Combinator::Subscript as RawValue {
                    indices.push(function_application.argument_raw(heap));
                }
            }
            collect_subscript_indices(heap, application.function_raw(heap).into(), indices);
            collect_subscript_indices(heap, application.argument_raw(heap).into(), indices);
        }
        Tag::Cons | Tag::Pair | Tag::TCons | Tag::Let | Tag::LetRec | Tag::Tries | Tag::Label | Tag::Show => {
            collect_subscript_indices(heap, heap[raw].head.into(), indices);
            collect_subscript_indices(heap, heap[raw].tail.into(), indices);
        }
        Tag::Share => {
            collect_subscript_indices(heap, heap[raw].head.into(), indices);
        }
        _ => {}
    }
}

fn application_spine(heap: &Heap, value: Value) -> (Value, Vec<Value>) {
    let mut arguments = Vec::new();
    let mut current = value;

    loop {
        let raw: RawValue = current.into();
        if raw < ATOM_LIMIT || heap[raw].tag != Tag::Ap {
            arguments.reverse();
            return (current, arguments);
        }

        let application = ApNodeRef::from_ref(raw);
        arguments.push(application.argument_raw(heap).into());
        current = application.function_raw(heap).into();
    }
}

fn subtree_contains_combinator(heap: &Heap, value: Value, combinator: Combinator) -> bool {
    let raw: RawValue = value.into();
    if raw < ATOM_LIMIT {
        return value == combinator.into();
    }

    match heap[raw].tag {
        Tag::Ap | Tag::Cons | Tag::Pair | Tag::TCons | Tag::Let | Tag::LetRec | Tag::Tries | Tag::Label | Tag::Show => {
            heap[raw].head == combinator as RawValue
                || subtree_contains_combinator(heap, heap[raw].head.into(), combinator)
                || subtree_contains_combinator(heap, heap[raw].tail.into(), combinator)
        }
        Tag::Share => subtree_contains_combinator(heap, heap[raw].head.into(), combinator),
        _ => false,
    }
}

/// Returns whether one lowered subtree still contains the selected committed identifier reference.
/// This exists so codegen tests can assert that dependency pruning removed dead local payloads and
/// retained transitively needed ones without duplicating one heap-tree walk per assertion. The
/// invariant is that the helper only reports identifiers still reachable in the lowered tree.
fn subtree_contains_identifier(heap: &Heap, value: Value, identifier: IdentifierRecordRef) -> bool {
    let raw: RawValue = value.into();
    if raw < ATOM_LIMIT {
        return raw == identifier.get_ref();
    }

    match heap[raw].tag {
        Tag::Id => raw == identifier.get_ref(),
        Tag::Ap | Tag::Cons | Tag::Pair | Tag::TCons | Tag::Let | Tag::LetRec | Tag::Tries | Tag::Label | Tag::Show => {
            subtree_contains_identifier(heap, heap[raw].head.into(), identifier)
                || subtree_contains_identifier(heap, heap[raw].tail.into(), identifier)
        }
        Tag::Share => subtree_contains_identifier(heap, heap[raw].head.into(), identifier),
        _ => false,
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();

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
        vm.classify_load_script_form("slice.m", "* $pair ** == (*, [num])")
            .expect("infix typeform synonym should classify"),
        LoadScriptForm::TopLevelScript
    );
    assert_eq!(
        vm.classify_load_script_form("slice.m", "* $pair ** ::= Pair * **")
            .expect("infix typeform algebraic declaration should classify"),
        LoadScriptForm::TopLevelScript
    );
    assert_eq!(
        vm.classify_load_script_form("slice.m", "%free { x :: num }")
            .expect("%free should classify"),
        LoadScriptForm::TopLevelScript
    );
    assert_eq!(
        vm.classify_load_script_form("slice.m", "abstype thing with showthing :: num")
            .expect("abstype should classify"),
        LoadScriptForm::TopLevelScript
    );
}

#[test]
fn parse_source_text_treats_bare_tuple_pattern_definition_as_script_input() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("bare_tuple_pattern_script_input.m");
    let source_path_str = source_path.to_string_lossy().to_string();

    let outcome = vm
        .parse_source_text(&source_path_str, "(x,y) = pair\n", UNIX_EPOCH, false)
        .expect("script parse should return diagnostics, not io failure");

    assert!(outcome.parsed_without_error);
    let payload = outcome
        .top_level_payload
        .as_ref()
        .expect("script parse should return top-level payload");
    assert_eq!(payload.pattern_definitions.len(), 1);
    assert!(vm.parser_diagnostics.is_empty());
}

#[test]
fn parse_source_text_treats_plain_expression_like_input_as_script_mode_request() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("expression_like_script_input.m");
    let source_path_str = source_path.to_string_lossy().to_string();

    let outcome = vm
        .parse_source_text(&source_path_str, "+", UNIX_EPOCH, false)
        .expect("script parse should return diagnostics, not io failure");

    assert!(!outcome.parsed_without_error);
    assert!(!vm.parser_diagnostics.is_empty());
}

#[test]
fn load_file_keeps_cons_pattern_tail_name_bound_after_constructor_formal_checks() {
    let mut vm = VM::new();
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
    assert_ne!(tail_value.get_ref(), RawValue::from(Combinator::Undef));
}

#[test]
fn load_file_accepts_negative_integer_literal_pattern_in_formal() {
    let mut vm = VM::new();
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
fn load_file_accepts_unparenthesized_negative_integer_literal_pattern_in_formal() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("unparenthesized_negative_integer_literal_formal.m");
    std::fs::write(&source_path, "sign -1 = 0\n").expect("failed to write source test file");
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
    let mut vm = VM::new();
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
fn load_file_accepts_unparenthesized_n_plus_k_pattern_and_binds_inner_name() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("unparenthesized_n_plus_k_formal.m");
    std::fs::write(&source_path, "pred x+1 = x\n").expect("failed to write source test file");
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
fn load_file_rejects_cons_inner_canonical_successor_pattern() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("invalid_successor_cons_formal.m");
    std::fs::write(&source_path, "bad (((x:xs)+1)) = xs\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::InvalidSuccessorPatternsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_reports_invalid_successor_pattern_before_undefined_name() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("invalid_successor_precedes_undefined_name.m");
    std::fs::write(&source_path, "bad (((x:xs)+1)) = missing\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::InvalidSuccessorPatternsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_rejects_undeclared_constructor_atom_in_formal() {
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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

    let first = vm
        .heap
        .get_identifier("first")
        .expect("expected first identifier to exist");
    let lowered = first
        .get_value(&vm.heap)
        .expect("expected first definition value to exist")
        .get_ref();
    assert_ne!(lowered, RawValue::from(Combinator::Undef));
    assert_ne!(vm.heap[lowered].tag, Tag::Lambda);
}

#[test]
fn load_file_accepts_unparenthesized_declared_infix_constructor_application_in_formal() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("unparenthesized_infix_constructor_application_formal.m");
    std::fs::write(
        &source_path,
        "pair * ** ::= * $Pair **\nfirst x $Pair y = x\n",
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

    let first = vm
        .heap
        .get_identifier("first")
        .expect("expected first identifier to exist");
    let lowered = first
        .get_value(&vm.heap)
        .expect("expected first definition value to exist")
        .get_ref();
    assert_ne!(lowered, RawValue::from(Combinator::Undef));
    assert_ne!(vm.heap[lowered].tag, Tag::Lambda);
}

#[test]
fn load_file_accepts_ordinary_multi_argument_function_form() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("ordinary_multi_argument_function_form.m");
    std::fs::write(&source_path, "id x y = x\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(
        result.is_ok(),
        "result={result:?} diagnostics={:?}",
        vm.parser_diagnostics
    );
    assert!(vm.undefined_names.is_empty());

    let id = vm
        .heap
        .get_identifier("id")
        .expect("expected id identifier to exist");
    let id_value = id
        .get_value(&vm.heap)
        .expect("expected id definition value to exist");
    let lowered = id_value.get_ref();
    assert_ne!(lowered, RawValue::from(Combinator::Undef));
    if lowered >= ATOM_LIMIT {
        assert_ne!(vm.heap[lowered].tag, Tag::Lambda);
    }
}

#[test]
fn load_file_rejects_infix_name_application_in_formal() {
    let mut vm = VM::new();
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
fn load_file_rejects_unparenthesized_infix_name_application_in_formal() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("unparenthesized_infix_name_application_formal.m");
    std::fs::write(&source_path, "bad x $merge y = x\n")
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
fn load_file_reports_unparenthesized_infix_name_formal_before_generic_undefined_name() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("unparenthesized_infix_name_formal_precedes_undefined_name.m");
    std::fs::write(&source_path, "bad x $merge y = missing\n")
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
fn load_file_rejects_unparenthesized_undeclared_infix_constructor_application_in_formal() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("unparenthesized_undeclared_infix_constructor_application_formal.m");
    std::fs::write(&source_path, "first x $Nope y = x\n")
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
fn load_file_reports_unparenthesized_undeclared_infix_constructor_before_generic_undefined_name() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("unparenthesized_undeclared_infix_constructor_precedes_undefined_name.m");
    std::fs::write(&source_path, "first x $Nope y = missing\n")
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
fn load_file_rejects_unparenthesized_infix_name_form_with_value_head_left_operand() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("unparenthesized_infix_name_value_head_left_operand.m");
    std::fs::write(&source_path, "bad g x $merge y = x\n")
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
fn load_file_reports_unparenthesized_infix_name_value_head_before_generic_undefined_name() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("unparenthesized_infix_name_value_head_precedes_undefined_name.m");
    std::fs::write(&source_path, "bad g x $merge y = missing\n")
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
fn load_file_rejects_unparenthesized_infix_name_form_with_multi_argument_value_head_left_operand() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("unparenthesized_infix_name_multi_argument_value_head_left_operand.m");
    std::fs::write(&source_path, "bad g h x $merge y = x\n")
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
fn load_file_reports_unparenthesized_infix_name_multi_argument_value_head_before_generic_undefined_name() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("unparenthesized_infix_name_multi_argument_value_head_precedes_undefined_name.m");
    std::fs::write(&source_path, "bad g h x $merge y = missing\n")
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
fn load_file_rejects_unparenthesized_infix_name_form_with_repeated_name_headed_left_operand() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("unparenthesized_infix_name_repeated_head_left_operand.m");
    std::fs::write(&source_path, "bad x (x y) $merge z = z\n")
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
fn load_file_reports_unparenthesized_infix_name_repeated_head_before_generic_undefined_name() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("unparenthesized_infix_name_repeated_head_precedes_undefined_name.m");
    std::fs::write(&source_path, "bad x (x y) $merge z = missing\n")
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
fn load_file_rejects_unparenthesized_infix_constructor_form_with_non_identifier_left_operand() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("unparenthesized_infix_constructor_non_identifier_left_operand.m");
    std::fs::write(
        &source_path,
        "pair * ** ::= * $Pair **\nfirst (x:xs) y $Pair z = z\n",
    )
    .expect("failed to write source test file");
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
fn load_file_reports_unparenthesized_infix_constructor_non_identifier_head_before_generic_undefined_name() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("unparenthesized_infix_constructor_non_identifier_head_precedes_undefined_name.m");
    std::fs::write(
        &source_path,
        "pair * ** ::= * $Pair **\nfirst (x:xs) y $Pair z = missing\n",
    )
    .expect("failed to write source test file");
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
fn load_file_reports_infix_name_formal_before_generic_undefined_name() {
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
fn load_file_rejects_symbolic_infix_pattern_in_formal() {
    for (file_name, source_text) in [
        ("symbolic_infix_times_formal.m", "bad (x*y) = y\n"),
        ("symbolic_infix_equal_formal.m", "bad (a=b) = b\n"),
        ("symbolic_infix_append_formal.m", "bad (u++v) = v\n"),
    ] {
        let mut vm = VM::new();
        vm.initializing = false;

        let source_path = unique_test_path(file_name);
        std::fs::write(&source_path, source_text).expect("failed to write source test file");
        let source_path_str = source_path.to_string_lossy().to_string();

        let result = vm.load_file(&source_path_str);

        assert!(matches!(
            result,
            Err(LoadFileError::Typecheck(
                TypecheckError::NonIdentifierApplicationHeadsInFormals { count: 1 }
            ))
        ));
    }
}

#[test]
fn load_file_rejects_unparenthesized_symbolic_infix_pattern_in_formal() {
    for (file_name, source_text) in [
        ("unparenthesized_symbolic_infix_times_formal.m", "bad x*y = y\n"),
        ("unparenthesized_symbolic_infix_append_formal.m", "bad u++v = v\n"),
        ("unparenthesized_symbolic_infix_power_formal.m", "bad p^q = q\n"),
        ("unparenthesized_symbolic_infix_divide_float_formal.m", "bad a/b = b\n"),
        ("unparenthesized_symbolic_infix_divide_int_formal.m", "bad c div d = d\n"),
        ("unparenthesized_symbolic_infix_remainder_formal.m", "bad e mod f = f\n"),
        ("unparenthesized_symbolic_infix_and_formal.m", "bad g&h = h\n"),
        ("unparenthesized_symbolic_infix_or_formal.m", "bad i\\/j = j\n"),
        ("unparenthesized_symbolic_infix_not_equal_formal.m", "bad k~=l = l\n"),
        ("unparenthesized_symbolic_infix_greater_formal.m", "bad m>n = n\n"),
        ("unparenthesized_symbolic_infix_greater_equal_formal.m", "bad o>=p = p\n"),
        ("unparenthesized_symbolic_infix_less_formal.m", "bad q<r = r\n"),
        ("unparenthesized_symbolic_infix_less_equal_formal.m", "bad s<=t = t\n"),
        ("unparenthesized_symbolic_infix_compose_formal.m", "bad u.v = v\n"),
        ("unparenthesized_symbolic_infix_subscript_formal.m", "bad w!x = x\n"),
    ] {
        let mut vm = VM::new();
        vm.initializing = false;

        let source_path = unique_test_path(file_name);
        std::fs::write(&source_path, source_text).expect("failed to write source test file");
        let source_path_str = source_path.to_string_lossy().to_string();

        let result = vm.load_file(&source_path_str);

        assert!(matches!(
            result,
            Err(LoadFileError::Typecheck(
                TypecheckError::NonIdentifierApplicationHeadsInFormals { count: 1 }
            ))
        ));
    }
}

#[test]
fn load_file_rejects_unparenthesized_symbolic_infix_left_operand_application_in_formal() {
    for (file_name, source_text) in [
        (
            "unparenthesized_symbolic_infix_left_operand_times_formal.m",
            "bad g x*y = x\n",
        ),
        (
            "unparenthesized_symbolic_infix_left_operand_divide_formal.m",
            "bad h x/y = x\n",
        ),
        (
            "unparenthesized_symbolic_infix_left_operand_and_formal.m",
            "bad k p&q = p\n",
        ),
        (
            "unparenthesized_symbolic_infix_left_operand_compose_formal.m",
            "bad m a.b = a\n",
        ),
        (
            "unparenthesized_symbolic_infix_left_operand_subscript_formal.m",
            "bad n i!j = i\n",
        ),
    ] {
        let mut vm = VM::new();
        vm.initializing = false;

        let source_path = unique_test_path(file_name);
        std::fs::write(&source_path, source_text).expect("failed to write source test file");

        let result = vm.load_file(&source_path.to_string_lossy());

        assert!(
            matches!(
                result,
                Err(LoadFileError::Typecheck(
                    TypecheckError::ValueHeadApplicationsInFormals { count: 1 }
                ))
            ),
            "result={result:?} diagnostics={:?}",
            vm.parser_diagnostics
        );
    }
}

#[test]
fn load_file_reports_nested_constructor_inside_symbolic_infix_before_generic_formal_error() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("symbolic_infix_nested_constructor_precedence.m");
    std::fs::write(&source_path, "bad ((Nope x)*y) = y\n")
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
fn load_file_repeated_name_application_head_still_reaches_value_head_bucket() {
    let mut vm = VM::new();
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
fn load_file_top_level_pattern_repeated_name_application_head_still_reaches_value_head_bucket() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_pattern_repeated_name_application_head.m");
    std::fs::write(&source_path, "(x, (x y)) = missing\n")
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
fn load_file_local_repeated_name_application_head_still_reaches_value_head_bucket_before_undefined_name() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_local_repeated_name_application_head.m");
    std::fs::write(&source_path, "f x = g x where g (y, (y z)) = missing\n")
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
fn load_file_accepts_unparenthesized_declared_constructor_application_in_formal() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("unparenthesized_constructor_application_formal.m");
    std::fs::write(
        &source_path,
        "maybe * ::= Nothing | Just *\nfromJust Just x = x\n",
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
fn load_file_rejects_unparenthesized_value_head_application_chain_in_formal() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("unparenthesized_value_head_application_chain_formal.m");
    std::fs::write(&source_path, "bad (g x) y = y\n")
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
fn load_file_rejects_unparenthesized_repeated_name_head_application_chain_in_formal() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("unparenthesized_repeated_name_head_application_chain_formal.m");
    std::fs::write(&source_path, "bad x x y = y\n")
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
fn load_file_rejects_unparenthesized_non_identifier_head_application_chain_in_formal() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("unparenthesized_non_identifier_head_application_chain_formal.m");
    std::fs::write(&source_path, "bad (x:xs) y = y\n")
        .expect("failed to write source test file");
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
fn load_file_rejects_non_canonical_plus_pattern_in_formal() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("load_file_non_canonical_plus_pattern_formal.m");
    std::fs::write(&source_path, "f (x+y) = x\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::NonCanonicalPlusPatternsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_rejects_unparenthesized_non_canonical_plus_pattern_in_formal() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("load_file_unparenthesized_non_canonical_plus_pattern_formal.m");
    std::fs::write(&source_path, "f x+y = x\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::NonCanonicalPlusPatternsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_rejects_unary_minus_pattern_in_formal() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("load_file_unary_minus_pattern_formal.m");
    std::fs::write(&source_path, "f (-x) = x\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::UnaryMinusPatternsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_rejects_unparenthesized_unary_minus_pattern_in_formal() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("load_file_unparenthesized_unary_minus_pattern_formal.m");
    std::fs::write(&source_path, "f -x = x\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::UnaryMinusPatternsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_arithmetic_head_application_reaches_malformed_plus_application_error() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("arithmetic_head_application_formal.m");
    std::fs::write(&source_path, "bad (((x+1)) y) = y\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::MalformedPlusApplicationsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_reports_arithmetic_head_application_before_undefined_name() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("arithmetic_head_precedes_undefined_name.m");
    std::fs::write(&source_path, "bad (((x+1)) y) = missing\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::MalformedPlusApplicationsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_reports_nested_constructor_inside_malformed_plus_before_generic_formal_error() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("malformed_plus_nested_constructor_formal_precedence.m");
    std::fs::write(&source_path, "bad (((Nope x)+y) z) = z\n")
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
fn load_file_rejects_unparenthesized_binary_minus_pattern_in_formal() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("load_file_unparenthesized_binary_minus_pattern_formal.m");
    std::fs::write(&source_path, "f x-y = y\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::MalformedMinusApplicationsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_rejects_unparenthesized_arithmetic_head_application_in_formal() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("load_file_unparenthesized_arithmetic_head_application_formal.m");
    std::fs::write(&source_path, "bad x+1 y = y\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::MalformedPlusApplicationsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_unparenthesized_arithmetic_head_application_still_binds_interior_names() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("load_file_unparenthesized_arithmetic_head_binding.m");
    std::fs::write(&source_path, "bad x+1 y = missing\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::MalformedPlusApplicationsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn parse_source_text_top_level_where_reaches_symbolic_infix_pattern_typecheck_error() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("source_top_level_where_symbolic_infix_pattern.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let outcome = vm
        .parse_source_text(
            &source_path_str,
            "f x = g x where g (x*y) = y\n",
            UNIX_EPOCH,
            false,
        )
        .expect("source parse should succeed");

    assert!(outcome.parsed_without_error);
    vm.files = outcome.files;

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::NonIdentifierApplicationHeadsInFormals { count: 1 })
    ));
}

#[test]
fn parse_source_text_top_level_where_reaches_unparenthesized_symbolic_infix_pattern_typecheck_error() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("source_top_level_where_unparenthesized_symbolic_infix_pattern.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let outcome = vm
        .parse_source_text(
            &source_path_str,
            "f x = g x where g x*y = y\n",
            UNIX_EPOCH,
            false,
        )
        .expect("source parse should succeed");

    assert!(outcome.parsed_without_error);
    vm.files = outcome.files;

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::NonIdentifierApplicationHeadsInFormals { count: 1 })
    ));
}

#[test]
fn parse_source_text_reaches_non_canonical_plus_pattern_typecheck_error() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("source_non_canonical_plus_pattern_formal.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let outcome = vm
        .parse_source_text(&source_path_str, "f (x+y) = x\n", UNIX_EPOCH, false)
        .expect("source parse should succeed");

    assert!(outcome.parsed_without_error);
    vm.files = outcome.files;

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::NonCanonicalPlusPatternsInFormals { count: 1 })
    ));
}

#[test]
fn parse_source_text_reaches_unparenthesized_non_canonical_plus_pattern_typecheck_error() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("source_unparenthesized_non_canonical_plus_pattern_formal.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let outcome = vm
        .parse_source_text(&source_path_str, "f x+y = x\n", UNIX_EPOCH, false)
        .expect("source parse should succeed");

    assert!(outcome.parsed_without_error);
    vm.files = outcome.files;

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::NonCanonicalPlusPatternsInFormals { count: 1 })
    ));
}

#[test]
fn parse_source_text_non_canonical_plus_pattern_still_binds_interior_names() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("source_non_canonical_plus_binding.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let outcome = vm
        .parse_source_text(&source_path_str, "f (x+y) = y\n", UNIX_EPOCH, false)
        .expect("source parse should succeed");

    assert!(outcome.parsed_without_error);
    vm.files = outcome.files;

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::NonCanonicalPlusPatternsInFormals { count: 1 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn parse_source_text_reaches_unary_minus_pattern_typecheck_error() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("source_unary_minus_pattern_formal.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let outcome = vm
        .parse_source_text(&source_path_str, "f (-x) = x\n", UNIX_EPOCH, false)
        .expect("source parse should succeed");

    assert!(outcome.parsed_without_error);
    vm.files = outcome.files;

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::UnaryMinusPatternsInFormals { count: 1 })
    ));
}

#[test]
fn parse_source_text_reaches_unparenthesized_unary_minus_pattern_typecheck_error() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("source_unparenthesized_unary_minus_pattern_formal.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let outcome = vm
        .parse_source_text(&source_path_str, "f -x = x\n", UNIX_EPOCH, false)
        .expect("source parse should succeed");

    assert!(outcome.parsed_without_error);
    vm.files = outcome.files;

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::UnaryMinusPatternsInFormals { count: 1 })
    ));
}

#[test]
fn parse_source_text_reaches_malformed_plus_application_typecheck_error() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("source_malformed_plus_application_formal.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let outcome = vm
        .parse_source_text(&source_path_str, "f (((x+y)) z) = z\n", UNIX_EPOCH, false)
        .expect("source parse should succeed");

    assert!(outcome.parsed_without_error);
    vm.files = outcome.files;

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::MalformedPlusApplicationsInFormals { count: 1 })
    ));
}

#[test]
fn parse_source_text_reaches_unparenthesized_malformed_plus_application_typecheck_error() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("source_unparenthesized_malformed_plus_application_formal.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let outcome = vm
        .parse_source_text(&source_path_str, "f x+1 y = y\n", UNIX_EPOCH, false)
        .expect("source parse should succeed");

    assert!(outcome.parsed_without_error);
    vm.files = outcome.files;

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::MalformedPlusApplicationsInFormals { count: 1 })
    ));
}

#[test]
fn parse_source_text_reaches_malformed_minus_application_typecheck_error() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("source_malformed_minus_pattern_formal.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let outcome = vm
        .parse_source_text(&source_path_str, "f (x-y) = y\n", UNIX_EPOCH, false)
        .expect("source parse should succeed");

    assert!(outcome.parsed_without_error);
    vm.files = outcome.files;

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::MalformedMinusApplicationsInFormals { count: 1 })
    ));
}

#[test]
fn parse_source_text_reaches_unparenthesized_malformed_minus_application_typecheck_error() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("source_unparenthesized_malformed_minus_pattern_formal.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let outcome = vm
        .parse_source_text(&source_path_str, "f x-y = y\n", UNIX_EPOCH, false)
        .expect("source parse should succeed");

    assert!(outcome.parsed_without_error);
    vm.files = outcome.files;

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::MalformedMinusApplicationsInFormals { count: 1 })
    ));
}

#[test]
fn parse_source_text_reaches_unparenthesized_application_headed_plus_pattern_typecheck_error() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("source_unparenthesized_application_headed_plus_pattern_formal.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let outcome = vm
        .parse_source_text(&source_path_str, "f g x+y = y\n", UNIX_EPOCH, false)
        .expect("source parse should succeed");

    assert!(outcome.parsed_without_error);
    vm.files = outcome.files;

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::NonCanonicalPlusPatternsInFormals { count: 1 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn parse_source_text_top_level_where_reaches_non_canonical_plus_pattern_typecheck_error() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("source_top_level_where_non_canonical_plus.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let outcome = vm
        .parse_source_text(
            &source_path_str,
            "f x = g x where g (a+b) = a\n",
            UNIX_EPOCH,
            false,
        )
        .expect("source parse should succeed");

    assert!(outcome.parsed_without_error);
    vm.files = outcome.files;

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::NonCanonicalPlusPatternsInFormals { count: 1 })
    ));
}

#[test]
fn parse_source_text_top_level_where_reaches_unparenthesized_non_canonical_plus_pattern_typecheck_error() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("source_top_level_where_unparenthesized_non_canonical_plus.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let outcome = vm
        .parse_source_text(
            &source_path_str,
            "f x = g x where g a+b = a\n",
            UNIX_EPOCH,
            false,
        )
        .expect("source parse should succeed");

    assert!(outcome.parsed_without_error);
    vm.files = outcome.files;

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::NonCanonicalPlusPatternsInFormals { count: 1 })
    ));
}

#[test]
fn parse_source_text_top_level_where_reaches_unparenthesized_unary_minus_pattern_typecheck_error() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("source_top_level_where_unparenthesized_unary_minus.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let outcome = vm
        .parse_source_text(
            &source_path_str,
            "f x = g x where g -y = y\n",
            UNIX_EPOCH,
            false,
        )
        .expect("source parse should succeed");

    assert!(outcome.parsed_without_error);
    vm.files = outcome.files;

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::UnaryMinusPatternsInFormals { count: 1 })
    ));
}

#[test]
fn parse_source_text_top_level_where_reaches_unparenthesized_malformed_minus_pattern_typecheck_error() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("source_top_level_where_unparenthesized_malformed_minus.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let outcome = vm
        .parse_source_text(
            &source_path_str,
            "f x = g x where g a-b = b\n",
            UNIX_EPOCH,
            false,
        )
        .expect("source parse should succeed");

    assert!(outcome.parsed_without_error);
    vm.files = outcome.files;

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::MalformedMinusApplicationsInFormals { count: 1 })
    ));
}

#[test]
fn parse_source_text_top_level_where_unparenthesized_application_headed_plus_still_binds_interior_names() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("source_top_level_where_unparenthesized_application_headed_plus.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let outcome = vm
        .parse_source_text(
            &source_path_str,
            "f x = q x where q g x+y = x\n",
            UNIX_EPOCH,
            false,
        )
        .expect("source parse should succeed");

    assert!(outcome.parsed_without_error);
    vm.files = outcome.files;

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::NonCanonicalPlusPatternsInFormals { count: 1 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn parse_source_text_top_level_where_malformed_plus_application_still_binds_interior_names() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("source_top_level_where_malformed_plus_binding.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let outcome = vm
        .parse_source_text(
            &source_path_str,
            "f x = g x where g (((a+b)) c) = c\n",
            UNIX_EPOCH,
            false,
        )
        .expect("source parse should succeed");

    assert!(outcome.parsed_without_error);
    vm.files = outcome.files;

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::MalformedPlusApplicationsInFormals { count: 1 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn load_file_commits_strict_constructor_field_metadata() {
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
fn load_file_commits_infix_synonym_type_lhs_arity() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("infix_synonym_type_lhs_arity.m");
    std::fs::write(&source_path, "* $pair ** == (*, [num])\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);

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
    assert_eq!(arity, 2);
    assert_eq!(
        value_type.get_identifier_value_type_kind(&vm.heap),
        IdentifierValueTypeKind::Synonym
    );
}

#[test]
fn load_file_commits_infix_algebraic_type_lhs_arity() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("infix_algebraic_type_lhs_arity.m");
    std::fs::write(&source_path, "* $pair ** ::= Pair * **\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);

    let pair = vm
        .heap
        .get_identifier("pair")
        .expect("expected pair type identifier");
    let constructor = vm
        .heap
        .get_identifier("Pair")
        .expect("expected Pair constructor");
    let pair_value = pair
        .get_value(&vm.heap)
        .expect("expected typed algebraic value");
    let IdentifierValueData::Typed {
        arity, value_type, ..
    } = pair_value.get_data(&vm.heap)
    else {
        panic!("expected typed identifier value")
    };
    assert_eq!(arity, 2);
    assert_eq!(
        value_type.get_identifier_value_type_kind(&vm.heap),
        IdentifierValueTypeKind::Algebraic
    );

    let constructor_type = constructor.get_type_expr(&vm.heap);
    assert!(vm.heap.is_arrow_type(constructor_type.value()));
    let outer_arrow = ApNodeRef::from_ref(constructor_type.value().into());
    let outer_operator = outer_arrow
        .function_application(&vm.heap)
        .expect("expected outer arrow application");
    assert!(vm
        .heap
        .is_type_variable(Value::from(outer_operator.argument_raw(&vm.heap))));

    let result_type = TypeExprRef::new(Value::from(outer_arrow.argument_raw(&vm.heap)));
    assert!(vm.heap.is_arrow_type(result_type.value()));
    let inner_arrow = ApNodeRef::from_ref(result_type.value().into());
    let inner_operator = inner_arrow
        .function_application(&vm.heap)
        .expect("expected inner arrow application");
    assert!(vm
        .heap
        .is_type_variable(Value::from(inner_operator.argument_raw(&vm.heap))));

    let parent_application = ApNodeRef::from_ref(inner_arrow.argument_raw(&vm.heap));
    let parent_function = ApNodeRef::from_ref(parent_application.function_raw(&vm.heap));
    assert_eq!(Value::from(parent_function.function_raw(&vm.heap)), pair.into());
    assert!(vm
        .heap
        .is_type_variable(Value::from(parent_function.argument_raw(&vm.heap))));
    assert!(vm
        .heap
        .is_type_variable(Value::from(parent_application.argument_raw(&vm.heap))));
}

#[test]
fn parse_source_text_commits_narrow_free_substrate_and_marks_file_unshareable() {
    let mut vm = VM::new();

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
    let mut vm = VM::new();

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
    let mut vm = VM::new();

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
    let mut vm = VM::new();

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
fn parse_source_text_commits_multi_entry_free_block_in_source_order() {
    let mut vm = VM::new();

    let source_path = unique_test_path("multi_entry_free_block.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let source_text = "%free { x :: num; xs :: [char] }\n";

    let result = vm.parse_source_text(&source_path_str, source_text, UNIX_EPOCH, false);

    assert!(result.is_ok());
    assert_eq!(vm.free_identifiers.len(&vm.heap), 2);

    let mut free_bindings = vm.free_identifiers;
    let first = free_bindings
        .pop(&vm.heap)
        .expect("expected first free binding");
    assert_eq!(vm.identifier_name(first.identifier(&vm.heap)), "x");
    assert!(first.type_expr(&vm.heap).is_builtin_type(Type::Number));

    let second = free_bindings
        .pop(&vm.heap)
        .expect("expected second free binding");
    assert_eq!(vm.identifier_name(second.identifier(&vm.heap)), "xs");
    assert!(vm.heap.is_list_type(second.type_expr(&vm.heap).value()));
}

#[test]
fn parse_source_text_commits_multi_name_free_spec() {
    let mut vm = VM::new();

    let source_path = unique_test_path("multi_name_free_spec.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let source_text = "%free { f, g :: num }\n";

    let result = vm.parse_source_text(&source_path_str, source_text, UNIX_EPOCH, false);

    assert!(result.is_ok());
    assert_eq!(vm.free_identifiers.len(&vm.heap), 2);

    let mut free_bindings = vm.free_identifiers;
    let first = free_bindings
        .pop(&vm.heap)
        .expect("expected first free binding");
    let second = free_bindings
        .pop(&vm.heap)
        .expect("expected second free binding");
    assert_eq!(vm.identifier_name(first.identifier(&vm.heap)), "f");
    assert_eq!(vm.identifier_name(second.identifier(&vm.heap)), "g");
    assert!(first.type_expr(&vm.heap).is_builtin_type(Type::Number));
    assert!(second.type_expr(&vm.heap).is_builtin_type(Type::Number));
}

#[test]
fn commit_parsed_top_level_script_materializes_tuple_pattern_bindings() {
    let mut vm = VM::new();
    let source_path = "top_level_tuple_pattern_bindings.m";
    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let lhs = vm.heap.pair_ref(x.into(), y.into());
    let rhs = IntegerRef::from_i64(&mut vm.heap, 7);
    let anchor = test_anchor(&mut vm, source_path, 1);

    let files = vm.commit_parsed_top_level_script(
        source_path,
        UNIX_EPOCH,
        &ParserTopLevelScriptPayload {
            pattern_definitions: vec![ParserPatternDefinitionPayload {
                lhs,
                body: rhs.get_ref(),
                anchor,
            }],
            ..Default::default()
        },
    );

    let current_file = files.head(&vm.heap).expect("expected current file");
    assert_eq!(current_file.get_definienda(&vm.heap).len(&vm.heap), 2);
    assert!(x.get_value(&vm.heap).is_some());
    assert!(y.get_value(&vm.heap).is_some());
}

#[test]
fn commit_parsed_top_level_script_materializes_three_tuple_pattern_bindings() {
    let mut vm = VM::new();
    let source_path = "top_level_three_tuple_pattern_bindings.m";
    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let z = vm.heap.make_empty_identifier("z");
    let tail = vm.heap.pair_ref(y.into(), z.into());
    let lhs = vm.heap.cons_ref(x.into(), tail);
    let rhs = IntegerRef::from_i64(&mut vm.heap, 7);
    let anchor = test_anchor(&mut vm, source_path, 1);

    let files = vm.commit_parsed_top_level_script(
        source_path,
        UNIX_EPOCH,
        &ParserTopLevelScriptPayload {
            pattern_definitions: vec![ParserPatternDefinitionPayload {
                lhs,
                body: rhs.get_ref(),
                anchor,
            }],
            ..Default::default()
        },
    );

    let current_file = files.head(&vm.heap).expect("expected current file");
    assert_eq!(current_file.get_definienda(&vm.heap).len(&vm.heap), 3);
    assert!(x.get_value(&vm.heap).is_some());
    assert!(y.get_value(&vm.heap).is_some());
    assert!(z.get_value(&vm.heap).is_some());
}

#[test]
fn commit_parsed_top_level_script_materializes_list_pattern_bindings() {
    let mut vm = VM::new();
    let source_path = "top_level_list_pattern_bindings.m";
    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let tail = vm.heap.cons_ref(y.into(), Combinator::Nil.into());
    let lhs = vm.heap.cons_ref(x.into(), tail);
    let rhs = IntegerRef::from_i64(&mut vm.heap, 7);
    let anchor = test_anchor(&mut vm, source_path, 1);

    let files = vm.commit_parsed_top_level_script(
        source_path,
        UNIX_EPOCH,
        &ParserTopLevelScriptPayload {
            pattern_definitions: vec![ParserPatternDefinitionPayload {
                lhs,
                body: rhs.get_ref(),
                anchor,
            }],
            ..Default::default()
        },
    );

    let current_file = files.head(&vm.heap).expect("expected current file");
    assert_eq!(current_file.get_definienda(&vm.heap).len(&vm.heap), 2);
    assert!(x.get_value(&vm.heap).is_some());
    assert!(y.get_value(&vm.heap).is_some());
}

#[test]
fn commit_parsed_top_level_script_empty_list_pattern_commits_no_bindings() {
    let mut vm = VM::new();
    let source_path = "top_level_empty_list_pattern_bindings.m";
    let lhs = vm.heap.nill;
    let rhs = IntegerRef::from_i64(&mut vm.heap, 7);
    let anchor = test_anchor(&mut vm, source_path, 1);

    let files = vm.commit_parsed_top_level_script(
        source_path,
        UNIX_EPOCH,
        &ParserTopLevelScriptPayload {
            pattern_definitions: vec![ParserPatternDefinitionPayload {
                lhs,
                body: rhs.get_ref(),
                anchor,
            }],
            ..Default::default()
        },
    );

    let current_file = files.head(&vm.heap).expect("expected current file");
    assert_eq!(current_file.get_definienda(&vm.heap).len(&vm.heap), 0);
}

#[test]
fn commit_parsed_top_level_script_repeated_name_pattern_does_not_create_duplicate_binding() {
    let mut vm = VM::new();
    let source_path = "top_level_repeated_name_pattern_bindings.m";
    let x = vm.heap.make_empty_identifier("x");
    let repeated = vm.heap.cons_ref(Token::Constant.into(), x.into());
    let tail = vm.heap.cons_ref(repeated, Combinator::Nil.into());
    let lhs = vm.heap.cons_ref(x.into(), tail);
    let rhs = IntegerRef::from_i64(&mut vm.heap, 7);
    let anchor = test_anchor(&mut vm, source_path, 1);

    let files = vm.commit_parsed_top_level_script(
        source_path,
        UNIX_EPOCH,
        &ParserTopLevelScriptPayload {
            pattern_definitions: vec![ParserPatternDefinitionPayload {
                lhs,
                body: rhs.get_ref(),
                anchor,
            }],
            ..Default::default()
        },
    );

    let current_file = files.head(&vm.heap).expect("expected current file");
    assert_eq!(current_file.get_definienda(&vm.heap).len(&vm.heap), 1);
    assert!(x.get_value(&vm.heap).is_some());
}

#[test]
fn commit_parsed_top_level_script_wrapped_constant_pattern_only_commits_variable_binding() {
    let mut vm = VM::new();
    let source_path = "top_level_wrapped_constant_pattern_bindings.m";
    let x = vm.heap.make_empty_identifier("x");
    let one = IntegerRef::from_i64(&mut vm.heap, 1);
    let wrapped_constant = vm.heap.cons_ref(Token::Constant.into(), one.get_ref().into());
    let tail = vm.heap.cons_ref(x.into(), Combinator::Nil.into());
    let lhs = vm.heap.cons_ref(wrapped_constant, tail);
    let rhs = IntegerRef::from_i64(&mut vm.heap, 7);
    let anchor = test_anchor(&mut vm, source_path, 1);

    let files = vm.commit_parsed_top_level_script(
        source_path,
        UNIX_EPOCH,
        &ParserTopLevelScriptPayload {
            pattern_definitions: vec![ParserPatternDefinitionPayload {
                lhs,
                body: rhs.get_ref(),
                anchor,
            }],
            ..Default::default()
        },
    );

    let current_file = files.head(&vm.heap).expect("expected current file");
    assert_eq!(current_file.get_definienda(&vm.heap).len(&vm.heap), 1);
    assert!(x.get_value(&vm.heap).is_some());
}

#[test]
fn commit_parsed_top_level_script_constant_pattern_commits_no_bindings() {
    let mut vm = VM::new();
    let source_path = "top_level_constant_pattern_bindings.m";
    let one = IntegerRef::from_i64(&mut vm.heap, 1);
    let lhs = vm.heap.cons_ref(Token::Constant.into(), one.get_ref().into());
    let rhs = IntegerRef::from_i64(&mut vm.heap, 7);
    let anchor = test_anchor(&mut vm, source_path, 1);

    let files = vm.commit_parsed_top_level_script(
        source_path,
        UNIX_EPOCH,
        &ParserTopLevelScriptPayload {
            pattern_definitions: vec![ParserPatternDefinitionPayload {
                lhs,
                body: rhs.get_ref(),
                anchor,
            }],
            ..Default::default()
        },
    );

    let current_file = files.head(&vm.heap).expect("expected current file");
    assert_eq!(current_file.get_definienda(&vm.heap).len(&vm.heap), 0);
}

#[test]
fn parse_source_text_commits_mixed_free_type_and_value_specs() {
    let mut vm = VM::new();

    let source_path = unique_test_path("mixed_free_type_and_value_specs.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let source_text = "%free { t :: type; f :: t }\n";

    let result = vm.parse_source_text(&source_path_str, source_text, UNIX_EPOCH, false);

    assert!(result.is_ok());

    let t = vm
        .heap
        .get_identifier("t")
        .expect("expected free typename to exist");
    let f = vm
        .heap
        .get_identifier("f")
        .expect("expected free value identifier to exist");
    assert!(t.get_type_expr(&vm.heap).is_builtin_type(Type::Type));
    assert_eq!(
        RawValue::from(f.get_type_expr(&vm.heap).value()),
        t.get_ref()
    );

    let t_value = t
        .get_value(&vm.heap)
        .expect("expected free typename payload");
    let IdentifierValueData::Typed { value_type, .. } = t_value.get_data(&vm.heap) else {
        panic!("expected typed free typename payload");
    };
    assert_eq!(
        value_type.get_identifier_value_type_kind(&vm.heap),
        IdentifierValueTypeKind::Free
    );
}

#[test]
fn load_file_reports_undefined_typename_in_rich_synonym_rhs() {
    let mut vm = VM::new();

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
    let mut vm = VM::new();

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
    let mut vm = VM::new();

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
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("include_export_directive_export_only.m");
    std::fs::write(&source_path, "foo = 1\n%export foo\n").expect("failed to write source test file");
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
fn load_file_export_of_missing_explicit_name_fails_before_commit() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("export_missing_explicit_name.m");
    std::fs::write(&source_path, "%export foo\nbar = 1\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::ExportValidation(
            ExportValidationError::UndefinedExportedIdentifier { name }
        )) if name == "foo"
    ));
    assert_eq!(vm.exported_identifiers, NIL);
    assert_eq!(vm.export_paths, NIL);
    assert_eq!(vm.export_embargoes, NIL);
    assert!(!vm.loading);
}

#[test]
fn load_file_include_and_exportfile_success_commits_after_validation() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("committed_include_target.m");
    std::fs::write(&include_path, "foo = 1\n").expect("failed to write include source test file");
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
fn load_file_export_closure_does_not_promote_free_type_dependency() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("export_closure_ignores_free_type_dependency.m");
    std::fs::write(&source_path, "%free { t :: type }\nfoo :: t -> t\nfoo x = x\n%export foo\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let exported = ConsList::<IdentifierRecordRef>::from_ref(vm.exported_identifiers.into());
    let foo = vm.heap.get_identifier("foo").expect("expected foo identifier");
    let t = vm.heap.get_identifier("t").expect("expected free type identifier");
    assert!(exported.contains(&vm.heap, foo));
    assert!(!exported.contains(&vm.heap, t));
}

#[test]
fn load_file_export_closure_adds_synonym_type_dependency() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("export_closure_synonym_dependency.m");
    std::fs::write(
        &source_path,
        "thing == num\nid :: thing -> thing\nid x = x\n%export id\n",
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let exported = ConsList::<IdentifierRecordRef>::from_ref(vm.exported_identifiers.into());
    let id = vm.heap.get_identifier("id").expect("expected id identifier");
    let thing = vm.heap.get_identifier("thing").expect("expected thing identifier");
    assert!(exported.contains(&vm.heap, id));
    assert!(exported.contains(&vm.heap, thing));
}

#[test]
fn load_file_export_closure_adds_algebraic_type_dependency_from_exported_value() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("export_closure_algebraic_value_dependency.m");
    std::fs::write(
        &source_path,
        "maybe ::= Just num | Nothing\nunwrap :: maybe -> num\nunwrap x = 0\n%export unwrap\n",
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let exported = ConsList::<IdentifierRecordRef>::from_ref(vm.exported_identifiers.into());
    let unwrap = vm.heap.get_identifier("unwrap").expect("expected unwrap identifier");
    let maybe = vm.heap.get_identifier("maybe").expect("expected maybe type identifier");
    assert!(exported.contains(&vm.heap, unwrap));
    assert!(exported.contains(&vm.heap, maybe));
}

#[test]
fn load_file_export_closure_adds_abstract_basis_dependency() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("export_closure_abstract_basis_dependency.m");
    std::fs::write(
        &source_path,
        "base == num\nabstype thing with showthing :: base\nthing == base\nshowthing = 0\n%export thing\n",
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let exported = ConsList::<IdentifierRecordRef>::from_ref(vm.exported_identifiers.into());
    let thing = vm.heap.get_identifier("thing").expect("expected thing identifier");
    let base = vm.heap.get_identifier("base").expect("expected base identifier");
    assert!(exported.contains(&vm.heap, thing));
    assert!(exported.contains(&vm.heap, base));
}

#[test]
fn load_file_export_plus_excludes_current_script_free_bindings() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("export_plus_excludes_current_free_bindings.m");
    std::fs::write(
        &source_path,
        "%free { t :: type }\nfoo :: t -> t\nfoo x = x\nbar :: t -> t\nbar x = x\n%export +\n",
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let exported = ConsList::<IdentifierRecordRef>::from_ref(vm.exported_identifiers.into());
    let foo = vm.heap.get_identifier("foo").expect("expected foo identifier");
    let bar = vm.heap.get_identifier("bar").expect("expected bar identifier");
    let t = vm.heap.get_identifier("t").expect("expected free type identifier");
    assert!(exported.contains(&vm.heap, foo));
    assert!(exported.contains(&vm.heap, bar));
    assert!(!exported.contains(&vm.heap, t));
}

#[test]
fn load_file_export_path_expansion_excludes_included_free_bindings() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("export_path_excludes_included_free_bindings_target.m");
    std::fs::write(&include_path, "%free { x :: num }\nfoo y = x\nbar y = y\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("export_path_excludes_included_free_bindings_driver.m");
    std::fs::write(
        &source_path,
        format!(
            "%include \"{}\" {{ x = 1 }}\n%export \"{}\"\n",
            include_path_str, include_path_str
        ),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let exported = ConsList::<IdentifierRecordRef>::from_ref(vm.exported_identifiers.into());
    let foo = vm.heap.get_identifier("foo").expect("expected included foo identifier");
    let bar = vm.heap.get_identifier("bar").expect("expected included bar identifier");
    let x = vm.heap.get_identifier("x").expect("expected included free identifier");
    assert!(exported.contains(&vm.heap, foo));
    assert!(exported.contains(&vm.heap, bar));
    assert!(!exported.contains(&vm.heap, x));
}

#[test]
fn load_file_materializes_included_source_definienda() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("materialized_include_target.m");
    std::fs::write(&include_path, "foo = 1\nbar = 2\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("materialized_include_driver.m");
    std::fs::write(&source_path, format!("%include \"{}\"\n", include_path_str))
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let included_files = vm.files.rest(&vm.heap).expect("expected included file list tail");
    let includee = included_files.head(&vm.heap).expect("expected included file");
    assert_eq!(includee.get_file_name(&vm.heap), include_path_str);

    let mut definienda = includee.get_definienda(&vm.heap);
    let mut names = Vec::new();
    while let Some(definiendum) = definienda.pop(&vm.heap) {
        names.push(vm.identifier_name(definiendum));
    }
    names.sort();
    assert_eq!(names, vec!["bar", "foo"]);
}

#[test]
fn load_file_routes_include_value_actual_bindings_through_bindparams() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("parameterized_include_value_target.m");
    std::fs::write(&include_path, "%free { x :: num }\nfoo = x\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("parameterized_include_value_driver.m");
    std::fs::write(&source_path, format!("%include \"{}\" {{ x = 42 }}\n", include_path_str))
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let x = vm.heap.get_identifier("x").expect("expected free identifier");
    assert_eq!(vm.missing_parameter_bindings.len(&vm.heap), 0);
    assert_eq!(vm.detritus_parameter_bindings.len(&vm.heap), 0);
    assert_eq!(vm.free_binding_sets.len(&vm.heap), 1);
    assert_eq!(
        IntegerRef::from_ref(vm.heap[x.get_ref()].tail).to_i64_lossy(&vm.heap),
        42
    );
}

#[test]
fn load_file_routes_include_type_actual_bindings_through_bindparams() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("parameterized_include_type_target.m");
    std::fs::write(&include_path, "%free { t :: type }\nid :: t -> t\nid x = x\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("parameterized_include_type_driver.m");
    std::fs::write(
        &source_path,
        format!("%include \"{}\" {{ t == num }}\n", include_path_str),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let t = vm.heap.get_identifier("t").expect("expected free type identifier");
    assert_eq!(vm.missing_parameter_bindings.len(&vm.heap), 0);
    assert_eq!(vm.detritus_parameter_bindings.len(&vm.heap), 0);
    assert_eq!(vm.free_binding_sets.len(&vm.heap), 1);

    let actual_type_value = t.get_value(&vm.heap).expect("expected bound type value");
    let IdentifierValueData::Typed {
        arity, value_type, ..
    } = actual_type_value.get_data(&vm.heap)
    else {
        panic!("expected typed include-bound type value");
    };
    assert_eq!(arity, 0);
    assert_eq!(
        value_type.get_identifier_value_type_kind(&vm.heap),
        IdentifierValueTypeKind::Synonym
    );
    assert_eq!(
        RawValue::from(value_type.synonym_rhs_type_expr(&vm.heap).value()),
        RawValue::from(Type::Number)
    );
}

#[test]
fn load_file_include_typename_audit_allows_parameterized_type_binding() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("parameterized_include_typename_audit_target.m");
    std::fs::write(&include_path, "%free { t :: type }\nid :: t -> t\nid x = x\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("parameterized_include_typename_audit_driver.m");
    std::fs::write(
        &source_path,
        format!("%include \"{}\" {{ t == num }}\n", include_path_str),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let id = vm.heap.get_identifier("id").expect("expected included id identifier");
    assert_ne!(
        id.get_value(&vm.heap)
            .expect("expected lowered included id body")
            .get_ref(),
        RawValue::from(Combinator::Undef)
    );
}

#[test]
fn load_file_parameterized_include_missing_actual_binding_errors_before_commit() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("parameterized_include_missing_target.m");
    std::fs::write(&include_path, "%free { x :: num; y :: num }\nfoo = x\nbar = y\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("parameterized_include_missing_driver.m");
    std::fs::write(&source_path, format!("%include \"{}\" {{ x = 42 }}\n", include_path_str))
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(TypecheckError::MissingFreeBindings { count })) if count == 1
    ));
    assert_eq!(vm.files.len(&vm.heap), 1);
    assert_eq!(vm.detritus_parameter_bindings.len(&vm.heap), 0);
    assert_eq!(vm.missing_parameter_bindings.len(&vm.heap), 1);
}

#[test]
fn load_file_parameterized_include_extra_actual_binding_errors_before_commit() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("parameterized_include_extra_target.m");
    std::fs::write(&include_path, "%free { x :: num }\nfoo = x\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("parameterized_include_extra_driver.m");
    std::fs::write(
        &source_path,
        format!("%include \"{}\" {{ x = 42; y = 99 }}\n", include_path_str),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(TypecheckError::InvalidFreeBindings { count })) if count == 1
    ));
    assert_eq!(vm.files.len(&vm.heap), 1);
    assert_eq!(vm.detritus_parameter_bindings.len(&vm.heap), 1);
    assert_eq!(vm.missing_parameter_bindings.len(&vm.heap), 0);
}

#[test]
fn load_file_preserves_parameterized_include_request_order() {
    let mut vm = VM::new();
    vm.initializing = false;

    let first_include_path = unique_test_path("parameterized_include_first_target.m");
    std::fs::write(&first_include_path, "%free { x :: num }\nfoo = x\n")
        .expect("failed to write first include source test file");
    let first_include_path_str = first_include_path.to_string_lossy().to_string();

    let second_include_path = unique_test_path("parameterized_include_second_target.m");
    std::fs::write(&second_include_path, "%free { y :: num }\nbar = y\n")
        .expect("failed to write second include source test file");
    let second_include_path_str = second_include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("parameterized_include_order_driver.m");
    std::fs::write(
        &source_path,
        format!(
            "%include \"{}\" {{ x = 1 }}\n%include \"{}\" {{ y = 2 }}\n",
            first_include_path_str, second_include_path_str,
        ),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    assert_eq!(vm.free_binding_sets.len(&vm.heap), 2);

    let mut included_files = vm.files.rest(&vm.heap).expect("expected included file list tail");
    let first_includee = included_files.pop(&vm.heap).expect("expected first included file");
    let second_includee = included_files.pop(&vm.heap).expect("expected second included file");
    assert_eq!(first_includee.get_file_name(&vm.heap), first_include_path_str);
    assert_eq!(second_includee.get_file_name(&vm.heap), second_include_path_str);
}

#[test]
fn load_file_shares_repeated_shareable_include_definition_identity_across_renamed_copy() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("shared_include_target.m");
    std::fs::write(&include_path, "foo = 1\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("shared_include_driver.m");
    std::fs::write(
        &source_path,
        format!(
            "%include \"{}\"\n%include \"{}\" renamed / foo\n",
            include_path_str, include_path_str,
        ),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let mut included_files = vm.files.rest(&vm.heap).expect("expected included file list tail");
    let first_includee = included_files.pop(&vm.heap).expect("expected first included file");
    let first_foo = first_includee
        .get_definienda(&vm.heap)
        .head(&vm.heap)
        .expect("expected first include definiendum");
    let renamed = vm
        .heap
        .get_identifier("renamed")
        .expect("expected renamed included identifier");
    assert_eq!(
        renamed
            .get_value(&vm.heap)
            .expect("expected renamed included value")
            .get_ref(),
        first_foo.get_ref()
    );
}

#[test]
fn load_file_preserves_include_order_after_repeated_shareable_include_sharing() {
    let mut vm = VM::new();
    vm.initializing = false;

    let shared_path = unique_test_path("shared_include_order_target.m");
    std::fs::write(&shared_path, "foo = 1\n").expect("failed to write shared include source test file");
    let shared_path_str = shared_path.to_string_lossy().to_string();

    let distinct_path = unique_test_path("distinct_include_order_target.m");
    std::fs::write(&distinct_path, "bar = 2\n")
        .expect("failed to write distinct include source test file");
    let distinct_path_str = distinct_path.to_string_lossy().to_string();

    let source_path = unique_test_path("shared_include_order_driver.m");
    std::fs::write(
        &source_path,
        format!(
            "%include \"{}\"\n%include \"{}\"\n%include \"{}\" renamed / foo\n",
            shared_path_str, distinct_path_str, shared_path_str,
        ),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let mut files = vm.files;
    let mut file_names = Vec::new();
    while let Some(file) = files.pop(&vm.heap) {
        file_names.push(file.get_file_name(&vm.heap));
    }
    assert_eq!(
        file_names,
        vec![
            source_path_str,
            shared_path_str.clone(),
            distinct_path_str,
            shared_path_str,
        ]
    );
}

#[test]
fn load_file_does_not_share_unshareable_current_script_include_components() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("unshareable_include_target.m");
    std::fs::write(&include_path, "%free { x :: num }\nfoo = x\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("unshareable_include_driver.m");
    std::fs::write(
        &source_path,
        format!(
            "%include \"{}\" {{ x = 1 }}\n%include \"{}\" {{ x = 2 }} renamed / foo\n",
            include_path_str, include_path_str,
        ),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let foo = vm.heap.get_identifier("foo").expect("expected first included foo identifier");
    let renamed = vm
        .heap
        .get_identifier("renamed")
        .expect("expected renamed included identifier");
    assert_ne!(
        renamed
            .get_value(&vm.heap)
            .expect("expected renamed included value")
            .get_ref(),
        foo.get_ref()
    );
}

#[test]
fn load_file_rejects_repeated_non_synonym_type_copies_from_same_include_file() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("typeclash_include_target.m");
    std::fs::write(
        &include_path,
        "abstype thing with showthing :: num\nthing == num\nshowthing = 0\n",
    )
    .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("typeclash_include_driver.m");
    std::fs::write(
        &source_path,
        format!(
            "%include \"{}\"\n%include \"{}\" other / thing\n%export thing other\n",
            include_path_str, include_path_str,
        ),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::IncludeDirective(IncludeDirectiveError::RepeatedTypeClash {
            path,
            names,
        })) if path == include_path_str && names.contains(&"thing".to_string()) && names.contains(&"other".to_string())
    ));
    assert_eq!(vm.files.len(&vm.heap), 1);
    assert_eq!(vm.exported_identifiers, NIL);
    assert_eq!(vm.export_paths, NIL);
    assert_eq!(vm.export_embargoes, NIL);
}

#[test]
fn load_file_allows_repeated_synonym_type_copies_from_same_include_file() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("synonym_include_target.m");
    std::fs::write(&include_path, "thing == num\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("synonym_include_driver.m");
    std::fs::write(
        &source_path,
        format!(
            "%include \"{}\"\n%include \"{}\" other / thing\n%export thing other\n",
            include_path_str, include_path_str,
        ),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let thing = vm
        .heap
        .get_identifier("thing")
        .expect("expected first synonym type identifier");
    let other = vm
        .heap
        .get_identifier("other")
        .expect("expected renamed synonym type identifier");
    assert_eq!(
        thing.get_value(&vm.heap).and_then(|value| value.typed_kind(&vm.heap)),
        Some(IdentifierValueTypeKind::Synonym)
    );
    assert_eq!(
        other.get_value(&vm.heap).and_then(|value| value.typed_kind(&vm.heap)),
        Some(IdentifierValueTypeKind::Synonym)
    );
}

#[test]
fn load_file_exports_repeated_shared_include_definitions_coherently() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("shared_export_include_target.m");
    std::fs::write(&include_path, "foo = 1\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("shared_export_include_driver.m");
    std::fs::write(
        &source_path,
        format!(
            "%include \"{}\"\n%include \"{}\" renamed / foo\n%export foo renamed\n",
            include_path_str, include_path_str,
        ),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let exported = ConsList::<IdentifierRecordRef>::from_ref(vm.exported_identifiers.into());
    let renamed = vm
        .heap
        .get_identifier("renamed")
        .expect("expected renamed included identifier");
    let mut included_files = vm.files.rest(&vm.heap).expect("expected included file list tail");
    let first_includee = included_files.pop(&vm.heap).expect("expected first included file");
    let first_foo = first_includee
        .get_definienda(&vm.heap)
        .head(&vm.heap)
        .expect("expected first include definiendum");
    assert!(exported.contains(&vm.heap, first_foo));
    assert!(exported.contains(&vm.heap, renamed));
    assert_eq!(
        renamed
            .get_value(&vm.heap)
            .expect("expected renamed included value")
            .get_ref(),
        first_foo.get_ref()
    );
}

#[test]
fn load_file_applies_include_rename_modifier_to_materialized_definienda() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("rename_include_target.m");
    std::fs::write(&include_path, "foo = 1\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("rename_include_driver.m");
    std::fs::write(
        &source_path,
        format!("%include \"{}\" renamed / foo\n", include_path_str),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let included_files = vm.files.rest(&vm.heap).expect("expected included file list tail");
    let includee = included_files.head(&vm.heap).expect("expected included file");

    let mut definienda = includee.get_definienda(&vm.heap);
    let mut names = Vec::new();
    while let Some(definiendum) = definienda.pop(&vm.heap) {
        names.push(vm.identifier_name(definiendum));
    }
    assert_eq!(names, vec!["renamed"]);
}

#[test]
fn load_file_applies_include_constructor_rename_modifier_to_materialized_definienda_and_metadata() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("constructor_rename_include_target.m");
    std::fs::write(&include_path, "maybe * ::= Nothing | Just *\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("constructor_rename_include_driver.m");
    std::fs::write(
        &source_path,
        format!("%include \"{}\" Renamed / Just\n", include_path_str),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let included_files = vm.files.rest(&vm.heap).expect("expected included file list tail");
    let includee = included_files.head(&vm.heap).expect("expected included file");

    let mut definienda = includee.get_definienda(&vm.heap);
    let mut names = Vec::new();
    while let Some(definiendum) = definienda.pop(&vm.heap) {
        names.push(vm.identifier_name(definiendum));
    }
    assert!(names.contains(&"maybe".to_string()));
    assert!(names.contains(&"Nothing".to_string()));
    assert!(names.contains(&"Renamed".to_string()));
    assert!(!names.contains(&"Just".to_string()));

    let maybe = vm.heap.get_identifier("maybe").expect("expected maybe type identifier");
    let maybe_value = maybe.get_value(&vm.heap).expect("expected typed algebraic value");
    let IdentifierValueData::Typed { value_type, .. } = maybe_value.get_data(&vm.heap) else {
        panic!("expected typed algebraic value")
    };
    let mut constructors = value_type
        .algebraic_constructor_metadata(&vm.heap)
        .expect("expected algebraic constructor metadata");
    let nothing_metadata = constructors.pop(&vm.heap).expect("expected Nothing metadata");
    let renamed_metadata = constructors.pop(&vm.heap).expect("expected Renamed metadata");
    assert_eq!(nothing_metadata.constructor(&vm.heap).get_name(&vm.heap), "Nothing");
    assert_eq!(renamed_metadata.constructor(&vm.heap).get_name(&vm.heap), "Renamed");

    let renamed = vm.heap.get_identifier("Renamed").expect("expected renamed constructor");
    let renamed_value = renamed.get_value(&vm.heap).expect("expected renamed constructor value");
    let renamed_constructor = ConstructorRef::from_ref(renamed_value.get_ref());
    assert_eq!(renamed_constructor.payload(&vm.heap), renamed.into());
}

#[test]
fn load_file_accepts_formal_matching_renamed_included_constructor() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("renamed_constructor_formal_include_target.m");
    std::fs::write(&include_path, "maybe * ::= Nothing | Just *\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("renamed_constructor_formal_driver.m");
    std::fs::write(
        &source_path,
        format!("%include \"{}\" Renamed / Just\nfromRenamed (Renamed x) = x\n", include_path_str),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    assert!(vm.undefined_names.is_empty());
}

#[test]
fn load_file_export_path_expansion_uses_renamed_included_constructor() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("renamed_constructor_export_include_target.m");
    std::fs::write(&include_path, "maybe * ::= Nothing | Just *\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("renamed_constructor_export_driver.m");
    std::fs::write(
        &source_path,
        format!("%include \"{}\" Renamed / Just\n%export \"{}\"\n", include_path_str, include_path_str),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let exported = ConsList::<IdentifierRecordRef>::from_ref(vm.exported_identifiers.into());
    let maybe = vm.heap.get_identifier("maybe").expect("expected maybe type identifier");
    let nothing = vm.heap.get_identifier("Nothing").expect("expected Nothing constructor");
    let renamed = vm.heap.get_identifier("Renamed").expect("expected Renamed constructor");
    let just = vm.heap.get_identifier("Just").expect("expected original constructor identifier");
    assert!(exported.contains(&vm.heap, maybe));
    assert!(exported.contains(&vm.heap, nothing));
    assert!(exported.contains(&vm.heap, renamed));
    assert!(!exported.contains(&vm.heap, just));
}

#[test]
fn load_file_rejects_include_constructor_rename_destination_clash() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("constructor_rename_clash_include_target.m");
    std::fs::write(&include_path, "maybe * ::= Nothing | Just *\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("constructor_rename_clash_driver.m");
    std::fs::write(
        &source_path,
        format!("%include \"{}\" Nothing / Just\n", include_path_str),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::IncludeDirective(
            IncludeDirectiveError::RenameDestinationClash { name }
        )) if name == "Nothing"
    ));
}

#[test]
fn load_file_applies_include_suppress_modifier_to_materialized_definienda() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("suppress_include_target.m");
    std::fs::write(&include_path, "foo = 1\nbar = 2\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("suppress_include_driver.m");
    std::fs::write(
        &source_path,
        format!("%include \"{}\" -foo\n", include_path_str),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let included_files = vm.files.rest(&vm.heap).expect("expected included file list tail");
    let includee = included_files.head(&vm.heap).expect("expected included file");

    let mut definienda = includee.get_definienda(&vm.heap);
    let mut names = Vec::new();
    while let Some(definiendum) = definienda.pop(&vm.heap) {
        names.push(vm.identifier_name(definiendum));
    }
    assert_eq!(names, vec!["bar"]);
}

#[test]
fn load_file_rejects_include_suppress_algebraic_typename() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("suppress_algebraic_typename_include_target.m");
    std::fs::write(&include_path, "maybe * ::= Nothing | Just *\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("suppress_algebraic_typename_driver.m");
    std::fs::write(
        &source_path,
        format!("%include \"{}\" -maybe\n", include_path_str),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::IncludeDirective(
            IncludeDirectiveError::IllegalTypeNameSuppression { name }
        )) if name == "maybe"
    ));
    assert_eq!(vm.files.len(&vm.heap), 1);
}

#[test]
fn load_file_rejects_include_suppress_abstract_typename() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("suppress_abstract_typename_include_target.m");
    std::fs::write(
        &include_path,
        "abstype thing with showthing :: num\nthing == num\nshowthing = 0\n",
    )
    .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("suppress_abstract_typename_driver.m");
    std::fs::write(
        &source_path,
        format!("%include \"{}\" -thing\n", include_path_str),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::IncludeDirective(
            IncludeDirectiveError::IllegalTypeNameSuppression { name }
        )) if name == "thing"
    ));
    assert_eq!(vm.files.len(&vm.heap), 1);
}

#[test]
fn load_file_reports_missing_include_typename_after_synonym_suppression_in_value_spec() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("missing_typename_value_spec_include_target.m");
    std::fs::write(&include_path, "thing == num\nfoo :: thing\nfoo = 1\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("missing_typename_value_spec_driver.m");
    std::fs::write(
        &source_path,
        format!("%include \"{}\" -thing\n", include_path_str),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::IncludeDirective(
            IncludeDirectiveError::MissingVisibleTypeNames { names }
        )) if names == vec!["thing"]
    ));
    assert_eq!(vm.files.len(&vm.heap), 1);
}

#[test]
fn load_file_reports_missing_include_typename_after_synonym_suppression_in_type_decl() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("missing_typename_type_decl_include_target.m");
    std::fs::write(&include_path, "thing == num\nother == thing\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("missing_typename_type_decl_driver.m");
    std::fs::write(
        &source_path,
        format!("%include \"{}\" -thing\n", include_path_str),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::IncludeDirective(
            IncludeDirectiveError::MissingVisibleTypeNames { names }
        )) if names == vec!["thing"]
    ));
    assert_eq!(vm.files.len(&vm.heap), 1);
}

#[test]
fn load_file_reports_missing_include_typename_after_synonym_rename() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("missing_typename_rename_include_target.m");
    std::fs::write(&include_path, "thing == num\nfoo :: thing\nfoo = 1\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("missing_typename_rename_driver.m");
    std::fs::write(
        &source_path,
        format!("%include \"{}\" renamed / thing\n", include_path_str),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::IncludeDirective(
            IncludeDirectiveError::MissingVisibleTypeNames { names }
        )) if names == vec!["thing"]
    ));
    assert_eq!(vm.files.len(&vm.heap), 1);
}

#[test]
fn load_file_include_modifier_failure_leaves_no_authoritative_commit() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("missing_modifier_target_include.m");
    std::fs::write(&include_path, "foo = 1\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("missing_modifier_target_driver.m");
    std::fs::write(
        &source_path,
        format!("%include \"{}\" -missing\n", include_path_str),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::IncludeDirective(
            IncludeDirectiveError::ModifierTargetNotFound { name }
        )) if name == "missing"
    ));
    assert_eq!(vm.files.len(&vm.heap), 1);
    assert_eq!(vm.exported_identifiers, NIL);
    assert_eq!(vm.export_paths, NIL);
    assert_eq!(vm.export_embargoes, NIL);
}

#[test]
fn load_file_materializes_nested_include_graph_in_source_order() {
    let mut vm = VM::new();
    vm.initializing = false;

    let leaf_path = unique_test_path("nested_include_leaf.m");
    std::fs::write(&leaf_path, "leaf = 1\n").expect("failed to write leaf include source test file");
    let leaf_path_str = leaf_path.to_string_lossy().to_string();

    let middle_path = unique_test_path("nested_include_middle.m");
    std::fs::write(
        &middle_path,
        format!("%include \"{}\"\nmiddle = 2\n", leaf_path_str),
    )
    .expect("failed to write middle include source test file");
    let middle_path_str = middle_path.to_string_lossy().to_string();

    let source_path = unique_test_path("nested_include_driver.m");
    std::fs::write(&source_path, format!("%include \"{}\"\nroot = 3\n", middle_path_str))
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let mut files = vm.files;
    let mut file_names = Vec::new();
    while let Some(file) = files.pop(&vm.heap) {
        file_names.push(file.get_file_name(&vm.heap));
    }
    assert_eq!(file_names, vec![source_path_str, middle_path_str, leaf_path_str]);
}

#[test]
fn load_file_nested_include_failure_leaves_no_authoritative_commit() {
    let mut vm = VM::new();
    vm.initializing = false;

    let missing_leaf_path = unique_test_path("missing_nested_leaf.m");
    let missing_leaf_path_str = missing_leaf_path.to_string_lossy().to_string();

    let middle_path = unique_test_path("missing_nested_middle.m");
    std::fs::write(
        &middle_path,
        format!("%include \"{}\"\nmiddle = 2\n", missing_leaf_path_str),
    )
    .expect("failed to write middle include source test file");
    let middle_path_str = middle_path.to_string_lossy().to_string();

    let source_path = unique_test_path("missing_nested_driver.m");
    std::fs::write(&source_path, format!("%include \"{}\"\n", middle_path_str))
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::SourceInput(SourceInputError::MissingFile { path }))
            if path == missing_leaf_path_str
    ));
    assert_eq!(vm.files.len(&vm.heap), 1);
    assert_eq!(vm.exported_identifiers, NIL);
    assert_eq!(vm.export_paths, NIL);
    assert_eq!(vm.export_embargoes, NIL);
}

#[test]
fn load_file_nested_include_modifier_missing_typename_failure_leaves_no_authoritative_commit() {
    let mut vm = VM::new();
    vm.initializing = false;

    let leaf_path = unique_test_path("nested_typename_leaf.m");
    std::fs::write(&leaf_path, "leaf = 1\n").expect("failed to write leaf include source test file");
    let leaf_path_str = leaf_path.to_string_lossy().to_string();

    let middle_path = unique_test_path("nested_typename_middle.m");
    std::fs::write(
        &middle_path,
        format!("%include \"{}\"\nthing == num\nfoo :: thing\nfoo = 1\n", leaf_path_str),
    )
    .expect("failed to write middle include source test file");
    let middle_path_str = middle_path.to_string_lossy().to_string();

    let source_path = unique_test_path("nested_typename_driver.m");
    std::fs::write(&source_path, format!("%include \"{}\" -thing\n", middle_path_str))
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::IncludeDirective(
            IncludeDirectiveError::MissingVisibleTypeNames { names }
        )) if names == vec!["thing"]
    ));
    assert_eq!(vm.files.len(&vm.heap), 1);
    let only_file = vm.files.head(&vm.heap).expect("expected source file anchor");
    assert_eq!(only_file.get_file_name(&vm.heap), source_path_str);
    assert_eq!(vm.exported_identifiers, NIL);
    assert_eq!(vm.export_paths, NIL);
    assert_eq!(vm.export_embargoes, NIL);
}

#[test]
fn load_file_accepts_simple_top_level_where_definition() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_simple.m");
    std::fs::write(&source_path, "id x = y where y = x\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
}

#[test]
fn load_file_accepts_top_level_where_with_grouped_local_helper() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_grouped_helper.m");
    std::fs::write(&source_path, "f x = g x where g 0 = 0; g y = y\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
}

#[test]
fn load_file_accepts_top_level_where_with_guarded_local_helper() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_guarded_local_helper.m");
    std::fs::write(&source_path, "f x = g x where g y = y, if x; z = x\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
}

#[test]
fn load_file_accepts_grouped_local_helper_with_guarded_first_equation() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("grouped_local_guarded_first_equation.m");
    std::fs::write(&source_path, "f x = g x where g y = y, if x = x; g z = z\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
}

#[test]
fn load_file_accepts_grouped_local_helper_with_multiple_guarded_equations_and_final_plain_equation() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("grouped_local_multiple_guarded_equations.m");
    std::fs::write(
        &source_path,
        "f x = g x where g y = y, if x = x; g z = z, if x = x; g w = w\n",
    )
    .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
}

#[test]
fn load_file_accepts_grouped_local_helper_with_terminal_otherwise_equation() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("grouped_local_terminal_otherwise_equation.m");
    std::fs::write(&source_path, "f x = g x where g y = y, if x = x; g z = z, otherwise\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
}

#[test]
fn load_file_reports_grouped_local_case_after_terminal_otherwise() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("grouped_local_case_after_otherwise.m");
    std::fs::write(&source_path, "f x = g x where g y = y, otherwise; g z = z\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
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
}

#[test]
fn load_file_accepts_grouped_local_helper_with_guarded_and_nested_local_family() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("grouped_local_guarded_nested_family.m");
    std::fs::write(
        &source_path,
        "f x = g x where\n  g y = y, if x = x\n  g z = z\n  h = k where\n    k = x\n",
    )
    .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
}

#[test]
fn parse_source_text_rejects_unreachable_grouped_local_case_with_nearby_local_pattern() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("unreachable_grouped_local_case_with_pattern.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let result = vm.parse_source_text(
        &source_path_str,
        "f xs = g 0 where g x = x; g z = z; h (y:ys) = y\n",
        UNIX_EPOCH,
        false,
    );

    let outcome = result.expect("parser boundary should complete with diagnostics");

    assert!(!outcome.parsed_without_error);
    assert!(vm.parser_diagnostics.iter().any(|diagnostic| {
        diagnostic.message.contains("unreachable case in defn of \"g\"")
    }));
}

#[test]
fn load_file_reports_undefined_local_name_in_top_level_where_definition() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_undefined_local_name.m");
    std::fs::write(&source_path, "f = y where z = 1\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(TypecheckError::UndefinedNames { count: 1 }))
    ));
}

#[test]
fn load_file_reports_undefined_name_inside_guarded_local_rhs() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_guarded_local_missing_name.m");
    std::fs::write(&source_path, "f x = g x where g y = missing, if x; z = x\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(TypecheckError::UndefinedNames { count: 1 }))
    ));
}

#[test]
fn load_file_accepts_top_level_where_with_nested_local_where() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_nested_local_where.m");
    std::fs::write(&source_path, "f = g where\n  g = h where\n    h = 1\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
}

#[test]
fn load_file_accepts_grouped_local_helper_when_nested_local_where_is_present() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_grouped_nested_local_where.m");
    std::fs::write(&source_path, "f = g 0 where\n  g x = h where\n    h = x\n  g y = y\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
}

#[test]
fn load_file_reports_undefined_name_inside_nested_local_where() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_nested_local_missing_name.m");
    std::fs::write(&source_path, "f = g where\n  g = h where\n    h = missing\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(TypecheckError::UndefinedNames { count: 1 }))
    ));
}

#[test]
fn load_file_accepts_top_level_where_with_local_cons_pattern_helper() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_local_cons_pattern_helper.m");
    std::fs::write(&source_path, "f xs = g xs where g (x:rest) = x\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
}

#[test]
fn load_file_accepts_top_level_where_with_local_n_plus_k_helper() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_local_n_plus_k_helper.m");
    std::fs::write(&source_path, "f x = g x where g (y+1) = y\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
}

#[test]
fn load_file_accepts_top_level_where_with_local_unparenthesized_n_plus_k_helper() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_local_unparenthesized_n_plus_k_helper.m");
    std::fs::write(&source_path, "f x = g x where g y+1 = y\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
}

#[test]
fn load_file_accepts_top_level_where_with_local_unparenthesized_negative_integer_literal_helper() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_local_unparenthesized_negative_literal_helper.m");
    std::fs::write(&source_path, "f x = h x where h -1 = 0\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
}

#[test]
fn load_file_rejects_top_level_where_with_local_non_canonical_plus_pattern() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_local_non_canonical_plus_pattern.m");
    std::fs::write(&source_path, "f x = g x where g (a+b) = a\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::NonCanonicalPlusPatternsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_rejects_top_level_where_with_local_unparenthesized_non_canonical_plus_pattern() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_local_unparenthesized_non_canonical_plus_pattern.m");
    std::fs::write(&source_path, "f x = g x where g a+b = a\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::NonCanonicalPlusPatternsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_rejects_top_level_where_with_local_unparenthesized_unary_minus_pattern() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_local_unparenthesized_unary_minus_pattern.m");
    std::fs::write(&source_path, "f x = g x where g -y = y\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::UnaryMinusPatternsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_rejects_top_level_where_with_local_unparenthesized_binary_minus_pattern() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_local_unparenthesized_binary_minus_pattern.m");
    std::fs::write(&source_path, "f x = g x where g a-b = b\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::MalformedMinusApplicationsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_rejects_undeclared_constructor_in_top_level_pattern_definition() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_pattern_undeclared_constructor.m");
    std::fs::write(&source_path, "Nope x = x\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::UndeclaredConstructorsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_rejects_invalid_successor_pattern_in_top_level_pattern_definition() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_pattern_invalid_successor.m");
    std::fs::write(&source_path, "(((x:xs)+1)) = missing\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::InvalidSuccessorPatternsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_top_level_pattern_symbolic_infix_still_precedes_undefined_name() {
    for (file_name, source_text) in [
        ("top_level_pattern_symbolic_infix_precedes_undefined_name.m", "(a=b) = b\n"),
        (
            "top_level_pattern_symbolic_double_equal_precedes_undefined_name.m",
            "(a==b) = b\n",
        ),
    ] {
        let mut vm = VM::new();
        vm.initializing = false;

        let source_path = unique_test_path(file_name);
        std::fs::write(&source_path, source_text).expect("failed to write source test file");
        let source_path_str = source_path.to_string_lossy().to_string();

        let result = vm.load_file(&source_path_str);

        assert!(matches!(
            result,
            Err(LoadFileError::Typecheck(
                TypecheckError::NonIdentifierApplicationHeadsInFormals { count: 1 }
            ))
        ));
    }
}

#[test]
fn load_file_top_level_pattern_unparenthesized_symbolic_infix_still_precedes_undefined_name() {
    for (file_name, source_text) in [
        ("top_level_pattern_unparenthesized_symbolic_times.m", "x*y = missing\n"),
        ("top_level_pattern_unparenthesized_symbolic_append.m", "u++v = missing\n"),
        ("top_level_pattern_unparenthesized_symbolic_power.m", "p^q = missing\n"),
        ("top_level_pattern_unparenthesized_symbolic_divide_float.m", "a/b = missing\n"),
        ("top_level_pattern_unparenthesized_symbolic_divide_int.m", "c div d = missing\n"),
        ("top_level_pattern_unparenthesized_symbolic_remainder.m", "e mod f = missing\n"),
        ("top_level_pattern_unparenthesized_symbolic_and.m", "g&h = missing\n"),
        ("top_level_pattern_unparenthesized_symbolic_or.m", "i\\/j = missing\n"),
        ("top_level_pattern_unparenthesized_symbolic_not_equal.m", "k~=l = missing\n"),
        ("top_level_pattern_unparenthesized_symbolic_greater.m", "m>n = missing\n"),
        ("top_level_pattern_unparenthesized_symbolic_greater_equal.m", "o>=p = missing\n"),
        ("top_level_pattern_unparenthesized_symbolic_less.m", "q<r = missing\n"),
        ("top_level_pattern_unparenthesized_symbolic_less_equal.m", "s<=t = missing\n"),
        ("top_level_pattern_unparenthesized_symbolic_compose.m", "u.v = missing\n"),
        ("top_level_pattern_unparenthesized_symbolic_subscript.m", "w!x = missing\n"),
    ] {
        let mut vm = VM::new();
        vm.initializing = false;

        let source_path = unique_test_path(file_name);
        std::fs::write(&source_path, source_text).expect("failed to write source test file");

        let result = vm.load_file(&source_path.to_string_lossy());

        assert!(matches!(
            result,
            Err(LoadFileError::Typecheck(
                TypecheckError::NonIdentifierApplicationHeadsInFormals { count: 1 }
            ))
        ));
    }
}

#[test]
fn load_file_top_level_pattern_non_canonical_plus_still_precedes_undefined_name() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_pattern_non_canonical_plus.m");
    std::fs::write(&source_path, "(x+y) = missing\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::NonCanonicalPlusPatternsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_reports_local_unparenthesized_symbolic_infix_before_undefined_name_under_top_level_where() {
    for (file_name, source_text) in [
        (
            "top_level_where_local_unparenthesized_symbolic_times_precedes_undefined_name.m",
            "f x = g x where g x*y = missing\n",
        ),
        (
            "top_level_where_local_unparenthesized_symbolic_append_precedes_undefined_name.m",
            "f x = g x where g u++v = missing\n",
        ),
        (
            "top_level_where_local_unparenthesized_symbolic_power_precedes_undefined_name.m",
            "f x = g x where g p^q = missing\n",
        ),
        (
            "top_level_where_local_unparenthesized_symbolic_divide_float_precedes_undefined_name.m",
            "f x = g x where g a/b = missing\n",
        ),
        (
            "top_level_where_local_unparenthesized_symbolic_divide_int_precedes_undefined_name.m",
            "f x = g x where g c div d = missing\n",
        ),
        (
            "top_level_where_local_unparenthesized_symbolic_remainder_precedes_undefined_name.m",
            "f x = g x where g e mod f = missing\n",
        ),
        (
            "top_level_where_local_unparenthesized_symbolic_and_precedes_undefined_name.m",
            "f x = g x where g h&i = missing\n",
        ),
        (
            "top_level_where_local_unparenthesized_symbolic_or_precedes_undefined_name.m",
            "f x = g x where g j\\/k = missing\n",
        ),
        (
            "top_level_where_local_unparenthesized_symbolic_not_equal_precedes_undefined_name.m",
            "f x = g x where g l~=m = missing\n",
        ),
        (
            "top_level_where_local_unparenthesized_symbolic_greater_precedes_undefined_name.m",
            "f x = g x where g n>o = missing\n",
        ),
        (
            "top_level_where_local_unparenthesized_symbolic_greater_equal_precedes_undefined_name.m",
            "f x = g x where g p>=q = missing\n",
        ),
        (
            "top_level_where_local_unparenthesized_symbolic_less_precedes_undefined_name.m",
            "f x = g x where g r<s = missing\n",
        ),
        (
            "top_level_where_local_unparenthesized_symbolic_less_equal_precedes_undefined_name.m",
            "f x = g x where g t<=u = missing\n",
        ),
        (
            "top_level_where_local_unparenthesized_symbolic_compose_precedes_undefined_name.m",
            "f x = g x where g v.w = missing\n",
        ),
        (
            "top_level_where_local_unparenthesized_symbolic_subscript_precedes_undefined_name.m",
            "f x = g x where g y!z = missing\n",
        ),
    ] {
        let mut vm = VM::new();
        vm.initializing = false;

        let source_path = unique_test_path(file_name);
        std::fs::write(&source_path, source_text).expect("failed to write source test file");

        let result = vm.load_file(&source_path.to_string_lossy());

        assert!(matches!(
            result,
            Err(LoadFileError::Typecheck(
                TypecheckError::NonIdentifierApplicationHeadsInFormals { count: 1 }
            ))
        ));
    }
}

#[test]
fn load_file_reports_local_malformed_plus_application_before_undefined_name_under_top_level_where() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_local_malformed_plus_precedes_undefined_name.m");
    std::fs::write(&source_path, "f x = g x where g (((a+b)) c) = missing\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::MalformedPlusApplicationsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_reports_nested_constructor_inside_local_malformed_plus_before_generic_formal_error() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_local_malformed_plus_nested_constructor_precedence.m");
    std::fs::write(&source_path, "f x = g x where g (((Nope a)+b) c) = c\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::UndeclaredConstructorsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_top_level_where_unparenthesized_application_headed_plus_still_binds_interior_names() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_local_unparenthesized_application_headed_plus.m");
    std::fs::write(&source_path, "f x = q x where q g x+y = missing\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::NonCanonicalPlusPatternsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_new_arithmetic_head_application_still_binds_interior_names() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("new_arithmetic_head_application_binding.m");
    std::fs::write(&source_path, "bad (((x+y)) z) = z\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::MalformedPlusApplicationsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_top_level_pattern_malformed_plus_application_still_precedes_undefined_name() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_pattern_malformed_plus_precedes_undefined_name.m");
    std::fs::write(&source_path, "(((x+y)) z) = missing\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::MalformedPlusApplicationsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_top_level_pattern_malformed_minus_application_still_precedes_undefined_name() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_pattern_malformed_minus_precedes_undefined_name.m");
    std::fs::write(&source_path, "(((x-y)) z) = missing\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::MalformedMinusApplicationsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_reports_nested_constructor_inside_top_level_pattern_unparenthesized_symbolic_infix_before_generic_formal_error() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_pattern_unparenthesized_symbolic_nested_constructor_precedence.m");
    std::fs::write(&source_path, "x*Nope y = y\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(
        matches!(
            result,
            Err(LoadFileError::Typecheck(
                TypecheckError::UndeclaredConstructorsInFormals { count: 1 }
            ))
        ),
        "result={result:?} diagnostics={:?}",
        vm.parser_diagnostics
    );
}

#[test]
fn load_file_reports_nested_constructor_inside_top_level_pattern_malformed_plus_before_generic_formal_error() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_pattern_malformed_plus_nested_constructor_precedence.m");
    std::fs::write(&source_path, "(((Nope x)+y) z) = z\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::UndeclaredConstructorsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_reports_nested_constructor_inside_top_level_pattern_malformed_minus_before_generic_formal_error() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_pattern_malformed_minus_nested_constructor_precedence.m");
    std::fs::write(&source_path, "(((Nope x)-y) z) = z\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::UndeclaredConstructorsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_accepts_grouped_local_helper_mixing_pattern_and_identifier_equations() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_grouped_local_pattern_helper.m");
    std::fs::write(&source_path, "f xs = g xs where g (x:rest) = x; g y = y\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
}

#[test]
fn load_file_rejects_undeclared_constructor_in_local_pattern_under_top_level_where() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_local_undeclared_constructor_pattern.m");
    std::fs::write(&source_path, "f = g 0 where g (Nope x) = x\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::UndeclaredConstructorsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_reports_local_pattern_error_before_undefined_name_under_top_level_where() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_local_pattern_precedes_undefined_name.m");
    std::fs::write(&source_path, "f = g 0 where g (((x:xs)+1)) = missing\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(
            TypecheckError::InvalidSuccessorPatternsInFormals { count: 1 }
        ))
    ));
}

#[test]
fn load_file_typechecks_include_target_before_parent_commit() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("typecheck_include_target.m");
    std::fs::write(&include_path, "foo = missing\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("typecheck_include_driver.m");
    std::fs::write(&source_path, format!("%include \"{}\"\n", include_path_str))
        .expect("failed to write source test file");
    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(TypecheckError::UndefinedNames { count: 1 }))
    ));
    assert_eq!(vm.files.len(&vm.heap), 1);
}

#[test]
fn load_file_lowers_include_target_definition_during_include_compilation() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("codegen_include_target.m");
    std::fs::write(&include_path, "id x = x\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("codegen_include_driver.m");
    std::fs::write(&source_path, format!("%include \"{}\"\n", include_path_str))
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let id = vm.heap.get_identifier("id").expect("expected included id identifier");
    assert_eq!(
        id.get_value(&vm.heap)
            .expect("expected lowered included id body")
            .get_ref(),
        RawValue::from(Combinator::I)
    );
}

#[test]
fn load_file_preserves_parent_include_bindings_and_modifier_scope_over_nested_include_graph() {
    let mut vm = VM::new();
    vm.initializing = false;

    let leaf_path = unique_test_path("nested_modifier_leaf.m");
    std::fs::write(&leaf_path, "leaf = 1\n").expect("failed to write leaf include source test file");
    let leaf_path_str = leaf_path.to_string_lossy().to_string();

    let middle_path = unique_test_path("nested_modifier_middle.m");
    std::fs::write(
        &middle_path,
        format!(
            "%free {{ x :: num }}\n%include \"{}\"\nfoo = x\nbar = 2\n",
            leaf_path_str
        ),
    )
    .expect("failed to write middle include source test file");
    let middle_path_str = middle_path.to_string_lossy().to_string();

    let source_path = unique_test_path("nested_modifier_driver.m");
    std::fs::write(
        &source_path,
        format!(
            "%include \"{}\" {{ x = 42 }} renamed / foo -bar\n",
            middle_path_str
        ),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let mut files = vm.files;
    let driver = files.pop(&vm.heap).expect("expected driver file");
    let middle = files.pop(&vm.heap).expect("expected direct include file");
    let leaf = files.pop(&vm.heap).expect("expected nested include file");
    assert_eq!(driver.get_file_name(&vm.heap), source_path_str);
    assert_eq!(middle.get_file_name(&vm.heap), middle_path_str);
    assert_eq!(leaf.get_file_name(&vm.heap), leaf_path_str);

    let mut middle_definienda = middle.get_definienda(&vm.heap);
    let mut middle_names = Vec::new();
    while let Some(definiendum) = middle_definienda.pop(&vm.heap) {
        middle_names.push(vm.identifier_name(definiendum));
    }
    middle_names.sort();
    assert_eq!(middle_names, vec!["renamed", "x"]);

    let mut leaf_definienda = leaf.get_definienda(&vm.heap);
    let mut leaf_names = Vec::new();
    while let Some(definiendum) = leaf_definienda.pop(&vm.heap) {
        leaf_names.push(vm.identifier_name(definiendum));
    }
    assert_eq!(leaf_names, vec!["leaf"]);
}

#[test]
fn load_file_lowers_identity_definition_body_during_codegen() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("codegen_identity_load.m");
    std::fs::write(&source_path, "id x = x\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let id = vm.heap.get_identifier("id").expect("expected id identifier");
    assert_eq!(
        id.get_value(&vm.heap)
            .expect("expected lowered id body")
            .get_ref(),
        RawValue::from(Combinator::I)
    );
}

#[test]
fn load_file_materializes_bare_list_pattern_definition_bindings() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("bare_list_pattern_load.m");
    std::fs::write(&source_path, "[x, y] = [1, 2]\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let x = vm.heap.get_identifier("x").expect("expected x identifier");
    let y = vm.heap.get_identifier("y").expect("expected y identifier");
    assert!(x.get_value(&vm.heap).is_some());
    assert!(y.get_value(&vm.heap).is_some());
    assert!(subtree_contains_combinator(
        &vm.heap,
        Value::from(x.get_value(&vm.heap).expect("expected lowered x body").get_ref()),
        Combinator::U_,
    ));
}

#[test]
fn load_file_materializes_bare_constructor_pattern_definition_bindings() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("bare_constructor_pattern_load.m");
    std::fs::write(&source_path, "maybe ::= Nothing | Just *\nJust x = Just 1\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let x = vm.heap.get_identifier("x").expect("expected x identifier");
    assert!(x.get_value(&vm.heap).is_some());
    assert!(!vm.files.is_empty());
}

#[test]
fn load_file_lowers_tuple_pattern_definition_body_during_codegen() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("codegen_tuple_pattern_load.m");
    std::fs::write(&source_path, "fst (x,y) = x\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let fst = vm.heap.get_identifier("fst").expect("expected fst identifier");
    let lowered_raw = fst
        .get_value(&vm.heap)
        .expect("expected lowered fst body")
        .get_ref();
    assert_eq!(vm.heap[lowered_raw].tag, Tag::Ap);
    assert_eq!(vm.heap[lowered_raw].head, RawValue::from(Combinator::U));
}

#[test]
fn load_file_runs_phase_pipeline_when_script_parse_succeeds() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("phase_pipeline.m");
    std::fs::write(&source_path, "entry = 1\n").expect("failed to write source test file");
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();

    let source_path = unique_test_path("uppercase_type_lhs_synonym.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let result = vm.parse_source_text(&source_path_str, "Maybe * == *\n", UNIX_EPOCH, false);

    let outcome = result.expect("expected parse outcome");
    assert!(!outcome.parsed_without_error);
    assert!(vm.parser_diagnostics.iter().any(|diagnostic| diagnostic
        .message
        .contains("upper case identifier out of context")));
}

#[test]
fn parse_source_text_rejects_repeated_type_variable_on_type_lhs() {
    let mut vm = VM::new();

    let source_path = unique_test_path("repeated_typevar_lhs_type_def.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let result = vm.parse_source_text(&source_path_str, "pair * * == *\n", UNIX_EPOCH, false);

    let outcome = result.expect("expected parse outcome");
    assert!(!outcome.parsed_without_error);
    assert!(vm.parser_diagnostics.iter().any(|diagnostic| {
        diagnostic
            .message
            .contains("repeated type variable on lhs of type def")
    }));
}

#[test]
fn parse_source_text_accepts_distinct_type_variables_on_algebraic_lhs() {
    let mut vm = VM::new();

    let source_path = unique_test_path("distinct_typevars_algebraic_lhs.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let result = vm.parse_source_text(
        &source_path_str,
        "pair * ** ::= Pair * **\n",
        UNIX_EPOCH,
        false,
    );

    let outcome = result.expect("expected parse outcome");
    assert!(
        outcome.parsed_without_error,
        "diagnostics={:?}",
        vm.parser_diagnostics
    );
}

#[test]
fn parse_source_text_commits_abstype_with_later_basis_binding() {
    let mut vm = VM::new();

    let source_path = unique_test_path("abstype_with_later_basis_binding.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let result = vm.parse_source_text(
        &source_path_str,
        "abstype thing with showthing :: num\nthing == num\nshowthing = 0\n",
        UNIX_EPOCH,
        false,
    );

    let outcome = result.expect("expected parse outcome");
    assert!(outcome.parsed_without_error);
    let thing = vm.intern_identifier("thing");
    let showthing = vm.intern_identifier("showthing");
    let IdentifierValueData::Typed {
        show_function,
        value_type,
        ..
    } = thing.get_value(&vm.heap).expect("missing type value").get_data(&vm.heap)
    else {
        panic!("expected typed type identifier value");
    };
    assert_eq!(
        value_type.get_identifier_value_type_kind(&vm.heap),
        IdentifierValueTypeKind::Abstract
    );
    assert_eq!(show_function, showthing.into());
    assert_eq!(
        RawValue::from(value_type.abstract_basis(&vm.heap).expect("missing abstract basis")),
        RawValue::from(Type::Number)
    );
    assert!(!vm.type_abstractions.is_empty());
}

#[test]
fn parse_source_text_commits_abstype_after_prior_basis_binding() {
    let mut vm = VM::new();

    let source_path = unique_test_path("abstype_after_prior_basis_binding.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let result = vm.parse_source_text(
        &source_path_str,
        "thing == num\nabstype thing with showthing :: num\nshowthing = 0\n",
        UNIX_EPOCH,
        false,
    );

    let outcome = result.expect("expected parse outcome");
    assert!(outcome.parsed_without_error);
    let thing = vm.intern_identifier("thing");
    let showthing = vm.intern_identifier("showthing");
    let IdentifierValueData::Typed {
        show_function,
        value_type,
        ..
    } = thing.get_value(&vm.heap).expect("missing type value").get_data(&vm.heap)
    else {
        panic!("expected typed type identifier value");
    };
    assert_eq!(
        value_type.get_identifier_value_type_kind(&vm.heap),
        IdentifierValueTypeKind::Abstract
    );
    assert_eq!(show_function, showthing.into());
    assert_eq!(
        RawValue::from(value_type.abstract_basis(&vm.heap).expect("missing abstract basis")),
        RawValue::from(Type::Number)
    );
    assert!(!vm.type_abstractions.is_empty());
}

#[test]
fn load_file_accepts_abstype_with_basis_and_function_show_function() {
    let mut vm = VM::new();
    let source_path = unique_test_path("abstype_with_basis_and_function_show_function.m");
    std::fs::write(&source_path, "abstype thing with showthing :: num\nthing == num\nshowthing = 0\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} undefined={:?}", vm.undefined_names);
    let thing = vm.intern_identifier("thing");
    let showthing = vm.intern_identifier("showthing");
    let IdentifierValueData::Typed {
        show_function,
        value_type,
        ..
    } = thing.get_value(&vm.heap).expect("missing type value").get_data(&vm.heap)
    else {
        panic!("expected typed type identifier value");
    };
    assert_eq!(
        value_type.get_identifier_value_type_kind(&vm.heap),
        IdentifierValueTypeKind::Abstract
    );
    assert_eq!(show_function, showthing.into());
    assert_eq!(
        RawValue::from(value_type.abstract_basis(&vm.heap).expect("missing abstract basis")),
        RawValue::from(Type::Number)
    );
    assert!(!vm.type_abstractions.is_empty());
}

#[test]
fn load_file_reports_unbound_abstype_without_basis() {
    let mut vm = VM::new();
    let source_path = unique_test_path("unbound_abstype_without_basis.m");
    std::fs::write(&source_path, "abstype thing with showthing :: num\nshowthing = 0\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(matches!(
        result,
        Err(LoadFileError::Typecheck(TypecheckError::UnboundAbstractTypeNames { count: 1 }))
    ));
    let mut undefined_names = vm.undefined_names;
    let mut found_thing = false;
    while let Some(identifier) = undefined_names.pop(&vm.heap) {
        if identifier.get_name(&vm.heap) == "thing" {
            found_thing = true;
            break;
        }
    }
    assert!(found_thing);
}

#[test]
fn parse_source_script_reads_file_and_delegates_to_text_entry() {
    let mut vm = VM::new();
    let source_path = unique_test_path("parse_source_script_wrapper.m");
    std::fs::write(&source_path, "%free { x :: num }\n").expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();
    let source_file = File::open(&source_path).expect("failed to open source test file");

    let outcome = vm
        .parse_source_script(&source_file, &source_path_str, UNIX_EPOCH, false)
        .expect("expected parse outcome");

    assert!(outcome.parsed_without_error);
    assert_eq!(vm.free_identifiers.len(&vm.heap), 1);
    let current_file = outcome.files.head(&vm.heap).expect("expected current file");
    assert!(!current_file.is_shareable(&vm.heap));
}

#[test]
fn success_postlude_phase_resets_syntax_editor_state() {
    let mut vm = VM::new();
    vm.initializing = false;
    vm.sorted = false;
    vm.error_line = 42;
    vm.error_locations = vec![Combinator::True.into()];

    let source_path = unique_test_path("success_postlude_reset.m");
    std::fs::write(&source_path, "entry = 1\n").expect("failed to write source test file");
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();

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
    let mut vm = VM::new();

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
    let mut vm = VM::new();

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
    let mut vm = VM::new();

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
    let mut vm = VM::new();

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
    let mut vm = VM::new();

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
    let mut vm = VM::new();

    let main_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("main.m").to_string_lossy().to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, main_file);

    let include_a_path = unique_test_path("include_a.m");
    std::fs::write(&include_a_path, "includeA = 1\n").expect("failed to write include_a test file");
    let include_a_path = include_a_path.to_string_lossy().to_string();
    let include_b_path = unique_test_path("include_b.m");
    std::fs::write(&include_b_path, "includeB = 1\n").expect("failed to write include_b test file");
    let include_b_path = include_b_path.to_string_lossy().to_string();
    let payload = ParserTopLevelDirectivePayload {
        include_requests: vec![
            include_request_payload(&mut vm, "main.m", 1, &include_a_path),
            include_request_payload(&mut vm, "main.m", 2, &include_b_path),
        ],
        export: None,
    };

    let result = vm.run_mkincludes_phase(Some(&payload));

    assert!(result.is_ok(), "result={result:?}");
    assert!(vm.included_files.is_empty());
    assert!(vm.include_rollback_files.is_empty());
    assert_eq!(vm.files.len(&vm.heap), 3);
}

#[test]
fn include_expansion_phase_is_noop_when_includees_empty() {
    let mut vm = VM::new();
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
    let mut vm = VM::new();
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_checktypes_phase();

    assert!(result.is_ok());
}

#[test]
fn typecheck_phase_fails_when_undefined_names_present() {
    let mut vm = VM::new();
    let missing_id = vm.heap.make_empty_identifier("missing_name");
    vm.undefined_names = ConsList::new(&mut vm.heap, missing_id);

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::UndefinedNames { count: 1 })
    ));
}

#[test]
fn typecheck_phase_tracks_bound_names_through_local_let_body() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("local_let_scope_ok.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let y = vm.heap.make_empty_identifier("y");
    let one = IntegerRef::from_i64(&mut vm.heap, 1);
    let definition = DefinitionRef::new(&mut vm.heap, y.into(), Type::Undefined.into(), one.into());
    let let_body = vm.heap.let_ref(definition.into(), y.into());
    f.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(let_body));
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let result = vm.run_checktypes_phase();

    assert!(result.is_ok());
}

#[test]
fn typecheck_phase_reports_undefined_name_inside_local_let_rhs() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("local_let_scope_missing_rhs.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let y = vm.heap.make_empty_identifier("y");
    let missing = vm.heap.make_empty_identifier("missing_local_rhs");
    let definition = DefinitionRef::new(
        &mut vm.heap,
        y.into(),
        Type::Undefined.into(),
        missing.into(),
    );
    let let_body = vm.heap.let_ref(definition.into(), y.into());
    f.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(let_body));
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let result = vm.run_checktypes_phase();

    assert!(matches!(result, Err(TypecheckError::UndefinedNames { count: 1 })));
}

#[test]
fn typecheck_phase_tracks_recursive_local_letrec_bindings_across_group() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("local_letrec_scope_ok.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let g = vm.heap.make_empty_identifier("g");
    let h = vm.heap.make_empty_identifier("h");
    let g_definition = DefinitionRef::new(&mut vm.heap, g.into(), Type::Undefined.into(), h.into());
    let one = IntegerRef::from_i64(&mut vm.heap, 1);
    let h_definition = DefinitionRef::new(&mut vm.heap, h.into(), Type::Undefined.into(), one.into());
    let mut definitions = NIL;
    definitions = ConsList::<Value>::insert_ordered_value(&mut vm.heap, definitions, h_definition.into());
    definitions = ConsList::<Value>::insert_ordered_value(&mut vm.heap, definitions, g_definition.into());
    let letrec_body = vm.heap.letrec_ref(definitions.into(), g.into());
    f.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(letrec_body));
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let result = vm.run_checktypes_phase();

    assert!(result.is_ok());
}

#[test]
fn typecheck_phase_does_not_bind_repeated_name_leaf_in_formal() {
    let mut vm = VM::new();

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
        .lambda_ref(repeated_name_pattern, missing.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
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
    let mut vm = VM::new();

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
        .lambda_ref(wrapped_constant_pattern, missing.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
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
    let mut vm = VM::new();

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
    let lambda_body = vm.heap.lambda_ref(malformed_pattern, x.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
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
    let mut vm = VM::new();

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
    let malformed_pattern = vm.heap.apply_ref(malformed_head, y.into());
    let lambda_body = vm.heap.lambda_ref(malformed_pattern, y.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::NonIdentifierApplicationHeadsInFormals { count: 1 })
    ));
}

#[test]
fn typecheck_phase_rejects_symbolic_infix_pattern_in_non_identifier_bucket() {
    for (file_name, operator, left_name, right_name, body_name) in [
        (
            "typecheck_symbolic_times_formal.m",
            Combinator::Times.into(),
            "x",
            "y",
            "y",
        ),
        (
            "typecheck_symbolic_equal_formal.m",
            Combinator::Nil.into(),
            "a",
            "b",
            "b",
        ),
        (
            "typecheck_symbolic_append_formal.m",
            Combinator::Append.into(),
            "u",
            "v",
            "v",
        ),
    ] {
        let mut vm = VM::new();

        let current_file = FileRecord::new(
            &mut vm.heap,
            unique_test_path(file_name).to_string_lossy().to_string(),
            UNIX_EPOCH,
            false,
            ConsList::EMPTY,
        );
        vm.files = ConsList::new(&mut vm.heap, current_file);

        let f = vm.heap.make_empty_identifier("f");
        let left = vm.heap.make_empty_identifier(left_name);
        let right = vm.heap.make_empty_identifier(right_name);
        let body = vm.heap.make_empty_identifier(body_name);
        let malformed_pattern = vm.heap.apply2(operator, left.into(), right.into());
        let lambda_body = vm.heap.lambda_ref(malformed_pattern, body.into());
        f.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(lambda_body));
        current_file.push_item_onto_definienda(&mut vm.heap, f);

        let result = vm.run_checktypes_phase();

        assert!(matches!(
            result,
            Err(TypecheckError::NonIdentifierApplicationHeadsInFormals { count: 1 })
        ));
    }
}

#[test]
fn typecheck_phase_rejects_repeated_name_application_head_in_value_head_bucket() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("repeated_name_formal_head.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let repeated_head = vm.heap.cons_ref(Token::Constant.into(), x.into());
    let malformed_pattern = vm.heap.apply_ref(repeated_head, y.into());
    let lambda_body = vm.heap.lambda_ref(malformed_pattern, y.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::ValueHeadApplicationsInFormals { count: 1 })
    ));
}

#[test]
fn typecheck_phase_non_identifier_application_head_still_binds_interior_names() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("non_identifier_application_binding.m")
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
    let malformed_pattern = vm.heap.apply_ref(malformed_head, y.into());
    let lambda_body = vm.heap.lambda_ref(malformed_pattern, y.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::NonIdentifierApplicationHeadsInFormals { count: 1 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn typecheck_phase_non_identifier_application_head_still_binds_head_subtree_names() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("non_identifier_application_head_binding.m")
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
    let malformed_pattern = vm.heap.apply_ref(malformed_head, y.into());
    let lambda_body = vm.heap.lambda_ref(malformed_pattern, xs.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::NonIdentifierApplicationHeadsInFormals { count: 1 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn typecheck_phase_symbolic_infix_pattern_still_binds_interior_names() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("symbolic_infix_pattern_binding.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let less = vm.heap.apply_ref(Combinator::C.into(), Combinator::Gr.into());
    let less_equal = vm.heap.apply_ref(Combinator::C.into(), Combinator::Gre.into());
    let subscript = vm.heap.apply_ref(Combinator::C.into(), Combinator::Subscript.into());

    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let times_pattern = vm.heap.apply2(Combinator::Times.into(), x.into(), y.into());

    let a = vm.heap.make_empty_identifier("a");
    let b = vm.heap.make_empty_identifier("b");
    let divide_float_pattern = vm.heap.apply2(Combinator::DivideFloat.into(), a.into(), b.into());

    let c = vm.heap.make_empty_identifier("c");
    let d = vm.heap.make_empty_identifier("d");
    let divide_int_pattern = vm.heap.apply2(Combinator::DivideInteger.into(), c.into(), d.into());

    let e = vm.heap.make_empty_identifier("e");
    let f_name = vm.heap.make_empty_identifier("f_name");
    let remainder_pattern = vm.heap.apply2(Combinator::Remainder.into(), e.into(), f_name.into());

    let g = vm.heap.make_empty_identifier("g");
    let h = vm.heap.make_empty_identifier("h");
    let and_pattern = vm.heap.apply2(Combinator::And.into(), g.into(), h.into());

    let i = vm.heap.make_empty_identifier("i");
    let j = vm.heap.make_empty_identifier("j");
    let or_pattern = vm.heap.apply2(Combinator::Or.into(), i.into(), j.into());

    let k = vm.heap.make_empty_identifier("k");
    let l = vm.heap.make_empty_identifier("l");
    let not_equal_pattern = vm.heap.apply2(Combinator::NEq.into(), k.into(), l.into());

    let m = vm.heap.make_empty_identifier("m");
    let n = vm.heap.make_empty_identifier("n");
    let greater_pattern = vm.heap.apply2(Combinator::Gr.into(), m.into(), n.into());

    let o = vm.heap.make_empty_identifier("o");
    let p = vm.heap.make_empty_identifier("p");
    let greater_equal_pattern = vm.heap.apply2(Combinator::Gre.into(), o.into(), p.into());

    let q = vm.heap.make_empty_identifier("q");
    let r = vm.heap.make_empty_identifier("r");
    let less_pattern = vm.heap.apply2(less, q.into(), r.into());

    let s = vm.heap.make_empty_identifier("s");
    let t = vm.heap.make_empty_identifier("t");
    let less_equal_pattern = vm.heap.apply2(less_equal, s.into(), t.into());

    let u = vm.heap.make_empty_identifier("u");
    let v = vm.heap.make_empty_identifier("v");
    let compose_pattern = vm.heap.apply2(Combinator::B.into(), u.into(), v.into());

    let w = vm.heap.make_empty_identifier("w");
    let x1 = vm.heap.make_empty_identifier("x1");
    let subscript_pattern = vm.heap.apply2(subscript, w.into(), x1.into());

    for (index, pattern) in [
        times_pattern,
        divide_float_pattern,
        divide_int_pattern,
        remainder_pattern,
        and_pattern,
        or_pattern,
        not_equal_pattern,
        greater_pattern,
        greater_equal_pattern,
        less_pattern,
        less_equal_pattern,
        compose_pattern,
        subscript_pattern,
    ]
    .into_iter()
    .enumerate()
    {
        let function = vm.heap.make_empty_identifier(&format!("f{index}"));
        let lambda_body = vm.heap.lambda_ref(pattern, Value::Data(index as isize));
        function.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(lambda_body));
        current_file.push_item_onto_definienda(&mut vm.heap, function);
    }

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::NonIdentifierApplicationHeadsInFormals { count: 13 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn typecheck_phase_rejects_infix_name_application_in_value_head_bucket() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("infix_name_formal_head.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let merge = vm.heap.make_empty_identifier("merge");
    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let malformed_pattern = vm.heap.apply2(merge.into(), x.into(), y.into());
    let lambda_body = vm.heap.lambda_ref(malformed_pattern, x.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::ValueHeadApplicationsInFormals { count: 1 })
    ));
}

#[test]
fn typecheck_phase_infix_name_value_head_left_operand_still_binds_interior_names() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("infix_value_head_left_operand_binding.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let merge = vm.heap.make_empty_identifier("merge");
    let g = vm.heap.make_empty_identifier("g");
    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let left_operand = vm.heap.apply_ref(g.into(), x.into());
    let malformed_pattern = vm.heap.apply2(merge.into(), left_operand, y.into());
    let xy_body = vm.heap.pair_ref(x.into(), y.into());
    let body = vm.heap.pair_ref(g.into(), xy_body);
    let lambda_body = vm.heap.lambda_ref(malformed_pattern, body);
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::ValueHeadApplicationsInFormals { count: 1 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn typecheck_phase_infix_name_repeated_head_left_operand_still_binds_interior_names() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("infix_repeated_head_left_operand_binding.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let merge = vm.heap.make_empty_identifier("merge");
    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let z = vm.heap.make_empty_identifier("z");
    let left_operand = vm.heap.apply_ref(x.into(), y.into());
    let malformed_pattern = vm.heap.apply2(merge.into(), left_operand, z.into());
    let yz_body = vm.heap.pair_ref(y.into(), z.into());
    let body = vm.heap.pair_ref(x.into(), yz_body);
    let lambda_body = vm.heap.lambda_ref(malformed_pattern, body);
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::ValueHeadApplicationsInFormals { count: 1 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn typecheck_phase_invalid_application_formal_still_binds_interior_names() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("invalid_application_binding.m")
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
    let lambda_body = vm.heap.lambda_ref(malformed_pattern, x.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::ValueHeadApplicationsInFormals { count: 1 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn typecheck_phase_invalid_application_formal_still_binds_head_name() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("invalid_application_head_binding.m")
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
    let lambda_body = vm.heap.lambda_ref(malformed_pattern, g.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::ValueHeadApplicationsInFormals { count: 1 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn typecheck_phase_repeated_name_application_head_still_binds_argument_names() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("repeated_name_application_binding.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let repeated_head = vm.heap.cons_ref(Token::Constant.into(), x.into());
    let malformed_pattern = vm.heap.apply_ref(repeated_head, y.into());
    let lambda_body = vm.heap.lambda_ref(malformed_pattern, y.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::ValueHeadApplicationsInFormals { count: 1 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn typecheck_phase_unparenthesized_value_head_application_chain_still_binds_all_names() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("unparenthesized_value_head_application_chain_binding.m")
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
    let y = vm.heap.make_empty_identifier("y");
    let inner_application = vm.heap.apply_ref(g.into(), x.into());
    let malformed_pattern = vm.heap.apply_ref(inner_application, y.into());
    let xy_body = vm.heap.pair_ref(x.into(), y.into());
    let body = vm.heap.pair_ref(g.into(), xy_body);
    let lambda_body = vm.heap.lambda_ref(malformed_pattern, body);
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::ValueHeadApplicationsInFormals { count: 1 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn typecheck_phase_unparenthesized_repeated_name_application_chain_still_binds_argument_name() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("unparenthesized_repeated_name_application_chain_binding.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let repeated_head = vm.heap.cons_ref(Token::Constant.into(), x.into());
    let malformed_pattern = vm.heap.apply_ref(repeated_head, y.into());
    let body = vm.heap.pair_ref(x.into(), y.into());
    let lambda_body = vm.heap.lambda_ref(malformed_pattern, body);
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let outer_lambda = vm.heap.lambda_ref(x.into(), lambda_body);
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(outer_lambda),
    );

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::ValueHeadApplicationsInFormals { count: 1 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn typecheck_phase_unparenthesized_non_identifier_application_chain_still_binds_interior_names() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("unparenthesized_non_identifier_application_chain_binding.m")
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
    let malformed_pattern = vm.heap.apply_ref(malformed_head, y.into());
    let xs_y_body = vm.heap.pair_ref(xs.into(), y.into());
    let body = vm.heap.pair_ref(x.into(), xs_y_body);
    let lambda_body = vm.heap.lambda_ref(malformed_pattern, body);
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::NonIdentifierApplicationHeadsInFormals { count: 1 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn typecheck_phase_successor_pattern_binds_inner_name() {
    let mut vm = VM::new();

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
    let lambda_body = vm.heap.lambda_ref(successor_pattern, x.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(result.failure.is_none());
    assert!(result.undefined_names.is_empty());
}

#[test]
fn typecheck_phase_rejects_structural_inner_successor_pattern() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("successor_pattern_structural_inner.m")
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
    let one = IntegerRef::from_i64(&mut vm.heap, 1);
    let inner_pattern = vm.heap.cons_ref(x.into(), xs.into());
    let successor_pattern = vm
        .heap
        .apply2(Combinator::Plus.into(), one.into(), inner_pattern);
    let lambda_body = vm.heap.lambda_ref(successor_pattern, xs.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::InvalidSuccessorPatternsInFormals { count: 1 })
    ));
}

#[test]
fn typecheck_phase_invalid_successor_pattern_still_binds_interior_names() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("successor_pattern_structural_binding.m")
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
    let one = IntegerRef::from_i64(&mut vm.heap, 1);
    let inner_pattern = vm.heap.cons_ref(x.into(), xs.into());
    let successor_pattern = vm
        .heap
        .apply2(Combinator::Plus.into(), one.into(), inner_pattern);
    let lambda_body = vm.heap.lambda_ref(successor_pattern, xs.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::InvalidSuccessorPatternsInFormals { count: 1 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn typecheck_phase_successor_pattern_preserves_inner_value_head_diagnostic() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("successor_pattern_value_head_inner.m")
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
    let one = IntegerRef::from_i64(&mut vm.heap, 1);
    let inner_pattern = vm.heap.apply_ref(g.into(), x.into());
    let successor_pattern = vm
        .heap
        .apply2(Combinator::Plus.into(), one.into(), inner_pattern);
    let lambda_body = vm.heap.lambda_ref(successor_pattern, x.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::ValueHeadApplicationsInFormals { count: 1 })
    ));
}

#[test]
fn typecheck_phase_rejects_undeclared_constructor_inside_successor_pattern() {
    let mut vm = VM::new();

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
            .apply2(Combinator::Plus.into(), one.into(), inner_pattern);
    let lambda_body = vm.heap.lambda_ref(successor_pattern, x.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::UndeclaredConstructorsInFormals { count: 1 })
    ));
}

#[test]
fn typecheck_phase_rejects_non_canonical_plus_pattern_in_formal() {
    let mut vm = VM::new();

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
    let lambda_body = vm.heap.lambda_ref(arithmetic_pattern, x.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::NonCanonicalPlusPatternsInFormals { count: 1 })
    ));
}

#[test]
fn typecheck_phase_non_canonical_plus_pattern_still_binds_interior_names() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("unsupported_arithmetic_binding.m")
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
    let lambda_body = vm.heap.lambda_ref(arithmetic_pattern, x.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::NonCanonicalPlusPatternsInFormals { count: 1 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn typecheck_phase_rejects_unary_minus_pattern_in_formal() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("unsupported_minus_formal.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let x = vm.heap.make_empty_identifier("x");
    let arithmetic_pattern = vm.heap.apply_ref(Combinator::Minus.into(), x.into());
    let lambda_body = vm.heap.lambda_ref(arithmetic_pattern, x.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::UnaryMinusPatternsInFormals { count: 1 })
    ));
}

#[test]
fn typecheck_phase_unary_minus_pattern_still_binds_interior_names() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("unsupported_minus_binding.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let x = vm.heap.make_empty_identifier("x");
    let arithmetic_pattern = vm.heap.apply_ref(Combinator::Minus.into(), x.into());
    let lambda_body = vm.heap.lambda_ref(arithmetic_pattern, x.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::UnaryMinusPatternsInFormals { count: 1 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn typecheck_phase_rejects_malformed_plus_application_in_formal() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("malformed_plus_application_formal.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let one = IntegerRef::from_i64(&mut vm.heap, 1);
    let partial_pattern = vm.heap.apply2(Combinator::Plus.into(), one.into(), x.into());
    let malformed_pattern = vm.heap.apply_ref(partial_pattern, y.into());
    let lambda_body = vm.heap.lambda_ref(malformed_pattern, y.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::MalformedPlusApplicationsInFormals { count: 1 })
    ));
}

#[test]
fn typecheck_phase_malformed_plus_application_still_binds_all_pattern_operands() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("malformed_plus_application_binding.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let one = IntegerRef::from_i64(&mut vm.heap, 1);
    let partial_pattern = vm.heap.apply2(Combinator::Plus.into(), one.into(), x.into());
    let malformed_pattern = vm.heap.apply_ref(partial_pattern, y.into());
    let lambda_body = vm.heap.lambda_ref(malformed_pattern, y.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::MalformedPlusApplicationsInFormals { count: 1 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn typecheck_phase_non_canonical_plus_still_reports_nested_constructor_misuse() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("non_canonical_plus_nested_constructor_formal.m")
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
    let y = vm.heap.make_empty_identifier("y");
    let constructor_operand = vm.heap.apply_ref(nope.into(), x.into());
    let arithmetic_pattern = vm.heap.apply2(Combinator::Plus.into(), constructor_operand, y.into());
    let lambda_body = vm.heap.lambda_ref(arithmetic_pattern, y.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::UndeclaredConstructorsInFormals { count: 1 })
    ));
}

#[test]
fn typecheck_phase_malformed_plus_application_still_reports_nested_constructor_misuse() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("malformed_plus_nested_constructor_formal.m")
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
    let y = vm.heap.make_empty_identifier("y");
    let z = vm.heap.make_empty_identifier("z");
    let constructor_operand = vm.heap.apply_ref(nope.into(), x.into());
    let partial_pattern = vm
        .heap
        .apply2(Combinator::Plus.into(), constructor_operand, y.into());
    let malformed_pattern = vm.heap.apply_ref(partial_pattern, z.into());
    let lambda_body = vm.heap.lambda_ref(malformed_pattern, z.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::UndeclaredConstructorsInFormals { count: 1 })
    ));
}

#[test]
fn typecheck_phase_malformed_plus_application_keeps_repeated_name_head_non_binding() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("malformed_plus_repeated_head_binding.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let z = vm.heap.make_empty_identifier("z");
    let w = vm.heap.make_empty_identifier("w");
    let repeated_head = vm.heap.cons_ref(Token::Constant.into(), x.into());
    let repeated_head_application = vm.heap.apply_ref(repeated_head, y.into());
    let partial_pattern = vm
        .heap
        .apply2(Combinator::Plus.into(), repeated_head_application, z.into());
    let malformed_pattern = vm.heap.apply_ref(partial_pattern, w.into());
    let lambda_body = vm.heap.lambda_ref(malformed_pattern, x.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::MalformedPlusApplicationsInFormals { count: 1 })
    ));
    assert_eq!(result.undefined_names.len(&vm.heap), 1);
}

#[test]
fn typecheck_phase_rejects_binary_minus_pattern_in_formal() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("binary_minus_formal.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let malformed_pattern = vm.heap.apply2(Combinator::Minus.into(), x.into(), y.into());
    let lambda_body = vm.heap.lambda_ref(malformed_pattern, y.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let result = vm.run_checktypes_phase();

    assert!(matches!(
        result,
        Err(TypecheckError::MalformedMinusApplicationsInFormals {
            count: 1
        })
    ));
}

#[test]
fn typecheck_phase_binary_minus_pattern_still_binds_both_operands() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("binary_minus_binding.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let malformed_pattern = vm.heap.apply2(Combinator::Minus.into(), x.into(), y.into());
    let lambda_body = vm.heap.lambda_ref(malformed_pattern, y.into());
    f.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(lambda_body),
    );
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::MalformedMinusApplicationsInFormals { count: 1 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn typecheck_phase_rejects_undeclared_constructor_in_local_pattern_definition() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("local_pattern_undeclared_constructor.m")
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
    let local_pattern = vm.heap.apply_ref(nope.into(), x.into());
    let local_definition = DefinitionRef::new(&mut vm.heap, local_pattern, Type::Undefined.into(), x.into());
    let definitions = vm.heap.cons_ref(local_definition.into(), NIL);
    let letrec_body = vm.heap.letrec_ref(definitions, x.into());
    f.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(letrec_body));
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::UndeclaredConstructorsInFormals { count: 1 })
    ));
    assert_eq!(result.undefined_names.len(&vm.heap), 1);
}

#[test]
fn typecheck_phase_rejects_invalid_successor_pattern_in_local_definition_and_preserves_binding() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("local_pattern_invalid_successor.m")
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
    let one = IntegerRef::from_i64(&mut vm.heap, 1);
    let inner_pattern = vm.heap.cons_ref(x.into(), xs.into());
    let local_pattern = vm
        .heap
        .apply2(Combinator::Plus.into(), one.into(), inner_pattern);
    let local_definition = DefinitionRef::new(&mut vm.heap, local_pattern, Type::Undefined.into(), xs.into());
    let definitions = vm.heap.cons_ref(local_definition.into(), NIL);
    let letrec_body = vm.heap.letrec_ref(definitions, xs.into());
    f.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(letrec_body));
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::InvalidSuccessorPatternsInFormals { count: 1 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn typecheck_phase_rejects_symbolic_infix_pattern_in_local_definition_and_preserves_binding() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("local_pattern_symbolic_infix.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let a = vm.heap.make_empty_identifier("a");
    let b = vm.heap.make_empty_identifier("b");
    let c = vm.heap.make_empty_identifier("c");
    let d = vm.heap.make_empty_identifier("d");
    let e = vm.heap.make_empty_identifier("e");
    let g = vm.heap.make_empty_identifier("g");
    let h = vm.heap.make_empty_identifier("h");
    let append_pattern = vm.heap.apply2(Combinator::Append.into(), x.into(), y.into());
    let divide_pattern = vm.heap.apply2(Combinator::DivideInteger.into(), a.into(), b.into());
    let and_pattern = vm.heap.apply2(Combinator::And.into(), c.into(), d.into());
    let compose_pattern = vm.heap.apply2(Combinator::B.into(), e.into(), g.into());
    let subscript_operator = vm.heap.apply_ref(Combinator::C.into(), Combinator::Subscript.into());
    let subscript_pattern = vm.heap.apply2(subscript_operator, h.into(), x.into());
    let append_definition = DefinitionRef::new(&mut vm.heap, append_pattern, Type::Undefined.into(), y.into());
    let divide_definition = DefinitionRef::new(&mut vm.heap, divide_pattern, Type::Undefined.into(), b.into());
    let and_definition = DefinitionRef::new(&mut vm.heap, and_pattern, Type::Undefined.into(), d.into());
    let compose_definition = DefinitionRef::new(&mut vm.heap, compose_pattern, Type::Undefined.into(), g.into());
    let subscript_definition = DefinitionRef::new(&mut vm.heap, subscript_pattern, Type::Undefined.into(), x.into());
    let definition_tail = vm.heap.cons_ref(subscript_definition.into(), NIL);
    let definition_tail = vm.heap.cons_ref(compose_definition.into(), definition_tail);
    let definition_tail = vm.heap.cons_ref(and_definition.into(), definition_tail);
    let definition_tail = vm.heap.cons_ref(divide_definition.into(), definition_tail);
    let definitions = vm.heap.cons_ref(append_definition.into(), definition_tail);
    let letrec_body = vm.heap.letrec_ref(definitions, y.into());
    f.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(letrec_body));
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::NonIdentifierApplicationHeadsInFormals { count: 5 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn typecheck_phase_rejects_malformed_plus_application_in_local_definition_and_preserves_binding() {
    let mut vm = VM::new();

    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("local_pattern_malformed_plus.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        ConsList::EMPTY,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let f = vm.heap.make_empty_identifier("f");
    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let one = IntegerRef::from_i64(&mut vm.heap, 1);
    let partial_pattern = vm.heap.apply2(Combinator::Plus.into(), one.into(), x.into());
    let local_pattern = vm.heap.apply_ref(partial_pattern, y.into());
    let local_definition = DefinitionRef::new(&mut vm.heap, local_pattern, Type::Undefined.into(), y.into());
    let definitions = vm.heap.cons_ref(local_definition.into(), NIL);
    let letrec_body = vm.heap.letrec_ref(definitions, y.into());
    f.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(letrec_body));
    current_file.push_item_onto_definienda(&mut vm.heap, f);

    let inputs = typecheck::TypecheckBoundaryInputs::from_vm(&vm);
    let result = typecheck::run_partial_typecheck(&mut vm.heap, inputs);

    assert!(matches!(
        &result.failure,
        Some(TypecheckError::MalformedPlusApplicationsInFormals { count: 1 })
    ));
    assert!(result.undefined_names.is_empty());
}

#[test]
fn export_closure_phase_is_noop_without_exports() {
    let mut vm = VM::new();

    let result = vm.run_export_closure_phase_partial(
        Some(&ParserTopLevelDirectivePayload::default()),
        ConsList::EMPTY,
    );

    assert!(result.is_ok());
}

#[test]
fn export_closure_phase_clears_exports_when_undefined_names_present() {
    let mut vm = VM::new();
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
    let mut vm = VM::new();
    vm.undefined_names = ConsList::EMPTY;
    let exported_name = vm.heap.make_empty_identifier("exported_name");
    let current_defs = ConsList::new(&mut vm.heap, exported_name);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("exported_name.m").to_string_lossy().to_string(),
        UNIX_EPOCH,
        false,
        current_defs,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
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
fn export_closure_phase_rejects_explicit_export_name_not_in_definienda() {
    let mut vm = VM::new();
    vm.undefined_names = ConsList::EMPTY;
    let current_name = vm.heap.make_empty_identifier("defined_here");
    let current_defs = ConsList::new(&mut vm.heap, current_name);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("defined_here.m").to_string_lossy().to_string(),
        UNIX_EPOCH,
        false,
        current_defs,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    let payload = export_payload(
        &mut vm,
        "defined_here.m",
        1,
        &["missing_export"],
        &[],
        false,
        &[],
    );

    let result = vm.run_export_closure_phase_partial(Some(&payload), ConsList::EMPTY);

    assert!(matches!(
        result,
        Err(ExportValidationError::UndefinedExportedIdentifier { name }) if name == "missing_export"
    ));
}

#[test]
fn export_closure_phase_accepts_explicit_export_name_from_materialized_include() {
    let mut vm = VM::new();
    vm.undefined_names = ConsList::EMPTY;
    let main_name = vm.heap.make_empty_identifier("main_name");
    let current_defs = ConsList::new(&mut vm.heap, main_name);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("main_export_driver.m").to_string_lossy().to_string(),
        UNIX_EPOCH,
        false,
        current_defs,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let included_name = vm.heap.make_empty_identifier("included_name");
    let include_defs = ConsList::new(&mut vm.heap, included_name);
    let includee = FileRecord::new(
        &mut vm.heap,
        unique_test_path("included_export_target.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        include_defs,
    );
    let materialized_includees = ConsList::new(&mut vm.heap, includee);

    let payload = export_payload(
        &mut vm,
        "main_export_driver.m",
        1,
        &["included_name"],
        &[],
        false,
        &[],
    );

    let result = vm.run_export_closure_phase_partial(Some(&payload), materialized_includees);

    let committed = result.expect("expected include-backed explicit export to succeed");
    let exported = ConsList::<IdentifierRecordRef>::from_ref(committed.exported_identifiers.into())
        .head(&vm.heap)
        .expect("expected committed export id");
    assert_eq!(vm.identifier_name(exported), "included_name");
}

#[test]
fn export_closure_phase_deduplicates_explicit_and_path_expanded_exports() {
    let mut vm = VM::new();
    vm.undefined_names = ConsList::EMPTY;
    let current_name = vm.heap.make_empty_identifier("foo");
    let current_defs = ConsList::new(&mut vm.heap, current_name);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("duplicate_export_driver.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        current_defs,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);

    let include_path = unique_test_path("duplicate_export_include.m");
    let include_path_str = include_path.to_string_lossy().to_string();
    let include_defs = ConsList::new(&mut vm.heap, current_name);
    let includee = FileRecord::new(
        &mut vm.heap,
        include_path_str.clone(),
        UNIX_EPOCH,
        false,
        include_defs,
    );
    let materialized_includees = ConsList::new(&mut vm.heap, includee);
    let payload = export_payload(
        &mut vm,
        "duplicate_export_driver.m",
        1,
        &["foo"],
        &[&include_path_str],
        false,
        &[],
    );

    let result = vm.run_export_closure_phase_partial(Some(&payload), materialized_includees);

    let committed = result.expect("expected duplicate exports to collapse");
    assert_eq!(
        ConsList::<IdentifierRecordRef>::from_ref(committed.exported_identifiers.into())
            .len(&vm.heap),
        1
    );
}

#[test]
fn export_closure_phase_preserves_embargo_filtering_after_explicit_validation() {
    let mut vm = VM::new();
    vm.undefined_names = ConsList::EMPTY;
    let exported_name = vm.heap.make_empty_identifier("exported_name");
    let current_defs = ConsList::new(&mut vm.heap, exported_name);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("embargoed_export_driver.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        current_defs,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    let payload = export_payload(
        &mut vm,
        "embargoed_export_driver.m",
        1,
        &["exported_name"],
        &[],
        false,
        &["exported_name"],
    );

    let result = vm.run_export_closure_phase_partial(Some(&payload), ConsList::EMPTY);

    let committed = result.expect("expected embargoed export path to succeed");
    assert_eq!(committed.exported_identifiers, NIL);
}

#[test]
fn export_closure_phase_adds_constructors_for_exported_algebraic_typename() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("export_closure_adds_algebraic_constructors.m");
    std::fs::write(
        &source_path,
        "maybe * ::= Nothing | Just *\n%export maybe\n",
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let exported = ConsList::<IdentifierRecordRef>::from_ref(vm.exported_identifiers.into());
    let maybe = vm.heap.get_identifier("maybe").expect("expected maybe typename");
    let nothing = vm.heap.get_identifier("Nothing").expect("expected Nothing constructor");
    let just = vm.heap.get_identifier("Just").expect("expected Just constructor");
    assert!(exported.contains(&vm.heap, maybe));
    assert!(exported.contains(&vm.heap, nothing));
    assert!(exported.contains(&vm.heap, just));
}

#[test]
fn export_closure_phase_adds_same_load_definition_dependency() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("export_closure_definition_dependency.m");
    std::fs::write(&source_path, "helper = 1\nentry = helper\n%export entry\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let exported = ConsList::<IdentifierRecordRef>::from_ref(vm.exported_identifiers.into());
    let entry = vm.heap.get_identifier("entry").expect("expected entry identifier");
    let helper = vm.heap.get_identifier("helper").expect("expected helper identifier");
    assert!(exported.contains(&vm.heap, entry));
    assert!(exported.contains(&vm.heap, helper));
}

#[test]
fn export_closure_phase_path_expansion_adds_include_constructors_and_type_dependencies() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("export_closure_include_target.m");
    std::fs::write(
        &include_path,
        "thing == num\nmaybe ::= Nothing | Just thing\n",
    )
    .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("export_closure_include_driver.m");
    std::fs::write(
        &source_path,
        format!("%include \"{}\"\n%export \"{}\"\n", include_path_str, include_path_str),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let exported = ConsList::<IdentifierRecordRef>::from_ref(vm.exported_identifiers.into());
    let maybe = vm.heap.get_identifier("maybe").expect("expected maybe typename");
    let nothing = vm.heap.get_identifier("Nothing").expect("expected Nothing constructor");
    let just = vm.heap.get_identifier("Just").expect("expected Just constructor");
    let thing = vm.heap.get_identifier("thing").expect("expected thing typename");
    assert!(exported.contains(&vm.heap, maybe));
    assert!(exported.contains(&vm.heap, nothing));
    assert!(exported.contains(&vm.heap, just));
    assert!(exported.contains(&vm.heap, thing));
}

#[test]
fn export_closure_phase_embargo_filters_constructed_closure_members_after_closure() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("export_closure_embargoed_constructor_member.m");
    std::fs::write(&source_path, "thing == num\nmaybe ::= Nothing | Just thing\n")
        .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let load_result = vm.load_file(&source_path_str);
    assert!(
        load_result.is_ok(),
        "result={load_result:?} diagnostics={:?}",
        vm.parser_diagnostics
    );

    let payload = export_payload(
        &mut vm,
        &source_path_str,
        1,
        &["maybe"],
        &[],
        false,
        &["Just", "thing"],
    );
    let result = vm.run_export_closure_phase_partial(Some(&payload), ConsList::EMPTY);

    let committed = result.expect("expected embargoed constructor closure to succeed");
    let exported = ConsList::<IdentifierRecordRef>::from_ref(committed.exported_identifiers.into());
    let maybe = vm.heap.get_identifier("maybe").expect("expected maybe typename");
    let nothing = vm.heap.get_identifier("Nothing").expect("expected Nothing constructor");
    let just = vm.heap.get_identifier("Just").expect("expected Just constructor");
    let thing = vm.heap.get_identifier("thing").expect("expected thing typename");
    assert!(exported.contains(&vm.heap, maybe));
    assert!(exported.contains(&vm.heap, nothing));
    assert!(!exported.contains(&vm.heap, just));
    assert!(!exported.contains(&vm.heap, thing));
}

#[test]
fn export_closure_phase_clears_bereaved_risk_when_closed_export_keeps_required_typename() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("bereaved_risk_closed_export_ok.m");
    std::fs::write(
        &source_path,
        "thing == num\nid :: thing -> thing\nid x = x\n%export id\n",
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    assert!(!vm.unused_types);
}

#[test]
fn export_closure_phase_sets_bereaved_risk_when_embargo_removes_required_typename() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("bereaved_risk_embargoed_typename.m");
    std::fs::write(
        &source_path,
        "thing == num\nid :: thing -> thing\nid x = x\n%export id - thing\n",
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    assert!(vm.unused_types);
}

#[test]
fn bereaved_analysis_free_type_binding_can_keep_required_typename_alive() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("bereaved_free_type_binding_keeps_typename.m");
    std::fs::write(
        &source_path,
        "%free { t :: type; id :: t -> t }\nid x = x\n%export id - t\n",
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    assert!(!vm.unused_types);
}

#[test]
fn bereaved_analysis_include_backed_load_without_explicit_export_still_flags_risk() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("bereaved_include_backed_target.m");
    std::fs::write(&include_path, "helper = 1\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("bereaved_include_backed_driver.m");
    std::fs::write(
        &source_path,
        format!(
            "thing == num\nid :: thing -> thing\nid x = x\n%include \"{}\"\n",
            include_path_str
        ),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    assert!(vm.exported_identifiers == NIL);
    assert!(vm.unused_types);
}

#[test]
fn bereaved_analysis_include_and_free_binding_basis_can_close_risk() {
    let mut vm = VM::new();
    vm.initializing = false;

    let include_path = unique_test_path("bereaved_include_and_free_target.m");
    std::fs::write(&include_path, "helper = 1\n")
        .expect("failed to write include source test file");
    let include_path_str = include_path.to_string_lossy().to_string();

    let source_path = unique_test_path("bereaved_include_and_free_driver.m");
    std::fs::write(
        &source_path,
        format!("%free {{ t :: type; id :: t -> t }}\nid x = x\n%include \"{}\"\n", include_path_str),
    )
    .expect("failed to write source test file");
    let source_path_str = source_path.to_string_lossy().to_string();

    let result = vm.load_file(&source_path_str);

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    assert!(!vm.unused_types);
}

#[test]
fn bereaved_warning_phase_is_noop_without_risk_flag() {
    let mut vm = VM::new();
    vm.exported_identifiers = NIL;
    vm.unused_types = false;

    vm.emit_bereaved_warnings_partial();
    assert!(!vm.unused_types);
}

#[test]
fn bereaved_warning_phase_returns_ok_when_risk_flagged() {
    let mut vm = VM::new();
    let export_id = vm.heap.make_empty_identifier("exported_name");
    vm.exported_identifiers = vm.heap.cons_ref(export_id.into(), NIL);
    vm.unused_types = true;

    vm.emit_bereaved_warnings_partial();
    assert!(vm.unused_types);
    assert_ne!(vm.exported_identifiers, NIL);
}

#[test]
fn unused_diagnostics_phase_clears_deferred_marker() {
    let mut vm = VM::new();
    vm.empty_production_nonterminals = Combinator::True.into();

    vm.emit_unused_definition_diagnostics_partial();
    assert_eq!(vm.empty_production_nonterminals, NIL);
}

#[test]
fn unused_diagnostics_phase_is_stable_when_marker_absent() {
    let mut vm = VM::new();
    vm.empty_production_nonterminals = NIL;

    vm.emit_unused_definition_diagnostics_partial();
    assert_eq!(vm.empty_production_nonterminals, NIL);
}

#[test]
fn codegen_phase_fails_without_loaded_files() {
    let mut vm = VM::new();
    vm.files = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(matches!(result, Err(CodegenError::NoLoadedFiles)));
}

#[test]
fn codegen_phase_fails_during_initialization_with_unresolved_names() {
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
fn codegen_phase_rewrites_plain_top_level_label_wrapped_body() {
    let mut vm = VM::new();
    vm.initializing = false;

    let value_id = vm.heap.make_empty_identifier("value_id");
    let one = IntegerRef::from_i64(&mut vm.heap, 1);
    let anchor = test_anchor(&mut vm, "codegen_plain_label_body.m", 1);
    let labeled_body = vm.heap.label_ref(anchor.into(), one.into());
    value_id.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(labeled_body),
    );

    let definienda = ConsList::new(&mut vm.heap, value_id);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_plain_label_body.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    assert_eq!(
        value_id
            .get_value(&vm.heap)
            .expect("expected rewritten value")
            .get_data(&vm.heap),
        IdentifierValueData::Arbitrary(one.into())
    );
}

#[test]
fn codegen_phase_rewrites_lambda_headed_function_body() {
    let mut vm = VM::new();
    vm.initializing = false;

    let xs = vm.heap.make_empty_identifier("xs");
    let keep = vm.heap.make_empty_identifier("keep");
    let anchor = test_anchor(&mut vm, "codegen_lambda_label_body.m", 1);
    let labeled_body = vm.heap.label_ref(anchor.into(), xs.into());
    let lambda = vm.heap.lambda_ref(xs.into(), labeled_body);
    let original_lambda = lambda;
    keep.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(lambda));

    let definienda = ConsList::new(&mut vm.heap, keep);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_lambda_label_body.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered_raw = keep
        .get_value(&vm.heap)
        .expect("expected keep value")
        .get_ref();
    assert_ne!(lowered_raw, RawValue::from(original_lambda));
    assert_ne!(lowered_raw, RawValue::from(Combinator::Undef));
}

#[test]
fn codegen_phase_normalizes_pair_body_into_cons() {
    let mut vm = VM::new();
    vm.initializing = false;

    let value_id = vm.heap.make_empty_identifier("pair_value");
    let one = IntegerRef::from_i64(&mut vm.heap, 1);
    let two = IntegerRef::from_i64(&mut vm.heap, 2);
    let pair_body = vm.heap.pair_ref(one.into(), two.into());
    value_id.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(pair_body));

    let definienda = ConsList::new(&mut vm.heap, value_id);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_pair_body.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered_raw = value_id
        .get_value(&vm.heap)
        .expect("expected rewritten value")
        .get_ref();
    assert_eq!(vm.heap[lowered_raw].tag, Tag::Cons);
    assert_eq!(vm.heap[lowered_raw].head, one.get_ref());
    assert_eq!(vm.heap[lowered_raw].tail, two.get_ref());
}

#[test]
fn codegen_phase_normalizes_tcons_body_into_cons() {
    let mut vm = VM::new();
    vm.initializing = false;

    let value_id = vm.heap.make_empty_identifier("tcons_value");
    let one = IntegerRef::from_i64(&mut vm.heap, 1);
    let two = IntegerRef::from_i64(&mut vm.heap, 2);
    let tcons_body = vm.heap.tcons_ref(one.into(), two.into());
    value_id.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(tcons_body));

    let definienda = ConsList::new(&mut vm.heap, value_id);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_tcons_body.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered_raw = value_id
        .get_value(&vm.heap)
        .expect("expected rewritten value")
        .get_ref();
    assert_eq!(vm.heap[lowered_raw].tag, Tag::Cons);
    assert_eq!(vm.heap[lowered_raw].head, one.get_ref());
    assert_eq!(vm.heap[lowered_raw].tail, two.get_ref());
}

#[test]
fn codegen_phase_lowers_let_body_through_translet_shape() {
    let mut vm = VM::new();
    vm.initializing = false;

    let x = vm.heap.make_empty_identifier("x");
    let holder = vm.heap.make_empty_identifier("holder");
    let definition = DefinitionRef::new(&mut vm.heap, x.into(), Type::Undefined.into(), Value::Data(1));
    let let_body = vm.heap.let_ref(definition.get_ref().into(), x.into());
    holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(let_body));

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_let_body.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered_raw = holder
        .get_value(&vm.heap)
        .expect("expected lowered let body")
        .get_ref();
    assert_eq!(vm.heap[lowered_raw].tag, Tag::Ap);
    assert_eq!(vm.heap[lowered_raw].head, RawValue::from(Combinator::I));
    assert_eq!(vm.heap[lowered_raw].tail, 1);
}

#[test]
fn codegen_phase_lowers_nonrecursive_simple_letrec_without_y() {
    let mut vm = VM::new();
    vm.initializing = false;

    let x = vm.heap.make_empty_identifier("x");
    let holder = vm.heap.make_empty_identifier("holder");
    let definition = DefinitionRef::new(&mut vm.heap, x.into(), Type::Undefined.into(), Value::Data(1));
    let definitions = vm.heap.cons_ref(definition.get_ref().into(), Combinator::Nil.into());
    let letrec_body = vm.heap.letrec_ref(definitions, x.into());
    holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(letrec_body));

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_nonrecursive_letrec_body.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered = holder
        .get_value(&vm.heap)
        .expect("expected lowered nonrecursive letrec body")
        .get_ref()
        .into();
    assert!(!subtree_contains_combinator(&vm.heap, lowered, Combinator::Y));
}

#[test]
fn codegen_phase_lowers_letrec_singleton_body_through_y() {
    let mut vm = VM::new();
    vm.initializing = false;

    let x = vm.heap.make_empty_identifier("x");
    let holder = vm.heap.make_empty_identifier("holder");
    let definition = DefinitionRef::new(&mut vm.heap, x.into(), Type::Undefined.into(), x.into());
    let definitions = vm.heap.cons_ref(definition.get_ref().into(), Combinator::Nil.into());
    let letrec_body = vm.heap.letrec_ref(definitions, x.into());
    holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(letrec_body));

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_recursive_letrec_body.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered_raw = holder
        .get_value(&vm.heap)
        .expect("expected lowered letrec body")
        .get_ref();
    assert_eq!(vm.heap[lowered_raw].tag, Tag::Ap);
    assert_eq!(vm.heap[lowered_raw].head, RawValue::from(Combinator::I));
    let recursive_rhs = vm.heap[lowered_raw].tail;
    assert_eq!(vm.heap[recursive_rhs].tag, Tag::Ap);
    assert_eq!(vm.heap[recursive_rhs].head, RawValue::from(Combinator::Y));
}

#[test]
fn codegen_phase_lowers_simple_mutual_recursion_through_y() {
    let mut vm = VM::new();
    vm.initializing = false;

    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let holder = vm.heap.make_empty_identifier("holder");
    let x_definition = DefinitionRef::new(&mut vm.heap, x.into(), Type::Undefined.into(), y.into());
    let y_definition = DefinitionRef::new(&mut vm.heap, y.into(), Type::Undefined.into(), x.into());
    let y_tail = vm.heap.cons_ref(y_definition.get_ref().into(), Combinator::Nil.into());
    let definitions = vm.heap.cons_ref(x_definition.get_ref().into(), y_tail);
    let letrec_body = vm.heap.letrec_ref(definitions, x.into());
    holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(letrec_body));

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_mutual_simple_letrec_body.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered = holder
        .get_value(&vm.heap)
        .expect("expected lowered mutually recursive letrec body")
        .get_ref()
        .into();
    assert!(subtree_contains_combinator(&vm.heap, lowered, Combinator::Y));
}

#[test]
fn codegen_phase_partitions_simple_dependency_chain_without_recursive_bundle() {
    let mut vm = VM::new();
    vm.initializing = false;

    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let holder = vm.heap.make_empty_identifier("holder");
    let x_definition = DefinitionRef::new(&mut vm.heap, x.into(), Type::Undefined.into(), y.into());
    let y_definition = DefinitionRef::new(&mut vm.heap, y.into(), Type::Undefined.into(), Value::Data(1));
    let y_tail = vm.heap.cons_ref(y_definition.get_ref().into(), Combinator::Nil.into());
    let definitions = vm.heap.cons_ref(x_definition.get_ref().into(), y_tail);
    let letrec_body = vm.heap.letrec_ref(definitions, x.into());
    holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(letrec_body));

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_simple_dependency_chain_body.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered = holder
        .get_value(&vm.heap)
        .expect("expected lowered dependency chain body")
        .get_ref()
        .into();
    assert!(!subtree_contains_combinator(&vm.heap, lowered, Combinator::Y));
}

#[test]
fn codegen_phase_lowers_pattern_letrec_body_through_recursive_carrier_and_subscripts() {
    let mut vm = VM::new();
    vm.initializing = false;

    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let holder = vm.heap.make_empty_identifier("holder");
    let binder = vm.heap.pair_ref(x.into(), y.into());
    let body = vm.heap.cons_ref(x.into(), y.into());
    let definition = DefinitionRef::new(&mut vm.heap, binder, Type::Undefined.into(), body);
    let definitions = vm.heap.cons_ref(definition.get_ref().into(), Combinator::Nil.into());
    let letrec_body = vm.heap.letrec_ref(definitions, x.into());
    holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(letrec_body));

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_pattern_letrec_body.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered_raw = holder
        .get_value(&vm.heap)
        .expect("expected lowered patterned letrec body")
        .get_ref();
    assert_eq!(vm.heap[lowered_raw].tag, Tag::Ap);
    let recursive_rhs = vm.heap[lowered_raw].tail;
    assert_eq!(vm.heap[recursive_rhs].tag, Tag::Ap);
    assert_eq!(vm.heap[recursive_rhs].head, RawValue::from(Combinator::Y));

    let abstraction_result = vm.heap[recursive_rhs].tail;
    let mut subscript_indices = Vec::new();
    collect_subscript_indices(&vm.heap, abstraction_result.into(), &mut subscript_indices);
    subscript_indices.sort();
    assert_eq!(subscript_indices, vec![0, 1]);
}

#[test]
fn codegen_phase_lowers_mixed_letrec_group_with_pattern_projection_order() {
    let mut vm = VM::new();
    vm.initializing = false;

    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let z = vm.heap.make_empty_identifier("z");
    let holder = vm.heap.make_empty_identifier("holder");
    let binder = vm.heap.pair_ref(x.into(), y.into());
    let patterned_definition = DefinitionRef::new(&mut vm.heap, binder, Type::Undefined.into(), z.into());
    let simple_definition = DefinitionRef::new(&mut vm.heap, z.into(), Type::Undefined.into(), Value::Data(7));
    let simple_tail = vm.heap.cons_ref(simple_definition.get_ref().into(), Combinator::Nil.into());
    let definitions = vm.heap.cons_ref(patterned_definition.get_ref().into(), simple_tail);
    let body = vm.heap.cons_ref(x.into(), z.into());
    let letrec_body = vm.heap.letrec_ref(definitions, body);
    holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(letrec_body));

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_mixed_letrec_body.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered = holder
        .get_value(&vm.heap)
        .expect("expected lowered mixed letrec body")
        .get_ref()
        .into();
    let mut subscript_indices = Vec::new();
    collect_subscript_indices(&vm.heap, lowered, &mut subscript_indices);
    subscript_indices.sort();
    assert!(subscript_indices.is_empty());
    assert!(!subtree_contains_combinator(&vm.heap, lowered, Combinator::Y));
}

#[test]
fn codegen_phase_lowers_tries_with_fallible_first_case_to_try_and_badcase() {
    let mut vm = VM::new();
    vm.initializing = false;

    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let holder = vm.heap.make_empty_identifier("holder");
    let tuple_binder = vm.heap.pair_ref(x.into(), y.into());
    let fallible = vm.heap.lambda_ref(tuple_binder, x.into());
    let alternatives = vm.heap.cons_ref(fallible, Combinator::Nil.into());
    let tries_body = vm.heap.tries_ref(holder.into(), alternatives);
    holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(tries_body));

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_tries_body.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered_raw = holder
        .get_value(&vm.heap)
        .expect("expected lowered tries body")
        .get_ref();
    assert_eq!(vm.heap[lowered_raw].tag, Tag::Ap);
    let try_head = vm.heap[lowered_raw].head;
    assert_eq!(vm.heap[try_head].tag, Tag::Ap);
    assert_eq!(vm.heap[try_head].head, RawValue::from(Combinator::Try));
    let fallback_raw = vm.heap[lowered_raw].tail;
    assert_eq!(vm.heap[fallback_raw].tag, Tag::Ap);
    assert_eq!(vm.heap[fallback_raw].head, RawValue::from(Combinator::BadCase));
}

#[test]
fn codegen_phase_lowers_identity_lambda_to_i() {
    let mut vm = VM::new();
    vm.initializing = false;

    let x = vm.heap.make_empty_identifier("x");
    let id = vm.heap.make_empty_identifier("id");
    let lambda = vm.heap.lambda_ref(x.into(), x.into());
    id.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(lambda));

    let definienda = ConsList::new(&mut vm.heap, id);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_identity_lambda.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    assert_eq!(
        id.get_value(&vm.heap)
            .expect("expected lowered identity body")
            .get_ref(),
        RawValue::from(Combinator::I)
    );
}

#[test]
fn codegen_phase_lowers_tuple_pattern_lambda_to_u_application() {
    let mut vm = VM::new();
    vm.initializing = false;

    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let fst = vm.heap.make_empty_identifier("fst");
    let binder = vm.heap.pair_ref(x.into(), y.into());
    let lambda = vm.heap.lambda_ref(binder, x.into());
    fst.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(lambda));

    let definienda = ConsList::new(&mut vm.heap, fst);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_tuple_pattern_lambda.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered_raw = fst
        .get_value(&vm.heap)
        .expect("expected lowered tuple-pattern body")
        .get_ref();
    assert_eq!(vm.heap[lowered_raw].tag, Tag::Ap);
    assert_eq!(vm.heap[lowered_raw].head, RawValue::from(Combinator::U));
}

#[test]
fn codegen_phase_lowers_shared_top_level_body_to_shared_payload() {
    let mut vm = VM::new();
    vm.initializing = false;

    let payload = vm.heap.make_empty_identifier("payload");
    let holder = vm.heap.make_empty_identifier("holder");
    let shared = vm.heap.share_ref(payload.into(), Combinator::Nil.into());
    let shared_raw = RawValue::from(shared);
    holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(shared));

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_shared_top_level_body.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    assert_eq!(
        holder
            .get_value(&vm.heap)
            .expect("expected lowered shared top-level body")
            .get_ref(),
        payload.get_ref()
    );
    assert_eq!(vm.heap[shared_raw].head, payload.get_ref());
    assert_eq!(vm.heap[shared_raw].tail, -1);
}

#[test]
fn codegen_phase_lowers_shared_letrec_tries_body_through_existing_owned_subset() {
    let mut vm = VM::new();
    vm.initializing = false;

    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let inner_x = vm.heap.make_empty_identifier("inner_x");
    let inner_y = vm.heap.make_empty_identifier("inner_y");
    let guard = vm.heap.make_empty_identifier("guard");
    let holder = vm.heap.make_empty_identifier("holder");
    let binder = vm.heap.pair_ref(x.into(), y.into());
    let cond_body = vm.heap.apply2(Combinator::Cond.into(), guard.into(), inner_x.into());
    let duplicated_inner = vm.heap.apply_ref(inner_x.into(), inner_y.into());
    let guarded_body = vm.heap.apply_ref(cond_body, duplicated_inner);
    let inner_binder = vm.heap.pair_ref(inner_x.into(), inner_y.into());
    let fallible_alternative = vm.heap.lambda_ref(inner_binder, guarded_body);
    let alternatives = vm.heap.cons_ref(fallible_alternative, Combinator::Nil.into());
    let tries_body = vm.heap.tries_ref(holder.into(), alternatives);
    let definition = DefinitionRef::new(&mut vm.heap, binder, Type::Undefined.into(), tries_body);
    let definitions = vm.heap.cons_ref(definition.get_ref().into(), Combinator::Nil.into());
    let letrec_body = vm.heap.letrec_ref(definitions, x.into());
    let shared_body = vm.heap.share_ref(letrec_body, Combinator::Nil.into());
    holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(shared_body));

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_shared_letrec_tries_guarded.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered = holder
        .get_value(&vm.heap)
        .expect("expected lowered shared letrec tries body")
        .get_ref()
        .into();
    let mut subscript_indices = Vec::new();
    collect_subscript_indices(&vm.heap, lowered, &mut subscript_indices);
    subscript_indices.sort();
    assert!(subscript_indices.is_empty());
    assert!(!subtree_contains_combinator(&vm.heap, lowered, Combinator::Y));
    assert!(subtree_contains_combinator(&vm.heap, lowered, Combinator::Try));
    assert!(subtree_contains_combinator(&vm.heap, lowered, Combinator::BadCase));
    assert!(subtree_contains_combinator(&vm.heap, lowered, Combinator::Cond));
}

#[test]
fn codegen_phase_reuses_one_lowered_result_for_repeated_shared_tuple_lambda() {
    let mut vm = VM::new();
    vm.initializing = false;

    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let holder = vm.heap.make_empty_identifier("holder");
    let binder = vm.heap.pair_ref(x.into(), y.into());
    let lambda = vm.heap.lambda_ref(binder, x.into());
    let shared = vm.heap.share_ref(lambda, Combinator::Nil.into());
    let shared_raw = RawValue::from(shared);
    let pair = vm.heap.pair_ref(shared, shared);
    holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(pair));

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_shared_tuple_lambda_reuse.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered_pair = holder
        .get_value(&vm.heap)
        .expect("expected lowered pair containing shared tuple lambda")
        .get_ref();
    assert_eq!(vm.heap[lowered_pair].tag, Tag::Cons);
    let lowered_head: Value = vm.heap[lowered_pair].head.into();
    let lowered_tail: Value = vm.heap[lowered_pair].tail.into();
    assert_eq!(lowered_head, lowered_tail);
    assert!(RawValue::from(lowered_head) >= ATOM_LIMIT);
    assert_eq!(vm.heap[RawValue::from(lowered_head)].tag, Tag::Ap);
    assert_eq!(vm.heap[RawValue::from(lowered_head)].head, RawValue::from(Combinator::U));
    assert_eq!(vm.heap[shared_raw].head, RawValue::from(lowered_head));
    assert_eq!(vm.heap[shared_raw].tail, -1);
}

#[test]
fn codegen_phase_leftfactors_all_active_g_alt_shapes_and_leaves_non_matches_unchanged() {
    let mut vm = VM::new();
    vm.initializing = false;

    let a = vm.heap.make_empty_identifier("a");
    let b = vm.heap.make_empty_identifier("b");
    let c = vm.heap.make_empty_identifier("c");
    let d = vm.heap.make_empty_identifier("d");

    let rule1_holder = vm.heap.make_empty_identifier("rule1_holder");
    let rule2_holder = vm.heap.make_empty_identifier("rule2_holder");
    let rule3_holder = vm.heap.make_empty_identifier("rule3_holder");
    let rule4_holder = vm.heap.make_empty_identifier("rule4_holder");
    let control_holder = vm.heap.make_empty_identifier("control_holder");

    let seq_ab = vm.heap.apply2(Combinator::G_Seq.into(), a.into(), b.into());
    let seq_ac = vm.heap.apply2(Combinator::G_Seq.into(), a.into(), c.into());
    let seq_cd = vm.heap.apply2(Combinator::G_Seq.into(), c.into(), d.into());

    let rule1_body = vm.heap.apply2(Combinator::G_Alt.into(), seq_ab, a.into());
    let rule2_body = vm.heap.apply2(Combinator::G_Alt.into(), seq_ab, seq_ac);
    let rule3_rhs = vm.heap.apply2(Combinator::G_Alt.into(), a.into(), d.into());
    let rule3_body = vm.heap.apply2(Combinator::G_Alt.into(), seq_ab, rule3_rhs);
    let rule4_rhs = vm.heap.apply2(Combinator::G_Alt.into(), seq_ac, d.into());
    let rule4_body = vm.heap.apply2(Combinator::G_Alt.into(), seq_ab, rule4_rhs);
    let control_body = vm.heap.apply2(Combinator::G_Alt.into(), seq_ab, seq_cd);

    rule1_holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(rule1_body));
    rule2_holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(rule2_body));
    rule3_holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(rule3_body));
    rule4_holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(rule4_body));
    control_holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(control_body));

    let mut definienda = ConsList::EMPTY;
    for definiendum in [
        control_holder,
        rule4_holder,
        rule3_holder,
        rule2_holder,
        rule1_holder,
    ] {
        definienda.push(&mut vm.heap, definiendum);
    }
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_leftfactor_shapes.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());

    let rule1 = rule1_holder
        .get_value(&vm.heap)
        .expect("expected leftfactored rule1 body")
        .get_ref()
        .into();
    let (rule1_head, rule1_args) = application_spine(&vm.heap, rule1);
    assert_eq!(rule1_head, Combinator::G_Seq.into());
    assert_eq!(rule1_args.len(), 2);
    assert_eq!(rule1_args[0], a.into());
    let (rule1_alt_head, rule1_alt_args) = application_spine(&vm.heap, rule1_args[1]);
    assert_eq!(rule1_alt_head, Combinator::G_Alt.into());
    assert_eq!(rule1_alt_args, vec![b.into(), Combinator::G_Unit.into()]);

    let rule2 = rule2_holder
        .get_value(&vm.heap)
        .expect("expected leftfactored rule2 body")
        .get_ref()
        .into();
    let (rule2_head, rule2_args) = application_spine(&vm.heap, rule2);
    assert_eq!(rule2_head, Combinator::G_Seq.into());
    assert_eq!(rule2_args.len(), 2);
    assert_eq!(rule2_args[0], a.into());
    let (rule2_alt_head, rule2_alt_args) = application_spine(&vm.heap, rule2_args[1]);
    assert_eq!(rule2_alt_head, Combinator::G_Alt.into());
    assert_eq!(rule2_alt_args, vec![b.into(), c.into()]);

    let rule3 = rule3_holder
        .get_value(&vm.heap)
        .expect("expected leftfactored rule3 body")
        .get_ref()
        .into();
    let (rule3_head, rule3_args) = application_spine(&vm.heap, rule3);
    assert_eq!(rule3_head, Combinator::G_Alt.into());
    assert_eq!(rule3_args.len(), 2);
    assert_eq!(rule3_args[1], d.into());
    let (rule3_seq_head, rule3_seq_args) = application_spine(&vm.heap, rule3_args[0]);
    assert_eq!(rule3_seq_head, Combinator::G_Seq.into());
    assert_eq!(rule3_seq_args[0], a.into());
    let (rule3_inner_alt_head, rule3_inner_alt_args) = application_spine(&vm.heap, rule3_seq_args[1]);
    assert_eq!(rule3_inner_alt_head, Combinator::G_Alt.into());
    assert_eq!(rule3_inner_alt_args, vec![b.into(), Combinator::G_Unit.into()]);

    let rule4 = rule4_holder
        .get_value(&vm.heap)
        .expect("expected leftfactored rule4 body")
        .get_ref()
        .into();
    let (rule4_head, rule4_args) = application_spine(&vm.heap, rule4);
    assert_eq!(rule4_head, Combinator::G_Alt.into());
    assert_eq!(rule4_args.len(), 2);
    assert_eq!(rule4_args[1], d.into());
    let (rule4_seq_head, rule4_seq_args) = application_spine(&vm.heap, rule4_args[0]);
    assert_eq!(rule4_seq_head, Combinator::G_Seq.into());
    assert_eq!(rule4_seq_args[0], a.into());
    let (rule4_inner_alt_head, rule4_inner_alt_args) = application_spine(&vm.heap, rule4_seq_args[1]);
    assert_eq!(rule4_inner_alt_head, Combinator::G_Alt.into());
    assert_eq!(rule4_inner_alt_args, vec![b.into(), c.into()]);

    let control = control_holder
        .get_value(&vm.heap)
        .expect("expected preserved non-matching alternation body")
        .get_ref()
        .into();
    let (control_head, control_args) = application_spine(&vm.heap, control);
    assert_eq!(control_head, Combinator::G_Alt.into());
    assert_eq!(control_args.len(), 2);
    let (control_left_head, control_left_args) = application_spine(&vm.heap, control_args[0]);
    assert_eq!(control_left_head, Combinator::G_Seq.into());
    assert_eq!(control_left_args, vec![a.into(), b.into()]);
    let (control_right_head, control_right_args) = application_spine(&vm.heap, control_args[1]);
    assert_eq!(control_right_head, Combinator::G_Seq.into());
    assert_eq!(control_right_args, vec![c.into(), d.into()]);
}

#[test]
fn codegen_phase_lowers_builtin_show_nodes_for_num_bool_char_void_and_function_types() {
    let mut vm = VM::new();
    vm.initializing = false;

    let shownum_holder = vm.heap.make_empty_identifier("shownum_holder");
    let bool_show_function_holder = vm.heap.make_empty_identifier("bool_show_function_holder");
    let char_show_function_holder = vm.heap.make_empty_identifier("char_show_function_holder");
    let void_show_function_holder = vm.heap.make_empty_identifier("void_show_function_holder");
    let function_show_function_holder = vm.heap.make_empty_identifier("function_show_function_holder");

    let shownum_body = vm.heap.show_ref(NIL, Type::Number.into());
    let bool_show_function_body = vm.heap.show_ref(NIL, Type::Bool.into());
    let char_show_function_body = vm.heap.show_ref(NIL, Type::Char.into());
    let void_show_function_body = vm.heap.show_ref(NIL, Type::Void.into());
    let function_type = vm.heap.arrow_type_ref(Type::Number.into(), Type::Bool.into());
    let function_show_function_body = vm.heap.show_ref(NIL, function_type);

    shownum_holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(shownum_body));
    bool_show_function_holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(bool_show_function_body));
    char_show_function_holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(char_show_function_body));
    void_show_function_holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(void_show_function_body));
    function_show_function_holder.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(function_show_function_body),
    );

    let mut definienda = ConsList::EMPTY;
    for definiendum in [
        function_show_function_holder,
        void_show_function_holder,
        char_show_function_holder,
        bool_show_function_holder,
        shownum_holder,
    ] {
        definienda.push(&mut vm.heap, definiendum);
    }
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_builtin_show_nodes.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    assert_eq!(
        shownum_holder
            .get_value(&vm.heap)
            .expect("expected lowered shownum body")
            .get_ref(),
        RawValue::from(Combinator::ShowNum)
    );
    assert_eq!(
        bool_show_function_holder
            .get_value(&vm.heap)
            .expect("expected lowered bool_show_function body")
            .get_ref(),
        vm.bool_show_function.get_ref()
    );
    assert_eq!(
        char_show_function_holder
            .get_value(&vm.heap)
            .expect("expected lowered char_show_function body")
            .get_ref(),
        vm.char_show_function.get_ref()
    );
    assert_eq!(
        void_show_function_holder
            .get_value(&vm.heap)
            .expect("expected lowered void_show_function body")
            .get_ref(),
        vm.void_show_function.get_ref()
    );
    assert_eq!(
        function_show_function_holder
            .get_value(&vm.heap)
            .expect("expected lowered function_show_function body")
            .get_ref(),
        vm.function_show_function.get_ref()
    );
}

#[test]
fn codegen_phase_lowers_list_string_and_tuple_show_nodes() {
    let mut vm = VM::new();
    vm.initializing = false;

    let list_show_function_holder = vm.heap.make_empty_identifier("list_show_function_holder");
    let string_show_function_holder = vm.heap.make_empty_identifier("string_show_function_holder");
    let showtuple_holder = vm.heap.make_empty_identifier("showtuple_holder");

    let list_num_type = vm.heap.list_type_ref(Type::Number.into());
    let string_type = vm.heap.list_type_ref(Type::Char.into());
    let tuple_type = vm.heap.pair_type_ref(Type::Bool.into(), Type::Number.into());

    let list_show_function_body = vm.heap.show_ref(NIL, list_num_type);
    let string_show_function_body = vm.heap.show_ref(NIL, string_type);
    let showtuple_body = vm.heap.show_ref(NIL, tuple_type);

    list_show_function_holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(list_show_function_body));
    string_show_function_holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(string_show_function_body));
    showtuple_holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(showtuple_body));

    let mut definienda = ConsList::EMPTY;
    for definiendum in [showtuple_holder, string_show_function_holder, list_show_function_holder] {
        definienda.push(&mut vm.heap, definiendum);
    }
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_composite_show_nodes.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());

    let list_show_function = list_show_function_holder
        .get_value(&vm.heap)
        .expect("expected lowered list_show_function body")
        .get_ref()
        .into();
    let (list_show_function_head, list_show_function_args) = application_spine(&vm.heap, list_show_function);
    assert_eq!(list_show_function_head, vm.list_show_function.into());
    assert_eq!(list_show_function_args, vec![Combinator::ShowNum.into()]);

    assert_eq!(
        string_show_function_holder
            .get_value(&vm.heap)
            .expect("expected lowered string_show_function body")
            .get_ref(),
        vm.string_show_function.get_ref()
    );

    let showtuple = showtuple_holder
        .get_value(&vm.heap)
        .expect("expected lowered showtuple body")
        .get_ref()
        .into();
    let (showtuple_head, showtuple_args) = application_spine(&vm.heap, showtuple);
    assert_eq!(showtuple_head, vm.paren_show_function.into());
    assert_eq!(showtuple_args.len(), 1);
    let (pair_show_function_head, pair_show_function_args) = application_spine(&vm.heap, showtuple_args[0]);
    assert_eq!(pair_show_function_head, vm.pair_show_function.into());
    assert_eq!(pair_show_function_args, vec![vm.bool_show_function.into(), Combinator::ShowNum.into()]);
}

#[test]
fn codegen_phase_lowers_direct_abstract_show_nodes_to_showabstract() {
    let mut vm = VM::new();
    vm.initializing = false;

    let thing_type = IdentifierValueTypeRef::new(
        &mut vm.heap,
        IdentifierValueTypeData::Abstract { basis: NIL },
    );
    let thing_type_value = IdentifierValueRef::new(
        &mut vm.heap,
        IdentifierValueData::Typed {
            arity: 0,
            show_function: Value::None,
            value_type: thing_type,
        },
    );
    let thing = IdentifierRecordRef::new(
        &mut vm.heap,
        "Thing".to_string(),
        IdentifierDefinitionRef::undefined(),
        Type::Type.into(),
        Some(thing_type_value),
    );

    let holder = vm.heap.make_empty_identifier("show_abstract_holder");
    let show_body = vm.heap.show_ref(NIL, thing.into());
    holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(show_body));

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_abstract_show_node.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    assert_eq!(
        holder
            .get_value(&vm.heap)
            .expect("expected lowered abstract show body")
            .get_ref(),
        vm.showabstract.get_ref()
    );
}

#[test]
fn codegen_phase_lowers_direct_type_variable_show_nodes_to_showwhat() {
    let mut vm = VM::new();
    vm.initializing = false;

    let holder = vm.heap.make_empty_identifier("show_type_variable_holder");
    let type_variable = vm.heap.type_var_ref(Value::None, Value::Data(1));
    let show_body = vm.heap.show_ref(NIL, type_variable);
    holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(show_body));

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_type_variable_show_node.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    assert_eq!(
        holder
            .get_value(&vm.heap)
            .expect("expected lowered type-variable show body")
            .get_ref(),
        vm.showwhat.get_ref()
    );
}

#[test]
fn codegen_phase_lowers_recursive_show_nodes_with_polymorphic_and_abstract_fallbacks() {
    let mut vm = VM::new();
    vm.initializing = false;

    let abstract_type = IdentifierValueTypeRef::new(
        &mut vm.heap,
        IdentifierValueTypeData::Abstract { basis: NIL },
    );
    let abstract_type_value = IdentifierValueRef::new(
        &mut vm.heap,
        IdentifierValueData::Typed {
            arity: 0,
            show_function: Value::None,
            value_type: abstract_type,
        },
    );
    let thing = IdentifierRecordRef::new(
        &mut vm.heap,
        "Thing".to_string(),
        IdentifierDefinitionRef::undefined(),
        Type::Type.into(),
        Some(abstract_type_value),
    );

    let list_holder = vm.heap.make_empty_identifier("show_list_type_variable_holder");
    let tuple_holder = vm.heap.make_empty_identifier("show_tuple_fallback_holder");

    let type_variable = vm.heap.type_var_ref(Value::None, Value::Data(1));
    let list_type = vm.heap.list_type_ref(type_variable);
    let list_show_body = vm.heap.show_ref(NIL, list_type);
    let tuple_type = vm.heap.pair_type_ref(type_variable, thing.into());
    let tuple_show_body = vm.heap.show_ref(NIL, tuple_type);

    list_holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(list_show_body));
    tuple_holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(tuple_show_body));

    let mut definienda = ConsList::EMPTY;
    for definiendum in [tuple_holder, list_holder] {
        definienda.push(&mut vm.heap, definiendum);
    }
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_recursive_show_fallbacks.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());

    let lowered_list = list_holder
        .get_value(&vm.heap)
        .expect("expected lowered list type-variable show body")
        .get_ref()
        .into();
    let (list_head, list_args) = application_spine(&vm.heap, lowered_list);
    assert_eq!(list_head, vm.list_show_function.into());
    assert_eq!(list_args, vec![vm.showwhat.into()]);

    let lowered_tuple = tuple_holder
        .get_value(&vm.heap)
        .expect("expected lowered tuple fallback show body")
        .get_ref()
        .into();
    let (tuple_head, tuple_args) = application_spine(&vm.heap, lowered_tuple);
    assert_eq!(tuple_head, vm.paren_show_function.into());
    assert_eq!(tuple_args.len(), 1);
    let (pair_head, pair_args) = application_spine(&vm.heap, tuple_args[0]);
    assert_eq!(pair_head, vm.pair_show_function.into());
    assert_eq!(pair_args, vec![vm.showwhat.into(), vm.showabstract.into()]);
}

#[test]
fn codegen_phase_keeps_showwhat_identifier_heads_unapplied() {
    let mut vm = VM::new();
    vm.initializing = false;

    let polymorphic_type = IdentifierValueTypeRef::new(
        &mut vm.heap,
        IdentifierValueTypeData::Abstract { basis: NIL },
    );
    let polymorphic_type_value = IdentifierValueRef::new(
        &mut vm.heap,
        IdentifierValueData::Typed {
            arity: 1,
            show_function: vm.showwhat.into(),
            value_type: polymorphic_type,
        },
    );
    let poly = IdentifierRecordRef::new(
        &mut vm.heap,
        "Poly".to_string(),
        IdentifierDefinitionRef::undefined(),
        Type::Type.into(),
        Some(polymorphic_type_value),
    );

    let holder = vm.heap.make_empty_identifier("showwhat_identifier_head_holder");
    let applied_type = vm.heap.apply_ref(poly.into(), Type::Number.into());
    let show_body = vm.heap.show_ref(NIL, applied_type);
    holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(show_body));

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_showwhat_identifier_head.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    assert_eq!(
        holder
            .get_value(&vm.heap)
            .expect("expected lowered showwhat identifier-head body")
            .get_ref(),
        vm.showwhat.get_ref()
    );
}

#[test]
fn codegen_phase_lowers_attached_type_show_functions_through_committed_identity() {
    let mut vm = VM::new();
    vm.initializing = false;

    let showthing = vm.intern_identifier("showThing");
    let thing_type = IdentifierValueTypeRef::new(
        &mut vm.heap,
        IdentifierValueTypeData::Abstract { basis: NIL },
    );
    let thing_type_value = IdentifierValueRef::new(
        &mut vm.heap,
        IdentifierValueData::Typed {
            arity: 1,
            show_function: showthing.into(),
            value_type: thing_type,
        },
    );
    let thing = IdentifierRecordRef::new(
        &mut vm.heap,
        "Thing".to_string(),
        IdentifierDefinitionRef::undefined(),
        Type::Type.into(),
        Some(thing_type_value),
    );

    let holder = vm.heap.make_empty_identifier("show_attached_holder");
    let applied_type = vm.heap.apply_ref(thing.into(), Type::Number.into());
    let attached_show_body = vm.heap.show_ref(NIL, applied_type);
    holder.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(attached_show_body),
    );

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_attached_show_node.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered = holder
        .get_value(&vm.heap)
        .expect("expected lowered attached show body")
        .get_ref()
        .into();
    let (head, args) = application_spine(&vm.heap, lowered);
    assert_eq!(head, showthing.into());
    assert_eq!(args, vec![vm.internal_number_show_function.into()]);
}

#[test]
fn codegen_phase_lowers_attached_type_show_functions_with_polymorphic_arguments() {
    let mut vm = VM::new();
    vm.initializing = false;

    let showthing = vm.intern_identifier("showThingPoly");
    let thing_type = IdentifierValueTypeRef::new(
        &mut vm.heap,
        IdentifierValueTypeData::Abstract { basis: NIL },
    );
    let thing_type_value = IdentifierValueRef::new(
        &mut vm.heap,
        IdentifierValueData::Typed {
            arity: 1,
            show_function: showthing.into(),
            value_type: thing_type,
        },
    );
    let thing = IdentifierRecordRef::new(
        &mut vm.heap,
        "ThingPoly".to_string(),
        IdentifierDefinitionRef::undefined(),
        Type::Type.into(),
        Some(thing_type_value),
    );

    let holder = vm.heap.make_empty_identifier("show_attached_polymorphic_holder");
    let type_variable = vm.heap.type_var_ref(Value::None, Value::Data(1));
    let applied_type = vm.heap.apply_ref(thing.into(), type_variable);
    let attached_show_body = vm.heap.show_ref(NIL, applied_type);
    holder.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Arbitrary(attached_show_body),
    );

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_attached_polymorphic_show_node.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered = holder
        .get_value(&vm.heap)
        .expect("expected lowered attached polymorphic show body")
        .get_ref()
        .into();
    let (head, args) = application_spine(&vm.heap, lowered);
    assert_eq!(head, showthing.into());
    assert_eq!(args, vec![vm.showwhat.into()]);
}

#[test]
fn codegen_phase_preserves_cond_shape_for_left_k_guarded_abstraction() {
    let mut vm = VM::new();
    vm.initializing = false;

    let x = vm.heap.make_empty_identifier("x");
    let guard = vm.heap.make_empty_identifier("guard");
    let then_branch = vm.heap.make_empty_identifier("then_branch");
    let holder = vm.heap.make_empty_identifier("holder");
    let cond_body = vm.heap.apply2(Combinator::Cond.into(), guard.into(), then_branch.into());
    let duplicated_x = vm.heap.apply_ref(x.into(), x.into());
    let body = vm.heap.apply_ref(cond_body, duplicated_x);
    let lambda = vm.heap.lambda_ref(x.into(), body);
    holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(lambda));

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_cond_left_k_lambda.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered = holder
        .get_value(&vm.heap)
        .expect("expected lowered guarded lambda body")
        .get_ref()
        .into();
    let (head, arguments) = application_spine(&vm.heap, lowered);
    assert_eq!(head, Combinator::Cond.into());
    assert_eq!(arguments.len(), 3);
    assert_eq!(arguments[0], guard.into());

    let (preserved_head, preserved_arguments) = application_spine(&vm.heap, arguments[1]);
    assert_eq!(preserved_head, Combinator::K.into());
    assert_eq!(preserved_arguments, vec![then_branch.into()]);
}

#[test]
fn codegen_phase_preserves_cond_shape_for_left_b_with_right_k_guarded_abstraction() {
    let mut vm = VM::new();
    vm.initializing = false;

    let x = vm.heap.make_empty_identifier("x");
    let guard = vm.heap.make_empty_identifier("guard");
    let y = vm.heap.make_empty_identifier("y");
    let holder = vm.heap.make_empty_identifier("holder");
    let cond_body = vm.heap.apply_ref(Combinator::Cond.into(), guard.into());
    let duplicated_x = vm.heap.apply_ref(x.into(), x.into());
    let guarded_function = vm.heap.apply_ref(cond_body, duplicated_x);
    let body = vm.heap.apply_ref(guarded_function, y.into());
    let lambda = vm.heap.lambda_ref(x.into(), body);
    holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(lambda));

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_cond_left_b_right_k_lambda.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered = holder
        .get_value(&vm.heap)
        .expect("expected lowered guarded application body")
        .get_ref()
        .into();
    let (head, arguments) = application_spine(&vm.heap, lowered);
    assert_eq!(head, Combinator::Cond.into());
    assert_eq!(arguments.len(), 3);
    assert_eq!(arguments[0], guard.into());
    assert_eq!(arguments[2], y.into());
}

#[test]
fn codegen_phase_lowers_nonrecursive_pattern_group_without_recursive_bundle() {
    let mut vm = VM::new();
    vm.initializing = false;

    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let pair = vm.heap.make_empty_identifier("pair");
    let z = vm.heap.make_empty_identifier("z");
    let holder = vm.heap.make_empty_identifier("holder");
    let binder = vm.heap.pair_ref(x.into(), y.into());
    let pattern_definition = DefinitionRef::new(&mut vm.heap, binder, Type::Undefined.into(), pair.into());
    let pair_value = vm.heap.pair_ref(Value::Data(1), Value::Data(2));
    let pair_definition = DefinitionRef::new(
        &mut vm.heap,
        pair.into(),
        Type::Undefined.into(),
        pair_value,
    );
    let z_definition = DefinitionRef::new(&mut vm.heap, z.into(), Type::Undefined.into(), Value::Data(7));
    let z_tail = vm.heap.cons_ref(z_definition.get_ref().into(), Combinator::Nil.into());
    let pair_tail = vm.heap.cons_ref(pair_definition.get_ref().into(), z_tail);
    let definitions = vm.heap.cons_ref(pattern_definition.get_ref().into(), pair_tail);
    let body = vm.heap.cons_ref(x.into(), z.into());
    let letrec_body = vm.heap.letrec_ref(definitions, body);
    holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(letrec_body));

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_nonrecursive_pattern_group.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered = holder
        .get_value(&vm.heap)
        .expect("expected lowered nonrecursive pattern group")
        .get_ref()
        .into();
    assert!(!subtree_contains_combinator(&vm.heap, lowered, Combinator::Y));
}

#[test]
fn codegen_phase_prunes_dead_simple_local_definition() {
    let mut vm = VM::new();
    vm.initializing = false;

    let used = vm.heap.make_empty_identifier("used");
    let dead = vm.heap.make_empty_identifier("dead");
    let dead_payload = vm.heap.make_empty_identifier("dead_payload");
    let holder = vm.heap.make_empty_identifier("holder");
    let used_definition = DefinitionRef::new(&mut vm.heap, used.into(), Type::Undefined.into(), Value::Data(1));
    let dead_definition = DefinitionRef::new(&mut vm.heap, dead.into(), Type::Undefined.into(), dead_payload.into());
    let dead_tail = vm.heap.cons_ref(dead_definition.get_ref().into(), Combinator::Nil.into());
    let definitions = vm.heap.cons_ref(used_definition.get_ref().into(), dead_tail);
    let letrec_body = vm.heap.letrec_ref(definitions, used.into());
    holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(letrec_body));

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_prunes_dead_simple_local.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered = holder
        .get_value(&vm.heap)
        .expect("expected lowered body after dead-simple pruning")
        .get_ref()
        .into();
    assert!(!subtree_contains_identifier(&vm.heap, lowered, dead_payload));
}

#[test]
fn codegen_phase_prunes_dead_pattern_local_definition() {
    let mut vm = VM::new();
    vm.initializing = false;

    let used = vm.heap.make_empty_identifier("used");
    let dead_x = vm.heap.make_empty_identifier("dead_x");
    let dead_y = vm.heap.make_empty_identifier("dead_y");
    let dead_payload = vm.heap.make_empty_identifier("dead_pattern_payload");
    let holder = vm.heap.make_empty_identifier("holder");
    let used_definition = DefinitionRef::new(&mut vm.heap, used.into(), Type::Undefined.into(), Value::Data(1));
    let dead_binder = vm.heap.pair_ref(dead_x.into(), dead_y.into());
    let dead_definition = DefinitionRef::new(
        &mut vm.heap,
        dead_binder,
        Type::Undefined.into(),
        dead_payload.into(),
    );
    let dead_tail = vm.heap.cons_ref(dead_definition.get_ref().into(), Combinator::Nil.into());
    let definitions = vm.heap.cons_ref(used_definition.get_ref().into(), dead_tail);
    let letrec_body = vm.heap.letrec_ref(definitions, used.into());
    holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(letrec_body));

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_prunes_dead_pattern_local.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered = holder
        .get_value(&vm.heap)
        .expect("expected lowered body after dead-pattern pruning")
        .get_ref()
        .into();
    assert!(!subtree_contains_identifier(&vm.heap, lowered, dead_payload));
}

#[test]
fn codegen_phase_retains_transitively_needed_local_definition() {
    let mut vm = VM::new();
    vm.initializing = false;

    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let dead = vm.heap.make_empty_identifier("dead");
    let needed_payload = vm.heap.make_empty_identifier("needed_payload");
    let dead_payload = vm.heap.make_empty_identifier("dead_payload");
    let holder = vm.heap.make_empty_identifier("holder");
    let x_definition = DefinitionRef::new(&mut vm.heap, x.into(), Type::Undefined.into(), y.into());
    let y_definition = DefinitionRef::new(&mut vm.heap, y.into(), Type::Undefined.into(), needed_payload.into());
    let dead_definition = DefinitionRef::new(&mut vm.heap, dead.into(), Type::Undefined.into(), dead_payload.into());
    let dead_tail = vm.heap.cons_ref(dead_definition.get_ref().into(), Combinator::Nil.into());
    let y_tail = vm.heap.cons_ref(y_definition.get_ref().into(), dead_tail);
    let definitions = vm.heap.cons_ref(x_definition.get_ref().into(), y_tail);
    let letrec_body = vm.heap.letrec_ref(definitions, x.into());
    holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(letrec_body));

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_retains_transitive_local.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered = holder
        .get_value(&vm.heap)
        .expect("expected lowered body after transitive pruning")
        .get_ref()
        .into();
    assert!(subtree_contains_identifier(&vm.heap, lowered, needed_payload));
    assert!(!subtree_contains_identifier(&vm.heap, lowered, dead_payload));
}

#[test]
fn codegen_phase_lowers_pattern_letrec_through_tries_and_guarded_lambda_interaction() {
    let mut vm = VM::new();
    vm.initializing = false;

    let x = vm.heap.make_empty_identifier("x");
    let y = vm.heap.make_empty_identifier("y");
    let inner_x = vm.heap.make_empty_identifier("inner_x");
    let inner_y = vm.heap.make_empty_identifier("inner_y");
    let guard = vm.heap.make_empty_identifier("guard");
    let holder = vm.heap.make_empty_identifier("holder");
    let binder = vm.heap.pair_ref(x.into(), y.into());
    let cond_body = vm.heap.apply2(Combinator::Cond.into(), guard.into(), inner_x.into());
    let duplicated_inner = vm.heap.apply_ref(inner_x.into(), inner_y.into());
    let guarded_body = vm.heap.apply_ref(cond_body, duplicated_inner);
    let inner_binder = vm.heap.pair_ref(inner_x.into(), inner_y.into());
    let fallible_alternative = vm.heap.lambda_ref(inner_binder, guarded_body);
    let alternatives = vm.heap.cons_ref(fallible_alternative, Combinator::Nil.into());
    let tries_body = vm.heap.tries_ref(holder.into(), alternatives);
    let definition = DefinitionRef::new(&mut vm.heap, binder, Type::Undefined.into(), tries_body);
    let definitions = vm.heap.cons_ref(definition.get_ref().into(), Combinator::Nil.into());
    let letrec_body = vm.heap.letrec_ref(definitions, x.into());
    holder.set_value_from_data(&mut vm.heap, IdentifierValueData::Arbitrary(letrec_body));

    let definienda = ConsList::new(&mut vm.heap, holder);
    let current_file = FileRecord::new(
        &mut vm.heap,
        unique_test_path("codegen_pattern_letrec_tries_guarded.m")
            .to_string_lossy()
            .to_string(),
        UNIX_EPOCH,
        false,
        definienda,
    );
    vm.files = ConsList::new(&mut vm.heap, current_file);
    vm.undefined_names = ConsList::EMPTY;

    let result = vm.run_codegen_phase();

    assert!(result.is_ok());
    let lowered = holder
        .get_value(&vm.heap)
        .expect("expected lowered letrec tries body")
        .get_ref()
        .into();
    let mut subscript_indices = Vec::new();
    collect_subscript_indices(&vm.heap, lowered, &mut subscript_indices);
    subscript_indices.sort();
    assert!(subscript_indices.is_empty());
    assert!(!subtree_contains_combinator(&vm.heap, lowered, Combinator::Y));
    assert!(subtree_contains_combinator(&vm.heap, lowered, Combinator::Try));
    assert!(subtree_contains_combinator(&vm.heap, lowered, Combinator::BadCase));
    assert!(subtree_contains_combinator(&vm.heap, lowered, Combinator::Cond));
}

#[test]
fn top_level_where_lowers_nonrecursive_local_without_y() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_codegen_nonrecursive_local.m");
    std::fs::write(&source_path, "f = g where g = 1\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let f = vm.heap.get_identifier("f").expect("expected f identifier");
    let lowered = f
        .get_value(&vm.heap)
        .expect("expected lowered top-level where body")
        .get_ref()
        .into();
    assert!(!subtree_contains_combinator(&vm.heap, lowered, Combinator::Y));
}

#[test]
fn top_level_where_partitions_simple_dependency_chain_without_y() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_codegen_dependency_chain.m");
    std::fs::write(&source_path, "f = x where x = y; y = 1\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let f = vm.heap.get_identifier("f").expect("expected f identifier");
    let lowered = f
        .get_value(&vm.heap)
        .expect("expected lowered partitioned top-level where body")
        .get_ref()
        .into();
    assert!(!subtree_contains_combinator(&vm.heap, lowered, Combinator::Y));
}

#[test]
fn top_level_where_lowers_recursive_pattern_subset_through_carrier_path() {
    let mut vm = VM::new();
    vm.initializing = false;

    let source_path = unique_test_path("top_level_where_codegen_recursive_pattern_subset.m");
    std::fs::write(&source_path, "f = x where (x,y) = pair; pair = x\n")
        .expect("failed to write source test file");

    let result = vm.load_file(&source_path.to_string_lossy());

    assert!(result.is_ok(), "result={result:?} diagnostics={:?}", vm.parser_diagnostics);
    let f = vm.heap.get_identifier("f").expect("expected f identifier");
    let lowered = f
        .get_value(&vm.heap)
        .expect("expected lowered recursive patterned where body")
        .get_ref()
        .into();
    let mut subscript_indices = Vec::new();
    collect_subscript_indices(&vm.heap, lowered, &mut subscript_indices);
    subscript_indices.sort();
    assert_eq!(subscript_indices, vec![0, 1]);
    assert!(subtree_contains_combinator(&vm.heap, lowered, Combinator::Y));
}

#[test]
fn dump_visibility_phase_runs_for_normal_m_source() {
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut writer_vm = VM::new();
    writer_vm.initializing = false;

    let source_path = unique_test_path("dump_decode_roundtrip.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    writer_vm.files = writer_vm.empty_environment_for_source(&source_path_str, UNIX_EPOCH);

    let write_result = writer_vm.run_dump_visibility_phase(&source_path_str);
    assert!(write_result.is_ok());

    let mut reader_vm = VM::new();
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
    let mut vm = VM::new();
    let bytes = vec![Bytecode::Short.code(), 0x80, Bytecode::Definition.code()];
    let value = vm
        .load_defs(&mut bytes.into_iter())
        .expect("expected short integer payload to decode");
    let integer = IntegerRef::from_ref(value.into());

    assert_eq!(integer.to_i64_lossy(&vm.heap), -128);
}

#[test]
fn load_defs_decodes_int_x_multi_cell_chain_through_integerref_boundary() {
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();

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
    let mut vm = VM::new();

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
    let mut vm = VM::new();

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
    let mut vm = VM::new();

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
    let mut vm = VM::new();

    vm.export_list_is_active = true;
    let internal = vm.heap.make_empty_identifier("internal_name");
    vm.internals = ConsList::new(&mut vm.heap, internal);

    vm.unfix_exports();

    assert!(!vm.export_list_is_active);
    assert!(vm.internals.is_empty());
}

#[test]
fn unfix_exports_preserves_internals_in_exports_mode() {
    let mut vm = VM::new();

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
    let mut vm = VM::new();

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
    let mut vm = VM::new();

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
    let mut vm = VM::new();

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
    let mut vm = VM::new();

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
    let mut vm = VM::new();

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
    let mut vm = VM::new();

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
        vm.free_binding_sets
            .head(&vm.heap)
            .map(|binding_set| binding_set.get_ref()),
        Some(formal_list.get_ref())
    );
}

#[test]
fn bindparams_records_wrong_arity_in_detritus_and_still_writes_formal_value() {
    let mut vm = VM::new();

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

#[test]
fn bind_include_request_actuals_routes_parser_fed_value_and_type_bindings_through_bindparams() {
    let mut vm = VM::new();

    let x = vm.heap.make_empty_identifier("x");
    let t = vm.heap.make_empty_identifier("t");
    let x_original =
        IdentifierDefinitionRef::alias_metadata_from_source_identifier(&mut vm.heap, x);
    let t_original =
        IdentifierDefinitionRef::alias_metadata_from_source_identifier(&mut vm.heap, t);

    let free_type = IdentifierValueTypeRef::new(&mut vm.heap, IdentifierValueTypeData::Free);
    t.set_value_from_data(
        &mut vm.heap,
        IdentifierValueData::Typed {
            arity: 0,
            show_function: Combinator::Nil.into(),
            value_type: free_type,
        },
    );

    let x_formal = FreeFormalBindingRef::new(
        &mut vm.heap,
        x,
        DataPair::from_ref(x_original.get_ref()),
        TypeExprRef::new(Type::Undefined.into()),
    );
    let t_formal = FreeFormalBindingRef::new(
        &mut vm.heap,
        t,
        DataPair::from_ref(t_original.get_ref()),
        TypeExprRef::new(Type::Type.into()),
    );

    let mut unsorted_formals: ConsList<FreeFormalBindingRef> = ConsList::EMPTY;
    unsorted_formals.push(&mut vm.heap, x_formal);
    unsorted_formals.push(&mut vm.heap, t_formal);
    let formal_list = ConsList::<FreeFormalBindingRef>::from_ref(
        super::bytecode::hdsort_binding_list_ref(&mut vm.heap, unsorted_formals.get_ref()),
    );

    let source_path = unique_test_path("parser_include_actuals.m");
    let source_path_str = source_path.to_string_lossy().to_string();
    let parse_outcome = vm
        .parse_source_text(
            &source_path_str,
            "%include \"inc.m\" { x = 42; t == num }\n",
            UNIX_EPOCH,
            false,
        )
        .expect("expected parser-fed include payload");
    let top_level_payload = parse_outcome
        .top_level_payload
        .expect("expected top-level payload");
    let include_request = top_level_payload
        .directives
        .include_requests
        .first()
        .expect("expected include request");

    let actuals = vm.lower_include_binding_actuals(&include_request.bindings);
    vm.bindparams(formal_list.get_ref().into(), actuals);

    assert_eq!(vm.missing_parameter_bindings.len(&vm.heap), 0);
    assert_eq!(vm.detritus_parameter_bindings.len(&vm.heap), 0);
    assert_eq!(
        IntegerRef::from_ref(vm.heap[x.get_ref()].tail).to_i64_lossy(&vm.heap),
        42
    );

    let actual_type_value = IdentifierValueRef::from_ref(vm.heap[t.get_ref()].tail);
    let IdentifierValueData::Typed {
        arity, value_type, ..
    } = actual_type_value.get_data(&vm.heap)
    else {
        panic!("expected typed include-bound type value");
    };
    assert_eq!(arity, 0);
    assert_eq!(
        value_type.get_identifier_value_type_kind(&vm.heap),
        IdentifierValueTypeKind::Synonym
    );
    assert_eq!(
        RawValue::from(value_type.synonym_rhs_type_expr(&vm.heap).value()),
        RawValue::from(Type::Number)
    );

    let recorded_binding_set = vm
        .free_binding_sets
        .head(&vm.heap)
        .expect("expected free binding set");
    assert_eq!(recorded_binding_set.get_ref(), formal_list.get_ref());
}
