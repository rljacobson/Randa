use super::*;

use crate::data::{
    api::{ConsList, FileInfoRef, HeapObjectProxy, IdentifierRecordRef},
    Combinator, Heap, RawValue, Tag, Value,
};

const TEST_VOID_TUPLE: Value = Value::Data(777);

fn run_parser(source_path: &str, source_text: &str) -> (Heap, ParserRunResult) {
    let mut heap = Heap::default();
    let lexer = Lexer::new(source_path, source_text);
    let activation = ParserActivation {
        heap: &mut heap,
        vm: ParserVmContext::new(Combinator::Undef.into(), TEST_VOID_TUPLE),
        session: ParserSessionState::default(),
        deferred: ParserDeferredState::default(),
    };
    let mut parser = parser::Parser::new(lexer, activation);
    let parsed = parser.parse();
    let result = parser.finish_run(parsed);
    (heap, result)
}

#[test]
fn here_info_from_source_location_counts_lines_and_handles_missing_location() {
    let source_text = "alpha\nbeta\ngamma\n";

    let here_info = HereInfo::from_source_location("demo.m", source_text, Some(Loc::new(6, 10)));
    assert_eq!(here_info.script_file, "demo.m");
    assert_eq!(here_info.line_number, 2);

    let missing = HereInfo::from_source_location("demo.m", source_text, None);
    assert_eq!(missing.line_number, 0);
}

#[test]
fn parser_parses_top_level_function_form_and_lowers_to_lambdas() {
    let (heap, result) = run_parser("function_form_substrate.m", "id x y = x\n");

    let ParserRunResult::ParsedTopLevelScript(payload) = result else {
        panic!("expected top-level parse success");
    };
    assert_eq!(payload.definitions.len(), 1);
    let definition = &payload.definitions[0];
    let outer_lambda_raw = definition.body;
    assert_eq!(heap[outer_lambda_raw].tag, Tag::Lambda);
    let inner_lambda_raw = heap[outer_lambda_raw].tail;
    assert_eq!(heap[inner_lambda_raw].tag, Tag::Lambda);
    let body_identifier = IdentifierRecordRef::from_ref(heap[inner_lambda_raw].tail);
    assert_eq!(body_identifier.get_name(&heap), "x");
}

#[test]
fn parser_parses_top_level_cons_pattern_form() {
    let (heap, result) = run_parser("cons_pattern_form_substrate.m", "head (x:xs) = x\n");

    let ParserRunResult::ParsedTopLevelScript(payload) = result else {
        panic!("expected top-level parse success");
    };
    assert_eq!(payload.definitions.len(), 1);
    let lambda_raw = payload.definitions[0].body;
    assert_eq!(heap[lambda_raw].tag, Tag::Lambda);
    let pattern_raw = heap[lambda_raw].head;
    assert_eq!(heap[pattern_raw].tag, Tag::Cons);
    let body_identifier = IdentifierRecordRef::from_ref(heap[lambda_raw].tail);
    assert_eq!(body_identifier.get_name(&heap), "x");
}

#[test]
fn parser_parses_top_level_tuple_pattern_form() {
    let (heap, result) = run_parser("tuple_pattern_form_substrate.m", "second (x,y) = y\n");

    let ParserRunResult::ParsedTopLevelScript(payload) = result else {
        panic!("expected top-level parse success");
    };
    assert_eq!(payload.definitions.len(), 1);
    let lambda_raw = payload.definitions[0].body;
    assert_eq!(heap[lambda_raw].tag, Tag::Lambda);
    let pattern_raw = heap[lambda_raw].head;
    assert_eq!(heap[pattern_raw].tag, Tag::Pair);
    let body_identifier = IdentifierRecordRef::from_ref(heap[lambda_raw].tail);
    assert_eq!(body_identifier.get_name(&heap), "y");
}

#[test]
fn parser_parses_top_level_constant_pattern_form() {
    let (heap, result) = run_parser("constant_pattern_form_substrate.m", "zero 0 = True\n");

    let ParserRunResult::ParsedTopLevelScript(payload) = result else {
        panic!("expected top-level parse success");
    };
    assert_eq!(payload.definitions.len(), 1);
    let lambda_raw = payload.definitions[0].body;
    assert_eq!(heap[lambda_raw].tag, Tag::Lambda);
    let pattern_raw = heap[lambda_raw].head;
    assert_eq!(heap[pattern_raw].tag, Tag::Cons);
    assert_eq!(heap[RawValue::from(heap[pattern_raw].tail)].tag, Tag::Int);
}

#[test]
fn parser_lowers_repeated_names_in_top_level_formals_as_wrapped_constants() {
    let (heap, result) = run_parser(
        "repeated_name_pattern_form_substrate.m",
        "eqpair (x,x) = x\n",
    );

    let ParserRunResult::ParsedTopLevelScript(payload) = result else {
        panic!("expected top-level parse success");
    };
    assert_eq!(payload.definitions.len(), 1);
    let lambda_raw = payload.definitions[0].body;
    assert_eq!(heap[lambda_raw].tag, Tag::Lambda);
    let pattern_raw = heap[lambda_raw].head;
    assert_eq!(heap[pattern_raw].tag, Tag::Pair);
    let repeated_raw: RawValue = heap[pattern_raw].tail;
    assert_eq!(heap[repeated_raw].tag, Tag::Cons);
    let body_identifier = IdentifierRecordRef::from_ref(heap[lambda_raw].tail);
    assert_eq!(body_identifier.get_name(&heap), "x");
}

#[test]
fn parser_parses_top_level_void_tuple_pattern_form() {
    let (heap, result) = run_parser("void_pattern_form_substrate.m", "unit () = True\n");

    let ParserRunResult::ParsedTopLevelScript(payload) = result else {
        panic!("expected top-level parse success");
    };
    assert_eq!(payload.definitions.len(), 1);
    let lambda_raw = payload.definitions[0].body;
    assert_eq!(heap[lambda_raw].tag, Tag::Lambda);
    assert_eq!(heap[lambda_raw].head, TEST_VOID_TUPLE.into());
}

#[test]
fn parser_parses_top_level_list_pattern_form() {
    let (heap, result) = run_parser("list_pattern_form_substrate.m", "second [x,y] = y\n");

    let ParserRunResult::ParsedTopLevelScript(payload) = result else {
        panic!("expected top-level parse success");
    };
    assert_eq!(payload.definitions.len(), 1);
    let lambda_raw = payload.definitions[0].body;
    assert_eq!(heap[lambda_raw].tag, Tag::Lambda);
    let pattern_raw = heap[lambda_raw].head;
    assert_eq!(heap[pattern_raw].tag, Tag::Cons);
    let tail_raw = heap[pattern_raw].tail;
    assert_eq!(heap[tail_raw].tag, Tag::Cons);
    let body_identifier = IdentifierRecordRef::from_ref(heap[lambda_raw].tail);
    assert_eq!(body_identifier.get_name(&heap), "y");
}

#[test]
fn parser_parses_nested_mixed_simple_patterns_in_top_level_formals() {
    let (heap, result) = run_parser(
        "nested_mixed_pattern_form_substrate.m",
        "pick ([x],()) = x\n",
    );

    let ParserRunResult::ParsedTopLevelScript(payload) = result else {
        panic!("expected top-level parse success");
    };
    assert_eq!(payload.definitions.len(), 1);
    let lambda_raw = payload.definitions[0].body;
    assert_eq!(heap[lambda_raw].tag, Tag::Lambda);
    let pattern_raw = heap[lambda_raw].head;
    assert_eq!(heap[pattern_raw].tag, Tag::Pair);
    let left_raw = heap[pattern_raw].head;
    assert_eq!(heap[left_raw].tag, Tag::Cons);
    assert_eq!(heap[pattern_raw].tail, TEST_VOID_TUPLE.into());
    let body_identifier = IdentifierRecordRef::from_ref(heap[lambda_raw].tail);
    assert_eq!(body_identifier.get_name(&heap), "x");
}

#[test]
fn parser_parses_nullary_constructor_pattern_form() {
    let (heap, result) = run_parser(
        "nullary_constructor_pattern_form_substrate.m",
        "maybe ::= Just | Nothing\nisJust Just = True\nisJust Nothing = False\n",
    );

    let ParserRunResult::ParsedTopLevelScript(payload) = result else {
        panic!("expected top-level parse success");
    };
    assert_eq!(payload.definitions.len(), 2);
    assert_eq!(payload.constructor_declarations.len(), 2);
    assert_eq!(
        IdentifierRecordRef::from_ref(payload.definitions[0].identifier).get_name(&heap),
        "isJust"
    );
    assert_eq!(
        IdentifierRecordRef::from_ref(payload.definitions[1].identifier).get_name(&heap),
        "isJust"
    );
}

#[test]
fn parser_records_constructor_field_metadata_in_payloads() {
    let (_heap, result) = run_parser(
        "constructor_field_payloads.m",
        "maybe * ::= Nothing | Just *\nstrictpair * ** ::= Pair *! **!\n",
    );

    let ParserRunResult::ParsedTopLevelScript(payload) = result else {
        panic!("expected top-level parse success");
    };
    assert_eq!(payload.constructor_declarations.len(), 3);

    let nothing = &payload.constructor_declarations[0];
    assert_eq!(nothing.arity, 0);
    assert!(nothing.fields.is_empty());

    let just = &payload.constructor_declarations[1];
    assert_eq!(just.arity, 1);
    assert_eq!(just.fields.len(), 1);
    assert!(!just.fields[0].is_strict);

    let pair = &payload.constructor_declarations[2];
    assert_eq!(pair.arity, 2);
    assert_eq!(pair.fields.len(), 2);
    assert!(pair.fields[0].is_strict);
    assert!(pair.fields[1].is_strict);
}

#[test]
fn parser_rejects_prefix_bang_constructor_field_syntax() {
    let (_heap, result) = run_parser(
        "constructor_field_prefix_bang.m",
        "strictpair * ** ::= Pair !* **!\n",
    );

    let ParserRunResult::SyntaxError(_diagnostics) = result else {
        panic!("expected syntax error result");
    };
}

#[test]
fn parser_returns_provisional_payload_for_include_export_directives() {
    let (heap, result) = run_parser(
        "directive_payload.m",
        "%include \"inc.m\"\n%export foo \"inc.m\" -bar\n",
    );

    let ParserRunResult::ParsedTopLevelScript(payload) = result else {
        panic!("expected top-level parse success");
    };
    assert_eq!(payload.directives.include_requests.len(), 1);

    let include_request = &payload.directives.include_requests[0];
    assert_eq!(
        heap.resolve_string(include_request.target_path.into())
            .expect("include target should be a heap string"),
        "inc.m"
    );
    let Value::Reference(include_anchor_ref) = Value::from(include_request.anchor) else {
        panic!("expected include anchor reference");
    };
    assert_eq!(
        FileInfoRef::from_ref(include_anchor_ref).line_number(&heap),
        1
    );

    let export = payload
        .directives
        .export
        .as_ref()
        .expect("expected export payload");
    let Value::Reference(export_anchor_ref) = Value::from(export.anchor) else {
        panic!("expected export anchor reference");
    };
    assert_eq!(
        FileInfoRef::from_ref(export_anchor_ref).line_number(&heap),
        2
    );
    assert_eq!(
        heap.resolve_string(
            ConsList::<Value>::from_ref(export.pathname_requests)
                .value_head(&heap)
                .expect("expected export pathname")
        )
        .expect("export pathname should be a heap string"),
        "inc.m"
    );
    assert_eq!(
        ConsList::<IdentifierRecordRef>::from_ref(export.exported_ids)
            .head(&heap)
            .expect("expected exported id")
            .get_name(&heap),
        "foo"
    );
}

#[test]
fn parser_returns_syntax_diagnostics_for_invalid_source() {
    let (_heap, result) = run_parser("parse_boundary_syntax.m", "1 +\n");

    let ParserRunResult::SyntaxError(diagnostics) = result else {
        panic!("expected syntax error result");
    };
    assert_eq!(diagnostics.diagnostics.len(), 1);
    assert!(diagnostics.diagnostics[0]
        .message
        .starts_with("unexpected token"));
    assert_eq!(
        diagnostics.diagnostics[0]
            .here_info
            .as_ref()
            .unwrap()
            .line_number,
        1
    );
    assert_eq!(
        diagnostics.diagnostics[0]
            .here_info
            .as_ref()
            .unwrap()
            .script_file,
        "parse_boundary_syntax.m"
    );
}

#[test]
fn parser_rejects_floating_point_literal_pattern_in_top_level_formals() {
    let (_heap, result) = run_parser("float_pattern_form_syntax.m", "bad 1.5 = 0\n");

    let ParserRunResult::SyntaxError(diagnostics) = result else {
        panic!("expected syntax error result");
    };
    assert!(diagnostics.diagnostics.iter().any(|diagnostic| {
        diagnostic
            .message
            .contains("floating point literal in pattern")
    }));
}

#[test]
fn parser_rejects_unary_minus_pattern_in_top_level_formals() {
    let (_heap, result) = run_parser("minus_pattern_form_syntax.m", "bad -1 = 0\n");

    let ParserRunResult::SyntaxError(_diagnostics) = result else {
        panic!("expected syntax error result");
    };
}

#[test]
fn parser_rejects_n_plus_k_pattern_in_top_level_formals() {
    let (_heap, result) = run_parser("n_plus_k_pattern_form_syntax.m", "bad (x+1) = x\n");

    let ParserRunResult::SyntaxError(_diagnostics) = result else {
        panic!("expected syntax error result");
    };
}
