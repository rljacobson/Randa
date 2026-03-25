use super::*;
use crate::compiler::Token;
use crate::data::{
    api::{ApNodeRef, DefinitionRef, IdentifierRecordRef},
    combinator::Combinator,
    tag::Tag,
    values::Value,
    Heap, RawValue, ATOM_LIMIT,
};

pub(super) struct CodegenBoundaryInputs {
    files: ConsList<FileRecord>,
    current_file: Option<FileRecord>,
    initializing: bool,
    undefined_names: ConsList<IdentifierRecordRef>,
}

impl CodegenBoundaryInputs {
    /// Captures the committed load-time substrate the partial codegen boundary reads.
    /// This exists so the codegen subsystem owns its input surface instead of re-reading phase state ad hoc from `load.rs`.
    /// The invariant is that codegen only sees committed file-graph state and post-typecheck unresolved-name state for the active load cycle.
    pub(super) fn from_vm(vm: &VM) -> Self {
        Self {
            files: vm.files,
            current_file: vm.files.head(&vm.heap),
            initializing: vm.initializing,
            undefined_names: vm.undefined_names,
        }
    }
}

pub(super) struct CodegenBoundaryResult {
    pub(super) processed_binding_count: usize,
    pub(super) failure: Option<CodegenError>,
}

/// Returns the payload `x` when `value` has the committed shape `ap(combinator, x)`. This exists so
/// codegen owns one stable unary-application recognizer for Miranda's active `combine` and
/// `liscomb` rules. The invariant is that only a direct application of the selected combinator
/// returns a payload.
fn unary_application_payload(heap: &Heap, value: Value, combinator: Combinator) -> Option<Value> {
    let raw: RawValue = value.into();
    (raw >= ATOM_LIMIT && heap[raw].tag == Tag::Ap).then(|| {
        let application = ApNodeRef::from_ref(raw);
        (application.function_raw(heap) == combinator as RawValue)
            .then(|| application.argument_raw(heap).into())
    })?
}

/// Returns `(a, b)` when `value` has the committed shape `ap2(combinator, a, b)`. This exists so
/// codegen owns one stable binary-application recognizer for the active `combine` rules that branch
/// on left-associated application spines. The invariant is that only the left-associated
/// `ap(ap(combinator, a), b)` shape returns payloads.
fn binary_application_payloads(
    heap: &Heap,
    value: Value,
    combinator: Combinator,
) -> Option<(Value, Value)> {
    let raw: RawValue = value.into();
    if raw < ATOM_LIMIT || heap[raw].tag != Tag::Ap {
        return None;
    }

    let application = ApNodeRef::from_ref(raw);
    let function_application = application.function_application(heap)?;
    (function_application.function_raw(heap) == combinator as RawValue).then(|| {
        (
            function_application.argument_raw(heap).into(),
            application.argument_raw(heap).into(),
        )
    })
}

/// Combines two active bracket-abstraction results using Miranda's current `combine` rule subset.
/// This exists so codegen owns application-family combinator selection in one place during active
/// lambda lowering. The invariant is that `K` propagation, `eta`, guarded `COND` preservation,
/// `B`/`C`/`S`, and the active `B1`/`C1`/`S1` introductions follow one consistent rule set.
fn combine_abstractions(heap: &mut Heap, left: Value, right: Value) -> Value {
    let left_k_payload = unary_application_payload(heap, left, Combinator::K);
    let right_k_payload = unary_application_payload(heap, right, Combinator::K);

    if let (Some(left_payload), Some(right_payload)) = (left_k_payload, right_k_payload) {
        let applied = heap.apply_ref(left_payload, right_payload);
        return heap.apply_ref(Combinator::K.into(), applied);
    }

    if let Some(left_payload) = left_k_payload {
        if right == Combinator::I.into() {
            return left_payload;
        }

        if let Some((right_left, right_right)) =
            binary_application_payloads(heap, right, Combinator::B)
        {
            return heap.apply3(Combinator::B1.into(), left_payload, right_left, right_right);
        }

        if let Some((condition, branch)) =
            binary_application_payloads(heap, left_payload, Combinator::Cond)
        {
            let preserved_branch = heap.apply_ref(Combinator::K.into(), branch);
            return heap.apply3(Combinator::Cond.into(), condition, preserved_branch, right);
        }

        return heap.apply2(Combinator::B.into(), left_payload, right);
    }

    if let Some(right_payload) = right_k_payload {
        if let Some((left_left, left_right)) = binary_application_payloads(heap, left, Combinator::B) {
            if let Some(condition) = unary_application_payload(heap, left_left, Combinator::Cond) {
                return heap.apply3(Combinator::Cond.into(), condition, left_right, right_payload);
            }

            return heap.apply3(Combinator::C1.into(), left_left, left_right, right_payload);
        }

        return heap.apply2(Combinator::C.into(), left, right_payload);
    }

    if let Some((left_left, left_right)) = binary_application_payloads(heap, left, Combinator::B) {
        if let Some(condition) = unary_application_payload(heap, left_left, Combinator::Cond) {
            return heap.apply3(Combinator::Cond.into(), condition, left_right, right);
        }

        return heap.apply3(Combinator::S1.into(), left_left, left_right, right);
    }

    heap.apply2(Combinator::S.into(), left, right)
}

/// Combines two active abstraction results for structural `Cons`-family cells using Miranda's
/// `liscomb` subset. This exists so codegen owns list/tuple structural combinator selection in one
/// place during active lambda lowering. The invariant is that structural abstraction preserves `K`
/// propagation and the active `P`/`B_p`/`C_p`/`S_p` selection.
fn combine_list_abstractions(heap: &mut Heap, left: Value, right: Value) -> Value {
    let left_k_payload = unary_application_payload(heap, left, Combinator::K);
    let right_k_payload = unary_application_payload(heap, right, Combinator::K);

    if let (Some(left_payload), Some(right_payload)) = (left_k_payload, right_k_payload) {
        let cons_value = heap.cons_ref(left_payload, right_payload);
        return heap.apply_ref(Combinator::K.into(), cons_value);
    }

    if let Some(left_payload) = left_k_payload {
        if right == Combinator::I.into() {
            return heap.apply_ref(Combinator::P.into(), left_payload);
        }

        return heap.apply2(Combinator::B_p.into(), left_payload, right);
    }

    if let Some(right_payload) = right_k_payload {
        return heap.apply2(Combinator::C_p.into(), left, right_payload);
    }

    heap.apply2(Combinator::S_p.into(), left, right)
}

/// Bracket-abstracts one active simple identifier binder from one lowered expression body. This
/// exists so codegen owns Miranda-shaped active simple-binder abstraction outside the `Tag::Lambda`
/// dispatcher. The invariant is that applications recurse through `combine`, structural cells
/// recurse through `liscomb`, and non-matching leaves become `K` applications.
fn abstract_variable_from_expression(
    heap: &mut Heap,
    variable: IdentifierRecordRef,
    expression: Value,
) -> Value {
    let raw: RawValue = expression.into();
    if raw < ATOM_LIMIT {
        return if expression == variable.into() {
            Combinator::I.into()
        } else {
            heap.apply_ref(Combinator::K.into(), expression)
        };
    }

    match heap[raw].tag {
        Tag::Cons | Tag::Pair | Tag::TCons => {
            let lowered_head = abstract_variable_from_expression(heap, variable, heap[raw].head.into());
            let lowered_tail = abstract_variable_from_expression(heap, variable, heap[raw].tail.into());
            combine_list_abstractions(heap, lowered_head, lowered_tail)
        }
        Tag::Ap => {
            let application = ApNodeRef::from_ref(raw);
            if matches!(
                application.function_raw(heap),
                raw if raw == Combinator::BadCase as RawValue
                    || raw == Combinator::ConfError as RawValue
            ) {
                return heap.apply_ref(Combinator::K.into(), expression);
            }

            let lowered_function =
                abstract_variable_from_expression(heap, variable, application.function_raw(heap).into());
            let lowered_argument =
                abstract_variable_from_expression(heap, variable, application.argument_raw(heap).into());
            combine_abstractions(heap, lowered_function, lowered_argument)
        }
        _ => {
            if expression == variable.into() {
                Combinator::I.into()
            } else {
                heap.apply_ref(Combinator::K.into(), expression)
            }
        }
    }
}

/// Returns the constructor payload value used by active Miranda constructor-pattern lowering. This
/// exists so codegen owns one query seam from committed template heads to the constructor payload
/// fed into `Ug`. The invariant is that constructor-valued identifiers reuse their committed value
/// payload, while direct constructor cells pass through unchanged.
fn constructor_template_value(heap: &Heap, template: Value) -> Option<Value> {
    let raw: RawValue = template.into();
    if raw < ATOM_LIMIT {
        return None;
    }

    match heap[raw].tag {
        Tag::Id => {
            let identifier = IdentifierRecordRef::from_ref(raw);
            identifier
                .is_constructor_valued(heap)
                .then(|| identifier.get_value(heap).map(|value| value.get_ref().into()))
                .flatten()
        }
        Tag::Constructor => Some(template),
        _ => None,
    }
}

/// Lowers one active committed binder/template against one lowered expression using the current Miranda-shaped subset.
/// This exists so codegen owns the active lambda-binder family in one place instead of scattering template cases through the `Tag::Lambda` branch.
/// The invariant is that simple binders use bracket abstraction, constant/structural patterns use `MATCH`/`MATCHINT`/`U`/`U_`, constructor applications use `Ug`, and canonical `n+k` uses `ATLEAST`.
fn abstract_template_from_expression(heap: &mut Heap, template: Value, expression: Value) -> Value {
    if let Some(constructor_value) = constructor_template_value(heap, template) {
        return heap.apply2(Combinator::Ug.into(), constructor_value, expression);
    }

    let raw: RawValue = template.into();
    if raw < ATOM_LIMIT {
        return expression;
    }

    match heap[raw].tag {
        Tag::Id => abstract_variable_from_expression(heap, IdentifierRecordRef::from_ref(raw), expression),
        Tag::Cons if heap[raw].head == RawValue::from(Value::Token(Token::Constant)) => {
            if heap[raw].tail >= ATOM_LIMIT && heap[heap[raw].tail].tag == Tag::Int {
                heap.apply2(Combinator::MatchInt.into(), heap[raw].tail.into(), expression)
            } else {
                let matched_value = if heap[raw].tail == Combinator::Nils.into() {
                    Combinator::Nil.into()
                } else {
                    heap[raw].tail.into()
                };
                heap.apply2(Combinator::Match.into(), matched_value, expression)
            }
        }
        Tag::Cons => {
            let lowered_tail =
                abstract_template_from_expression(heap, heap[raw].tail.into(), expression);
            let lowered =
                abstract_template_from_expression(heap, heap[raw].head.into(), lowered_tail);
            heap.apply_ref(Combinator::U_.into(), lowered)
        }
        Tag::Pair | Tag::TCons => {
            let lowered_tail =
                abstract_template_from_expression(heap, heap[raw].tail.into(), expression);
            let lowered =
                abstract_template_from_expression(heap, heap[raw].head.into(), lowered_tail);
            heap.apply_ref(Combinator::U.into(), lowered)
        }
        Tag::Ap => {
            let application = ApNodeRef::from_ref(raw);
            if let Some(function_application) = application.function_application(heap) {
                if function_application.function_raw(heap) == Combinator::Plus as RawValue {
                    let lowered_argument = abstract_template_from_expression(
                        heap,
                        application.argument_raw(heap).into(),
                        expression,
                    );
                    return heap.apply2(
                        Combinator::AtLeast.into(),
                        function_application.argument_raw(heap).into(),
                        lowered_argument,
                    );
                }
            }

            let mut lowered_expression = expression;
            let mut cursor = raw;
            while cursor >= ATOM_LIMIT && heap[cursor].tag == Tag::Ap {
                let next_application = ApNodeRef::from_ref(cursor);
                lowered_expression = abstract_template_from_expression(
                    heap,
                    next_application.argument_raw(heap).into(),
                    lowered_expression,
                );
                cursor = next_application.function_raw(heap);
            }

            constructor_template_value(heap, cursor.into()).map_or(lowered_expression, |constructor_value| {
                heap.apply2(Combinator::Ug.into(), constructor_value, lowered_expression)
            })
        }
        _ => expression,
    }
}

/// Lowers one committed `Tag::Let` body through the current Miranda-shaped active subset. This
/// exists so codegen owns `translet`-style lowering in one place instead of open-coding definition
/// projection in the main dispatcher. The invariant is that the lowered shape is `ap(abstract(lhs,
/// codegen(body)), codegen(rhs))` over the committed definition payload.
fn lower_let_value(heap: &mut Heap, let_value: Value) -> Value {
    let let_raw: RawValue = let_value.into();
    let definition = DefinitionRef::from_ref(heap[let_raw].head);
    let lowered_body = lower_codegen_value(heap, heap[let_raw].tail.into());
    let lowered_rhs = lower_codegen_value(heap, definition.body_value(heap));
    let lowered_function =
        abstract_template_from_expression(heap, definition.lhs_value(heap), lowered_body);
    heap.apply_ref(lowered_function, lowered_rhs)
}

/// Bracket-abstracts a list of active simple identifier binders from one lowered expression. This
/// exists so codegen owns the `abstrlist` subset needed by active `LetRec` lowering over simple-id
/// definition groups. The invariant is that matching identifiers lower to `SUBSCRIPT` indices while
/// other leaves become `K` applications.
fn abstract_variable_list_from_expression(heap: &mut Heap, variables: Value, expression: Value) -> Value {
    let expression_raw: RawValue = expression.into();
    if expression_raw < ATOM_LIMIT {
        let mut index: RawValue = 0;
        let mut cursor = variables;
        while RawValue::from(cursor) >= ATOM_LIMIT && heap[RawValue::from(cursor)].tag == Tag::Cons {
            let cons_ref = RawValue::from(cursor);
            if heap[cons_ref].head == expression_raw {
                return heap.apply_ref(Combinator::Subscript.into(), Value::Data(index));
            }
            cursor = heap[cons_ref].tail.into();
            index += 1;
        }
        return heap.apply_ref(Combinator::K.into(), expression);
    }

    match heap[expression_raw].tag {
        Tag::Cons | Tag::Pair | Tag::TCons => {
            let lowered_head = abstract_variable_list_from_expression(heap, variables, heap[expression_raw].head.into());
            let lowered_tail = abstract_variable_list_from_expression(heap, variables, heap[expression_raw].tail.into());
            combine_list_abstractions(heap, lowered_head, lowered_tail)
        }
        Tag::Ap => {
            let application = ApNodeRef::from_ref(expression_raw);
            if matches!(
                application.function_raw(heap),
                raw if raw == Combinator::BadCase as RawValue
                    || raw == Combinator::ConfError as RawValue
            ) {
                return heap.apply_ref(Combinator::K.into(), expression);
            }
            let lowered_function = abstract_variable_list_from_expression(
                heap,
                variables,
                application.function_raw(heap).into(),
            );
            let lowered_argument = abstract_variable_list_from_expression(
                heap,
                variables,
                application.argument_raw(heap).into(),
            );
            combine_abstractions(heap, lowered_function, lowered_argument)
        }
        _ => heap.apply_ref(Combinator::K.into(), expression),
    }
}

/// Collects the active bound identifiers from one committed recursive-template pattern.
/// This exists so codegen owns the identifier-extraction step needed by Miranda's `new_mklazy`
/// `LetRec` lowering without depending on the typecheck boundary. The invariant is that binder
/// identifiers are collected in first-occurrence order while constructor heads, wrapped constants,
/// and successor offsets remain non-binding.
fn collect_recursive_pattern_identifiers(
    heap: &Heap,
    pattern: Value,
    bound_identifiers: &mut Vec<IdentifierRecordRef>,
) {
    let raw: RawValue = pattern.into();
    if raw < ATOM_LIMIT {
        return;
    }

    match heap[raw].tag {
        Tag::Id => {
            let identifier = IdentifierRecordRef::from_ref(raw);
            if !identifier.is_constructor_valued(heap) && !bound_identifiers.contains(&identifier) {
                bound_identifiers.push(identifier);
            }
        }
        Tag::Cons => {
            if heap[raw].head == RawValue::from(Value::Token(Token::Constant)) {
                return;
            }

            collect_recursive_pattern_identifiers(heap, heap[raw].head.into(), bound_identifiers);
            collect_recursive_pattern_identifiers(heap, heap[raw].tail.into(), bound_identifiers);
        }
        Tag::Pair | Tag::TCons => {
            collect_recursive_pattern_identifiers(heap, heap[raw].head.into(), bound_identifiers);
            collect_recursive_pattern_identifiers(heap, heap[raw].tail.into(), bound_identifiers);
        }
        Tag::Ap => {
            let application = ApNodeRef::from_ref(raw);
            if let Some(function_application) = application.function_application(heap) {
                if function_application.function_raw(heap) == Combinator::Plus as RawValue {
                    collect_recursive_pattern_identifiers(
                        heap,
                        application.argument_raw(heap).into(),
                        bound_identifiers,
                    );
                    return;
                }
            }

            collect_recursive_pattern_identifiers(
                heap,
                application.argument_raw(heap).into(),
                bound_identifiers,
            );
            let function = application.function_raw(heap).into();
            if constructor_template_value(heap, function).is_none() {
                collect_recursive_pattern_identifiers(heap, function, bound_identifiers);
            }
        }
        _ => {}
    }
}

/// Lowers one committed `Tag::LetRec` body through the current Miranda-shaped active subset.
/// This exists so codegen owns the active `transletrec` lowering slice over committed recursive
/// definition groups, including Miranda's private-carrier projection strategy for non-identifier
/// lhs definitions. The invariant is that singleton simple-id groups lower through `Y` with
/// `abstr`, while broader groups lower through `abstrlist` over the Miranda-shaped lhs/rhs order.
fn lower_letrec_value(heap: &mut Heap, letrec_value: Value) -> Value {
    let letrec_raw: RawValue = letrec_value.into();
    let mut definitions: Value = heap[letrec_raw].head.into();
    let mut lhs = Combinator::Nil.into();
    let mut rhs = Combinator::Nil.into();

    while RawValue::from(definitions) >= ATOM_LIMIT && heap[RawValue::from(definitions)].tag == Tag::Cons {
        let definitions_ref = RawValue::from(definitions);
        let definition = DefinitionRef::from_ref(heap[definitions_ref].head);
        let lhs_value = definition.lhs_value(heap);
        let lhs_raw: RawValue = lhs_value.into();
        let lowered_rhs = lower_codegen_value(heap, definition.body_value(heap));

        if lhs_raw >= ATOM_LIMIT && heap[lhs_raw].tag == Tag::Id {
            lhs = heap.cons_ref(lhs_value, lhs);
            rhs = heap.cons_ref(lowered_rhs, rhs);
        } else {
            let recursive_carrier = match heap.make_private_symbol_ref(Combinator::Undef.into()) {
                Value::Reference(reference) => Value::from(reference),
                _ => unreachable!("make_private_symbol_ref should return a heap reference"),
            };
            lhs = heap.cons_ref(recursive_carrier, lhs);
            rhs = heap.cons_ref(lowered_rhs, rhs);

            let mut bound_identifiers = Vec::new();
            collect_recursive_pattern_identifiers(heap, lhs_value, &mut bound_identifiers);
            for (index, identifier) in bound_identifiers.into_iter().enumerate() {
                lhs = heap.cons_ref(identifier.into(), lhs);
                let projection = heap.apply2(
                    Combinator::Subscript.into(),
                    Value::Data(index as RawValue),
                    recursive_carrier,
                );
                rhs = heap.cons_ref(projection, rhs);
            }
        }

        definitions = heap[definitions_ref].tail.into();
    }

    let lowered_body = lower_codegen_value(heap, heap[letrec_raw].tail.into());
    let lhs_raw: RawValue = lhs.into();
    if lhs_raw >= ATOM_LIMIT && heap[lhs_raw].tail == Combinator::Nil.into() {
        let binder = heap[lhs_raw].head;
        let rhs_head = heap[RawValue::from(rhs)].head;
        let lowered_function = abstract_variable_from_expression(
            heap,
            IdentifierRecordRef::from_ref(binder),
            lowered_body,
        );
        let lowered_recursive_rhs = abstract_variable_from_expression(
            heap,
            IdentifierRecordRef::from_ref(binder),
            rhs_head.into(),
        );
        let recursive_application = heap.apply_ref(Combinator::Y.into(), lowered_recursive_rhs);
        return heap.apply_ref(lowered_function, recursive_application);
    }

    let lowered_function = abstract_variable_list_from_expression(heap, lhs, lowered_body);
    let lowered_recursive_rhs = abstract_variable_list_from_expression(heap, lhs, rhs);
    let recursive_application = heap.apply_ref(Combinator::Y.into(), lowered_recursive_rhs);
    heap.apply_ref(lowered_function, recursive_application)
}

/// Returns whether the committed rhs is fallible in the currently owned `transtries` subset.
/// This exists so codegen owns the active default-`BADCASE` decision for `Tag::Tries` lowering instead of leaving it implicit in tests.
/// The invariant is that refutable lambda binders and explicit `FAIL` survive as fallible, while irrefutable simple-id lambdas recurse into their bodies.
fn is_fallible_rhs(heap: &Heap, mut expression: Value) -> bool {
    loop {
        let raw: RawValue = expression.into();
        if raw < ATOM_LIMIT {
            return expression == Combinator::Fail.into();
        }

        match heap[raw].tag {
            Tag::Label => expression = heap[raw].tail.into(),
            Tag::Let | Tag::LetRec => expression = heap[raw].tail.into(),
            Tag::Lambda => {
                let binder_raw = heap[raw].head;
                if binder_raw >= ATOM_LIMIT && heap[binder_raw].tag == Tag::Id {
                    expression = heap[raw].tail.into();
                } else {
                    return true;
                }
            }
            _ => return expression == Combinator::Fail.into(),
        }
    }
}

/// Lowers one committed `Tag::Tries` body through the current Miranda-shaped active subset.
/// This exists so codegen owns nested `TRY` construction and active default-case insertion for multi-clause definition bodies.
/// The invariant is that reversed alternatives lower into right-associated `TRY` applications, with a terminal `BADCASE` only when the earliest alternative is fallible.
fn lower_tries_value(heap: &mut Heap, tries_value: Value) -> Value {
    let tries_raw: RawValue = tries_value.into();
    let _diagnostic_id: Value = heap[tries_raw].head.into();
    let mut alternatives: Value = heap[tries_raw].tail.into();

    if alternatives == Combinator::Nil.into() {
        return tries_value;
    }

    let earliest_ref = RawValue::from(alternatives);
    let earliest = heap[earliest_ref].head.into();
    let mut lowered = if is_fallible_rhs(heap, earliest) {
        heap.apply_ref(Combinator::BadCase.into(), Combinator::Nil.into())
    } else {
        alternatives = heap[earliest_ref].tail.into();
        lower_codegen_value(heap, earliest)
    };

    while RawValue::from(alternatives) >= ATOM_LIMIT && heap[RawValue::from(alternatives)].tag == Tag::Cons {
        let alternatives_ref = RawValue::from(alternatives);
        let next_alternative = heap[alternatives_ref].head.into();
        let lowered_alternative = lower_codegen_value(heap, next_alternative);
        lowered = heap.apply2(Combinator::Try.into(), lowered_alternative, lowered);
        alternatives = heap[alternatives_ref].tail.into();
    }

    lowered
}

/// Lowers one committed definition body through the currently owned codegen rewrite subset. This
/// exists so the codegen subsystem owns recursive body rewrites in one place instead of scattering
/// shape handling across the file-definition walk. The invariant is that `Label` nodes lower away
/// to their rewritten payload, application nodes preserve order while applying the current `APPEND
/// [] x -> x` rewrite, Miranda tuple/list-like `Pair`/`TCons` cells normalize into `Cons`, active
/// lambda binders lower through the current Miranda-shaped abstraction subset, and the current
/// `Let` / `LetRec` / `Tries` family lowers through the corresponding owned local-control subset.
fn lower_codegen_value(heap: &mut Heap, value: Value) -> Value {
    let raw: RawValue = value.into();
    if raw < ATOM_LIMIT {
        return value;
    }

    match heap[raw].tag {
        Tag::Ap => {
            let application = ApNodeRef::from_ref(raw);
            if let Some(function_application) = application.function_application(heap) {
                if function_application.function_raw(heap) == Combinator::Append as RawValue
                    && function_application.argument_raw(heap) == Combinator::Nil as RawValue
                {
                    return lower_codegen_value(heap, application.argument_raw(heap).into());
                }
            }

            let lowered_function = lower_codegen_value(heap, application.function_raw(heap).into());
            let lowered_argument = lower_codegen_value(heap, application.argument_raw(heap).into());
            heap.apply_ref(lowered_function, lowered_argument)
        }
        Tag::Cons | Tag::Pair | Tag::TCons => {
            let lowered_head = lower_codegen_value(heap, heap[raw].head.into());
            let lowered_tail = lower_codegen_value(heap, heap[raw].tail.into());
            heap.cons_ref(lowered_head, lowered_tail)
        }
        Tag::Label => lower_codegen_value(heap, heap[raw].tail.into()),
        Tag::Lambda => {
            let lowered_body = lower_codegen_value(heap, heap[raw].tail.into());
            abstract_template_from_expression(heap, heap[raw].head.into(), lowered_body)
        }
        Tag::Let => lower_let_value(heap, value),
        Tag::LetRec => lower_letrec_value(heap, value),
        Tag::Tries => lower_tries_value(heap, value),
        _ => value,
    }
}

/// Executes the current load-time partial codegen boundary over committed source substrate.
pub(super) fn run_partial_codegen(
    heap: &mut Heap,
    inputs: CodegenBoundaryInputs,
) -> CodegenBoundaryResult {
    if inputs.files.is_empty() {
        return CodegenBoundaryResult {
            processed_binding_count: 0,
            failure: Some(CodegenError::NoLoadedFiles),
        };
    }

    let mut processed_binding_count = 0usize;
    if let Some(current_file) = inputs.current_file {
        let mut definienda = current_file.get_definienda(heap);
        while let Some(identifier) = definienda.pop(heap) {
            let Some(value) = identifier.get_value(heap) else {
                continue;
            };

            if let IdentifierValueData::Arbitrary(body) = value.get_data(heap) {
                processed_binding_count += 1;
                let lowered_body = lower_codegen_value(heap, body);
                identifier.set_value_from_data(heap, IdentifierValueData::Arbitrary(lowered_body));
            }
        }
    }

    let failure = if inputs.initializing && !inputs.undefined_names.is_empty() {
        Some(CodegenError::InitializationBlockedByUnresolvedNames)
    } else {
        None
    };

    CodegenBoundaryResult {
        processed_binding_count,
        failure,
    }
}
