use super::*;
use crate::data::api::{IdentifierRecordRef, IdentifierValueTypeKind, IdentifierValueTypeRef};
use crate::data::heap::is_capitalized;
use crate::data::{Heap, RawValue, Tag, Type, Value, ATOM_LIMIT};

pub(super) struct TypecheckBoundaryInputs {
    current_file: Option<FileRecord>,
    detritus_parameter_bindings: ConsList<Value>,
    missing_parameter_bindings: ConsList<Value>,
}

impl TypecheckBoundaryInputs {
    /// Captures the committed load-time substrate the partial typecheck boundary reads.
    /// This exists so the typecheck subsystem owns its input surface instead of reaching through `load.rs` state ad hoc.
    /// The invariant is that the boundary sees only already-committed current-file and `%free` state from the active load cycle.
    pub(super) fn from_vm(vm: &VM) -> Self {
        Self {
            current_file: vm.files.head(&vm.heap),
            detritus_parameter_bindings: vm.detritus_parameter_bindings,
            missing_parameter_bindings: vm.missing_parameter_bindings,
        }
    }
}

pub(super) struct TypecheckBoundaryResult {
    pub(super) undefined_names: ConsList<IdentifierRecordRef>,
    pub(super) checked_definition_count: usize,
    pub(super) failure: Option<TypecheckError>,
}

/// Executes the current load-time partial typecheck boundary over committed source substrate.
pub(super) fn run_partial_typecheck(
    heap: &mut Heap,
    inputs: TypecheckBoundaryInputs,
) -> TypecheckBoundaryResult {
    let mut undefined_names = ConsList::EMPTY;
    let mut type_names_used_as_identifiers = ConsList::EMPTY;
    let mut undefined_type_names = ConsList::EMPTY;
    let mut non_type_identifiers = ConsList::EMPTY;
    let mut arity_mismatch_type_names = ConsList::EMPTY;
    let mut unbound_abstract_type_names = ConsList::EMPTY;
    let mut specified_but_not_defined = ConsList::EMPTY;
    let mut undeclared_constructors_in_formals = ConsList::EMPTY;
    let mut constructor_arity_mismatch_in_formals = ConsList::EMPTY;
    let mut checked_definition_count = 0usize;

    if let Some(current_file) = inputs.current_file {
        let mut definienda = current_file.get_definienda(heap);
        while let Some(identifier) = definienda.pop(heap) {
            let identifier_type_raw = RawValue::from(identifier.get_type(heap));
            if identifier_type_raw != RawValue::from(Type::Undefined)
                && identifier_type_raw != RawValue::from(Type::Type)
            {
                collect_type_expression_issues(
                    heap,
                    identifier.get_type(heap),
                    &mut undefined_type_names,
                    &mut non_type_identifiers,
                    &mut arity_mismatch_type_names,
                );
            }

            let Some(value) = identifier.get_value(heap) else {
                continue;
            };

            if is_specified_but_not_defined(heap, identifier) {
                specified_but_not_defined.insert_ordered(heap, identifier);
            }

            match value.get_data(heap) {
                IdentifierValueData::Arbitrary(body) => {
                    checked_definition_count += 1;
                    collect_formal_pattern_issues(
                        heap,
                        body,
                        &mut undeclared_constructors_in_formals,
                        &mut constructor_arity_mismatch_in_formals,
                    );
                    let mut bound_identifiers = Vec::new();
                    collect_unresolved_identifier_references(
                        heap,
                        body,
                        &mut bound_identifiers,
                        &mut undefined_names,
                        &mut type_names_used_as_identifiers,
                    );
                }
                IdentifierValueData::Typed { value_type, .. } => {
                    match value_type.get_identifier_value_type_kind(heap) {
                        IdentifierValueTypeKind::Synonym => {
                            collect_type_expression_issues(
                                heap,
                                synonym_rhs_type_expr(heap, value_type),
                                &mut undefined_type_names,
                                &mut non_type_identifiers,
                                &mut arity_mismatch_type_names,
                            );
                        }
                        IdentifierValueTypeKind::Abstract => {
                            let basis = abstract_type_basis(heap, value_type);
                            if RawValue::from(basis) == RawValue::from(Type::Undefined) {
                                unbound_abstract_type_names.insert_ordered(heap, identifier);
                            }
                        }
                        _ => {}
                    }
                }
                IdentifierValueData::Undefined => {}
            }
        }
    }

    let detritus_count = inputs.detritus_parameter_bindings.len(heap);
    let missing_count = inputs.missing_parameter_bindings.len(heap);
    let undefined_type_name_count = undefined_type_names.len(heap);
    let type_names_used_as_identifiers_count = type_names_used_as_identifiers.len(heap);
    let non_type_identifier_count = non_type_identifiers.len(heap);
    let arity_mismatch_count = arity_mismatch_type_names.len(heap);
    let unbound_abstract_count = unbound_abstract_type_names.len(heap);
    let specified_but_not_defined_count = specified_but_not_defined.len(heap);
    let undeclared_constructor_formal_count = undeclared_constructors_in_formals.len(heap);
    let constructor_formal_arity_mismatch_count = constructor_arity_mismatch_in_formals.len(heap);
    while let Some(identifier) = undefined_type_names.pop(heap) {
        undefined_names.insert_ordered(heap, identifier);
    }
    while let Some(identifier) = type_names_used_as_identifiers.pop(heap) {
        undefined_names.insert_ordered(heap, identifier);
    }
    while let Some(identifier) = non_type_identifiers.pop(heap) {
        undefined_names.insert_ordered(heap, identifier);
    }
    while let Some(identifier) = arity_mismatch_type_names.pop(heap) {
        undefined_names.insert_ordered(heap, identifier);
    }
    while let Some(identifier) = unbound_abstract_type_names.pop(heap) {
        undefined_names.insert_ordered(heap, identifier);
    }
    while let Some(identifier) = specified_but_not_defined.pop(heap) {
        undefined_names.insert_ordered(heap, identifier);
    }
    while let Some(identifier) = undeclared_constructors_in_formals.pop(heap) {
        undefined_names.insert_ordered(heap, identifier);
    }
    while let Some(identifier) = constructor_arity_mismatch_in_formals.pop(heap) {
        undefined_names.insert_ordered(heap, identifier);
    }
    let undefined_count = undefined_names.len(heap);
    let failure = if detritus_count > 0 {
        Some(TypecheckError::InvalidFreeBindings {
            count: detritus_count,
        })
    } else if missing_count > 0 {
        Some(TypecheckError::MissingFreeBindings {
            count: missing_count,
        })
    } else if undefined_type_name_count > 0 {
        Some(TypecheckError::UndefinedTypeNames {
            count: undefined_type_name_count,
        })
    } else if type_names_used_as_identifiers_count > 0 {
        Some(TypecheckError::TypeNamesUsedAsIdentifiers {
            count: type_names_used_as_identifiers_count,
        })
    } else if non_type_identifier_count > 0 {
        Some(TypecheckError::NonTypeIdentifiersInTypeExpr {
            count: non_type_identifier_count,
        })
    } else if arity_mismatch_count > 0 {
        Some(TypecheckError::TypeArityMismatch {
            count: arity_mismatch_count,
        })
    } else if unbound_abstract_count > 0 {
        Some(TypecheckError::UnboundAbstractTypeNames {
            count: unbound_abstract_count,
        })
    } else if specified_but_not_defined_count > 0 {
        Some(TypecheckError::SpecifiedButNotDefined {
            count: specified_but_not_defined_count,
        })
    } else if undeclared_constructor_formal_count > 0 {
        Some(TypecheckError::UndeclaredConstructorsInFormals {
            count: undeclared_constructor_formal_count,
        })
    } else if constructor_formal_arity_mismatch_count > 0 {
        Some(TypecheckError::ConstructorArityMismatchInFormals {
            count: constructor_formal_arity_mismatch_count,
        })
    } else if undefined_count > 0 {
        Some(TypecheckError::UndefinedNames {
            count: undefined_count,
        })
    } else {
        None
    };

    TypecheckBoundaryResult {
        undefined_names,
        checked_definition_count,
        failure,
    }
}

/// Walks committed lambda-head patterns and records constructor-formal misuse in the active subset.
/// This exists so the typecheck boundary owns formal-pattern diagnostics over the already lowered top-level forms.
/// The invariant is that only constructor-intent heads are diagnosed; lowercase binders remain value binders.
fn collect_formal_pattern_issues(
    heap: &mut Heap,
    expression: Value,
    undeclared_constructors_in_formals: &mut ConsList<IdentifierRecordRef>,
    constructor_arity_mismatch_in_formals: &mut ConsList<IdentifierRecordRef>,
) {
    let raw_reference: RawValue = expression.into();
    if raw_reference < ATOM_LIMIT {
        return;
    }

    match heap[raw_reference].tag {
        Tag::Lambda => {
            let pattern = heap[raw_reference].head.into();
            collect_formal_pattern_issues_in_pattern(
                heap,
                pattern,
                undeclared_constructors_in_formals,
                constructor_arity_mismatch_in_formals,
            );
            let body = heap[raw_reference].tail.into();
            collect_formal_pattern_issues(
                heap,
                body,
                undeclared_constructors_in_formals,
                constructor_arity_mismatch_in_formals,
            );
        }
        Tag::Label | Tag::Show => {
            collect_formal_pattern_issues(
                heap,
                heap[raw_reference].tail.into(),
                undeclared_constructors_in_formals,
                constructor_arity_mismatch_in_formals,
            );
        }
        Tag::Share => {
            collect_formal_pattern_issues(
                heap,
                heap[raw_reference].head.into(),
                undeclared_constructors_in_formals,
                constructor_arity_mismatch_in_formals,
            );
        }
        _ => {}
    }
}

fn collect_formal_pattern_issues_in_pattern(
    heap: &mut Heap,
    pattern: Value,
    undeclared_constructors_in_formals: &mut ConsList<IdentifierRecordRef>,
    constructor_arity_mismatch_in_formals: &mut ConsList<IdentifierRecordRef>,
) {
    let raw_reference: RawValue = pattern.into();
    if raw_reference < ATOM_LIMIT {
        return;
    }

    match heap[raw_reference].tag {
        Tag::Id => {
            let identifier = IdentifierRecordRef::from_ref(raw_reference);
            if identifier_has_constructor_intent(heap, identifier)
                && !is_constructor_identifier(heap, identifier)
            {
                undeclared_constructors_in_formals.insert_ordered(heap, identifier);
            }
        }
        Tag::Ap => {
            if let Some((head, arguments)) = pattern_application_spine(heap, pattern) {
                if is_constructor_identifier(heap, head) {
                    constructor_arity_mismatch_in_formals.insert_ordered(heap, head);
                } else {
                    undeclared_constructors_in_formals.insert_ordered(heap, head);
                }

                for argument in arguments {
                    collect_formal_pattern_issues_in_pattern(
                        heap,
                        argument,
                        undeclared_constructors_in_formals,
                        constructor_arity_mismatch_in_formals,
                    );
                }
                return;
            }

            collect_formal_pattern_issues_in_pattern(
                heap,
                heap[raw_reference].head.into(),
                undeclared_constructors_in_formals,
                constructor_arity_mismatch_in_formals,
            );
            collect_formal_pattern_issues_in_pattern(
                heap,
                heap[raw_reference].tail.into(),
                undeclared_constructors_in_formals,
                constructor_arity_mismatch_in_formals,
            );
        }
        Tag::Cons | Tag::Pair | Tag::TCons => {
            collect_formal_pattern_issues_in_pattern(
                heap,
                heap[raw_reference].head.into(),
                undeclared_constructors_in_formals,
                constructor_arity_mismatch_in_formals,
            );
            collect_formal_pattern_issues_in_pattern(
                heap,
                heap[raw_reference].tail.into(),
                undeclared_constructors_in_formals,
                constructor_arity_mismatch_in_formals,
            );
        }
        _ => {}
    }
}

/// Walks one expression subtree and records unresolved free identifier references.
/// This exists so the typecheck boundary owns undefined-name production over committed definition bodies.
/// The invariant is that locally bound identifiers are excluded before unresolved names are inserted into the result list.
fn collect_unresolved_identifier_references(
    heap: &mut Heap,
    expression: Value,
    bound_identifiers: &mut Vec<IdentifierRecordRef>,
    undefined_names: &mut ConsList<IdentifierRecordRef>,
    type_names_used_as_identifiers: &mut ConsList<IdentifierRecordRef>,
) {
    let raw_reference: RawValue = expression.into();
    if raw_reference < ATOM_LIMIT {
        return;
    }

    match heap[raw_reference].tag {
        Tag::Id => {
            let identifier = IdentifierRecordRef::from_ref(raw_reference);
            if bound_identifiers.contains(&identifier)
                || is_constructor_identifier(heap, identifier)
            {
                return;
            }

            match classify_expression_identifier_reference(heap, identifier) {
                ExpressionIdentifierReferenceKind::ValidRuntimeIdentifier => {}
                ExpressionIdentifierReferenceKind::UndefinedName => {
                    undefined_names.insert_ordered(heap, identifier);
                }
                ExpressionIdentifierReferenceKind::TypeNameUsedAsIdentifier => {
                    type_names_used_as_identifiers.insert_ordered(heap, identifier);
                }
            }
        }
        Tag::Ap | Tag::Cons | Tag::Pair | Tag::TCons => {
            let left = heap[raw_reference].head.into();
            let right = heap[raw_reference].tail.into();
            collect_unresolved_identifier_references(
                heap,
                left,
                bound_identifiers,
                undefined_names,
                type_names_used_as_identifiers,
            );
            collect_unresolved_identifier_references(
                heap,
                right,
                bound_identifiers,
                undefined_names,
                type_names_used_as_identifiers,
            );
        }
        Tag::Lambda => {
            let pattern = heap[raw_reference].head.into();
            let prior_bound_len = bound_identifiers.len();
            collect_pattern_bound_identifiers(heap, pattern, bound_identifiers);
            let body = heap[raw_reference].tail.into();
            collect_unresolved_identifier_references(
                heap,
                body,
                bound_identifiers,
                undefined_names,
                type_names_used_as_identifiers,
            );
            bound_identifiers.truncate(prior_bound_len);
        }
        Tag::Label | Tag::Show => {
            let nested = heap[raw_reference].tail.into();
            collect_unresolved_identifier_references(
                heap,
                nested,
                bound_identifiers,
                undefined_names,
                type_names_used_as_identifiers,
            );
        }
        Tag::Share => {
            let shared = heap[raw_reference].head.into();
            collect_unresolved_identifier_references(
                heap,
                shared,
                bound_identifiers,
                undefined_names,
                type_names_used_as_identifiers,
            );
        }
        _ => {}
    }
}

/// Walks one committed type-expression subtree and records typename classification and arity issues.
/// This exists so the typecheck boundary owns spec/type meta-checking over the active source-fed substrate.
/// The invariant is that typename leaves are classified and arity-checked from the canonical application spine view.
fn collect_type_expression_issues(
    heap: &mut Heap,
    type_expr: Value,
    undefined_type_names: &mut ConsList<IdentifierRecordRef>,
    non_type_identifiers: &mut ConsList<IdentifierRecordRef>,
    arity_mismatch_type_names: &mut ConsList<IdentifierRecordRef>,
) {
    let raw_reference: RawValue = type_expr.into();
    if raw_reference < ATOM_LIMIT {
        return;
    }

    if let Some((identifier, applied_argument_count, arguments)) =
        type_application_spine(heap, type_expr)
    {
        match classify_type_identifier_reference(heap, identifier) {
            TypeIdentifierReferenceKind::ValidTypeName => {
                if let Some(expected_arity) = type_identifier_arity(heap, identifier) {
                    if expected_arity != applied_argument_count {
                        arity_mismatch_type_names.insert_ordered(heap, identifier);
                    }
                }
            }
            TypeIdentifierReferenceKind::UndefinedTypeName => {
                undefined_type_names.insert_ordered(heap, identifier);
            }
            TypeIdentifierReferenceKind::NonTypeIdentifier => {
                non_type_identifiers.insert_ordered(heap, identifier);
            }
        }

        for argument in arguments {
            collect_type_expression_issues(
                heap,
                argument,
                undefined_type_names,
                non_type_identifiers,
                arity_mismatch_type_names,
            );
        }
        return;
    }

    match heap[raw_reference].tag {
        Tag::Id => {}
        Tag::Cons | Tag::Pair | Tag::TCons => {
            collect_type_expression_issues(
                heap,
                heap[raw_reference].head.into(),
                undefined_type_names,
                non_type_identifiers,
                arity_mismatch_type_names,
            );
            collect_type_expression_issues(
                heap,
                heap[raw_reference].tail.into(),
                undefined_type_names,
                non_type_identifiers,
                arity_mismatch_type_names,
            );
        }
        Tag::Label | Tag::Show | Tag::Share => {
            collect_type_expression_issues(
                heap,
                heap[raw_reference].tail.into(),
                undefined_type_names,
                non_type_identifiers,
                arity_mismatch_type_names,
            );
        }
        _ => {}
    }
}

/// Collects identifiers bound by a pattern node into the active local-binding scope.
/// This exists so lambda-bound names are excluded from undefined-name diagnostics in the body they bind.
/// The invariant is that constructor heads and literal forms do not add local bindings.
fn collect_pattern_bound_identifiers(
    heap: &Heap,
    pattern: Value,
    bound_identifiers: &mut Vec<IdentifierRecordRef>,
) {
    let raw_reference: RawValue = pattern.into();
    if raw_reference < ATOM_LIMIT {
        return;
    }

    match heap[raw_reference].tag {
        Tag::Id => {
            let identifier = IdentifierRecordRef::from_ref(raw_reference);
            if !identifier_has_constructor_intent(heap, identifier)
                && !bound_identifiers.contains(&identifier)
            {
                bound_identifiers.push(identifier);
            }
        }
        Tag::Cons | Tag::Pair | Tag::TCons => {
            collect_pattern_bound_identifiers(
                heap,
                heap[raw_reference].head.into(),
                bound_identifiers,
            );
            collect_pattern_bound_identifiers(
                heap,
                heap[raw_reference].tail.into(),
                bound_identifiers,
            );
        }
        Tag::Ap => {
            if let Some((_head, arguments)) = pattern_application_spine(heap, pattern) {
                for argument in arguments {
                    collect_pattern_bound_identifiers(heap, argument, bound_identifiers);
                }
            } else {
                collect_pattern_bound_identifiers(
                    heap,
                    heap[raw_reference].head.into(),
                    bound_identifiers,
                );
                collect_pattern_bound_identifiers(
                    heap,
                    heap[raw_reference].tail.into(),
                    bound_identifiers,
                );
            }
        }
        _ => {}
    }
}

fn identifier_has_constructor_intent(heap: &Heap, identifier: IdentifierRecordRef) -> bool {
    is_capitalized(identifier.get_name(heap).as_str())
}

fn pattern_application_spine(
    heap: &Heap,
    pattern: Value,
) -> Option<(IdentifierRecordRef, Vec<Value>)> {
    let mut raw_reference: RawValue = pattern.into();
    if raw_reference < ATOM_LIMIT {
        return None;
    }

    let mut arguments = Vec::new();
    while heap[raw_reference].tag == Tag::Ap {
        arguments.push(heap[raw_reference].tail.into());
        raw_reference = heap[raw_reference].head;
        if raw_reference < ATOM_LIMIT {
            return None;
        }
    }

    if heap[raw_reference].tag != Tag::Id {
        return None;
    }

    arguments.reverse();
    Some((IdentifierRecordRef::from_ref(raw_reference), arguments))
}

/// Returns whether an identifier currently denotes a constructor-valued binding.
/// This exists so constructor names are not misclassified as unresolved variable references during typecheck scanning.
/// The invariant is that only identifiers whose value payload points at a `Tag::Constructor` cell are treated as constructors here.
fn is_constructor_identifier(heap: &Heap, identifier: IdentifierRecordRef) -> bool {
    let raw_value: RawValue = identifier.get_value_field(heap).into();
    raw_value >= ATOM_LIMIT && heap[raw_value].tag == Tag::Constructor
}

/// Projects the committed synonym RHS type-expression payload from a typed type-identifier value.
/// This exists so synonym meta-checking reads the parser-committed RHS payload through one subsystem-owned seam.
/// The invariant is that the returned value is the `tail` payload of the `value_type` node for synonym type identifiers.
fn synonym_rhs_type_expr(heap: &Heap, value_type: IdentifierValueTypeRef) -> Value {
    debug_assert_eq!(
        value_type.get_identifier_value_type_kind(heap),
        IdentifierValueTypeKind::Synonym
    );
    heap[value_type.get_ref()].tail.into()
}

/// Projects the committed basis payload from a typed abstract type-identifier value.
/// This exists so abstract-type declaration checks read the parser/runtime-committed basis payload through one subsystem-owned seam.
/// The invariant is that the returned value is the `tail` payload of the `value_type` node for abstract type identifiers.
fn abstract_type_basis(heap: &Heap, value_type: IdentifierValueTypeRef) -> Value {
    debug_assert_eq!(
        value_type.get_identifier_value_type_kind(heap),
        IdentifierValueTypeKind::Abstract
    );
    heap[value_type.get_ref()].tail.into()
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum TypeIdentifierReferenceKind {
    ValidTypeName,
    UndefinedTypeName,
    NonTypeIdentifier,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum ExpressionIdentifierReferenceKind {
    ValidRuntimeIdentifier,
    UndefinedName,
    TypeNameUsedAsIdentifier,
}

/// Classifies one identifier leaf in a committed type expression.
/// This exists so the partial typecheck boundary uses one owner for undefined-typename vs wrong-kind diagnostics.
/// The invariant is that only identifiers whose committed type slot is `type_t` are accepted as legal typename leaves.
fn classify_type_identifier_reference(
    heap: &Heap,
    identifier: IdentifierRecordRef,
) -> TypeIdentifierReferenceKind {
    let identifier_type = RawValue::from(identifier.get_type(heap));
    if identifier_type == RawValue::from(Type::Type) {
        TypeIdentifierReferenceKind::ValidTypeName
    } else if identifier_type == RawValue::from(Type::Undefined)
        && identifier
            .get_value(heap)
            .is_some_and(|value| matches!(value.get_data(heap), IdentifierValueData::Undefined))
    {
        TypeIdentifierReferenceKind::UndefinedTypeName
    } else {
        TypeIdentifierReferenceKind::NonTypeIdentifier
    }
}

/// Classifies one identifier leaf in an expression context.
/// This exists so the partial typecheck boundary can distinguish undefined names from typenames used as runtime identifiers.
/// The invariant is that identifiers whose committed type slot is `type_t` are rejected as expression-side identifiers unless they are constructor-valued.
fn classify_expression_identifier_reference(
    heap: &Heap,
    identifier: IdentifierRecordRef,
) -> ExpressionIdentifierReferenceKind {
    let identifier_type = RawValue::from(identifier.get_type(heap));
    if identifier_type == RawValue::from(Type::Type) {
        ExpressionIdentifierReferenceKind::TypeNameUsedAsIdentifier
    } else if identifier_type == RawValue::from(Type::Undefined)
        && identifier
            .get_value(heap)
            .is_some_and(|value| matches!(value.get_data(heap), IdentifierValueData::Undefined))
    {
        ExpressionIdentifierReferenceKind::UndefinedName
    } else {
        ExpressionIdentifierReferenceKind::ValidRuntimeIdentifier
    }
}

/// Returns the canonical application-spine view for a type-expression rooted at an identifier head.
/// This exists so typename classification and arity checking use one normalized representation.
/// The invariant is that returned `applied_argument_count` equals `arguments.len()` and counts only explicit applied arguments.
fn type_application_spine(
    heap: &Heap,
    type_expr: Value,
) -> Option<(IdentifierRecordRef, usize, Vec<Value>)> {
    let mut raw_reference: RawValue = type_expr.into();
    if raw_reference < ATOM_LIMIT {
        return None;
    }

    let mut arguments = Vec::new();
    while heap[raw_reference].tag == Tag::Ap {
        arguments.push(heap[raw_reference].tail.into());
        raw_reference = heap[raw_reference].head;
        if raw_reference < ATOM_LIMIT {
            return None;
        }
    }

    if heap[raw_reference].tag != Tag::Id {
        return None;
    }

    arguments.reverse();
    Some((
        IdentifierRecordRef::from_ref(raw_reference),
        arguments.len(),
        arguments,
    ))
}

/// Returns the declared arity for a legal typename identifier when available.
/// This exists so the partial typecheck boundary can perform Miranda-shaped typename arity checking.
/// The invariant is that only typed typename identifiers expose an arity through this helper.
fn type_identifier_arity(heap: &Heap, identifier: IdentifierRecordRef) -> Option<usize> {
    let value = identifier.get_value(heap)?;
    let IdentifierValueData::Typed { arity, .. } = value.get_data(heap) else {
        return None;
    };
    Some(arity.max(0) as usize)
}

/// Returns whether the identifier currently has a committed specification/type but no committed value binding.
/// This exists so the partial typecheck boundary can surface Miranda's specified-but-not-defined state as a typed subsystem failure.
/// The invariant is that type identifiers themselves are excluded; only value-level names with a real type payload and undefined value qualify.
fn is_specified_but_not_defined(heap: &Heap, identifier: IdentifierRecordRef) -> bool {
    let identifier_type = RawValue::from(identifier.get_type(heap));
    if identifier_type == RawValue::from(Type::Undefined)
        || identifier_type == RawValue::from(Type::Type)
    {
        return false;
    }

    identifier
        .get_value(heap)
        .is_some_and(|value| matches!(value.get_data(heap), IdentifierValueData::Undefined))
}
