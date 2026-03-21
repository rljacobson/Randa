use super::*;
use crate::big_num::SIGN_BIT_MASK;
use crate::compiler::Token;
use crate::data::api::{
    AlgebraicConstructorMetadataRef, IdentifierRecordRef, IdentifierValueTypeKind,
    IdentifierValueTypeRef, TypeExprRef,
};
use crate::data::heap::is_capitalized;
use crate::data::{Combinator, Heap, RawValue, Tag, Type, Value, ATOM_LIMIT};

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
    let mut unsupported_arithmetic_patterns_in_formals = 0usize;
    let mut value_head_applications_in_formals = 0usize;
    let mut non_identifier_application_heads_in_formals = 0usize;
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
                    identifier.get_type_expr(heap),
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
                        &mut unsupported_arithmetic_patterns_in_formals,
                        &mut value_head_applications_in_formals,
                        &mut non_identifier_application_heads_in_formals,
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
                                value_type.synonym_rhs_type_expr(heap),
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
    } else if unsupported_arithmetic_patterns_in_formals > 0 {
        Some(TypecheckError::UnsupportedArithmeticPatternsInFormals {
            count: unsupported_arithmetic_patterns_in_formals,
        })
    } else if value_head_applications_in_formals > 0 {
        Some(TypecheckError::ValueHeadApplicationsInFormals {
            count: value_head_applications_in_formals,
        })
    } else if non_identifier_application_heads_in_formals > 0 {
        Some(TypecheckError::NonIdentifierApplicationHeadsInFormals {
            count: non_identifier_application_heads_in_formals,
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

/// Walks committed lambda-head patterns and records formal-pattern misuse in the active subset.
/// This exists so the typecheck boundary owns formal-pattern diagnostics over the already lowered top-level forms.
/// The invariant is that constructor misuse, canonical successor-pattern recursion, deferred arithmetic forms, and invalid application shapes are diagnosed before generic undefined-name reporting.
fn collect_formal_pattern_issues(
    heap: &mut Heap,
    expression: Value,
    undeclared_constructors_in_formals: &mut ConsList<IdentifierRecordRef>,
    constructor_arity_mismatch_in_formals: &mut ConsList<IdentifierRecordRef>,
    unsupported_arithmetic_patterns_in_formals: &mut usize,
    value_head_applications_in_formals: &mut usize,
    non_identifier_application_heads_in_formals: &mut usize,
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
                unsupported_arithmetic_patterns_in_formals,
                value_head_applications_in_formals,
                non_identifier_application_heads_in_formals,
            );
            let body = heap[raw_reference].tail.into();
            collect_formal_pattern_issues(
                heap,
                body,
                undeclared_constructors_in_formals,
                constructor_arity_mismatch_in_formals,
                unsupported_arithmetic_patterns_in_formals,
                value_head_applications_in_formals,
                non_identifier_application_heads_in_formals,
            );
        }
        Tag::Label | Tag::Show => {
            collect_formal_pattern_issues(
                heap,
                heap[raw_reference].tail.into(),
                undeclared_constructors_in_formals,
                constructor_arity_mismatch_in_formals,
                unsupported_arithmetic_patterns_in_formals,
                value_head_applications_in_formals,
                non_identifier_application_heads_in_formals,
            );
        }
        Tag::Share => {
            collect_formal_pattern_issues(
                heap,
                heap[raw_reference].head.into(),
                undeclared_constructors_in_formals,
                constructor_arity_mismatch_in_formals,
                unsupported_arithmetic_patterns_in_formals,
                value_head_applications_in_formals,
                non_identifier_application_heads_in_formals,
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
    unsupported_arithmetic_patterns_in_formals: &mut usize,
    value_head_applications_in_formals: &mut usize,
    non_identifier_application_heads_in_formals: &mut usize,
) {
    match classify_committed_formal_pattern(heap, pattern) {
        CommittedFormalPattern::Binder(_) => {}
        CommittedFormalPattern::ConstructorIntentIdentifier(identifier) => {
            if let Some(expected_arity) = constructor_identifier_arity(heap, identifier) {
                if expected_arity != 0 {
                    constructor_arity_mismatch_in_formals.insert_ordered(heap, identifier);
                }
            } else {
                undeclared_constructors_in_formals.insert_ordered(heap, identifier);
            }
        }
        CommittedFormalPattern::ConstructorApplication { head, arguments } => {
            if let Some(expected_arity) = constructor_identifier_arity(heap, head) {
                if expected_arity != arguments.len() {
                    constructor_arity_mismatch_in_formals.insert_ordered(heap, head);
                }
            } else {
                undeclared_constructors_in_formals.insert_ordered(heap, head);
            }

            for argument in arguments {
                collect_formal_pattern_issues_in_pattern(
                    heap,
                    argument,
                    undeclared_constructors_in_formals,
                    constructor_arity_mismatch_in_formals,
                    unsupported_arithmetic_patterns_in_formals,
                    value_head_applications_in_formals,
                    non_identifier_application_heads_in_formals,
                );
            }
        }
        CommittedFormalPattern::SuccessorPattern { inner, .. } => {
            collect_formal_pattern_issues_in_pattern(
                heap,
                inner,
                undeclared_constructors_in_formals,
                constructor_arity_mismatch_in_formals,
                unsupported_arithmetic_patterns_in_formals,
                value_head_applications_in_formals,
                non_identifier_application_heads_in_formals,
            );
        }
        CommittedFormalPattern::UnsupportedArithmeticPattern { .. } => {
            *unsupported_arithmetic_patterns_in_formals += 1;
        }
        CommittedFormalPattern::ValueHeadApplication { .. } => {
            *value_head_applications_in_formals += 1;
        }
        CommittedFormalPattern::NonIdentifierHeadApplication { .. } => {
            *non_identifier_application_heads_in_formals += 1;
        }
        CommittedFormalPattern::StructuralCons { head, tail }
        | CommittedFormalPattern::StructuralTuple { head, tail } => {
            collect_formal_pattern_issues_in_pattern(
                heap,
                head,
                undeclared_constructors_in_formals,
                constructor_arity_mismatch_in_formals,
                unsupported_arithmetic_patterns_in_formals,
                value_head_applications_in_formals,
                non_identifier_application_heads_in_formals,
            );
            collect_formal_pattern_issues_in_pattern(
                heap,
                tail,
                undeclared_constructors_in_formals,
                constructor_arity_mismatch_in_formals,
                unsupported_arithmetic_patterns_in_formals,
                value_head_applications_in_formals,
                non_identifier_application_heads_in_formals,
            );
        }
        CommittedFormalPattern::WrappedConstantLeaf(_)
        | CommittedFormalPattern::RepeatedNameLeaf(_)
        | CommittedFormalPattern::LiteralOrVoidLeaf => {}
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

/// Walks one type-expression subtree and records typename classification and arity issues.
/// This exists so the typecheck boundary owns spec/type meta-checking over the active source-fed substrate.
/// The invariant is that typename leaves are classified and arity-checked from the canonical application spine view.
fn collect_type_expression_issues(
    heap: &mut Heap,
    type_expr: TypeExprRef,
    undefined_type_names: &mut ConsList<IdentifierRecordRef>,
    non_type_identifiers: &mut ConsList<IdentifierRecordRef>,
    arity_mismatch_type_names: &mut ConsList<IdentifierRecordRef>,
) {
    if let Some((identifier, arguments)) = type_expr.identifier_head_application_spine(heap) {
        match classify_type_identifier_reference(heap, identifier) {
            TypeIdentifierReferenceKind::ValidTypeName => {
                if let Some(expected_arity) = type_identifier_arity(heap, identifier) {
                    if expected_arity != arguments.len() {
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

    if let Some((head, tail)) = type_expr.binary_children(heap) {
        collect_type_expression_issues(
            heap,
            head,
            undefined_type_names,
            non_type_identifiers,
            arity_mismatch_type_names,
        );
        collect_type_expression_issues(
            heap,
            tail,
            undefined_type_names,
            non_type_identifiers,
            arity_mismatch_type_names,
        );
        return;
    }

    if let Some(tail) = type_expr.tail_child(heap) {
        collect_type_expression_issues(
            heap,
            tail,
            undefined_type_names,
            non_type_identifiers,
            arity_mismatch_type_names,
        );
    }
}

/// Collects identifiers bound by a pattern node into the active local-binding scope.
/// This exists so lambda-bound names are excluded from undefined-name diagnostics in the body they bind.
/// The invariant is that constructor heads, literal forms, canonical successor offsets, deferred arithmetic forms, and invalid application shapes do not add local bindings.
fn collect_pattern_bound_identifiers(
    heap: &Heap,
    pattern: Value,
    bound_identifiers: &mut Vec<IdentifierRecordRef>,
) {
    match classify_committed_formal_pattern(heap, pattern) {
        CommittedFormalPattern::Binder(identifier) => {
            if !bound_identifiers.contains(&identifier) {
                bound_identifiers.push(identifier);
            }
        }
        CommittedFormalPattern::ConstructorIntentIdentifier(_)
        | CommittedFormalPattern::WrappedConstantLeaf(_)
        | CommittedFormalPattern::RepeatedNameLeaf(_)
        | CommittedFormalPattern::LiteralOrVoidLeaf => {}
        CommittedFormalPattern::ConstructorApplication { arguments, .. } => {
            for argument in arguments {
                collect_pattern_bound_identifiers(heap, argument, bound_identifiers);
            }
        }
        CommittedFormalPattern::SuccessorPattern { inner, .. } => {
            collect_pattern_bound_identifiers(heap, inner, bound_identifiers);
        }
        CommittedFormalPattern::UnsupportedArithmeticPattern { .. }
        | CommittedFormalPattern::ValueHeadApplication { .. }
        | CommittedFormalPattern::NonIdentifierHeadApplication { .. } => {}
        CommittedFormalPattern::StructuralCons { head, tail }
        | CommittedFormalPattern::StructuralTuple { head, tail } => {
            collect_pattern_bound_identifiers(heap, head, bound_identifiers);
            collect_pattern_bound_identifiers(heap, tail, bound_identifiers);
        }
    }
}

#[derive(Debug)]
enum CommittedFormalPattern {
    Binder(IdentifierRecordRef),
    ConstructorIntentIdentifier(IdentifierRecordRef),
    ConstructorApplication {
        head: IdentifierRecordRef,
        arguments: Vec<Value>,
    },
    SuccessorPattern {
        offset: Value,
        inner: Value,
    },
    UnsupportedArithmeticPattern {
        operator: Value,
        arguments: Vec<Value>,
    },
    ValueHeadApplication {
        head: IdentifierRecordRef,
        arguments: Vec<Value>,
    },
    NonIdentifierHeadApplication {
        head: Value,
        arguments: Vec<Value>,
    },
    StructuralCons {
        head: Value,
        tail: Value,
    },
    StructuralTuple {
        head: Value,
        tail: Value,
    },
    WrappedConstantLeaf(Value),
    RepeatedNameLeaf(IdentifierRecordRef),
    LiteralOrVoidLeaf,
}

/// Classifies one committed formal-pattern node into the shared semantic taxonomy used by typecheck.
/// This exists so bound-name collection and formal diagnostics interpret the same committed shapes consistently.
/// The invariant is that wrapped constant cells, canonical successor patterns, deferred arithmetic families, and invalid application families are distinguished before either walk recurses or records bindings.
fn classify_committed_formal_pattern(heap: &Heap, pattern: Value) -> CommittedFormalPattern {
    let raw_reference: RawValue = pattern.into();
    if raw_reference < ATOM_LIMIT {
        return CommittedFormalPattern::LiteralOrVoidLeaf;
    }

    match heap[raw_reference].tag {
        Tag::Id => {
            let identifier = IdentifierRecordRef::from_ref(raw_reference);
            if identifier_has_constructor_intent(heap, identifier) {
                CommittedFormalPattern::ConstructorIntentIdentifier(identifier)
            } else {
                CommittedFormalPattern::Binder(identifier)
            }
        }
        Tag::Ap => {
            let (head_raw, arguments) = pattern_application_spine(heap, pattern);
            if head_raw >= ATOM_LIMIT && heap[head_raw].tag == Tag::Id {
                let identifier = IdentifierRecordRef::from_ref(head_raw);
                if identifier_has_constructor_intent(heap, identifier) {
                    CommittedFormalPattern::ConstructorApplication {
                        head: identifier,
                        arguments,
                    }
                } else {
                    CommittedFormalPattern::ValueHeadApplication {
                        head: identifier,
                        arguments,
                    }
                }
            } else {
                let head = Value::from(head_raw);
                match head {
                    Value::Combinator(Combinator::Plus | Combinator::Minus) => {
                        if let Some((offset, inner)) =
                            canonical_successor_pattern(heap, head, &arguments)
                        {
                            CommittedFormalPattern::SuccessorPattern { offset, inner }
                        } else {
                            CommittedFormalPattern::UnsupportedArithmeticPattern {
                                operator: head,
                                arguments,
                            }
                        }
                    }
                    _ => CommittedFormalPattern::NonIdentifierHeadApplication { head, arguments },
                }
            }
        }
        Tag::Cons => {
            let constant_tag = RawValue::from(Value::Token(Token::Constant));
            if heap[raw_reference].head == constant_tag {
                let wrapped: Value = heap[raw_reference].tail.into();
                let wrapped_raw: RawValue = wrapped.into();
                if wrapped_raw >= ATOM_LIMIT && heap[wrapped_raw].tag == Tag::Id {
                    CommittedFormalPattern::RepeatedNameLeaf(IdentifierRecordRef::from_ref(
                        wrapped_raw,
                    ))
                } else {
                    CommittedFormalPattern::WrappedConstantLeaf(wrapped)
                }
            } else {
                CommittedFormalPattern::StructuralCons {
                    head: heap[raw_reference].head.into(),
                    tail: heap[raw_reference].tail.into(),
                }
            }
        }
        Tag::Pair | Tag::TCons => CommittedFormalPattern::StructuralTuple {
            head: heap[raw_reference].head.into(),
            tail: heap[raw_reference].tail.into(),
        },
        _ => CommittedFormalPattern::LiteralOrVoidLeaf,
    }
}

fn identifier_has_constructor_intent(heap: &Heap, identifier: IdentifierRecordRef) -> bool {
    is_capitalized(identifier.get_name(heap).as_str())
}

/// Returns the head value plus source-order arguments for one committed pattern-application spine.
/// This exists so the shared committed-formal classifier can distinguish identifier-headed applications, canonical successor patterns, and other non-identifier heads.
/// The invariant is that the returned arguments preserve the original left-to-right source order of the committed application tree.
fn pattern_application_spine(heap: &Heap, pattern: Value) -> (RawValue, Vec<Value>) {
    let mut raw_reference: RawValue = pattern.into();
    debug_assert!(raw_reference >= ATOM_LIMIT);
    debug_assert_eq!(heap[raw_reference].tag, Tag::Ap);

    let mut arguments = Vec::new();
    while heap[raw_reference].tag == Tag::Ap {
        arguments.push(heap[raw_reference].tail.into());
        raw_reference = heap[raw_reference].head;
        if raw_reference < ATOM_LIMIT {
            break;
        }
    }

    arguments.reverse();
    (raw_reference, arguments)
}

// Todo: promote this raw heap-shape natural-integer check to a bigint-owned query once `IntegerRef`
// exposes a crate-visible Miranda `isnat` predicate.
fn canonical_successor_pattern(
    heap: &Heap,
    head: Value,
    arguments: &[Value],
) -> Option<(Value, Value)> {
    if head != Value::Combinator(Combinator::Plus) || arguments.len() != 2 {
        return None;
    }

    let offset = arguments[0];
    let inner = arguments[1];
    let offset_raw: RawValue = offset.into();
    if offset_raw < ATOM_LIMIT || heap[offset_raw].tag != Tag::Int {
        return None;
    }

    ((heap[offset_raw].head & SIGN_BIT_MASK) == 0).then_some((offset, inner))
}

/// Returns whether an identifier currently denotes a constructor-valued binding.
/// This exists so constructor names are not misclassified as unresolved variable references during typecheck scanning.
/// The invariant is that only identifiers whose value payload points at a `Tag::Constructor` cell are treated as constructors here.
fn is_constructor_identifier(heap: &Heap, identifier: IdentifierRecordRef) -> bool {
    let raw_value: RawValue = identifier.get_value_field(heap).into();
    raw_value >= ATOM_LIMIT && heap[raw_value].tag == Tag::Constructor
}

/// Returns the declared arity for a committed constructor identifier when available.
/// This exists so formal-pattern validation consumes constructor arity from committed declaration metadata instead of parser-era conventions.
/// The invariant is that successful results come only from real committed constructor metadata.
fn constructor_identifier_arity(heap: &Heap, identifier: IdentifierRecordRef) -> Option<usize> {
    committed_constructor_metadata(heap, identifier)
        .map(|metadata| metadata.arity(heap).max(0) as usize)
}

/// Returns the committed constructor-metadata entry for one constructor identifier.
/// This exists so typecheck owns one query seam from constructor identifiers to declaration-side constructor facts.
/// The invariant is that the returned metadata entry belongs to `constructor` and comes from its parent algebraic type's committed metadata list.
fn committed_constructor_metadata(
    heap: &Heap,
    constructor: IdentifierRecordRef,
) -> Option<AlgebraicConstructorMetadataRef> {
    let parent_identifier = constructor_parent_type_identifier(heap, constructor)?;
    let parent_value = parent_identifier.get_value(heap)?;
    let IdentifierValueData::Typed { value_type, .. } = parent_value.get_data(heap) else {
        return None;
    };
    let mut constructors = value_type.algebraic_constructor_metadata(heap)?;
    while let Some(metadata) = constructors.pop(heap) {
        if metadata.constructor(heap) == constructor {
            return Some(metadata);
        }
    }

    None
}

/// Returns the parent algebraic type identifier implied by a constructor's type.
/// This exists so constructor metadata lookup can recover the parent declaration owner without depending on parser-local side tables.
/// The invariant is that the returned identifier is the head of the constructor result-type application spine after stripping declared field arrows.
fn constructor_parent_type_identifier(
    heap: &Heap,
    constructor: IdentifierRecordRef,
) -> Option<IdentifierRecordRef> {
    let mut result_type = constructor.get_type_expr(heap);
    while let Some(next_result_type) = result_type.arrow_result_type(heap) {
        result_type = next_result_type;
    }

    result_type
        .identifier_head_application_spine(heap)
        .map(|(identifier, _)| identifier)
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

/// Classifies one identifier leaf in a type expression.
/// This exists so the partial typecheck boundary uses one owner for undefined-typename vs wrong-kind diagnostics.
/// The invariant is that only identifiers whose type slot is `type_t` are accepted as legal typename leaves.
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
