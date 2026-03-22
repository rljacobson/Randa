use super::*;
use crate::data::{api::{ApNodeRef, IdentifierRecordRef}, tag::Tag, values::Value, RawValue, ATOM_LIMIT};

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

/// Lowers one committed definition body through the currently owned codegen rewrite subset. This
/// exists so the codegen subsystem owns recursive body rewrites in one place instead of scattering
/// shape handling across the file-definition walk. The invariant is that `Label` nodes lower away
/// to their rewritten payload, application nodes preserve order while applying the current `APPEND
/// [] x -> x` rewrite, and lambda binders are preserved while their bodies recurse.
fn lower_codegen_value(heap: &mut Heap, value: Value) -> Value {
    let raw: RawValue = value.into();
    if raw < ATOM_LIMIT {
        return value;
    }

    match heap[raw].tag {
        Tag::Ap => {
            let application = ApNodeRef::from_ref(raw);
            if let Some(function_application) = application.function_application(heap) {
                if function_application.function_raw(heap) == crate::data::combinator::Combinator::Append as RawValue
                    && function_application.argument_raw(heap)
                        == crate::data::combinator::Combinator::Nil as RawValue
                {
                    return lower_codegen_value(heap, application.argument_raw(heap).into());
                }
            }

            let lowered_function = lower_codegen_value(heap, application.function_raw(heap).into());
            let lowered_argument = lower_codegen_value(heap, application.argument_raw(heap).into());
            heap.apply_ref(lowered_function, lowered_argument)
        }
        Tag::Cons => {
            let lowered_head = lower_codegen_value(heap, heap[raw].head.into());
            let lowered_tail = lower_codegen_value(heap, heap[raw].tail.into());
            heap.cons_ref(lowered_head, lowered_tail)
        }
        Tag::Label => lower_codegen_value(heap, heap[raw].tail.into()),
        Tag::Lambda => {
            let binder = heap[raw].head;
            let lowered_body = lower_codegen_value(heap, heap[raw].tail.into());
            heap.lambda_ref(binder.into(), lowered_body)
        }
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
