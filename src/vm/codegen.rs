use super::*;
use crate::data::api::IdentifierRecordRef;

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

/// Executes the current load-time partial codegen boundary over committed source substrate.
pub(super) fn run_partial_codegen(
    heap: &Heap,
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

            if matches!(value.get_data(heap), IdentifierValueData::Arbitrary(_)) {
                processed_binding_count += 1;
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
