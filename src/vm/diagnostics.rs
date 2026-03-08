use super::*;

pub(super) fn alfasort(
    heap: &mut Heap,
    mut items: ConsList<IdentifierRecordRef>,
) -> ConsList<IdentifierRecordRef> {
    let mut identifiers: Vec<IdentifierRecordRef> = vec![];
    while let Some(identifier) = items.pop(heap) {
        identifiers.push(identifier);
    }

    identifiers.sort_by(|left, right| {
        let left_name = identifier_name_for_diagnostics(heap, *left).unwrap_or_default();
        let right_name = identifier_name_for_diagnostics(heap, *right).unwrap_or_default();
        left_name
            .cmp(&right_name)
            .then(left.get_ref().cmp(&right.get_ref()))
    });

    let mut sorted = ConsList::EMPTY;
    for identifier in identifiers {
        sorted.append(heap, identifier);
    }

    sorted
}

pub(super) fn printlist(heap: &Heap, mut items: ConsList<IdentifierRecordRef>) -> String {
    let mut names: Vec<String> = vec![];
    while let Some(identifier) = items.pop(heap) {
        let name =
            identifier_name_for_diagnostics(heap, identifier).unwrap_or_else(|| "<invalid-id>".to_string());
        names.push(name);
    }

    names.join(", ")
}

fn identifier_name_for_diagnostics(heap: &Heap, identifier: IdentifierRecordRef) -> Option<String> {
    Some(identifier.get_name(heap))
}

pub(super) fn source_update_check(heap: &Heap, mut loaded_files: ConsList<FileRecord>) -> bool {
    while let Some(file_record) = loaded_files.pop(heap) {
        let path = file_record.get_file_name(heap);
        let dumped_modified_time = file_record.get_last_modified(heap);

        let source_modified_time = match std::fs::metadata(&path).and_then(|m| m.modified()) {
            Ok(modified_time) => modified_time,
            Err(_) => return true,
        };

        if source_modified_time > dumped_modified_time {
            return true;
        }
    }

    false
}
