use crate::data::{Heap, RawValue, Tag, Value};

use super::{HeapObjectProxy, HeapString, LineNumber};

/// Reference-semantics view of one `fileinfo` payload.
///
/// Heap shape mapped by this proxy:
/// a `Tag::FileInfo` cell with `head = script_file_string` and `tail = line_number_or_mtime`.
///
/// This proxy is used for source locations in identifier definitions and diagnostics, and also for
/// file-record headers where Miranda reuses the numeric slot for last-modified time.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct FileInfoRef {
    reference: RawValue,
}

impl FileInfoRef {
    pub fn new(heap: &mut Heap, script_file: Value, line_number: Value) -> Self {
        let reference: RawValue = heap.file_info_ref(script_file, line_number).into();
        FileInfoRef { reference }
    }

    /// Constructs `fileinfo(script_file, line_number)` from semantic parts.
    pub fn from_script_file(
        heap: &mut Heap,
        script_file: HeapString,
        line_number: LineNumber,
    ) -> Self {
        let script_file_ref = heap.string(script_file);
        FileInfoRef::new(
            heap,
            Value::Reference(script_file_ref),
            Value::Data(line_number as RawValue),
        )
    }

    pub fn script_file(&self, heap: &Heap) -> HeapString {
        let file_info = heap[self.reference];
        debug_assert_eq!(file_info.tag, Tag::FileInfo);
        heap.resolve_string(file_info.head.into())
            .expect("FileInfoRef points to an invalid script-file string.")
    }

    pub fn line_number(&self, heap: &Heap) -> LineNumber {
        let file_info = heap[self.reference];
        debug_assert_eq!(file_info.tag, Tag::FileInfo);
        file_info.tail
    }
}

impl HeapObjectProxy for FileInfoRef {
    fn from_ref(reference: RawValue) -> Self {
        FileInfoRef { reference }
    }

    fn get_ref(&self) -> RawValue {
        self.reference
    }
}
