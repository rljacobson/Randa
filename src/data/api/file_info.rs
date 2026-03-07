use crate::data::{Heap, RawValue, Tag, Value};

use super::{HeapObjectProxy, HeapString, LineNumber};

/// A proxy for `Tag::FileInfo` heap cells.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct FileInfoRef {
    reference: RawValue,
}

impl FileInfoRef {
    pub fn new(heap: &mut Heap, script_file: Value, line_number: Value) -> Self {
        let reference: RawValue = heap.file_info_ref(script_file, line_number).into();
        FileInfoRef { reference }
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
        file_info.tail.into()
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
