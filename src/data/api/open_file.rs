use crate::data::api::{FileRecord, HeapObjectProxy, StrConsRef};
use crate::data::{Heap, RawValue, Value};

/**
The `fileq` member of vm is a ConsList of files open for input of the form
  `cons(strcons(stream,<ptr to element of 'files'>),...)`.
This corresponds to the Randa API structure
  `OpenFile{ stream, FileRecord }`
and
 `ConsList<OpenFile>`.

Of course, the `OpenFile` structure does not have `stream` and `file_record` as members. The stream is represented by
a handle, an index into a vector of streams owned by the VM.
*/

/// Reference-semantics view of one open-file queue entry.
///
/// Heap shape mapped by this proxy:
/// `strcons(stream_handle, file_record_ref)`.
///
/// This proxy is used by the VM's `fileq` / `file_queue` machinery to pair an open input stream
/// with the corresponding committed `FileRecord` entry.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct OpenFile {
    reference: RawValue,
}

impl OpenFile {
    pub fn new(heap: &mut Heap, stream: Value, file_record: FileRecord) -> Self {
        let reference: RawValue = heap
            .strcons_ref(stream, file_record.get_ref().into())
            .into();
        OpenFile { reference }
    }

    fn as_str_cons(&self) -> StrConsRef {
        StrConsRef::from_ref(self.reference)
    }

    pub fn get_stream(&self, heap: &Heap) -> Value {
        self.as_str_cons().head_value(heap)
    }

    pub fn get_file_record(&self, heap: &Heap) -> FileRecord {
        let reference = self.as_str_cons().tail_raw(heap);
        FileRecord::from_ref(reference)
    }
}

impl HeapObjectProxy for OpenFile {
    fn from_ref(reference: RawValue) -> Self {
        OpenFile { reference }
    }

    fn get_ref(&self) -> RawValue {
        self.reference
    }
}
