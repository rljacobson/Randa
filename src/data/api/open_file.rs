use crate::data::api::{FileRecord, HeapObjectProxy};
use crate::data::{Heap, RawValue, Value, ValueRepresentationType};

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


#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct OpenFile
{
  reference: RawValue
}

impl OpenFile {
  pub fn new(heap: &mut Heap, stream: Value, file_record: FileRecord) -> Self {
    let reference = heap.strcons(stream, file_record.get_ref().into());
    OpenFile{
      reference: reference.into()
    }
  }

  pub fn get_stream(&self, heap: &mut Heap) -> Value {
    heap[self.reference].head.into()
  }

  pub fn get_file_record(&self, heap: &mut Heap) -> FileRecord {
    let reference = heap[self.reference].tail;
    FileRecord::from_ref(reference)
  }
}


impl HeapObjectProxy for OpenFile {
  fn from_ref(reference: RawValue) -> Self {
    OpenFile{
      reference
    }
  }

  fn get_ref(&self) -> RawValue {
    self.reference
  }
}
