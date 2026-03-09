/*!

 From Miranda:

`files` is a cons list of elements, each of which is of the form
  `cons(cons(fileinfo(filename,mtime),share),definienda)`
where `share` (=0,1) says if repeated instances are shareable. Current script at
the front followed by subsidiary files due to `%insert` and `%include` elements due
to `%insert` have `self.nill` `definienda` (they are attributed to the inserting script).


The "definienda" is itself a cons list of items (types, identifiers, etc.) that are defined in the current file.
(A definiendum is a term that is being defined or clarified. The plural form of definiendum is definienda.)

*/

use super::{FileInfoRef, HeapObjectProxy, HeapString};
use crate::data::api::{ConsList, IdentifierRecordRef};
use crate::data::{Combinator, Heap, RawValue, Value};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

// Todo: Rename `FileRecord` if a clearer semantic name is selected.
//       Blocker: naming depends on broader runtime naming decisions beyond this tranche.
//       Migration target: post-tranche naming pass coordinated with `llm/project-map.md`.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub(crate) struct FileRecord {
    reference: RawValue,
    defs_are_sorted: bool,
}

impl FileRecord {
    /// Create a new file record of the form `cons(cons(fileinfo(filename ,mtime), share), definienda)` on the heap.
    /// The `definienda` should be a cons list of items, but we take a value for flexibility.
    pub fn new(
        heap: &mut Heap,
        file_name: HeapString,
        last_modified: SystemTime,
        share: bool,
        definienda: ConsList<IdentifierRecordRef>,
    ) -> Self {
        // `mtime` is stored in `fileinfo(...).line_number` as UNIX-epoch seconds,
        // matching the existing file-record projection contract in this runtime.
        let h_last_modified: RawValue = last_modified
            .duration_since(UNIX_EPOCH)
            .map(|duration| duration.as_secs() as RawValue)
            .unwrap_or_default();

        let h_file_info = FileInfoRef::from_script_file(heap, file_name, h_last_modified);
        let h_share = if share {
            Combinator::True as RawValue
        } else {
            Combinator::False as RawValue
        };

        let inner_cons = heap.cons_ref(h_file_info.into(), h_share.into());
        let outer_cons = heap.cons_ref(inner_cons, definienda.get_ref().into());

        FileRecord {
            reference: outer_cons.into(),
            defs_are_sorted: false,
        }
    }

    /// Returns the inner header cell `cons(fileinfo, share)` for this file record.
    fn header_cons_ref(&self, heap: &Heap) -> RawValue {
        heap[self.reference].head
    }

    /// Returns the `fileinfo(filename, mtime)` payload for this file record.
    pub fn get_file_info(&self, heap: &Heap) -> FileInfoRef {
        let header_cons_ref = self.header_cons_ref(heap);
        FileInfoRef::from_ref(heap[header_cons_ref].head)
    }

    fn definienda_value(&self, heap: &Heap) -> Value {
        heap[self.reference].tail.into()
    }

    fn set_definienda_value(&self, heap: &mut Heap, value: Value) {
        heap[self.reference].tail = value.into();
    }

    pub fn get_file_name(&self, heap: &Heap) -> String {
        self.get_file_info(heap).script_file(heap)
    }

    pub fn get_last_modified(&self, heap: &Heap) -> SystemTime {
        let modified_seconds: RawValue = self.get_file_info(heap).line_number(heap);

        if modified_seconds <= 0 {
            UNIX_EPOCH
        } else {
            UNIX_EPOCH + Duration::from_secs(modified_seconds as u64)
        }
    }

    pub fn get_definienda(&self, heap: &Heap) -> ConsList {
        // cons(  cons(  fileinfo(filename,mtime),  share),    definienda)
        ConsList::from_ref(self.definienda_value(heap).into())
    }

    /// Sets definienda to NIL
    #[inline(always)]
    pub fn clear_definienda(&self, heap: &mut Heap) {
        self.set_definienda_value(heap, Combinator::Nil.into());
    }

    #[inline(always)]
    pub fn push_item(&self, heap: &mut Heap, item: Value) {
        // Works even if definienda is NIL.
        let definienda_tail: Value = self.definienda_value(heap);
        let new_list: Value = heap.cons_ref(item, definienda_tail);
        self.set_definienda_value(heap, new_list);
    }
}

impl HeapObjectProxy for FileRecord {
    fn from_ref(file_ref: RawValue) -> Self {
        FileRecord {
            reference: file_ref,
            defs_are_sorted: false,
        }
    }

    fn get_ref(&self) -> RawValue {
        self.reference
    }
}
