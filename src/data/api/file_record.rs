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

use super::{HeapObjectProxy, HeapString};
use crate::data::api::{ConsList, IdentifierRecord};
use crate::data::{Combinator, Heap, RawValue, Value};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

// Todo: What should this be called? Heap file record? ScriptFile? FileWrapper?
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
        definienda: ConsList<IdentifierRecord>,
    ) -> Self {
        // Todo: How is mtime stored on the heap?
        let h_last_modified: RawValue = last_modified
            .duration_since(UNIX_EPOCH)
            .map(|duration| duration.as_secs() as RawValue)
            .unwrap_or_default();

        let file_name = heap.string(file_name);
        let h_file_info =
            heap.file_info_ref(Value::Reference(file_name), Value::Data(h_last_modified));
        let h_share = if share {
            Combinator::True as RawValue
        } else {
            Combinator::False as RawValue
        };

        let inner_cons = heap.cons_ref(h_file_info, h_share.into());
        let outer_cons = heap.cons_ref(inner_cons, definienda.get_ref().into());

        FileRecord {
            reference: outer_cons.into(),
            defs_are_sorted: false,
        }
    }

    pub fn get_file_name(&self, heap: &Heap) -> String {
        let inner_cons_ref = heap[self.reference].head;
        let file_info = heap[inner_cons_ref].head;
        let file_name_ref = heap[file_info].head;

        heap.resolve_string(Value::from(file_name_ref))
            .expect("FileRecord has no file name. This is a bug.")
    }

    pub fn get_last_modified(&self, heap: &Heap) -> SystemTime {
        let inner_cons_ref = heap[self.reference].head;
        let file_info = heap[inner_cons_ref].head;
        let modified_seconds = heap[file_info].tail;

        if modified_seconds <= 0 {
            UNIX_EPOCH
        } else {
            UNIX_EPOCH + Duration::from_secs(modified_seconds as u64)
        }
    }

    pub fn get_definienda(&self, heap: &Heap) -> ConsList {
        // cons(  cons(  fileinfo(filename,mtime),  share),    definienda)
        ConsList::from_ref(heap[self.reference].tail)
    }

    #[inline(always)]
    pub fn set_definienda(&self, heap: &mut Heap, value: Value) {
        heap[self.reference].tail = value.into();
    }

    /// Sets definienda to NIL
    #[inline(always)]
    pub fn clear_definienda(&self, heap: &mut Heap) {
        heap[self.reference].tail = Combinator::Nil.into();
    }

    #[inline(always)]
    pub fn push_item(&self, heap: &mut Heap, item: Value) {
        // Works even if definienda is NIL.
        let definienda_tail: Value = Value::from(heap[self.reference].tail);
        let new_list: Value = Value::from(heap.cons_ref(item, definienda_tail));
        self.set_definienda(heap, new_list);
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
