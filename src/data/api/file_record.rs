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


use std::time::SystemTime;
use super::{HeapObjectProxy, HeapString};
use crate::data::{Heap, ValueRepresentationType, RawValue, Value, Combinator, Tag, HeapCell};
use crate::data::api::ConsList;


// Todo: What should this be called? Heap file record? ScriptFile? FileWrapper?
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub(crate) struct FileRecord {
  reference      : RawValue,
  defs_are_sorted: bool,
}

impl FileRecord {
  /// Create a new file record of the form `cons(cons(fileinfo(filename ,mtime), share), definienda)` on the heap.
  /// The `definienda` should be a cons list of items, but we take a value for flexibility.
  pub fn new(
    heap         : &mut Heap,
    file_name    : HeapString,
    last_modified: SystemTime,
    share        : bool,
    definienda   : ConsList
  ) -> Self
  {
    // Todo: How is mtime stored on the heap?
    let h_last_modified: ValueRepresentationType = last_modified.into();

    let file_name   = heap.string(file_name);
    let h_file_info = heap.file_info(file_name, h_last_modified.into());
    let h_share     = if share {
          Combinator::True
        } else {
          Combinator::False
        };

    let inner_cons = heap.cons(h_file_info, h_share.into());
    let outer_cons = heap.cons(inner_cons, definienda.get_ref().into());

    FileRecord{
      reference      : outer_cons.into(),
      defs_are_sorted: false
    }
  }


  pub fn get_definienda(&self, heap: &Heap) -> ConsList {
    // cons(  cons(  fileinfo(filename,mtime),  share),    definienda)
    ConsList::from_ref( heap[self.reference].tail)
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
    let new_list = heap.cons(item, heap[self.reference].tail.into());
    self.set_definienda(heap, new_list);
  }
}


impl HeapObjectProxy for FileRecord {

  fn from_ref(file_ref: RawValue) -> Self {
    FileRecord{
      reference      : file_ref,
      defs_are_sorted: false
    }
  }

  fn get_ref(&self) -> RawValue {
    self.reference
  }

}


#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}
