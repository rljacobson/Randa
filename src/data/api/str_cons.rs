use crate::data::{Heap, RawValue, Tag, Value};

use super::HeapObjectProxy;

/// Reference-semantics view of a generic `strcons` cell.
///
/// Heap shape mapped by this proxy:
/// a `Tag::StrCons` cell with `head = left_payload` and `tail = right_payload`.
///
/// This is the shared low-level proxy for the runtime's `strcons`-backed records, including
/// identifier-name headers, open-file queue entries, and private-name entries.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct StrConsRef {
    reference: RawValue,
}

impl StrConsRef {
    pub fn new(heap: &mut Heap, head: Value, tail: Value) -> Self {
        let reference: RawValue = heap.strcons_ref(head, tail).into();
        StrConsRef { reference }
    }

    pub fn head_value(&self, heap: &Heap) -> Value {
        let cell = heap[self.reference];
        debug_assert_eq!(cell.tag, Tag::StrCons);
        cell.head.into()
    }

    pub fn tail_raw(&self, heap: &Heap) -> RawValue {
        let cell = heap[self.reference];
        debug_assert_eq!(cell.tag, Tag::StrCons);
        cell.tail
    }

    pub fn set_tail_raw(&self, heap: &mut Heap, tail: RawValue) {
        debug_assert_eq!(heap[self.reference].tag, Tag::StrCons);
        heap[self.reference].tail = tail;
    }
}

impl HeapObjectProxy for StrConsRef {
    fn from_ref(reference: RawValue) -> Self {
        StrConsRef { reference }
    }

    fn get_ref(&self) -> RawValue {
        self.reference
    }
}
