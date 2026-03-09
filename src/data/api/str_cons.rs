use crate::data::{Heap, RawValue, Tag, Value};

use super::HeapObjectProxy;

/// A generic proxy for `Tag::StrCons` heap cells.
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
