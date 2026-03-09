use crate::data::{Heap, RawValue, Tag, Value};

use super::HeapObjectProxy;

/// A proxy for `Tag::Constructor` cells.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct ConstructorRef {
    reference: RawValue,
}

impl ConstructorRef {
    pub fn new(heap: &mut Heap, constructor_index: i16, payload: Value) -> Self {
        let reference: RawValue = heap
            .put_ref(
                Tag::Constructor,
                Value::Data(constructor_index as RawValue),
                payload,
            )
            .into();
        ConstructorRef { reference }
    }

    pub fn constructor_index(&self, heap: &Heap) -> i16 {
        let cell = heap[self.reference];
        debug_assert_eq!(cell.tag, Tag::Constructor);
        cell.head as i16
    }

    pub fn payload(&self, heap: &Heap) -> Value {
        let cell = heap[self.reference];
        debug_assert_eq!(cell.tag, Tag::Constructor);
        cell.tail.into()
    }

    pub fn set_payload(&self, heap: &mut Heap, payload: Value) {
        debug_assert_eq!(heap[self.reference].tag, Tag::Constructor);
        heap[self.reference].tail = payload.into();
    }
}

impl HeapObjectProxy for ConstructorRef {
    fn from_ref(reference: RawValue) -> Self {
        ConstructorRef { reference }
    }

    fn get_ref(&self) -> RawValue {
        self.reference
    }
}
