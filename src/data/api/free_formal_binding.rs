use crate::data::{Heap, RawValue, Tag, Value};

use super::{DataPair, HeapObjectProxy, IdentifierRecordRef};

/// A proxy for `%free` formal-binding entries stored as `cons(id, cons(datapair(original_name, 0), type))`.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct FreeFormalBindingRef {
    reference: RawValue,
}

impl FreeFormalBindingRef {
    /// Constructs one `%free` formal-binding entry from the bound identifier, original-name metadata, and type payload.
    /// This exists so `%free` producer paths can own the Miranda binding-entry shape through one typed heap-boundary surface.
    /// The invariant is that formal-binding entries remain `cons(id, cons(datapair(original_name, 0), type))`.
    pub fn new(
        heap: &mut Heap,
        identifier: IdentifierRecordRef,
        original_name_metadata: DataPair,
        type_expr: Value,
    ) -> Self {
        let payload = heap.cons_ref(original_name_metadata.into(), type_expr);
        FreeFormalBindingRef {
            reference: heap.cons_ref(identifier.into(), payload).into(),
        }
    }

    /// Returns the bound identifier for this `%free` formal-binding entry.
    /// This exists so `%free` consumers can project the formal identifier without open-coding nested-cons traversal.
    /// The invariant is that the returned identifier is the head of the outer cons cell.
    pub fn identifier(&self, heap: &Heap) -> IdentifierRecordRef {
        let outer = heap[self.reference];
        debug_assert_eq!(outer.tag, Tag::Cons);
        IdentifierRecordRef::from_ref(outer.head)
    }

    /// Returns the original-name metadata pair for this `%free` formal-binding entry.
    /// This exists so `%free` consumers can project the Miranda `datapair(original_name, 0)` payload through the binding owner.
    /// The invariant is that the returned pair is the head of the inner cons cell.
    pub fn original_name_metadata(&self, heap: &Heap) -> DataPair {
        let payload_ref = heap[self.reference].tail;
        debug_assert_eq!(heap[self.reference].tag, Tag::Cons);
        debug_assert_eq!(heap[payload_ref].tag, Tag::Cons);
        DataPair::from_ref(heap[payload_ref].head)
    }

    /// Returns the type payload for this `%free` formal-binding entry.
    /// This exists so `%free` consumers can project the binding type without repeating nested-cons indexing.
    /// The invariant is that the returned value is the tail of the inner cons cell.
    pub fn type_expr(&self, heap: &Heap) -> Value {
        let payload_ref = heap[self.reference].tail;
        debug_assert_eq!(heap[self.reference].tag, Tag::Cons);
        debug_assert_eq!(heap[payload_ref].tag, Tag::Cons);
        heap[payload_ref].tail.into()
    }
}

impl HeapObjectProxy for FreeFormalBindingRef {
    fn from_ref(reference: RawValue) -> Self {
        FreeFormalBindingRef { reference }
    }

    fn get_ref(&self) -> RawValue {
        self.reference
    }
}
