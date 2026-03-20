use crate::data::{Heap, RawValue, Tag, Value};

use super::HeapObjectProxy;

/// Reference-semantics view of one Miranda `datapair` cell.
///
/// Heap shape mapped by this proxy:
/// a `Tag::DataPair` cell with `head = left` and `tail = right`.
///
/// This proxy is used for small two-slot metadata payloads such as alias source-name data and
/// `%free` original-name metadata.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct DataPair {
    reference: RawValue,
}

impl DataPair {
    /// Constructs a new `DataPair` on the heap.
    pub fn new(heap: &mut Heap, left: Value, right: Value) -> Self {
        let reference: RawValue = heap.data_pair_ref(left, right).into();
        DataPair { reference }
    }

    pub fn left_value(&self, heap: &Heap) -> Value {
        let cell = heap[self.reference];
        debug_assert_eq!(cell.tag, Tag::DataPair);
        cell.head.into()
    }

    pub fn right_value(&self, heap: &Heap) -> Value {
        let cell = heap[self.reference];
        debug_assert_eq!(cell.tag, Tag::DataPair);
        cell.tail.into()
    }
}

impl HeapObjectProxy for DataPair {
    fn from_ref(reference: RawValue) -> Self {
        DataPair { reference }
    }

    fn get_ref(&self) -> RawValue {
        self.reference
    }
}
