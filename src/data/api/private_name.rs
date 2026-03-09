use crate::data::{Heap, RawValue};

use super::{HeapObjectProxy, IdentifierValueRef, StrConsRef};

/// A proxy for private-name cells stored as `strcons(index, value)`.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct PrivateNameRef {
    reference: RawValue,
}

impl PrivateNameRef {
    fn as_str_cons(&self) -> StrConsRef {
        StrConsRef::from_ref(self.reference)
    }

    pub fn value(&self, heap: &Heap) -> IdentifierValueRef {
        IdentifierValueRef::from_ref(self.as_str_cons().tail_raw(heap))
    }

    pub fn set_value(&self, heap: &mut Heap, value: IdentifierValueRef) {
        self.as_str_cons().set_tail_raw(heap, value.get_ref());
    }
}

impl HeapObjectProxy for PrivateNameRef {
    fn from_ref(reference: RawValue) -> Self {
        PrivateNameRef { reference }
    }

    fn get_ref(&self) -> RawValue {
        self.reference
    }
}
