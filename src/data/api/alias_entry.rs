use super::{
    HeapObjectProxy, IdentifierCoreData, IdentifierCoreRef, IdentifierRecordRef, IdentifierValueRef,
};
use crate::data::{Combinator, Heap, RawValue, Tag};

/// A proxy for an entry in the aliases list.
///
/// Alias entries are heap cells of the form `cons(new_id, old_id)` during alias installation.
/// The head is later reused to store the temporary hold payload used by `unalias`.
///
/// This yields two physical layouts for a single logical alias entry:
///
/// - Install phase (`obey_aliases`):
///   `alias_entry = cons(new_target, old_id)`
/// - Rollback/missing-check phase (`unalias`):
///   `alias_entry = cons(hold, old_id)` where `hold = cons(new_target, cons(old_type, old_val))`
///
/// The semantic API below hides this representation shift. Callers can ask for the "new target"
/// without caring whether it currently lives in `alias_entry.head` (install phase) or `hold.head`
/// (rollback phase).
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct AliasEntry {
    reference: RawValue,
}

impl AliasEntry {
    fn head_ref(&self, heap: &Heap) -> RawValue {
        heap[self.reference].head
    }

    fn old_identifier_ref(&self, heap: &Heap) -> RawValue {
        heap[self.reference].tail
    }

    /// Returns the alias destination (`new`) as an `IdentifierValueRef`.
    ///
    /// The destination is represented differently across phases:
    /// - install phase: `new` is `alias_entry.head`
    /// - rollback phase: `new` is `hold.head`, where `hold = alias_entry.head`
    pub fn get_new_target(&self, heap: &Heap) -> IdentifierValueRef {
        let head_ref = self.head_ref(heap);
        if heap[head_ref].tag == Tag::Cons {
            IdentifierValueRef::from_ref(heap[head_ref].head)
        } else {
            IdentifierValueRef::from_ref(head_ref)
        }
    }

    pub fn get_old_identifier_record(&self, heap: &Heap) -> IdentifierRecordRef {
        IdentifierRecordRef::from_ref(self.old_identifier_ref(heap))
    }

    pub fn get_new_identifier_record(&self, heap: &Heap) -> Option<IdentifierRecordRef> {
        let new_ref = self.get_new_target(heap).get_ref();
        (heap[new_ref].tag == Tag::Id).then(|| IdentifierRecordRef::from_ref(new_ref))
    }

    pub fn get_hold(&self, heap: &Heap) -> Option<IdentifierCoreRef> {
        let head_ref = self.head_ref(heap);
        (heap[head_ref].tag == Tag::Cons).then(|| IdentifierCoreRef::from_ref(head_ref))
    }

    pub fn set_hold(&self, heap: &mut Heap, hold: IdentifierCoreRef) {
        heap[self.reference].head = hold.get_ref();
    }

    pub fn set_hold_value(&self, heap: &mut Heap, value: IdentifierValueRef) {
        let hold_ref = self.head_ref(heap);
        debug_assert_eq!(heap[hold_ref].tag, Tag::Cons);
        heap[hold_ref].head = value.get_ref();
    }

    pub fn old_identifier_matches(&self, heap: &Heap, record: IdentifierRecordRef) -> bool {
        self.old_identifier_ref(heap) == record.get_ref()
    }

    pub fn set_cyclic_hold_data(&self, heap: &mut Heap, data: IdentifierCoreData) {
        let alias_head_ref = self.head_ref(heap);
        debug_assert_eq!(heap[alias_head_ref].tag, Tag::Cons);
        heap[alias_head_ref].head = data.definition.get_ref();
        let alias_type_value_ref = heap[alias_head_ref].tail;
        heap[alias_type_value_ref].head = data.datatype.into();
        heap[alias_type_value_ref].tail = data
            .value
            .map_or(Combinator::Nil.into(), |value| value.get_ref());
    }
}

impl HeapObjectProxy for AliasEntry {
    fn from_ref(reference: RawValue) -> Self {
        AliasEntry { reference }
    }

    fn get_ref(&self) -> RawValue {
        self.reference
    }
}
