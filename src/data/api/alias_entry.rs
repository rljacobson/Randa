/*!
Alias-entry proxy for Miranda load-time aliasing.

Miranda aliasing is applied during dump load (`load_script` / `VM::obey_aliases`).
Each alias list entry is conceptually `cons(new, old)`: make `old` an alias of `new` by
temporarily mutating `old` (`id_type(old)=alias_t`, `id_val(old)=new`).

Because this mutation overwrites `old`'s original payload, the loader first saves a rollback
snapshot called a "hold" value:

`hold = cons(id_who(old), cons(id_type(old), id_val(old)))`

That hold payload is the same information represented by `IdentifierCoreData` / `IdentifierCoreRef`.

The alias-entry head is therefore reused across phases:

- install phase (`obey_aliases`): `alias_entry = cons(new_target, old_id)`
- rollback/check phase (`unalias`/C `unscramble`): `alias_entry = cons(hold, old_id)`

`AliasEntry` centralizes this phase-dependent representation so VM code can ask for semantic
operations (get destination, get old id, set hold, patch cyclic hold data) without doing raw
heap indexing at every call site.

The cyclic-alias path in definition loading also writes through this proxy. In that case,
"cyclic hold data" means: update the saved rollback snapshot for one alias entry so later
`unalias` restores the correct `who`/`type`/`value` tuple.
*/

use super::{
    HeapObjectProxy, IdentifierCoreData, IdentifierCoreRef, IdentifierRecordRef, IdentifierValueRef,
};
use crate::data::{Heap, RawValue, Tag};

/// A proxy for one node in the load-time aliases list.
///
/// This wraps the aliasing mechanism used by `load_script`/`obey_aliases` and
/// rollback (`unscramble`/`unalias` in the C implementation).
///
/// Alias entries are heap cells of the form `cons(new_id, old_id)` during installation.
/// The head is later reused to store the temporary hold payload consumed by rollback.
///
/// This yields two physical layouts for a single logical alias entry:
///
/// - Install phase (`obey_aliases`):
///   `alias_entry = cons(new_target, old_id)`
/// - Rollback restore phase (`unalias` pass 1):
///   `alias_entry = cons(hold, old_id)` where `hold = cons(old_who, cons(old_type, old_val))`
/// - Rollback/missing-check phase (`unalias` pass 2):
///   `alias_entry = cons(new_target, old_id)`
///
/// The semantic API below keeps tuple access centralized so VM code avoids ad hoc heap indexing.
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
    /// Valid when `alias_entry.head` currently stores `new` (install phase and rollback pass 2).
    pub fn get_new_target(&self, heap: &Heap) -> IdentifierValueRef {
        IdentifierValueRef::from_ref(self.head_ref(heap))
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

    /// Writes `new` into `alias_entry.head` while keeping `alias_entry.tail = old` unchanged.
    ///
    /// Mirrors C `unscramble` step `hd[hd[aliases]] = new` so pass-2 missing-target checks read
    /// the resolved destination from the alias entry after pass 1 restores `old` from `hold`.
    pub fn set_hold_value(&self, heap: &mut Heap, value: IdentifierValueRef) {
        heap[self.reference].head = value.get_ref();
    }

    pub fn old_identifier_matches(&self, heap: &Heap, record: IdentifierRecordRef) -> bool {
        self.old_identifier_ref(heap) == record.get_ref()
    }

    /// Replaces the saved rollback payload for a cyclic-alias entry.
    ///
    /// "Cyclic hold data" is the temporary pre-alias snapshot of the old identifier core fields:
    /// `who`, identifier-level `type`, and identifier-level `value`.
    ///
    /// Heap shape written is the hold shape used by rollback:
    /// `cons(id_who(old), cons(id_type(old), id_val(old)))`.
    ///
    /// This corresponds to the C cyclic-alias fix-up writes in `load_defs` that target
    /// `hd[hd[hd[a]]]`, `hd[tl[hd[hd[a]]]]`, and `tl[tl[hd[hd[a]]]]`.
    pub fn set_cyclic_hold_data(&self, heap: &mut Heap, data: IdentifierCoreData) {
        let alias_head_ref = self.head_ref(heap);
        debug_assert_eq!(heap[alias_head_ref].tag, Tag::Cons);
        IdentifierCoreRef::from_ref(alias_head_ref).set_data(heap, data);
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
