/*!

A cons list is a linked list of the form `cons(head, cons(r1, cons(r2, ... cons(rn, nil)…)))`. A new item is added
to the list by consing the item with the list and updating the cons list reference to the new list. Therefore,
`conslist.push(item)` will modify its internal reference. Likewise with `pop()`. For immutable semantics, use `head()`
and `rest()`.

It is convenient to allow `ConsList.reference` to be NIL and interpret it as an empty list, because
almost everywhere we are likely to use `ConsList` the reference to the cons list will be nullable. This
would also change the semantics of `ConsList.pop(..)`.

ToDo: Should `ConsList` have a type parameter `ConstList<T>` such that `pop` returns a `T` and `push` takes a `T`?

*/

use std::marker::PhantomData;

use crate::data::{api::HeapObjectProxy, Combinator, Heap, RawValue, Value};

/// Reference-semantics view of a Miranda cons list.
///
/// Heap shapes covered by this proxy:
/// - the empty list sentinel `NIL`
/// - zero or more `cons(head, tail)` cells ending in `NIL`
///
/// `ConsList<T>` is the typed traversal/mutation surface for the many heap-resident lists used by
/// parser, loader, and VM code. The generic parameter describes how each list element is projected
/// when read back from the list.
#[derive(Eq, PartialEq, Debug)]
pub struct ConsList<T = RawValue>
where
    T: Clone,
{
    reference: RawValue,
    phantom_data: PhantomData<T>,
}

impl<T> Clone for ConsList<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for ConsList<T> where T: Clone {}

impl<T> ConsList<T>
where
    T: Clone + Into<Value>,
{
    /// A `ConsList` that isn't an actual cons list on the heap but rather the literal NIL. This represents an
    /// "empty" list.
    pub(crate) const EMPTY: Self = ConsList {
        reference: Combinator::Nil as RawValue,
        phantom_data: PhantomData,
    };

    /// Constructs a new cons list on the heap with the given initial value.
    pub fn new(heap: &mut Heap, initial_value: T) -> Self {
        let reference = heap.cons_ref(Into::<Value>::into(initial_value), Combinator::Nil.into());

        Self {
            reference: reference.into(),
            phantom_data: Default::default(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.reference == Combinator::Nil.into()
    }

    /// Returns the number of elements in the list. This runs in O(n) time.
    pub fn len(&self, heap: &Heap) -> usize {
        if self.is_empty() {
            0
        } else {
            1 + self.rest_unchecked(heap).len(heap)
        }
    }

    pub fn push(&mut self, heap: &mut Heap, item: T) {
        let new_list = heap.cons_ref(item.into(), self.reference.into());

        self.reference = new_list.into();
    }

    fn push_raw(&mut self, heap: &mut Heap, item: RawValue) {
        let new_list = heap.cons_ref(Value::from(item), self.reference.into());

        self.reference = new_list.into();
    }

    /// Like push, but places the new item at the rear of the list instead of on the front.
    /// Miranda's `append1`
    pub fn append(&mut self, heap: &mut Heap, item: T) {
        // Special case, as there is no tail.
        if self.is_empty() {
            let new_list = heap.cons_ref(item.into(), self.reference.into());
            self.reference = new_list.into();
        } else {
            // Walk to the last cons, which will be cons(last_item, NIL).
            let mut cursor = *self;
            let mut rest = cursor.rest_unchecked(heap);
            while !rest.is_empty() {
                cursor = rest;
                rest = cursor.rest_unchecked(heap);
            }

            // Place item in the tail
            let new_cons = heap.cons_ref(item.into(), Combinator::NIL);
            heap[cursor.reference].tail = new_cons.into();
        }
    }

    fn raw_head_unchecked(&self, heap: &Heap) -> RawValue {
        heap[self.reference].head
    }

    fn value_head_unchecked(&self, heap: &Heap) -> Value {
        self.raw_head_unchecked(heap).into()
    }

    fn raw_head(&self, heap: &Heap) -> Option<RawValue> {
        if self.is_empty() {
            None
        } else {
            Some(heap[self.reference].head)
        }
    }

    pub fn value_head(&self, heap: &Heap) -> Option<Value> {
        self.raw_head(heap).map(Into::into)
    }

    /// Returns the tail of this cons list without checking if `self` is empty.
    fn rest_unchecked(&self, heap: &Heap) -> Self {
        let rest: RawValue = heap[self.reference].tail;
        Self::from_ref(rest)
    }

    /// Returns the tail of this cons list, if it exists.
    pub fn rest(&self, heap: &Heap) -> Option<Self> {
        if self.is_empty() {
            None
        } else {
            let rest: RawValue = heap[self.reference].tail;
            Some(Self::from_ref(rest))
        }
    }

    /// Inserts `item` into the `ConsList` `list` while maintaining ascending address order. Because the `item` might be inserted
    /// at the front of the `list`, the internal reference of `list` might change. (Miranda's `add1`.)
    pub fn insert_ordered(&mut self, heap: &mut Heap, item: T) {
        let item: Value = item.into();
        let item_address: RawValue = item.into();

        // If the list is empty, then `self == NIL`.
        if self.is_empty() || item_address < self.raw_head_unchecked(heap) {
            let new_cons = heap.cons_ref(item, self.reference.into());
            self.reference = new_cons.into();
            return;
        }

        // First position is a special case, because the logic below only considers the next link in the list.
        if item_address == self.raw_head_unchecked(heap) {
            // Item is already in the list. No duplicates allowed.
            return;
        }

        // Henceforth the invariant is item_address > cursor.head().

        // Move forward until we're at the last (nonempty) cons or at the position at which `item` should be inserted.
        let mut cursor = *self;
        while !cursor.rest_unchecked(heap).is_empty()
            && item_address > cursor.rest_unchecked(heap).raw_head_unchecked(heap)
        {
            cursor = cursor.rest_unchecked(heap);
        }

        if cursor.rest_unchecked(heap).is_empty() {
            let new_cons = heap.cons_ref(item, Combinator::NIL);
            // `item` goes on the end
            heap[cursor.reference].tail = new_cons.into();
        } else if item_address != cursor.rest_unchecked(heap).raw_head_unchecked(heap) {
            // Item is not already in the list.
            let new_cons = heap.cons_ref(item, cursor.rest_unchecked(heap).into());
            heap[cursor.reference].tail = new_cons.into();
        }
        // The remaining case is `item_address != cursor.rest_unchecked(heap).raw_head_unchecked(heap).into()`, which is a noop,
        // because no duplicates are allowed.
    }

    /// Returns a shallow copy of the `ConsList` with items in reverse order.
    pub fn reversed(&self, heap: &mut Heap) -> Self {
        let mut new_list = Self::EMPTY;
        let mut cursor: ConsList<T> = *self;
        while let Some(hd) = cursor.raw_head(heap) {
            new_list.push_raw(heap, hd);
            cursor = cursor.rest_unchecked(heap);
        }

        new_list
    }

    /// If `self = cons(head, rest)`, returns `head` and modifies the internal reference to point to `rest`.
    /// If the list is empty, returns `None`.
    fn pop_raw(&mut self, heap: &Heap) -> Option<RawValue> {
        if self.is_empty() {
            None
        } else {
            let head = self.raw_head_unchecked(heap);
            self.reference = heap[self.reference].tail;

            Some(head)
        }
    }

    pub fn pop_value(&mut self, heap: &Heap) -> Option<Value> {
        self.pop_raw(heap).map(Into::into)
    }

    /// Returns `true` if the given value is one of the elements of the cons list, `false` otherwise.
    pub fn contains(&self, heap: &Heap, item: T) -> bool {
        let item_value: Value = item.into();
        let mut cursor = *self;

        while let Some(next) = cursor.pop_value(heap) {
            if next == item_value {
                return true;
            }
        }
        false
    }
}

impl<T> ConsList<T>
where
    T: HeapObjectProxy,
{
    /// If `self = cons(head, rest)`, returns `head` and modifies the internal reference to point to `rest`.
    /// If the list is empty, returns `None`.
    pub fn pop(&mut self, heap: &Heap) -> Option<T> {
        if self.is_empty() {
            None
        } else {
            let head = self.head_unchecked(heap);
            self.reference = heap[self.reference].tail;

            Some(head)
        }
    }

    #[inline(always)]
    fn head_unchecked(&self, heap: &Heap) -> T {
        let head = heap[self.reference].head;
        T::from_ref(head)
    }

    #[inline(always)]
    pub fn head(&self, heap: &Heap) -> Option<T> {
        self.raw_head(heap).map(T::from_ref)
    }
}

impl ConsList<Value> {
    /// Returns the reversed form of a parser-facing cons-list value.
    /// This exists so parser-facing code can route `reverse`-style list rewrites through `ConsList` instead of open-coding raw cons traversal.
    /// The invariant is that the returned list preserves the same elements as `list` in reverse order, with `NIL` remaining the empty list.
    pub fn reversed_value(heap: &mut Heap, list: Value) -> Value {
        let list = Self::from_ref(list.into());
        list.reversed(heap).get_ref().into()
    }

    /// Returns whether a parser-facing cons-list value contains `item`.
    /// This exists so parser-facing code can route `member`-style queries through `ConsList` instead of repeating raw list walks.
    /// The invariant is that the result is true iff one element of `list` is equal to `item`.
    pub fn contains_value(heap: &Heap, list: Value, item: Value) -> bool {
        let list = Self::from_ref(list.into());
        list.contains(heap, item)
    }

    /// Inserts `item` into a parser-facing cons-list value using Miranda `add1` ordering semantics and returns the updated list.
    /// This exists so parser-facing code can perform ordered set insertion through `ConsList` instead of mutating raw cons-list references directly.
    /// The invariant is that insertion preserves ascending raw-address order and suppresses duplicates exactly like Miranda `add1`.
    pub fn insert_ordered_value(heap: &mut Heap, list: Value, item: Value) -> Value {
        let mut list = Self::from_ref(list.into());
        list.insert_ordered(heap, item);
        list.get_ref().into()
    }
}

impl<T> HeapObjectProxy for ConsList<T>
where
    T: Clone,
{
    /// Creates a `ConsList` from an existing cons list heap object. No validation is performed.
    fn from_ref(reference: RawValue) -> Self {
        Self {
            reference,
            phantom_data: Default::default(),
        }
    }

    fn get_ref(&self) -> RawValue {
        self.reference
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn value_list_helpers_reverse_and_query_membership() {
        let mut heap = Heap::new();
        let alpha = heap.make_empty_identifier("alpha");
        let beta = heap.make_empty_identifier("beta");
        let gamma = heap.make_empty_identifier("gamma");

        let tail = heap.cons_ref(beta.into(), Combinator::Nil.into());
        let list = heap.cons_ref(alpha.into(), tail);
        let reversed = ConsList::<Value>::reversed_value(&mut heap, list);
        let mut reversed_list: ConsList<Value> = ConsList::from_ref(reversed.into());

        assert_eq!(reversed_list.pop_value(&heap), Some(beta.into()));
        assert_eq!(reversed_list.pop_value(&heap), Some(alpha.into()));
        assert!(ConsList::<Value>::contains_value(&heap, list, alpha.into()));
        assert!(!ConsList::<Value>::contains_value(
            &heap,
            list,
            gamma.into()
        ));
    }

    #[test]
    fn value_list_helper_inserts_in_add1_order_without_duplicates() {
        let mut heap = Heap::new();
        let alpha = heap.make_empty_identifier("alpha");
        let beta = heap.make_empty_identifier("beta");
        let gamma = heap.make_empty_identifier("gamma");

        let mut list = Value::from(Combinator::Nil);
        list = ConsList::<Value>::insert_ordered_value(&mut heap, list, beta.into());
        list = ConsList::<Value>::insert_ordered_value(&mut heap, list, gamma.into());
        list = ConsList::<Value>::insert_ordered_value(&mut heap, list, alpha.into());
        list = ConsList::<Value>::insert_ordered_value(&mut heap, list, beta.into());

        let mut list: ConsList<Value> = ConsList::from_ref(list.into());
        assert_eq!(list.pop_value(&heap), Some(alpha.into()));
        assert_eq!(list.pop_value(&heap), Some(beta.into()));
        assert_eq!(list.pop_value(&heap), Some(gamma.into()));
        assert_eq!(list.pop_value(&heap), None);
    }
}
