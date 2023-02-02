/*!

A cons list is a linked list of the form `cons(head, cons(r1, const(r2, ... cons(rn, nil)…)))`. A new item is added
to the list by consing the item with the list and updating the cons list reference to the new list. Therefore,
`conslist.push(item)` will modify its internal reference. Likewise with `pop()`. For immutable semantics, use `head()`
and `rest()`.

It is convenient to allow `ConsList.reference` to be NIL and interpret it as an empty list, because
almost everywhere we are likely to use `ConsList` the reference to the cons list will be nullable. This
would also change the semantics of `ConsList.pop(..)`.

ToDo: Should `ConsList` have a type parameter `ConstList<T>` such that `pop` returns a `T` and `push` takes a `T`?

*/

use crate::data::{Combinator, Heap, RawValue, Value};
use crate::data::heap::HeapCell;
use crate::data::tag::Tag;
use crate::data::api::HeapObjectProxy;


#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct ConsList<T=RawValue> where T: Into<Value>
{
  reference: RawValue
}

impl<T> ConsList<T>
    where T: Into<Value>
{
  /// A `ConsList` that isn't an actual cons list on the heap but rather the literal NIL. This represents an
  /// "empty" list.
  pub(crate) const EMPTY: Self = ConsList { reference: RawValue(Combinator::Nil as isize) };

  /// Constructs a new cons list on the heap with the given initial value.
  #[inline(always)]
  pub fn new(heap: &mut Heap, initial_value: T) -> Self {
    let reference = heap.cons(initial_value.into(), Combinator::Nil.into());

    Self {
      reference: reference.into()
    }
  }

  #[inline(always)]
  pub fn is_empty(&self) -> bool {
    self.reference == Combinator::Nil.into()
  }

  /// Returns the number of elements in the list. This runs in O(n) time.
  pub fn len(&self, heap: &Heap) -> usize {
    if self.is_empty() {
      return 0;
    } else {
      return 1 + self.rest_unchecked(heap).len(heap);
    }
  }

  #[inline(always)]
  pub fn push(&mut self, heap: &mut Heap, item: T) {
    let new_list = heap.cons(item.into(), self.reference.into());

    self.reference = new_list.into();
  }

  /// Like push, but places the new item at the rear of the list instead of on the front.
  /// Miranda's `append1`
  #[inline(always)]
  pub fn append(&mut self, heap: &mut Heap, item: T) {
    // Special case, as there is no tail.
    if self.is_empty() {
      let new_list = heap.cons(item.into(), self.reference.into());
      self.reference = new_list.into();
    }
    else {
      // Walk to the last cons, which will be cons(last_item, NIL).
      let mut cursor = (*self).clone();
      let mut rest = cursor.rest_unchecked(heap);
      while !rest.is_empty(){
        cursor = rest;
        rest = cursor.rest_unchecked(heap);
      }

      // Place item in the tail
      let new_cons = heap.cons(item.into(), Combinator::NIL);
      heap[cursor.reference].tail = new_cons.into();
    }

  }



  // Does not check if `self` is empty.
  #[inline(always)]
  pub fn rest_unchecked(&self, heap: &Heap) -> Self {
    let rest: RawValue = heap[self.reference].tail;
    Self::from_reference(rest)
  }

  #[inline(always)]
  pub fn rest(&self, heap: &Heap) -> Option<Self> {
    if self.reference == NIL {
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
    if self.is_empty() || item_address < self.head(heap).into() {
      let new_cons = heap.cons(item, self.reference.into());
      self.reference = new_cons.into();
      return;
    }

    // First position is a special case, because the logic below only considers the next link in the list.
    if item_address == self.head(heap).into() {
      // Item is already in the list. No duplicates allowed.
      return;
    }

    // Henceforth the invariant is item_address > cursor.head().

    // Move forward until we're at the last (nonempty) cons or at the position at which `item` should be inserted.
    let mut cursor = (*self).clone();
    while !cursor.rest_unchecked(heap).is_empty()
          && item_address > cursor.rest_unchecked(heap).head(heap).into()
    {
      cursor = cursor.rest_unchecked(heap);
    }

    if cursor.rest_unchecked(heap).is_empty() {
      let new_cons = heap.cons(item, NIL);
      // `item` goes on the end
      heap[cursor.reference].tail = new_cons.into();
    }
    else if item_address != cursor.rest_unchecked(heap).head(heap).into() {
      // Item is not already in the list.
      let new_cons = heap.cons(item, cursor.rest_unchecked(heap).into());
      heap[cursor.reference].tail = new_cons.into();
    }
    // The remaining case is `item_address != cursor.rest_unchecked(heap).head(heap).into()`, which is a noop,
    // because no duplicates are allowed.
  }

  /// Returns a shallow copy of the `ConsList` with items in reverse order.
  pub fn reversed(&self, heap: &mut Heap) -> Self {
    let mut new_list = Self::EMPTY;
    let mut cursor: ConsList<T> = (*self).clone();
    while !cursor.is_empty() {
      let hd = cursor.head_unchecked(heap);
      new_list.push(heap, hd);
      cursor = cursor.rest_unchecked(heap);
    }

    new_list
  }

  /// Returns `true` if the given value is one of the elements of the cons list, `false` otherwise.
  pub fn contains(&self, heap: &Heap, item: T) -> bool {
    let mut cursor: Self = (*self).clone();
    while !cursor.is_empty() {
      let next = cursor.pop_unchecked(heap);
      if next == item {
        return true;
      }
    }
    false
  }
}

impl<T> ConsList<T>
    where T: HeapObjectProxy + Into<Value>
{
  /// If `self = cons(head, rest)`, returns `head` and modifies the internal reference to point to `rest`.
  /// If the list is empty, returns `None`.
  pub fn pop(&mut self, heap: &Heap) -> Option<T> {
    if self.reference == Combinator::Nil.into() {
      None
    }
    else {
      let head       = self.head_unchecked(heap);
      self.reference = heap[self.reference].tail;

      Some(T::from_ref(head))
    }
  }

  #[inline(always)]
  pub fn head_unchecked(&self, heap: &Heap) -> T {
    let head = heap[self.reference].head;
    T::from_ref(head)
  }

  #[inline(always)]
  pub fn head(&self, heap: &Heap) -> Option<T> {
    if self.reference == Combinator::Nil.into() {
      let head = heap[self.reference].head;
      Some(T::from_ref(head))
    } else {
      None
    }
  }
}

impl ConsList<RawValue> {
  /// If `self = cons(head, rest)`, returns `head` and modifies the internal reference to point to `rest`.
  /// If the list is empty, returns `None`.
  pub fn pop(&mut self, heap: &Heap) -> Option<T> {
    if self.reference == Combinator::Nil.into() {
      None
    }
    else {
      let head       = self.head_unchecked(heap);
      self.reference = heap[self.reference].tail;
      Some(head)
    }
  }
  /// If `self = cons(head, rest)`, returns `head` and modifies the internal reference to point to `rest`.
  /// This method does not check first if self is empty, nor does it check that `self` is pointing to a well-formed
  /// cons list.
  pub fn pop_unchecked(&mut self, heap: &Heap) -> T {
    let head       = self.head_unchecked(heap);
    self.reference = heap[self.reference].tail;
    head
  }

  #[inline(always)]
  pub fn head_unchecked(&self, heap: &Heap) -> T {
    heap[self.reference].head
  }

  #[inline(always)]
  pub fn head(&self, heap: &Heap) -> Option<T> {
    if self.reference == Combinator::Nil.into() {
      Some(heap[self.reference].head)
    } else {
      None
    }
  }
}


impl HeapObjectProxy for ConsList {
  /// Creates a `ConsList` from an existing cons list heap object. No validation is performed.
  fn from_ref(reference: RawValue) -> Self {
    Self{
      reference
    }
  }

  fn get_ref(&self) -> RawValue {
    self.reference
  }

}

impl<T> From<ConsList<T>> for RawValue {
  fn from(cons_list: ConsList) -> Self {
    cons_list.get_ref()
  }
}

impl<T> From<ConsList<T>> for Value {
  fn from(cons_list: ConsList) -> Self {
    cons_list.get_ref().into()
  }
}
