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
pub struct ConsList<T=RawValue>
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
  pub fn new(heap: &mut Heap, initial_value: T) -> Self {
    let reference = heap.cons(initial_value.into(), Combinator::Nil.into());

    Self {
      reference: reference.into()
    }
  }


  pub fn is_empty(&self) -> bool {
    self.reference == Combinator::Nil.into()
  }


  pub fn push(&mut self, heap: &mut Heap, item: T) {
    let new_list = heap.cons(item.into(), self.reference.into());

    self.reference = new_list.into();
  }

  pub fn rest_unchecked(&self, heap: &Heap) -> ConsList {
    let rest: RawValue = heap[self.reference].tail;
    Self::from_reference(rest)
  }

  pub fn rest(&self, heap: &Heap) -> Option<ConsList> {
    if self.reference == Combinator::Nil.into() {
      None
    } else {
      let rest: RawValue = heap[self.reference].tail;
      Some(Self::from_ref(rest))
    }
  }
}

impl<T> ConsList<T>
    where T: HeapObjectProxy
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

  pub fn head_unchecked(&self, heap: &Heap) -> T {
    let head = heap[self.reference].head;
    T::from_ref(head)
  }

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

  pub fn head_unchecked(&self, heap: &Heap) -> T {
    let head = heap[self.reference].head;
    head
  }

  pub fn head(&self, heap: &Heap) -> Option<T> {
    if self.reference == Combinator::Nil.into() {
      let head = heap[self.reference].head;
      Some(head)
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
