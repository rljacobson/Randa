/*!

This module provides a high-level API for working with heap-resident objects. The
API consists of wrapper structures for heap objects that provide lazy accessors and
facilities for manipulating the object on the heap. These wrappers implement `HeapObjectProxy`.

There's a tricky issue with what data can be cached and what data needs to be looked up. The underlying data could
conceivably be completely changed, silently invalidating a wrapper. It is up to the client code to ensure
this doesn't happen. Think of a wrapper as a pointer to an object and the accessors as `object->accessor()` functions
as in C++. In general, implementations should NOT cache values of members but rather look them up in the heap for
every access.

A `HeapObjectProxy` is meant to be lightweight and must implement `Copy`.

Because a wrapper does not keep a mutable reference to the heap, most methods take such a reference as a
parameter.

*/

mod file_record;
mod cons_list;
mod identifier_record;

// Implementors of HeapObjectProxy (defined below)
pub(crate) use file_record::FileRecord;
pub(crate) use identifier_record::*;
pub(crate) use cons_list::ConsList;

use crate::{
  data::{
    Value,
    ValueRepresentationType,
    RawValue,
    Heap
  }
};



pub type HeapString = String;
pub type LineNumber = ValueRepresentationType;
/// A lightweight proxy for an object that lives on the heap.
pub trait HeapObjectProxy: Copy + Clone + Eq + PartialEq {
  /// Constructs a `Self` from an existing object on the heap at `reference`.
  fn from_ref(reference: RawValue) -> Self;

  // Creates a new object on the heap and returns a `Self` referencing that object.
  // Because `new(..)` needs to take the members of the object as parameters, it isn't part of the trait.
  // fn new(&mut heap: Heap) -> Self;

  /// Get a raw reference to the underlying heap object.
  fn get_ref(&self) -> RawValue;

  // Fetches the data referenced by this `HeapProxyObject`. The data will contain other `HeapProxyObject`s where
  // appropriate.
  // fn get_data(&self, heap: &Heap) -> HeapObject;
}

// region Conversion to/from the different value representations

impl From<T> for ValueRepresentationType
  where T: HeapObjectProxy
{
  fn from(value: &T) -> ValueRepresentationType {
    value.get_ref().0
  }
}


impl From<T> for Value
  where T: HeapObjectProxy
{
  fn from(value: &T) -> Value {
    value.get_ref().into()
  }
}


impl From<T> for RawValue
  where T: HeapObjectProxy
{
  fn from(value: &T) -> RawValue {
    value.get_ref()
  }
}

// endregion
