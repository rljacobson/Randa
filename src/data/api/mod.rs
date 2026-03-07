/*!
This module provides a high-level API for working with heap-resident objects. It defines
lightweight wrapper types that implement HeapObjectProxy and provide lazy accessors along with
utilities for manipulating the underlying object on the heap. Because a proxy does not retain
a mutable reference to the heap, most methods take such a reference as an explicit parameter.

A HeapObjectProxy is intended to behave like a pointer to a heap object, with its methods acting like
object->accessor() calls in C++. Proxies must therefore be lightweight and implement Copy. In general,
implementations should avoid caching field values and instead retrieve them from the heap on each
access. It is possible for the underlying heap object to change independently, which could silently
invalidate a proxy. Client code must ensure that such mutations do not occur while the proxy is in use.

Within the runtime, the fallibility of proxy field accessors follows the system’s trust boundaries. Objects
constructed through the runtime’s typed public APIs are assumed to satisfy internal invariants, so proxy accessors
in these trusted paths should be infallible and may index directly into the heap shape. This is analogous to field
access in a language like Rust: once a value has been constructed correctly, accessing its fields should not fail.

Heap-shape validation should instead occur at boundary ingress points—such as bytecode or input decoding—where
untrusted data first enters the system. Within trusted runtime code, Option should represent only true
semantic optionality (a value may legitimately be absent), not validation failures. Similarly, patterns
like Result<_, ()> should be reserved for boundary checks rather than used in trusted proxy getters.
*/

mod alias_entry;
mod cons_list;
mod data_pair;
mod file_info;
mod file_record;
mod identifier_record;
mod open_file;

// Implementors of HeapObjectProxy (defined below)
pub(crate) use alias_entry::AliasEntry;
pub(crate) use cons_list::ConsList;
pub(crate) use data_pair::DataPair;
pub(crate) use file_info::FileInfoRef;
pub(crate) use file_record::FileRecord;
pub(crate) use identifier_record::*;
pub(crate) use open_file::OpenFile;

use crate::data::{RawValue, Value};

pub type HeapString = String;
pub type LineNumber = isize;

/// A lightweight proxy for an object that lives on the heap.
pub trait HeapObjectProxy: Copy + Clone {
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

// impl From<T> for RawValue
//   where T: HeapObjectProxy
// {
//   fn from(value: &T) -> RawValue {
//     value.get_ref()
//   }
// }

impl<T> From<T> for Value
where
    T: HeapObjectProxy,
{
    fn from(value: T) -> Value {
        value.get_ref().into()
    }
}

// endregion
