/*!
This module provides a high-level API for working with heap-resident objects. Most exported wrapper
types implement HeapObjectProxy and provide lazy accessors along with utilities for manipulating the
underlying object on the heap. It also hosts narrowly scoped semantic wrappers, such as
`TypeExprRef`, for recursive value domains that do not correspond to one heap-cell shape. Because a
proxy does not retain a mutable reference to the heap, most methods take such a reference as an
explicit parameter.

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

mod algebraic_constructor;
mod alias_entry;
mod ap_node;
mod cons_list;
mod constructor;
mod data_pair;
mod definition;
mod file_info;
mod file_record;
mod free_formal_binding;
mod identifier_record;
mod open_file;
mod private_name;
mod str_cons;
mod type_expr;

// HeapObjectProxy implementors and related semantic wrappers.
pub(crate) use algebraic_constructor::*;
pub(crate) use alias_entry::AliasEntry;
pub(crate) use ap_node::ApNodeRef;
pub(crate) use cons_list::ConsList;
pub(crate) use constructor::ConstructorRef;
pub(crate) use data_pair::DataPair;
#[allow(unused_imports)]
pub(crate) use definition::DefinitionRef;
pub(crate) use file_info::FileInfoRef;
pub(crate) use file_record::FileRecord;
pub(crate) use free_formal_binding::FreeFormalBindingRef;
pub(crate) use identifier_record::*;
pub(crate) use open_file::OpenFile;
pub(crate) use private_name::PrivateNameRef;
pub(crate) use str_cons::StrConsRef;
pub(crate) use type_expr::TypeExprRef;

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
