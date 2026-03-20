use crate::data::{Heap, RawValue, Tag, Value, ATOM_LIMIT};

use super::HeapObjectProxy;

/// Reference-semantics view of one Miranda application node.
///
/// Heap shape mapped by this proxy:
/// a `Tag::Ap` cell with `head = function` and `tail = argument`.
///
/// This proxy is used anywhere the runtime wants to inspect or rewrite application spines through a
/// typed API instead of open-coding `Tag::Ap` heap indexing.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct ApNodeRef {
    reference: RawValue,
}

impl ApNodeRef {
    /// Constructs a new application node from a function value and an argument value.
    /// This exists so parser-facing code can own `Tag::Ap` construction through one typed heap-boundary surface.
    /// The invariant is that Miranda application nodes remain `Tag::Ap` cells with `head=function` and `tail=argument`.
    pub fn new(heap: &mut Heap, function: Value, argument: Value) -> Self {
        let reference: RawValue = heap.apply_ref(function, argument).into();
        ApNodeRef { reference }
    }

    /// Returns the raw function/head payload stored in this application node.
    /// This exists so parser-facing code can own `Tag::Ap` shape access even where the payload domain is not yet fully lifted beyond raw Miranda atoms/references.
    /// The invariant is that the returned payload is exactly the `head` field of the underlying `Tag::Ap` cell.
    pub fn function_raw(&self, heap: &Heap) -> RawValue {
        let cell = heap[self.reference];
        debug_assert_eq!(cell.tag, Tag::Ap);
        cell.head
    }

    /// Returns the raw argument/tail payload stored in this application node.
    /// This exists so parser-facing code can own `Tag::Ap` shape access even where the payload domain is not yet fully lifted beyond raw Miranda atoms/references.
    /// The invariant is that the returned payload is exactly the `tail` field of the underlying `Tag::Ap` cell.
    pub fn argument_raw(&self, heap: &Heap) -> RawValue {
        let cell = heap[self.reference];
        debug_assert_eq!(cell.tag, Tag::Ap);
        cell.tail
    }

    /// Projects the function side as another application node when the nested shape is also `Tag::Ap`.
    /// This exists so parser-facing spine walks can test nested application structure at one typed seam.
    /// The invariant is that projection succeeds only when the raw function payload is a reference to a `Tag::Ap` heap cell.
    pub fn function_application(&self, heap: &Heap) -> Option<Self> {
        let function = self.function_raw(heap);
        if function < ATOM_LIMIT {
            return None;
        }

        (heap[function].tag == Tag::Ap).then_some(ApNodeRef {
            reference: function,
        })
    }

    /// Replaces the argument/tail payload stored in this application node.
    /// This exists so parser-facing rewrites can mutate application structure through the typed `Value` surface instead of direct `tail` writes.
    /// The invariant is that only the `tail` payload changes and the cell remains a `Tag::Ap` node.
    pub fn set_argument(&self, heap: &mut Heap, argument: Value) {
        debug_assert_eq!(heap[self.reference].tag, Tag::Ap);
        heap[self.reference].tail = argument.into();
    }
}

impl HeapObjectProxy for ApNodeRef {
    fn from_ref(reference: RawValue) -> Self {
        ApNodeRef { reference }
    }

    fn get_ref(&self) -> RawValue {
        self.reference
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data::Combinator;

    #[test]
    fn ap_node_proxy_reads_nested_application_shape() {
        let mut heap = Heap::new();
        let inner = ApNodeRef::new(&mut heap, Combinator::Plus.into(), Combinator::Minus.into());
        let outer = ApNodeRef::new(&mut heap, inner.into(), Combinator::Times.into());

        assert_eq!(outer.function_raw(&heap), inner.get_ref());
        assert_eq!(outer.argument_raw(&heap), Combinator::Times.into());
        assert_eq!(outer.function_application(&heap), Some(inner));
    }

    #[test]
    fn ap_node_proxy_updates_argument_without_changing_shape() {
        let mut heap = Heap::new();
        let node = ApNodeRef::new(&mut heap, Combinator::Plus.into(), Combinator::Minus.into());

        node.set_argument(&mut heap, Combinator::Times.into());

        assert_eq!(node.argument_raw(&heap), Combinator::Times.into());
        assert_eq!(heap[node.get_ref()].tag, Tag::Ap);
    }
}
