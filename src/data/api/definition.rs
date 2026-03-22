use crate::data::{Heap, RawValue, Tag, Value};

use super::HeapObjectProxy;

/// A proxy for parser definition records stored as `cons(lhs, cons(type, value))`.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct DefinitionRef {
    reference: RawValue,
}

impl DefinitionRef {
    /// Constructs a new parser definition record from lhs, type, and body values.
    /// This exists so parser-facing code can own the nested-cons definition shape through one typed heap-boundary surface.
    /// The invariant is that Miranda definition records remain `cons(lhs, cons(type, value))`.
    pub fn new(heap: &mut Heap, lhs: Value, type_value: Value, body_value: Value) -> Self {
        let type_and_body = heap.cons_ref(type_value, body_value);
        let reference: RawValue = heap.cons_ref(lhs, type_and_body).into();
        DefinitionRef { reference }
    }

    /// Returns the left-hand-side payload of the definition record.
    /// This exists so parser-facing code can project the definition lhs without open-coding `dlhs` shape access.
    /// The invariant is that the returned value is the head of the outer cons cell.
    pub fn lhs_value(&self, heap: &Heap) -> Value {
        let outer = heap[self.reference];
        debug_assert_eq!(outer.tag, Tag::Cons);
        outer.head.into()
    }

    /// Returns the raw type payload of the definition record.
    /// This exists so parser-facing code can project the definition type without open-coding nested-cons indexing before the payload domain is fully lifted.
    /// The invariant is that the returned payload is the head of the inner cons cell.
    pub fn type_raw(&self, heap: &Heap) -> RawValue {
        let outer = heap[self.reference];
        debug_assert_eq!(outer.tag, Tag::Cons);

        let inner_ref = outer.tail;
        let inner = heap[inner_ref];
        debug_assert_eq!(inner.tag, Tag::Cons);

        inner.head
    }

    /// Returns the body/value payload of the definition record.
    /// This exists so parser-facing code can project the definition body without open-coding `dval` shape access.
    /// The invariant is that the returned value is the tail of the inner cons cell.
    pub fn body_value(&self, heap: &Heap) -> Value {
        let outer = heap[self.reference];
        debug_assert_eq!(outer.tag, Tag::Cons);

        let inner_ref = outer.tail;
        let inner = heap[inner_ref];
        debug_assert_eq!(inner.tag, Tag::Cons);

        inner.tail.into()
    }

    /// Replaces the body/value payload of the definition record.
    /// This exists so parser-facing rewrites can update the definition body through the typed `Value` surface instead of direct nested-cons writes.
    /// The invariant is that only the tail of the inner cons cell changes and the overall definition shape stays `cons(lhs, cons(type, value))`.
    pub fn set_body_value(&self, heap: &mut Heap, body_value: Value) {
        let inner_ref = heap[self.reference].tail;
        debug_assert_eq!(heap[self.reference].tag, Tag::Cons);
        debug_assert_eq!(heap[inner_ref].tag, Tag::Cons);
        heap[inner_ref].tail = body_value.into();
    }
}

impl HeapObjectProxy for DefinitionRef {
    fn from_ref(reference: RawValue) -> Self {
        DefinitionRef { reference }
    }

    fn get_ref(&self) -> RawValue {
        self.reference
    }
}

#[cfg(test)]
mod tests {
    use crate::data::Combinator;
    use super::*;

    #[test]
    fn definition_proxy_reads_nested_cons_shape() {
        let mut heap = Heap::new();
        let definition = DefinitionRef::new(
            &mut heap,
            Combinator::Plus.into(),
            Combinator::Minus.into(),
            Combinator::Times.into(),
        );

        assert_eq!(
            definition.lhs_value(&heap),
            Combinator::Plus.into()
        );
        assert_eq!(
            definition.type_raw(&heap),
            Combinator::Minus.into()
        );
        assert_eq!(
            definition.body_value(&heap),
            Combinator::Times.into()
        );
    }

    #[test]
    fn definition_proxy_updates_body_without_changing_shape() {
        let mut heap = Heap::new();
        let definition = DefinitionRef::new(
            &mut heap,
            Combinator::Plus.into(),
            Combinator::Minus.into(),
            Combinator::Times.into(),
        );

        definition.set_body_value(&mut heap, Combinator::DivideInteger.into());

        assert_eq!(
            definition.body_value(&heap),
            Combinator::DivideInteger.into()
        );
        assert_eq!(heap[definition.get_ref()].tag, Tag::Cons);
        assert_eq!(heap[heap[definition.get_ref()].tail].tag, Tag::Cons);
    }
}
