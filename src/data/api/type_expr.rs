/*!
Semantic wrapper for type-expression roots.

`TypeExprRef` is not a `HeapObjectProxy` in that it is not a single heap-cell shape. The wrapped
root may be a builtin type atom, a type identifier, a type variable, or a heap reference into
application and wrapper nodes used to build larger type expressions.

The wrapper exists so VM-side seams can talk about type expressions as one domain and share basic
navigation helpers without open-coding raw `Value` walks in each subsystem.
*/

use super::{HeapObjectProxy, IdentifierRecordRef};
use crate::data::{Heap, RawValue, Tag, Type, Value, ATOM_LIMIT};

/// Semantic reference to one type-expression root.
///
/// Value shapes covered by this wrapper:
/// - builtin Miranda type atoms such as `Type::Bool`
/// - identifier leaves (`Tag::Id`)
/// - type-variable leaves (`Tag::TypeVar`)
/// - heap application and wrapper nodes that compose larger type expressions
///
/// This wrapper is used by VM-side API seams and typecheck so type-expression consumers share one
/// representation-navigation surface while keeping semantic classification outside the wrapper.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct TypeExprRef {
    value: Value,
}

impl TypeExprRef {
    /// Wraps one type-expression root value.
    /// This exists so VM-side APIs can state that a payload is a type expression without forcing a single heap-cell shape.
    /// The invariant is that trusted callers pass a Miranda type-expression root value.
    pub fn new(value: Value) -> Self {
        Self { value }
    }

    /// Returns the wrapped Miranda value.
    /// This exists so existing heap-writing APIs can keep their raw `Value` surface while `TypeExprRef` is introduced incrementally.
    /// The invariant is that the returned value is exactly the type-expression root captured by this wrapper.
    pub fn value(self) -> Value {
        self.value
    }

    /// Returns whether the wrapped root is exactly one builtin/immediate Miranda type atom.
    /// This exists so callers can test for immediate type constants without depending on `Value` variant details such as `Data` versus `Char` encodings.
    /// The invariant is that this compares only the encoded immediate word for `expected` and does not inspect heap structure.
    pub fn is_builtin_type(self, expected: Type) -> bool {
        RawValue::from(self.value) == RawValue::from(expected)
    }

    /// Returns the canonical application-spine view for a type expression rooted at an identifier head.
    /// This exists so VM-side typename classification and arity checking can share one normalized type-application traversal.
    /// The invariant is that successful results preserve source-order applied arguments and return only identifier-headed spines.
    pub fn identifier_head_application_spine(
        &self,
        heap: &Heap,
    ) -> Option<(IdentifierRecordRef, Vec<TypeExprRef>)> {
        let mut raw_reference: RawValue = self.value.into();
        if raw_reference < ATOM_LIMIT {
            return None;
        }

        let mut arguments = Vec::new();
        while heap[raw_reference].tag == Tag::Ap {
            arguments.push(TypeExprRef::new(heap[raw_reference].tail.into()));
            raw_reference = heap[raw_reference].head;
            if raw_reference < ATOM_LIMIT {
                return None;
            }
        }

        if heap[raw_reference].tag != Tag::Id {
            return None;
        }

        arguments.reverse();
        Some((IdentifierRecordRef::from_ref(raw_reference), arguments))
    }

    /// Returns the two child type-expression roots for the pair-like recursive nodes typecheck already traverses.
    /// This exists so VM-side recursive type-expression walks do not open-code `Tag::Cons`, `Tag::Pair`, and `Tag::TCons` projection.
    /// The invariant is that successful projection returns the exact `head` and `tail` payloads of the underlying pair-like cell.
    pub fn binary_children(&self, heap: &Heap) -> Option<(TypeExprRef, TypeExprRef)> {
        let raw_reference: RawValue = self.value.into();
        if raw_reference < ATOM_LIMIT {
            return None;
        }

        matches!(heap[raw_reference].tag, Tag::Cons | Tag::Pair | Tag::TCons).then(|| {
            (
                TypeExprRef::new(heap[raw_reference].head.into()),
                TypeExprRef::new(heap[raw_reference].tail.into()),
            )
        })
    }

    /// Returns the tail child for wrapper nodes whose head payload is not part of type-expression recursion.
    /// This exists so VM-side type-expression walks can recurse through `Tag::Label`, `Tag::Show`, and `Tag::Share` uniformly.
    /// The invariant is that successful projection returns the exact `tail` payload of the underlying wrapper node.
    pub fn tail_child(&self, heap: &Heap) -> Option<TypeExprRef> {
        let raw_reference: RawValue = self.value.into();
        if raw_reference < ATOM_LIMIT {
            return None;
        }

        matches!(heap[raw_reference].tag, Tag::Label | Tag::Show | Tag::Share)
            .then(|| TypeExprRef::new(heap[raw_reference].tail.into()))
    }

    /// Returns the result side of an arrow type.
    /// This exists so consumers that peel leading arrow arguments can do so through the type-expression wrapper instead of reopening `Tag::Ap` layout.
    /// The invariant is that projection succeeds only for Miranda arrow types and returns the outer application tail.
    pub fn arrow_result_type(&self, heap: &Heap) -> Option<TypeExprRef> {
        if !heap.is_arrow_type(self.value) {
            return None;
        }

        let raw_reference: RawValue = self.value.into();
        Some(TypeExprRef::new(heap[raw_reference].tail.into()))
    }

    /// Returns the ordinal of a type-variable leaf.
    /// This exists so consumers can inspect type-variable payloads through the wrapper instead of indexing the heap with raw references.
    /// The invariant is that projection succeeds only for `Tag::TypeVar` roots and returns the stored tail ordinal unchanged.
    pub fn type_variable_ordinal(&self, heap: &Heap) -> Option<isize> {
        let raw_reference: RawValue = self.value.into();
        if raw_reference < ATOM_LIMIT {
            return None;
        }

        (heap[raw_reference].tag == Tag::TypeVar).then_some(heap[raw_reference].tail)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data::{Heap, Type, Value};

    #[test]
    fn identifier_head_application_spine_preserves_source_order_arguments() {
        let mut heap = Heap::new();
        let pair = heap.make_empty_identifier("pair");
        let first = TypeExprRef::new(heap.type_var_ref(Value::None, Value::Data(1)));
        let second = TypeExprRef::new(heap.apply_ref(Type::List.into(), Type::Char.into()));
        let applied_head = heap.apply_ref(pair.into(), first.value());
        let applied = TypeExprRef::new(heap.apply_ref(applied_head, second.value()));

        let (head, arguments) = applied
            .identifier_head_application_spine(&heap)
            .expect("expected identifier-headed type application spine");

        assert_eq!(head, pair);
        assert_eq!(arguments, vec![first, second]);
    }

    #[test]
    fn identifier_head_application_spine_rejects_non_identifier_roots() {
        let mut heap = Heap::new();
        let builtin = TypeExprRef::new(Type::Bool.into());
        let type_var = TypeExprRef::new(heap.type_var_ref(Value::None, Value::Data(1)));

        assert!(builtin.identifier_head_application_spine(&heap).is_none());
        assert!(type_var.identifier_head_application_spine(&heap).is_none());
    }

    #[test]
    fn binary_and_tail_projection_cover_existing_typecheck_recursive_shapes() {
        let mut heap = Heap::new();
        let pair_like = TypeExprRef::new(heap.pair_ref(Type::Char.into(), Type::Bool.into()));
        let wrapped = TypeExprRef::new(heap.label_ref(Type::Number.into(), pair_like.value()));

        let (head, tail) = pair_like
            .binary_children(&heap)
            .expect("expected pair-like binary children");
        assert!(head.is_builtin_type(Type::Char));
        assert!(tail.is_builtin_type(Type::Bool));
        assert_eq!(
            wrapped
                .tail_child(&heap)
                .expect("expected wrapped tail child"),
            pair_like
        );
    }

    #[test]
    fn arrow_and_type_variable_projection_hide_raw_heap_shape() {
        let mut heap = Heap::new();
        let typevar = TypeExprRef::new(heap.type_var_ref(Value::None, Value::Data(2)));
        let arrow =
            TypeExprRef::new(heap.apply2(Type::Arrow.into(), typevar.value(), Type::Char.into()));

        assert_eq!(typevar.type_variable_ordinal(&heap), Some(2));
        assert_eq!(
            arrow
                .arrow_result_type(&heap)
                .expect("expected arrow result type")
                .is_builtin_type(Type::Char),
            true
        );
    }
}
