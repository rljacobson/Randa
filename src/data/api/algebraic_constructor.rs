/*!
Typed proxies for algebraic constructor metadata.

Algebraic type identifiers store declaration-side constructor information in two related heap
shapes:

- one constructor entry per declared constructor:
  `cons(constructor_id, cons(arity, fields))`
- one field entry per declared field inside `fields`:
  `cons(field_type, strict_flag)`

`AlgebraicConstructorMetadataRef` owns the outer constructor entry, while
`AlgebraicConstructorFieldRef` owns each field entry inside its `fields` list. Together they let
load/typecheck/codegen consume constructor declaration facts after parser-local trees are gone.
*/

use super::{ConsList, HeapObjectProxy, IdentifierRecordRef, TypeExprRef};
use crate::data::{Combinator, Heap, RawValue};

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct AlgebraicConstructorFieldParts {
    pub type_expr: TypeExprRef,
    pub is_strict: bool,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct AlgebraicConstructorMetadataParts {
    pub constructor: IdentifierRecordRef,
    pub arity: isize,
    pub fields: ConsList<AlgebraicConstructorFieldRef>,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
/// Reference-semantics view of one constructor-field metadata entry.
///
/// Heap shape mapped by this proxy:
/// `cons(field_type, strict_flag)` where `strict_flag` is `True` or `False`.
///
/// This proxy is used inside algebraic constructor metadata so later phases can inspect field
/// types and strict-field markers without depending on parser-local declaration trees.
pub struct AlgebraicConstructorFieldRef {
    reference: RawValue,
}

impl AlgebraicConstructorFieldRef {
    /// Constructs one algebraic constructor-field metadata cell.
    /// This exists so declaration commit can encode field type/strictness facts through a typed proxy instead of open-coding cons pairs.
    /// The invariant is that the heap shape remains `cons(field_type, strict_flag)` where `strict_flag` is `True` or `False`.
    pub fn new(heap: &mut Heap, parts: AlgebraicConstructorFieldParts) -> Self {
        let strict_flag = if parts.is_strict {
            Combinator::True.into()
        } else {
            Combinator::False.into()
        };
        let reference = heap.cons_ref(parts.type_expr.value(), strict_flag).into();

        Self { reference }
    }

    /// Returns the field type expression.
    /// This exists so constructor metadata consumers can read field type payloads through the shared `TypeExprRef` seam.
    /// The invariant is that the returned wrapper references the exact `head` payload of the field metadata cell.
    pub fn type_expr(&self, heap: &Heap) -> TypeExprRef {
        TypeExprRef::new(heap[self.reference].head.into())
    }

    /// Returns whether the field carries a strictness marker.
    /// This exists so later declaration/typecheck/codegen consumers can query strict-field positions without re-parsing type shapes.
    /// The invariant is that only the canonical `True` strict flag decodes as strict here.
    pub fn is_strict(&self, heap: &Heap) -> bool {
        heap[self.reference].tail == RawValue::from(Combinator::True)
    }

    /// Decodes the field metadata into value-semantics form.
    /// This exists so tests and higher-level consumers can read the full field payload through one proxy-owned helper.
    /// The invariant is that the decoded payload matches `type_expr()` plus `is_strict()` for the same heap cell.
    pub fn get_data(&self, heap: &Heap) -> AlgebraicConstructorFieldParts {
        AlgebraicConstructorFieldParts {
            type_expr: self.type_expr(heap),
            is_strict: self.is_strict(heap),
        }
    }
}

impl HeapObjectProxy for AlgebraicConstructorFieldRef {
    fn from_ref(reference: RawValue) -> Self {
        Self { reference }
    }

    fn get_ref(&self) -> RawValue {
        self.reference
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
/// Reference-semantics view of one constructor metadata entry.
///
/// Heap shape mapped by this proxy:
/// `cons(constructor_id, cons(arity, fields))` where `fields` is a source-order cons list of
/// `AlgebraicConstructorFieldRef` cells.
///
/// This proxy is used by algebraic type metadata consumers to recover constructor order, declared
/// arity, and per-field declaration facts from VM state.
pub struct AlgebraicConstructorMetadataRef {
    reference: RawValue,
}

impl AlgebraicConstructorMetadataRef {
    /// Constructs one algebraic constructor-metadata entry.
    /// This exists so algebraic type info can own constructor identifier, arity, and field metadata through one typed heap proxy.
    /// The invariant is that the heap shape remains `cons(constructor, cons(arity, fields))`.
    pub fn new(heap: &mut Heap, parts: AlgebraicConstructorMetadataParts) -> Self {
        let metadata = heap.cons_ref(parts.arity.into(), parts.fields.into());
        let reference = heap.cons_ref(parts.constructor.into(), metadata).into();

        Self { reference }
    }

    /// Returns the constructor identifier for this metadata entry.
    /// This exists so constructor-order and constructor-lookup consumers can recover the owning constructor without reopening heap shape details.
    /// The invariant is that the returned identifier is the `head` payload of the metadata cell.
    pub fn constructor(&self, heap: &Heap) -> IdentifierRecordRef {
        IdentifierRecordRef::from_ref(heap[self.reference].head)
    }

    /// Returns the declared arity for this constructor.
    /// This exists so formal checking and later codegen can consume constructor arity from declaration metadata.
    /// The invariant is that the returned value is the `head` of the nested metadata pair.
    pub fn arity(&self, heap: &Heap) -> isize {
        let metadata_ref = heap[self.reference].tail;
        heap[metadata_ref].head
    }

    /// Returns the source-order field metadata list for this constructor.
    /// This exists so downstream consumers can traverse field payloads without depending on parser-local declaration trees.
    /// The invariant is that the returned value is the `tail` of the nested metadata pair.
    pub fn fields(&self, heap: &Heap) -> ConsList<AlgebraicConstructorFieldRef> {
        let metadata_ref = heap[self.reference].tail;
        ConsList::from_ref(heap[metadata_ref].tail)
    }

    /// Decodes the full constructor metadata entry into value-semantics form.
    /// This exists so tests and future boundary code can read one constructor metadata record through one proxy-owned helper.
    /// The invariant is that the decoded payload matches `constructor()`, `arity()`, and `fields()` for the same heap cell.
    pub fn get_data(&self, heap: &Heap) -> AlgebraicConstructorMetadataParts {
        AlgebraicConstructorMetadataParts {
            constructor: self.constructor(heap),
            arity: self.arity(heap),
            fields: self.fields(heap),
        }
    }
}

impl HeapObjectProxy for AlgebraicConstructorMetadataRef {
    fn from_ref(reference: RawValue) -> Self {
        Self { reference }
    }

    fn get_ref(&self) -> RawValue {
        self.reference
    }
}
