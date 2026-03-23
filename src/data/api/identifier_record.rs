/*!

Identifiers are complicated in Miranda.

The `head` of an `Id` contains `cons(strcons(name,who),type)` and the `tail` has the value.
The `name` is a reference to a heap string (see Strings below). The `who` field contains one of:

* `NIL` (the combinator literal) for a name that is totally undefined
* `hereinfo` for a name that has been defined or specified, where `hereinfo` is `fileinfo(script,line_no)`
* `cons(aka,hereinfo)` for a name that has been aliased, where `aka`
  is of the form `datapair(oldn,0)`, `oldn` being a string.

The value field (tail) of type identifier takes one of the following forms:
 * `cons(cons(arity,showfn),cons(algebraic_t,constructors)`
 * `cons(cons(arity,showfn),cons(synonym_t,rhs))`
 * `cons(cons(arity,showfn),cons(abstract_t,basis))`
 * `cons(cons(arity,showfn),cons(placeholder_t,NIL))`
 * `cons(cons(arity,showfn),cons(free_t,NIL))`
 * `UNDEF` (combinator literal)
 * _Some other value_

An example of "some other value" is in the case of identifiers that are also constructors. Their value field
contains `construct(value, id_ref)`, where `id_ref` is a (circular) reference back to the identifier itself.

|              | Arity    | Show Functions |            | Type             | Info             |
|:-------------| :------- | :------------- | :--------- | :--------------- | :--------------- |
| `cons(cons(` | `arity,` | `showfn`       | `), cons(` | `algebraic_t,`   | `constructors )` |
| `cons(cons(` | `arity,` | `showfn`       | `), cons(` | `synonym_t,`     | `rhs  ))`        |
| `cons(cons(` | `arity,` | `showfn`       | `), cons(` | `abstract_t,`    | `basis ))`       |
| `cons(cons(` | `arity,` | `showfn`       | `), cons(` | `placeholder_t,` | `NIL ))`         |
| `cons(cons(` | `arity,` | `showfn`       | `), cons(` | `free_t,`        | `NIL ))`         |
 | `UNDEF`      |
 | _Value_      |

where

```C
#define algebraic_t 0
#define synonym_t 1
#define abstract_t 2
#define placeholder_t 3
#define free_t 4
```

(Note: The Miranda source fails to mention the `UNDEF` and _"other literal value"_ cases.)

See [Data Representation.md](Data%20Representation.md) for a diagram of how Identifiers are represented on the heap.

*/

use super::{
    AlgebraicConstructorMetadataRef, ConsList, DataPair, FileInfoRef, HeapObjectProxy, HeapString,
    LineNumber, PrivateNameRef, TypeExprRef,
};
use crate::compiler::HereInfo;
use crate::data::{Combinator, Heap, RawValue, Tag, Type, Value};
use enum_primitive_derive::Primitive;
use num_traits::FromPrimitive;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct IdentifierRecordData {
    pub name: HeapString, // `String` or `&str`?
    /// The `who` field
    pub definition: IdentifierDefinitionRef,
    /// Identifier-level datatype (the `type` in `cons(strcons(name,who),type)`)
    // Note: `datatype` cannot be type `Type` because, e.g. `char_list_type` is of type Value.
    pub datatype: Value,
    /// Identifier-level value field (`tail` of the `Tag::Id` cell)
    pub value: Option<IdentifierValueRef>,
}

/// A strict subset of the fields of `IdentifierRecordData` for use with `IdentifierCoreRef`
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct IdentifierCoreData {
    /// The `who` field
    pub definition: IdentifierDefinitionRef,
    /// Identifier-level datatype (the `type` in `cons(strcons(name,who),type)`)
    pub datatype: Value,
    /// Identifier-level value field (`tail` of the `Tag::Id` cell)
    pub value: Option<IdentifierValueRef>,
}

/// Reference-semantics view of alias hold payloads used during alias rollback.
///
/// Heap shape mapped by this proxy:
/// `cons(id_who(old), cons(id_type(old), id_val(old)))`
///
/// Corresponding value-semantics type: `IdentifierCoreData`.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct IdentifierCoreRef {
    reference: RawValue,
}

impl IdentifierCoreRef {
    pub fn new(heap: &mut Heap, data: IdentifierCoreData) -> Self {
        let value = data
            .value
            .map_or(Value::Combinator(Combinator::Nil), Value::from);
        let tail_value = heap.cons_ref(data.datatype, value);
        let hold_value = heap.cons_ref(data.definition.get_ref().into(), tail_value);
        let reference = match hold_value {
            Value::Reference(reference) => reference,
            _ => unreachable!(),
        };

        IdentifierCoreRef { reference }
    }

    pub fn from_old_identifier(heap: &mut Heap, old: IdentifierRecordRef) -> Self {
        // cons(  id_who(old), cons(  id_type(old), id_val(old))  );
        let core_data = old.get_core_data(heap);

        IdentifierCoreRef::new(heap, core_data)
    }

    pub fn definition(&self, heap: &Heap) -> IdentifierDefinitionRef {
        IdentifierDefinitionRef::from_ref(heap[self.reference].head)
    }

    pub fn datatype(&self, heap: &Heap) -> Value {
        let type_value_ref = heap[self.reference].tail;
        heap[type_value_ref].head.into()
    }

    pub fn value(&self, heap: &Heap) -> Option<IdentifierValueRef> {
        let type_value_ref = heap[self.reference].tail;
        let value_ref = heap[type_value_ref].tail;
        if value_ref == Combinator::Nil.into() {
            None
        } else {
            Some(IdentifierValueRef::from_ref(value_ref))
        }
    }

    pub fn get_data(&self, heap: &Heap) -> IdentifierCoreData {
        IdentifierCoreData {
            definition: self.definition(heap),
            datatype: self.datatype(heap),
            value: self.value(heap),
        }
    }

    pub fn set_definition(&self, heap: &mut Heap, definition: IdentifierDefinitionRef) {
        heap[self.reference].head = definition.get_ref();
    }

    pub fn set_datatype(&self, heap: &mut Heap, datatype: Value) {
        let type_value_ref = heap[self.reference].tail;
        heap[type_value_ref].head = datatype.into();
    }

    pub fn set_value(&self, heap: &mut Heap, value: Option<IdentifierValueRef>) {
        let type_value_ref = heap[self.reference].tail;
        heap[type_value_ref].tail = value.map_or(Combinator::Nil.into(), |value| value.get_ref());
    }

    pub fn set_data(&self, heap: &mut Heap, data: IdentifierCoreData) {
        self.set_definition(heap, data.definition);
        self.set_datatype(heap, data.datatype);
        self.set_value(heap, data.value);
    }
}

impl HeapObjectProxy for IdentifierCoreRef {
    fn from_ref(reference: RawValue) -> Self {
        IdentifierCoreRef { reference }
    }

    fn get_ref(&self) -> RawValue {
        self.reference
    }
}

/// Reference-semantics view of the top-level identifier heap object (`Tag::Id`).
///
/// Heap shape mapped by this proxy:
/// `cons(cons(strcons(name, who), type), value)`
///
/// Component mapping:
/// - `who` -> `IdentifierDefinitionRef`
/// - `value` -> `Option<IdentifierValueRef>`
/// - `name` is decoded as `HeapString` (`get_name`)
/// - identifier-level `type` is exposed either as the raw `datatype` slot (`get_datatype`/`get_type`) or through `TypeExprRef` when the caller knows the slot holds a real type expression (`get_type_expr`)
///
/// Corresponding value-semantics type: `IdentifierRecordData`.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct IdentifierRecordRef {
    reference: RawValue,
}

impl IdentifierRecordRef {
    pub(crate) const UNINITIALIZED: IdentifierRecordRef = IdentifierRecordRef {
        reference: Combinator::Nil as RawValue,
    };

    pub fn new(
        heap: &mut Heap,
        name: String,
        definition: IdentifierDefinitionRef,
        data_type: Value,
        value: Option<IdentifierValueRef>,
    ) -> IdentifierRecordRef {
        let h_name = heap.string(name.as_str());

        let mut h_id_head = heap.strcons_ref(Value::Reference(h_name), definition.get_ref().into());
        h_id_head = heap.cons_ref(h_id_head, data_type);
        let h_id_tail: Value = if let Some(h_value) = value {
            h_value.get_ref().into()
        } else {
            Combinator::Undef.into()
        };

        let reference: RawValue = heap.put_ref(Tag::Id, h_id_head, h_id_tail).into();
        heap.register_identifier_name(name.as_str(), reference);

        IdentifierRecordRef { reference }
    }

    /// Returns the name of the identifier
    pub fn get_name(&self, heap: &Heap) -> HeapString {
        let name_ref = self.name_ref(heap);
        heap.resolve_string(Value::from(name_ref))
            .or_else(|_| heap.resolve_string(Value::Data(name_ref)))
            .expect("IdentifierRecordRef name does not resolve to a heap string.")
    }

    fn name_value(&self, heap: &Heap) -> Value {
        self.name_ref(heap).into()
    }

    fn name_ref(&self, heap: &Heap) -> RawValue {
        let cons_ref = heap[self.reference].head;
        let str_cons_ref = heap[cons_ref].head;
        heap[str_cons_ref].head
    }

    /// Fetches the `IdentifierDefinitionRef` (a HeapProxyObject) from the heap resident data structure.
    pub fn get_definition(&self, heap: &Heap) -> IdentifierDefinitionRef {
        let id_cell = heap[self.reference];
        let cons_cell = heap[id_cell.head];
        let strcons_cell = heap[cons_cell.head];

        IdentifierDefinitionRef::from_ref(strcons_cell.tail)
    }

    /// Fetches the identifier's raw datatype slot from the heap resident structure.
    /// This exists as the full-domain accessor for identifier type-slot payloads, including bookkeeping sentinels such as `Undefined`, `Alias`, and `New`.
    /// The invariant is that the returned value is exactly the tail payload of `cons(strcons(name, who), type)`.
    pub fn get_datatype(&self, heap: &Heap) -> Value {
        let id_cell = heap[self.reference];
        let cons_cell = heap[id_cell.head];

        cons_cell.tail.into()
    }

    pub fn get_value(&self, heap: &Heap) -> Option<IdentifierValueRef> {
        let id_cell = heap[self.reference];

        if id_cell.tail == Combinator::Nil.into() {
            None
        } else {
            Some(IdentifierValueRef::from_ref(id_cell.tail))
        }
    }

    /// Returns the raw value field of the identifier as a typed `Value`.
    pub fn get_value_field(&self, heap: &Heap) -> Value {
        heap[self.reference].tail.into()
    }

    pub fn set_value_from_data(&self, heap: &mut Heap, value: IdentifierValueData) {
        let id_value = IdentifierValueRef::new(heap, value);
        self.set_value(heap, id_value);
    }

    pub fn set_value(&self, heap: &mut Heap, value: IdentifierValueRef) {
        heap[self.reference].tail = value.get_ref();
    }

    /// Sets the identifier's "who" field.
    pub fn set_definition(&self, heap: &mut Heap, definition: IdentifierDefinitionRef) {
        // ID HEAD: cons(strcons(name,who),type)
        // ID TAIL: cons(cons(arity, showfn), cons(type, NIL))
        let id_head = heap[self.reference].head;
        let strcons_ref = heap[id_head].head;
        heap[strcons_ref].tail = definition.get_ref();
    }

    /// Fetches the identifier's raw datatype slot.
    /// This exists as a historical convenience alias over `get_datatype()` for callers that still use `type` terminology.
    /// The invariant is that this method returns the same raw payload as `get_datatype()`.
    pub fn get_type(&self, heap: &Heap) -> Value {
        self.get_datatype(heap)
    }

    /// Fetches the identifier datatype through the `TypeExprRef` semantic wrapper.
    /// This exists for callers that know the identifier type slot currently holds a real type-expression root rather than alias/load bookkeeping sentinels.
    /// The invariant is that the returned wrapper references the same slot payload returned by `get_datatype()`.
    pub fn get_type_expr(&self, heap: &Heap) -> TypeExprRef {
        TypeExprRef::new(self.get_datatype(heap))
    }

    /// Sets the identifier's raw datatype slot.
    /// This exists as the full-domain write surface for identifier type-slot payloads, including bookkeeping sentinels and non-`Type` user type identifiers.
    /// The invariant is that only the datatype payload changes and the surrounding identifier heap shape remains unchanged.
    pub fn set_datatype(&self, heap: &mut Heap, datatype: Value) {
        let id_head = heap[self.reference].head;
        heap[id_head].tail = datatype.into()
    }

    /// Sets the identifier's raw datatype slot using historical `type` terminology.
    /// This exists as a convenience alias over `set_datatype()` for callers that still speak in terms of identifier `type` rather than `datatype`.
    /// The invariant is that this method writes the same raw payload as `set_datatype()`.
    pub fn set_type(&self, heap: &mut Heap, new_type: Value) {
        self.set_datatype(heap, new_type)
    }

    /// Sets the identifier datatype through the `TypeExprRef` semantic wrapper.
    /// This exists for callers that are writing a true type-expression root into the identifier type slot and want a typed API boundary.
    /// The invariant is that the written slot payload is exactly `type_expr.value()`.
    pub fn set_type_expr(&self, heap: &mut Heap, type_expr: TypeExprRef) {
        self.set_datatype(heap, type_expr.value())
    }

    pub fn get_core_data(&self, heap: &Heap) -> IdentifierCoreData {
        IdentifierCoreData {
            definition: self.get_definition(heap),
            datatype: self.get_datatype(heap),
            value: self.get_value(heap),
        }
    }

    pub fn set_core_data(&self, heap: &mut Heap, data: IdentifierCoreData) {
        self.set_definition(heap, data.definition);
        self.set_datatype(heap, data.datatype);
        match data.value {
            Some(value) => self.set_value(heap, value),
            None => self.set_value(heap, IdentifierValueRef::from_ref(Combinator::Nil.into())),
        }
    }

    /// Fetches all of the data associated with the `IdentifierRecordRef`.
    pub fn get_data(&self, heap: &Heap) -> IdentifierRecordData {
        let name: HeapString = self.get_name(heap);
        let core_data = self.get_core_data(heap);

        IdentifierRecordData {
            name,
            definition: core_data.definition,
            datatype: core_data.datatype,
            value: core_data.value,
        }
    }

    /// An IdentifierRecordRef is "unset" if its `value` is `UNDEF`, its `who` is `NIL`, and its `type` is `undef_t`
    pub fn unset_id(&self, heap: &mut Heap) {
        assert_eq!(heap[self.reference].tag, Tag::Id);

        self.set_value(heap, IdentifierValueRef::from_ref(Combinator::Undef.into()));
        self.set_definition(heap, IdentifierDefinitionRef::undefined());
        self.set_datatype(heap, Type::Undefined.into());
    }
}

impl HeapObjectProxy for IdentifierRecordRef {
    fn from_ref(reference: RawValue) -> Self {
        IdentifierRecordRef { reference }
    }

    fn get_ref(&self) -> RawValue {
        self.reference
    }
}

/// An `IdentifierDefinitionData` is the `who` field in Miranda.
pub enum IdentifierDefinitionData {
    /// The identifier is completely undefined.
    Undefined,

    /// `hereinfo` is `fileinfo(script,line_no)`
    HereInfo(HereInfo), // source_file and line_number

    /// `cons(aka,hereinfo)` for a name that has been aliased, where `aka`
    ///      is of the form `datapair(oldn,0)`, `oldn` being a string.
    Alias {
        here_info: HereInfo, // source_file and line_number
        source: HeapString,  // Old name
    },
}

/// Reference-semantics view of the identifier `who` field.
///
/// Heap shapes mapped by this proxy:
/// - `NIL` (undefined)
/// - `fileinfo(script, line_no)`
/// - `cons(datapair(old_name, 0), fileinfo(script, line_no))`
///
/// Component mapping:
/// - alias metadata pair -> `DataPair`
/// - location payload -> `FileInfoRef`
///
/// Corresponding value-semantics type: `IdentifierDefinitionData`.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct IdentifierDefinitionRef {
    reference: RawValue,
}

impl IdentifierDefinitionRef {
    /// Creates a new identifier definition on the heap and constructs an `IdentifierDefinitionRef` referencing it.
    ///
    /// Semantics:
    /// - `here_info.script_file` is source text that is interned into a canonical heap string.
    /// - `source` (when present) is alias source-name text and is encoded as
    ///   `datapair(source_name_ref, 0)` metadata.
    pub fn new(
        heap: &mut Heap,
        here_info: HereInfo,
        source: Option<String>,
    ) -> IdentifierDefinitionRef {
        let h_here_info =
            FileInfoRef::from_script_file(heap, here_info.script_file, here_info.line_number);
        if let Some(source_name) = source {
            let h_source = heap.string(source_name);
            let h_data_pair = DataPair::new(heap, h_source.into(), Value::None);
            let h_who = heap.cons_ref(h_data_pair.into(), h_here_info.into());
            IdentifierDefinitionRef {
                reference: h_who.into(),
            }
        } else {
            IdentifierDefinitionRef {
                reference: h_here_info.get_ref(),
            }
        }
    }

    /// Constructs an aliased `who` payload by prepending alias metadata for `source_identifier`
    /// onto an existing destination definition.
    ///
    /// Heap shape produced by this constructor:
    /// `cons(datapair(source_name, 0), destination_definition)`
    pub fn from_alias_source(
        heap: &mut Heap,
        source_identifier: IdentifierRecordRef,
        destination_definition: IdentifierDefinitionRef,
    ) -> IdentifierDefinitionRef {
        let alias_source = Self::alias_metadata_from_source_identifier(heap, source_identifier);
        let who = heap.cons_ref(alias_source.into(), destination_definition.get_ref().into());

        IdentifierDefinitionRef::from_ref(who.into())
    }

    /// Constructs alias metadata payload `datapair(source_name, 0)` from an identifier source.
    pub fn alias_metadata_from_source_identifier(
        heap: &mut Heap,
        source_identifier: IdentifierRecordRef,
    ) -> DataPair {
        DataPair::new(heap, source_identifier.name_value(heap), Value::None)
    }

    /// Constructs alias metadata payload `datapair(get_id(name()), 0)` from source text.
    ///
    /// This mirrors Miranda's `get_id(name())` behavior by ensuring the identifier exists
    /// before extracting its canonical name reference for the datapair payload.
    pub fn alias_metadata_from_source_name(heap: &mut Heap, source_name: &str) -> DataPair {
        let source_identifier = heap.make_empty_identifier(source_name);
        Self::alias_metadata_from_source_identifier(heap, source_identifier)
    }

    /// An undefined identifier has `who = NIL`.
    pub fn undefined() -> Self {
        Self::from_ref(Combinator::Nil.into())
    }

    pub fn is_undefined(&self) -> bool {
        self.reference == Combinator::Nil.into() || self.reference == Type::Undefined.into()
    }

    fn here_info_ref(&self, heap: &Heap) -> Option<FileInfoRef> {
        if self.is_undefined() {
            return None;
        }

        let who_cell = heap[self.reference];
        if who_cell.tag == Tag::Cons {
            Some(FileInfoRef::from_ref(who_cell.tail))
        } else {
            Some(FileInfoRef::from_ref(self.reference))
        }
    }

    /// Returns alias metadata when this definition has alias shape
    /// `cons(datapair(source, 0), here_info)`.
    pub fn alias_metadata_pair(&self, heap: &Heap) -> Option<DataPair> {
        if self.is_undefined() {
            return None;
        }

        let who_cell = heap[self.reference];
        (who_cell.tag == Tag::Cons).then(|| DataPair::from_ref(who_cell.head))
    }

    /// Retrieves the `script_file` and `line_number` for this FileDefinition in the form of a `HereInfo`.
    pub fn get_here_info(&self, heap: &Heap) -> Option<HereInfo> {
        let here_info = self.here_info_ref(heap)?;
        let script_file = here_info.script_file(heap);
        let line_number = here_info.line_number(heap);

        Some(HereInfo {
            script_file,
            line_number,
        })
    }

    pub fn get_line_number(&self, heap: &Heap) -> Option<LineNumber> {
        self.get_here_info(heap)
            .map(|here_info| here_info.line_number)
    }

    pub fn get_script_file(&self, heap: &Heap) -> Option<HeapString> {
        self.get_here_info(heap)
            .map(|here_info| here_info.script_file)
    }

    pub fn get_source(&self, heap: &Heap) -> Option<HeapString> {
        self.alias_metadata_pair(heap).map(|alias_metadata| {
            heap.resolve_string(alias_metadata.left_value(heap))
                .expect("Identifier alias source does not resolve to a heap string.")
        })
    }

    pub fn get_data(&self, heap: &Heap) -> IdentifierDefinitionData {
        if self.is_undefined() {
            return IdentifierDefinitionData::Undefined;
        }

        let here_info = self
            .get_here_info(heap)
            .expect("IdentifierDefinitionRef is not undefined but has no here info.");
        if let Some(source) = self.get_source(heap) {
            IdentifierDefinitionData::Alias { here_info, source }
        } else {
            IdentifierDefinitionData::HereInfo(here_info)
        }
    }
}

impl HeapObjectProxy for IdentifierDefinitionRef {
    fn from_ref(reference: RawValue) -> Self {
        IdentifierDefinitionRef { reference }
    }

    fn get_ref(&self) -> RawValue {
        self.reference
    }
}

/**
Miranda:
The value field of type identifier takes one of the following forms

|              | Arity    | Show Functions |            | Type             | Info             |
| :----------- | :------- | :------------- | :--------- | :--------------- | :--------------- |
| `cons(cons(` | `arity,` | `showfn`       | `), cons(` | `algebraic_t,`   | `constructors )` |
| `cons(cons(` | `arity,` | `showfn`       | `), cons(` | `synonym_t,`     | `rhs  ))`        |
| `cons(cons(` | `arity,` | `showfn`       | `), cons(` | `abstract_t,`    | `basis ))`       |
| `cons(cons(` | `arity,` | `showfn`       | `), cons(` | `placeholder_t,` | `NIL ))`         |
| `cons(cons(` | `arity,` | `showfn`       | `), cons(` | `free_t,`        | `NIL ))`         |
| `UNDEF` |


Note that:
```c
#define algebraic_t   0
#define synonym_t     1
#define abstract_t    2
#define placeholder_t 3
#define free_t        4
```
*/

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum IdentifierValueData {
    Undefined,
    Typed {
        arity: isize,
        show_function: Value,
        value_type: IdentifierValueTypeRef, // What is a show function?
    },
    Arbitrary(Value),
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct TypeIdentifierValueParts {
    pub arity: isize,
    pub show_function: Option<Value>,
    pub kind: IdentifierValueTypeKind,
    pub info: Value,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
/// Reference-semantics view of the identifier value field (`id.tail`).
///
/// Heap shapes mapped by this proxy:
/// - typed: `cons(cons(arity, showfn), value_type)`
/// - undefined/immediate values (for example `UNDEF`/`NIL`)
/// - arbitrary runtime value payloads
///
/// Component mapping:
/// - typed `value_type` node -> `IdentifierValueTypeRef`
///
/// Corresponding value-semantics type: `IdentifierValueData`.
pub struct IdentifierValueRef(RawValue);

impl IdentifierValueRef {
    /// Constructs a typed type-identifier value payload from parser-facing parts.
    /// This exists so parser-facing code can build `make_typ`-style values through an intent-first mixed typed surface instead of open-coding nested cons cells and a zero show-function sentinel.
    /// The invariant is that the encoded heap shape remains `cons(cons(arity, showfn_or_zero), cons(kind, info))`.
    pub fn from_type_identifier_parts(
        heap: &mut Heap,
        parts: TypeIdentifierValueParts,
    ) -> IdentifierValueRef {
        let show_function = parts.show_function.unwrap_or(Value::None);
        let head = heap.cons_ref(parts.arity.into(), show_function);
        let tail = heap.cons_ref(Value::Data(parts.kind as RawValue), parts.info);

        IdentifierValueRef::from_ref(heap.cons_ref(head, tail).into())
    }

    /// Constructs a new identifier value on the heap. The shape of this structure depends on `data`.
    pub fn new(heap: &mut Heap, data: IdentifierValueData) -> IdentifierValueRef {
        let reference: RawValue = match data {
            IdentifierValueData::Undefined => Combinator::Nil.into(),
            IdentifierValueData::Typed {
                arity,
                show_function,
                value_type,
            } => {
                let inner = heap.cons_ref(arity.into(), show_function);
                let outer = heap.cons_ref(inner, value_type.get_ref().into());
                outer.into()
            }
            IdentifierValueData::Arbitrary(value) => {
                // This doesn't work if value isn't a reference.
                value.into()
            }
        };

        IdentifierValueRef::from_ref(reference)
    }

    pub fn get_data(&self, heap: &Heap) -> IdentifierValueData {
        if self.0 == Combinator::Nil.into() || self.0 == Combinator::Undef.into() {
            return IdentifierValueData::Undefined;
        }

        let value_cell = heap[self.0];

        if value_cell.tag == Tag::Cons {
            // Assume it's a typed value?
            // `cons(cons(arity, showfn), cons(algebraic_t,  constructors))`
            let arity: isize = heap[value_cell.head].head;
            let show_function_raw = heap[value_cell.head].tail;
            let show_function = if show_function_raw == 0 {
                Value::None
            } else {
                show_function_raw.into()
            };
            let value_type = IdentifierValueTypeRef::from_ref(value_cell.tail);

            IdentifierValueData::Typed {
                arity,
                show_function,
                value_type,
            }
        } else {
            IdentifierValueData::Arbitrary(self.0.into())
        }
    }

    pub fn is_undefined(&self) -> bool {
        self.0 == Combinator::Nil.into() || self.0 == Combinator::Undef.into()
    }

    pub fn typed_kind(&self, heap: &Heap) -> Option<IdentifierValueTypeKind> {
        match self.get_data(heap) {
            IdentifierValueData::Typed { value_type, .. } => {
                Some(value_type.get_identifier_value_type_kind(heap))
            }
            _ => None,
        }
    }

    /// Returns true when this value is a typed, non-synonym type name.
    // Todo: Lift `new_id_type: RawValue` to a semantic decoded-type wrapper.
    //       Blocker: DEF_X stack decode still yields immediate discriminator words.
    //       Migration target: typed DEF_X decode structures in `src/vm/bytecode.rs`.
    pub fn is_non_synonym_typed_name(&self, heap: &Heap, new_id_type: RawValue) -> bool {
        if new_id_type != Type::Type.into() {
            return false;
        }

        match self.typed_kind(heap) {
            Some(kind) => kind != IdentifierValueTypeKind::Synonym,
            None => false,
        }
    }

    /// Writes this value into a private-name cell (`strcons(index, value)`).
    pub fn store_in_private_name(&self, heap: &mut Heap, private_name: PrivateNameRef) {
        private_name.set_value(heap, *self);
    }
}

impl HeapObjectProxy for IdentifierValueRef {
    fn from_ref(reference: RawValue) -> Self {
        IdentifierValueRef(reference)
    }

    fn get_ref(&self) -> RawValue {
        self.0
    }
}

/// The Department of Redundancy Department has been made redundant. The Ministry of Synonyms and Antonyms has taken
/// over naming duties.
// Todo: Introduce semantic wrappers for the remaining raw `constructors` and `basis` payloads.
//       Blocker: the value-semantics decode surface still lags the richer typed query seams.
//       Migration target: post-tranche lift of the remaining raw payloads in `src/data/api/*`.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum IdentifierValueTypeData {
    Algebraic {
        //  "type"       = numerical representation within Miranda
        constructors: Value,
    }, // algebraic_t   = 0
    Synonym {
        source_type: Type,
    }, // synonym_t     = 1
    Abstract {
        basis: Value,
    }, // abstract_t    = 2
    PlaceHolder, // placeholder_t = 3
    Free,        // free_t        = 4
}

impl IdentifierValueTypeData {
    pub fn get_identifier_value_type_kind(&self) -> IdentifierValueTypeKind {
        match self {
            Self::Algebraic { .. } => IdentifierValueTypeKind::Algebraic,
            Self::Synonym { .. } => IdentifierValueTypeKind::Synonym,
            Self::Abstract { .. } => IdentifierValueTypeKind::Abstract,
            Self::PlaceHolder => IdentifierValueTypeKind::PlaceHolder,
            Self::Free => IdentifierValueTypeKind::Free,
        }
    }
}

/// Oh for crying out loud!
#[derive(Copy, Clone, Eq, PartialEq, Debug, Primitive)]
#[repr(isize)]
pub enum IdentifierValueTypeKind {
    //  "type"       = numerical representation within Miranda
    // ------------    ---------------------------------------
    Algebraic = 0,   // algebraic_t   = 0
    Synonym = 1,     // synonym_t     = 1
    Abstract = 2,    // abstract_t    = 2
    PlaceHolder = 3, // placeholder_t = 3
    Free = 4,        // free_t        = 4
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
/// Reference-semantics view of the typed value-type node nested inside `IdentifierValueRef::Typed`.
///
/// Heap shape mapped by this proxy:
/// `cons(kind, info)` where `kind` is a Miranda numeric type discriminator.
///
/// Component mapping:
/// - `kind` -> `IdentifierValueTypeKind` (`get_identifier_value_type_kind`)
/// - `info` is shape-dependent payload accessed through kind-specific query seams
///
/// Corresponding value-semantics type: `IdentifierValueTypeData`.
pub struct IdentifierValueTypeRef {
    reference: RawValue,
}

impl IdentifierValueTypeRef {
    pub fn new(heap: &mut Heap, type_data: IdentifierValueTypeData) -> IdentifierValueTypeRef {
        let reference = // the value of the following match
      match type_data {
        IdentifierValueTypeData::Algebraic { constructors } => {
          // Todo: Lift `constructors: Value` to a semantic wrapper parameter.
          //       Blocker: constructor-list runtime shape is not yet modeled by a dedicated proxy.
          //       Migration target: typed constructor-list proxy in `src/data/api/*`.
          heap.cons_ref(Value::Data(0), constructors)
        }

        IdentifierValueTypeData::Synonym { source_type } => {
          heap.cons_ref(Value::Data(1), source_type.into())
        }

        IdentifierValueTypeData::Abstract { basis } => {
          // Todo: Lift `basis: Value` to a semantic basis wrapper parameter.
          //       Blocker: basis payload semantics are not yet represented as a dedicated type.
          //       Migration target: typed basis proxy/wrapper in `src/data/api/*`.
          heap.cons_ref(Value::Data(2), basis)
        }

        IdentifierValueTypeData::PlaceHolder => {
          heap.cons_ref(Value::Data(3), Combinator::Nil.into())
        }

        IdentifierValueTypeData::Free => {
          heap.cons_ref(Value::Data(4), Combinator::Nil.into())
        }
      };

        IdentifierValueTypeRef {
            reference: reference.into(),
        }
    }

    pub fn is_algebraic(&self, heap: &Heap) -> bool {
        self.get_identifier_value_type_kind(heap) == IdentifierValueTypeKind::Algebraic
    }

    pub fn get_identifier_value_type_kind(&self, heap: &Heap) -> IdentifierValueTypeKind {
        let cons_cell = heap[self.reference];
        let number: RawValue = cons_cell.head;

        IdentifierValueTypeKind::from_isize(number)
            .expect("Identifier value type discriminant is not a valid IdentifierValueTypeKind.")
    }

    /// Returns the constructor-metadata list for an algebraic type value.
    /// This exists so load/typecheck consumers can access algebraic constructor facts through one typed query seam.
    /// The invariant is that only algebraic typed values produce a list here; all other typed kinds return `None`.
    pub fn algebraic_constructor_metadata(
        &self,
        heap: &Heap,
    ) -> Option<ConsList<AlgebraicConstructorMetadataRef>> {
        (self.get_identifier_value_type_kind(heap) == IdentifierValueTypeKind::Algebraic)
            .then(|| ConsList::from_ref(heap[self.reference].tail))
    }

    /// Returns the synonym RHS type-expression payload. This exists so VM-side synonym consumers
    /// read the richer RHS through the shared `TypeExprRef` seam. The invariant is that callers use
    /// this only for synonym typed values, and the returned wrapper references the `tail` payload
    /// of the value-type cell.
    pub fn synonym_rhs_type_expr(&self, heap: &Heap) -> TypeExprRef {
        debug_assert_eq!(
            self.get_identifier_value_type_kind(heap),
            IdentifierValueTypeKind::Synonym
        );
        TypeExprRef::new(heap[self.reference].tail.into())
    }

    /// Returns the committed basis payload for an abstract typed identifier value.
    /// This exists so abstype consumers read basis state through the typed identifier-value seam instead of raw heap-tail access.
    /// The invariant is that only abstract typed values yield `Some(...)`, and that payload is exactly the value-type node tail.
    pub fn abstract_basis(&self, heap: &Heap) -> Option<Value> {
        (self.get_identifier_value_type_kind(heap) == IdentifierValueTypeKind::Abstract)
            .then(|| heap[self.reference].tail.into())
    }
}

impl HeapObjectProxy for IdentifierValueTypeRef {
    fn from_ref(reference: RawValue) -> Self {
        IdentifierValueTypeRef { reference }
    }

    fn get_ref(&self) -> RawValue {
        self.reference
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_identifier_builder_uses_optional_show_function_surface() {
        let mut heap = Heap::new();
        let built = IdentifierValueRef::from_type_identifier_parts(
            &mut heap,
            TypeIdentifierValueParts {
                arity: 2,
                show_function: None,
                kind: IdentifierValueTypeKind::Synonym,
                info: Type::Char.into(),
            },
        );

        let IdentifierValueData::Typed {
            arity,
            show_function,
            value_type,
        } = built.get_data(&heap)
        else {
            panic!("expected typed identifier value")
        };

        assert_eq!(arity, 2);
        assert_eq!(show_function, Value::None);
        assert_eq!(
            value_type.get_identifier_value_type_kind(&heap),
            IdentifierValueTypeKind::Synonym
        );
        assert_eq!(heap[value_type.get_ref()].tail, RawValue::from(Type::Char));
    }

    #[test]
    fn get_data_decodes_zero_show_function_as_none() {
        let mut heap = Heap::new();
        let value_type = IdentifierValueTypeRef::new(
            &mut heap,
            IdentifierValueTypeData::Abstract {
                basis: Combinator::Nil.into(),
            },
        );
        let value = IdentifierValueRef::new(
            &mut heap,
            IdentifierValueData::Typed {
                arity: 0,
                show_function: Value::None,
                value_type,
            },
        );

        let IdentifierValueData::Typed { show_function, .. } = value.get_data(&heap) else {
            panic!("expected typed identifier value")
        };

        assert_eq!(show_function, Value::None);
    }
}
