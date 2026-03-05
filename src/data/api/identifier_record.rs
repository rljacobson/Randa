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

use super::{HeapObjectProxy, HeapString, LineNumber};
use crate::compiler::HereInfo;
use crate::data::{Combinator, Heap, HeapCell, RawValue, Tag, Type, Value};
use enum_primitive_derive::Primitive;
use num_traits::FromPrimitive;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct IdentifierRecordData {
    pub name: HeapString, // `String` or `&str`?
    /// The `who` field
    pub definition: IdentifierDefinitionValue,
    // Note: `datatype` cannot be type `Type` because, e.g. `char_list_type` is of type Value.
    pub datatype: Value,
    pub value: Option<IdentifierValueReference>,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct IdentifierRecord {
    reference: RawValue,
}

impl IdentifierRecord {
    pub(crate) const UNINITIALIZED: IdentifierRecord = IdentifierRecord {
        reference: Combinator::Nil as RawValue,
    };

    pub fn new(
        heap: &mut Heap,
        name: String,
        definition: IdentifierDefinitionValue,
        data_type: Value,
        value: Option<IdentifierValueReference>,
    ) -> IdentifierRecord {
        let h_name = heap.string(name);

        let mut h_id_head = heap.strcons_ref(h_name.into(), definition.get_ref().into());
        h_id_head = heap.cons_ref(h_id_head, data_type);
        let h_id_tail: Value = if let Some(h_value) = value {
            h_value.get_ref().into()
        } else {
            Combinator::Undef.into()
        };

        let reference: RawValue = heap.put_ref(Tag::Id, h_id_head, h_id_tail).into();

        IdentifierRecord { reference }
    }

    /// Returns the name of the identifier
    pub fn get_name(&self, heap: &Heap) -> Result<String, ()> {
        let cons_ref = heap[self.reference].head;
        let str_cons_ref = heap[cons_ref].head;
        let name_ref = heap[str_cons_ref].head;
        heap.resolve_string(name_ref.into())
    }

    /// Fetches the `IdentifierDefinitionValue` (a HeapProxyObject) from the heap resident data structure.
    pub fn get_definition(&self, heap: &Heap) -> Result<IdentifierDefinitionValue, ()> {
        let id_cell: HeapCell = heap.expect(Tag::Id, self.reference.into())?;
        let cons_cell: HeapCell = heap.expect(Tag::Cons, id_cell.head.into())?;
        let strcons_cell: HeapCell = heap.expect(Tag::StrCons, cons_cell.head.into())?;

        Ok(IdentifierDefinitionValue::from_ref(
            strcons_cell.tail.into(),
        ))
    }

    /// Fetches the identifier's data type from the heap resident structure. Note: This is not the data type in the
    /// value part of the identifier heap structure.
    pub fn get_datatype(&self, heap: &Heap) -> Result<Value, ()> {
        let id_cell: HeapCell = heap.expect(Tag::Id, self.reference.into())?;
        let cons_cell: HeapCell = heap.expect(Tag::Cons, id_cell.head.into())?;

        Ok(cons_cell.tail.into())
    }

    pub fn get_value(&self, heap: &Heap) -> Result<Option<IdentifierValueReference>, ()> {
        let id_cell: HeapCell = heap.expect(Tag::Id, self.reference.into())?;

        let value: Option<IdentifierValueReference> = if id_cell.tail == Combinator::Nil.into() {
            None
        } else {
            Some(IdentifierValueReference::from_ref(id_cell.tail.into()))
        };

        Ok(value)
    }

    /// Returns the raw value field of the identifier as a typed `Value`.
    pub fn get_value_field(&self, heap: &Heap) -> Value {
        heap[self.reference].tail.into()
    }

    pub fn set_value_from_data(&self, heap: &mut Heap, value: IdentifierHeapValueData) {
        let id_value = IdentifierValueReference::new(heap, value);
        heap[self.reference].tail = id_value.get_ref();
    }

    pub fn set_value(&self, heap: &mut Heap, value: IdentifierValueReference) {
        heap[self.reference].tail = value.get_ref();
    }

    /// Sets the identifier's "who" field.
    pub fn set_definition(&self, heap: &mut Heap, definition: IdentifierDefinitionValue) {
        // ID HEAD: cons(strcons(name,who),type)
        // ID TAIL: cons(cons(arity, showfn), cons(type, NIL))
        let id_head = heap[self.reference].head;
        let strcons_ref = heap[id_head].head;
        heap[strcons_ref].tail = definition.get_ref();
    }

    /// Sets the identifier's type (not the value type)
    pub fn get_type(&self, heap: &Heap) -> Value {
        let id_head = heap[self.reference].head;
        heap[id_head].tail.into()
    }

    /// Sets the identifier's type (not the value type). Note that `new_type` is of type `Value`, because user defined
    /// types do not have type `Type`.
    pub fn set_type(&self, heap: &mut Heap, new_type: Value) {
        let id_head = heap[self.reference].head;
        heap[id_head].tail = new_type.into()
    }

    /// Fetches all of the data associated with the `IdentifierRecord`.
    pub fn get_data(&self, heap: &Heap) -> Result<IdentifierRecordData, ()> {
        let id_cell: HeapCell = heap.expect(Tag::Id, self.reference.into())?;

        let value: Option<IdentifierValueReference> = if id_cell.tail == Combinator::Nil.into() {
            None
        } else {
            Some(IdentifierValueReference::from_ref(id_cell.tail.into()))
        };

        let cons_cell: HeapCell = heap.expect(Tag::Cons, id_cell.head.into())?;
        let strcons_cell: HeapCell = heap.expect(Tag::StrCons, cons_cell.head.into())?;
        let id_definition: IdentifierDefinitionValue =
            IdentifierDefinitionValue::from_ref(strcons_cell.tail.into());
        let datatype: Value = cons_cell.tail.into();
        let name: HeapString = heap.resolve_string(strcons_cell.head.into())?;
        // let name         : HeapString           = HeapString::from_ref(strcons_cell.head);

        Ok(IdentifierRecordData {
            name,
            definition: id_definition,
            datatype,
            value,
        })
    }

    /// An IdentifierRecord is "unset" if its `value` is `UNDEF`, its `who` is `NIL`, and its `type` is `undef_t`
    pub fn unset_id(&self, heap: &mut Heap) {
        assert_eq!(heap[self.reference].tag, Tag::Id.into());

        self.set_value(
            heap,
            IdentifierValueReference::from_ref(Combinator::Undef.into()),
        );
        self.set_definition(heap, IdentifierDefinitionValue::undefined());
        self.set_type(heap, Type::Undefined.into());
    }
}

impl HeapObjectProxy for IdentifierRecord {
    fn from_ref(reference: RawValue) -> Self {
        IdentifierRecord { reference }
    }

    fn get_ref(&self) -> RawValue {
        self.reference
    }
}

/// An `IdentifierDefinitionData` is the `who` field in Miranda. The `who` field contains one of:
///
///    * `NIL` (for a name that is totally undefined)
///    * `hereinfo` for a name that has been defined or specified, where `hereinfo` is `fileinfo(script,line_no)`
///    * `cons(aka,hereinfo)` for a name that has been aliased, where `aka`
///      is of the form `datapair(oldn,0)`, `oldn` being a string.
pub enum IdentifierDefinitionData {
    Undefined, // The identifier is completely undefined.

    HereInfo(HereInfo), // source_file and line_number

    Alias {
        here_info: HereInfo, // source_file and line_number
        source: HeapString,  // Old name
    },
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct IdentifierDefinitionValue {
    reference: RawValue,
}

impl IdentifierDefinitionValue {
    // Todo: Is `script_file` a string? A `FileRecord` (or equivalent)? A reference? An arbitrary value?
    // Todo: Same question, but for `source`.
    /// Creates a new identifier definition on the heap and constructs an `IdentifierDefinitionValue` referencing it.
    pub fn new(
        heap: &mut Heap,
        here_info: HereInfo,
        source: Option<String>,
    ) -> IdentifierDefinitionValue {
        let h_script_file = heap.string(here_info.script_file);
        let h_here_info = heap.file_info_ref(h_script_file.into(), here_info.line_number.into());
        if let Some(source_name) = source {
            let h_source = heap.string(source_name);
            let h_data_pair = heap.data_pair_ref(h_source.into(), 0.into());
            let h_who = heap.cons_ref(h_data_pair, h_here_info);
            IdentifierDefinitionValue {
                reference: h_who.into(),
            }
        } else {
            IdentifierDefinitionValue {
                reference: h_here_info.into(),
            }
        }
    }

    /// An undefined identifier has `who = NIL`.
    pub fn undefined() -> Self {
        Self::from_ref(Combinator::Nil.into())
    }

    /// Retrieves the `script_file` and `line_number` for this FileDefinition in the form of a `HereInfo`.
    pub fn get_here_info(&self, heap: &Heap) -> Result<HereInfo, ()> {
        let mut here_info_cell: HeapCell = heap.resolve(self.reference.into())?;

        // Check if the alias info is cons'ed on to the here info cell.
        if here_info_cell.tag == Tag::Cons {
            here_info_cell = heap.expect(Tag::FileInfo, here_info_cell.tail.into())?;
        }

        let script_file: HeapString = heap.resolve_string(here_info_cell.head.into())?;
        let line_number: LineNumber = here_info_cell.tail.into();

        Ok(HereInfo {
            script_file,
            line_number,
        })
    }

    pub fn get_line_number(&self, heap: &Heap) -> Result<LineNumber, ()> {
        self.get_here_info(heap)
            .map(|here_info| here_info.line_number)
    }

    pub fn get_script_file(&self, heap: &Heap) -> Result<HeapString, ()> {
        self.get_here_info(heap)
            .map(|here_info| here_info.script_file)
    }

    pub fn get_source(&self, heap: &Heap) -> Result<Option<HeapString>, ()> {
        let who_cell: HeapCell = heap.resolve(self.reference.into())?;

        // Check if the alias info is cons'ed on to the here info cell.
        if who_cell.tag == Tag::Cons {
            let aka_cell: HeapCell = heap.expect(Tag::DataPair, who_cell.head.into())?;

            Ok(Some(heap.resolve_string(aka_cell.head.into())?))
        } else {
            Ok(None)
        }
    }

    pub fn get_data(&self, heap: &Heap) -> Result<IdentifierDefinitionData, ()> {
        // Fail quickly
        if self.reference == Combinator::Nil.into() || self.reference == Type::Undefined.into() {
            return Ok(IdentifierDefinitionData::Undefined);
        }

        let mut here_info_cell: HeapCell = heap.resolve(self.reference.into())?;
        let mut source: String = String::new();

        // Check if the alias info is cons'ed on to the here info cell.
        let is_alias: bool = here_info_cell.tag == Tag::Cons;

        if is_alias {
            here_info_cell = heap.expect(Tag::FileInfo, here_info_cell.tail.into())?;
            let aka_cell: HeapCell = heap.expect(Tag::DataPair, here_info_cell.head.into())?;
            // if aka_cell.tail != 0 { return Err(()); }
            source = heap.resolve_string(aka_cell.head.into())?;
        }

        let script_file: HeapString = heap.resolve_string(here_info_cell.head.into())?;
        let line_number: LineNumber = here_info_cell.tail.into();
        let here_info = HereInfo {
            line_number,
            script_file,
        };

        if is_alias {
            Ok(IdentifierDefinitionData::Alias { here_info, source })
        } else {
            Ok(IdentifierDefinitionData::HereInfo(here_info))
        }
    }
}

impl HeapObjectProxy for IdentifierDefinitionValue {
    fn from_ref(reference: RawValue) -> Self {
        IdentifierDefinitionValue { reference }
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
/// `IdentifierValueData` has been named by the Department of Redundancy Department.

// #[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum IdentifierHeapValueData {
    Undefined,
    Typed {
        arity: isize,
        show_function: Value,
        value_type: IdentifierHeapValueType, // What is a show function?
    },
    Arbitrary(Value),
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct IdentifierValueReference(RawValue);

impl IdentifierValueReference {
    pub fn new(heap: &mut Heap, data: IdentifierHeapValueData) -> IdentifierValueReference {
        let reference: RawValue = match data {
            IdentifierHeapValueData::Undefined => Combinator::Nil.into(),
            IdentifierHeapValueData::Typed {
                arity,
                show_function,
                value_type,
            } => {
                let inner = heap.cons_ref(arity.into(), show_function);
                let outer = heap.cons_ref(inner, value_type.get_ref().into());
                outer.into()
            }
            IdentifierHeapValueData::Arbitrary(value) => {
                // This doesn't work if value isn't a reference.
                value.into()
            }
        };

        IdentifierValueReference::from_ref(reference)
    }

    pub fn get_data(&self, heap: &Heap) -> IdentifierHeapValueData {
        if self.0 == Combinator::Nil.into() || self.0 == Combinator::Undef.into() {
            return IdentifierHeapValueData::Undefined;
        }

        let value_cell = heap[self.0];

        if value_cell.tag == Tag::Cons {
            // Assume it's a typed value?
            // `cons(cons(arity, showfn), cons(algebraic_t,  constructors))`
            let arity: isize = heap[value_cell.head].head;
            let show_function = heap[value_cell.head].tail.into();
            let value_type = IdentifierHeapValueType::from_ref(value_cell.tail);

            IdentifierHeapValueData::Typed {
                arity,
                show_function,
                value_type,
            }
        } else {
            IdentifierHeapValueData::Arbitrary(self.0.into())
        }
    }
}

impl HeapObjectProxy for IdentifierValueReference {
    fn from_ref(reference: RawValue) -> Self {
        IdentifierValueReference(reference)
    }

    fn get_ref(&self) -> RawValue {
        self.0
    }
}

/// The Department of Redundancy Department has been made redundant. The Ministry of Synonyms and Antonyms has taken
/// over naming duties.
// Todo: Should there be a `HeapObjectProxy` for `constructors`? `source_type`? `basis`?
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
    pub fn get_numeric_type_specifier(&self) -> IdentifierValueTypeDataSpecifier {
        match self {
            Self::Algebraic { .. } => IdentifierValueTypeDataSpecifier::Algebraic,
            Self::Synonym { .. } => IdentifierValueTypeDataSpecifier::Synonym,
            Self::Abstract { .. } => IdentifierValueTypeDataSpecifier::Abstract,
            Self::PlaceHolder => IdentifierValueTypeDataSpecifier::PlaceHolder,
            Self::Free => IdentifierValueTypeDataSpecifier::Free,
        }
    }
}

/// Oh for crying out loud!
#[derive(Copy, Clone, Eq, PartialEq, Debug, Primitive)]
#[repr(isize)]
pub enum IdentifierValueTypeDataSpecifier {
    //  "type"       = numerical representation within Miranda
    // ------------    ---------------------------------------
    Algebraic = 0,   // algebraic_t   = 0
    Synonym = 1,     // synonym_t     = 1
    Abstract = 2,    // abstract_t    = 2
    PlaceHolder = 3, // placeholder_t = 3
    Free = 4,        // free_t        = 4
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct IdentifierHeapValueType {
    reference: RawValue,
}

impl IdentifierHeapValueType {
    pub fn new(heap: &mut Heap, type_data: IdentifierValueTypeData) -> IdentifierHeapValueType {
        let reference = // the value of the following match
      match type_data {
        IdentifierValueTypeData::Algebraic { constructors } => {
          // Todo: Figure out the type of `constructors` and fix this.
          heap.cons_ref(Value::Data(0), constructors)
        }

        IdentifierValueTypeData::Synonym { source_type } => {
          heap.cons_ref(Value::Data(1), source_type.into())
        }

        IdentifierValueTypeData::Abstract { basis } => {
          // Todo: Figure out the type of `basis` and fix this.
          heap.cons_ref(Value::Data(2), basis)
        }

        IdentifierValueTypeData::PlaceHolder => {
          heap.cons_ref(Value::Data(3), Combinator::Nil.into())
        }

        IdentifierValueTypeData::Free => {
          heap.cons_ref(Value::Data(4), Combinator::Nil.into())
        }
      };

        IdentifierHeapValueType {
            reference: reference.into(),
        }
    }

    pub fn is_algebraic(&self, heap: &Heap) -> bool {
        self.get_numeric_type_specifier(heap) == Ok(IdentifierValueTypeDataSpecifier::Algebraic)
    }

    pub fn get_numeric_type_specifier(
        &self,
        heap: &Heap,
    ) -> Result<IdentifierValueTypeDataSpecifier, ()> {
        let cons_cell = heap.expect(Tag::Cons, self.reference.into())?;
        let number: RawValue = cons_cell.head;

        IdentifierValueTypeDataSpecifier::from_isize(number).ok_or(())
    }
}

impl HeapObjectProxy for IdentifierHeapValueType {
    fn from_ref(reference: RawValue) -> Self {
        IdentifierHeapValueType { reference }
    }

    fn get_ref(&self) -> RawValue {
        self.reference
    }
}
