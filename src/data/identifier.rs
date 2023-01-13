/*!

See [Data Representation.md](Data%20Representation.md) for details about how Identifiers are represented on the heap.

*/


use saucepan::LineNumber;
use num_traits::{FromPrimitive};

use crate::{
  data::{Combinator, Value},
  data::tag::Tag,
  data::heap::{Heap, HeapCell},
  data::values::RawValue
};
use super::{
  Type,
  ValueRepresentationType
};

// #[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Identifier {
  pub name: String,
  // `String` or `&str`?
  pub definition: IdentifierDefinition,
  // Note: `datatype` cannot be type `Type` because, e.g. `char_list_type` is of type Value.
  pub datatype: Value,
  pub value: Option<IdentifierValue>
}

impl Identifier {
  /// Compiles the identifier onto the heap, registering the string and creating a symbol table entry for the name
  /// of the identifier.
  pub fn compile(&self, heap: &mut Heap) -> Value {
    let who = self.definition.compile(heap);
    let string_ref = heap.string(self.name.clone());
    let mut id_head = heap.strcons(string_ref, who);
    id_head = heap.cons(id_head, self.datatype);
    let value = if let Some(value) = &self.value {
      value.compile(heap)
    } else {
      Combinator::Undef.into()
    };

    let id_ref = heap.put_cell(
      HeapCell::new(Tag::Id, id_head, value)
    );


    id_ref
  }

  /// Retrieves the identifier information pointed to by the given reference.
  /// The argument must be a reference.
  pub fn get(reference: Value, heap: &Heap) -> Result<Identifier, ()> {
    // Id: {
    //   hd:  cons(strcons(name,who),type)
    //   tl: value
    // }
    let id_cell: HeapCell = heap.expect(Tag::Id, reference)?;

    let value: Option<IdentifierValue>
        = IdentifierValue::get(id_cell.tail.into(), heap)?;

    let cons_cell    : HeapCell             = heap.expect(Tag::Cons, id_cell.head.into() )?;
    let strcons_cell : HeapCell             = heap.expect(Tag::StrCons, cons_cell.head.into())?;
    let id_definition: IdentifierDefinition = IdentifierDefinition::get(strcons_cell.tail.into(), heap)?;
    let datatype     : Value                = cons_cell.tail.into();
    let name         : String               = heap.resolve_string(strcons_cell.head)?;

    Ok(
      Identifier{
        name,
        definition: id_definition,
        datatype,
        value
      }
    )
  }

}

// #[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum IdentifierDefinition {
  Undefined,          // The identifier is completely undefined.

  HereInfo(HereInfo), // source_file and line_number

  Alias{
    here_info: HereInfo, // source_file and line_number
    source   : HeapString,   // Old name
  }
}

impl IdentifierDefinition {
  /// Compiles to the `who` part of an ID
  pub fn compile(&self, heap: &mut Heap) -> Value {
    match self{

      IdentifierDefinition::Undefined => Combinator::Nil.into(),

      IdentifierDefinition::DefinedAt {
        script_file,
        line
      } => {
        // hereinfo := `fileinfo(script,line_no)`
        let script = heap.string(script_file);
        let here_info = heap.file_info(script, RawValue(line.0 as ValueRepresentationType).into());
        here_info
      }

      IdentifierDefinition::Alias {
        here_info,
        source
      } => {
        // `cons(aka,hereinfo)` for a name that has been aliased, where `aka`
        // is of the form `datapair(oldn,0)`, `oldn` being a string.
        // hereinfo := `fileinfo(script,line_no)`
        let h_source = heap.string(source);
        let h_aka = heap.data_pair(source, RawValue(0).into());
        let h_script = heap.string(script_file);
        let h_here_info = heap.file_info(script, RawValue(here_info.line.0 as ValueRepresentationType).into());

        heap.cons(aka, here_info)
      }
    }
  }


  /**
  Reads the `who` field of an identifier off of the stack.

  The `who` field contains one of:

   * `NIL` (for a name that is totally undefined)
   * `hereinfo` for a name that has been defined or specified, where `hereinfo` is `fileinfo(script,line_no)`
   * `cons(aka,hereinfo)` for a name that has been aliased, where `aka`
       is of the form `datapair(oldn,0)`, `oldn` being a string.

   */
  pub fn get(reference: Value, heap: &Heap) -> Result<IdentifierDefinition, ()> {
    if reference == Value::Combinator(Combinator::Nil){
      // It's not a reference but a Nil combinator, which means the variable is undefined.
      return Ok(IdentifierDefinition::Undefined);
    }

    let who_cell: HeapCell = heap.resolve(reference)?;

    // Check if the alias info is cons'ed on to the here info cell.
    if who_cell.tag == Tag::Cons {
      let aka_cell: HeapCell = heap.expect(Tag::DataPair, who_cell.head.into())?;
      let source  : String   = heap.resolve_string(aka_cell.head)?;

      let (script_file, line): (String, LineNumber) = IdentifierDefinition::get_here_info(who_cell.tail.into(), heap)?;

      Ok(IdentifierDefinition::Alias{
        script_file,
        line,
        source
      })
    } else if who_cell.tag == Tag::FileInfo {
      let (script_file, line): (String, LineNumber) = IdentifierDefinition::get_here_info(reference, heap)?;

      Ok(IdentifierDefinition::DefinedAt {
        script_file,
        line
      })
    } else {
      println!("Failed to fetch who info: {:?}", reference);
      Err(()) // Malformed identifier, shouldn't happen.
    }
  }


  fn get_here_info(reference: Value, heap: &Heap) -> Result<(String, LineNumber), ()> {
    let here_info_cell = heap.expect(Tag::FileInfo, reference)?;
    let line_number: LineNumber = LineNumber::from(here_info_cell.tail.0 as usize);
    let script_file = heap.resolve_string(here_info_cell.head)?;

    Ok((script_file, line_number))
  }

  /*
  fn get_here_info_unchecked<'t>(&'t self, reference: ValueRepresentationType) -> (&'t str, LineNumber){
    let here_info_cell: HeapCell   = heap.data[reference - ATOM_LIMIT];
    let script_file   : &str       = heap.strings[here_info_cell.head.0].as_str();
    let line_number   : LineNumber = here_info_cell.tail.0.into();

    (script_file, line_number)
  }
  */

  /*
  fn get_who_info_unchecked<'t>(reference: ValueRepresentationType, heap: &Heap) -> IdentifierDefinition<'t> {
    if reference == Combinator::Nil as ValueRepresentationType {
      // It's not a reference but a Nil combinator, which means the variable is undefined.
      return IdentifierDefinition::Undefined;
    }

    let who_cell: HeapCell = heap.data[reference - ATOM_LIMIT];

    // Check if the alias info is cons'ed on to the here info cell.
    if who_cell.tag == Tag::Cons {
      let aka_cell = heap.data[who_cell.head.0 - ATOM_LIMIT];
      let (script_file, line): (&str, LineNumber) = heap.get_here_info_unchecked(who_cell.tail.0);
      let source: &str = heap.strings[aka_cell.head.0].as_str();

      IdentifierDefinition::Alias{
        script_file,
        line,
        source
      }
    } else if who_cell.tag == Tag::FileInfo {
      IdentifierDefinition::get_here_info_unchecked(reference, heap);
    } else {
      IdentifierDefinition::Undefined // Malformed identifier, shouldn't happen.
    }
  }
  */
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
pub enum IdentifierValue {
  Undefined,
  Typed {
    arity        : ValueRepresentationType,
    show_function: Value,
    value_type   : IdentifierValueType // What is a show function?
  },
  Arbitrary(Value)
}

impl IdentifierValue {
  pub fn compile(&self, heap: &mut Heap) -> Value {
    // Constructing the value is complicated. From Miranda:
    //
    //   The value field of type identifier takes one of the following forms:
    //   cons(cons(arity,showfn),cons(algebraic_t,constructors))
    //   cons(cons(arity,showfn),cons(synonym_t,rhs))
    //   cons(cons(arity,showfn),cons(abstract_t,basis))
    //   cons(cons(arity,showfn),cons(placeholder_t,NIL))
    //   cons(cons(arity,showfn),cons(free_t,NIL))
    //   UNDEF
    match self {

      IdentifierValue::Undefined => Combinator::Undef.into(),

      IdentifierValue::Typed {
        arity,
        show_function,
        value_type
      } => {
        let value_head = heap.cons(RawValue(*arity).into(), *show_function);
        let value_tail = value_type.compile(heap);
        heap.cons(value_head, value_tail)
      }

      IdentifierValue::Arbitrary(v) => *v

    }
  }


  pub fn get(reference: Value, heap: &Heap) -> Result<Option<IdentifierValue>, ()> {
    if reference == Combinator::Undef.into() {

      Ok(None)

    } else {
      // We assume a reference to a cons cell must be a typed identifier value.
      if let Value::Reference(idx) = reference {
        let outer_cons_cell = heap[RawValue(idx)];
        if outer_cons_cell.tag == Tag::Cons {
          let inner_cons_cell: HeapCell = heap.expect(Tag::Cons, outer_cons_cell.head.into())?;
          let show_function = inner_cons_cell.tail.into();
          let arity: ValueRepresentationType = inner_cons_cell.head.0;
          let value_type = IdentifierValueType::get(outer_cons_cell.tail.into(), heap)?;

          Ok(Some(IdentifierValue::Typed { arity, show_function, value_type }))

        } else {

          Ok(Some(IdentifierValue::Arbitrary(reference))) // Not actually a cons cell.

        }

      } else if reference == Combinator::Undef.into() {

        Ok(Some(IdentifierValue::Undefined))

      } else {

        Ok(Some(IdentifierValue::Arbitrary(reference))) // Not actually a reference.

      }
    }
  }
}



#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum IdentifierValueType{
  Algebraic{                     //  "type"       = numerical representation within Miranda.
    constructors: Value
  },                             // algebraic_t   = 0
  Synonym{ source_type: Type },  // synonym_t     = 1
  Abstract{ basis: Value },      // abstract_t    = 2
  PlaceHolder,                   // placeholder_t = 3
  Free                           // free_t        = 4
}

impl IdentifierValueType {
  pub fn compile(&self, heap: &mut Heap) -> Value {
    // Has the form CONS(class, info).

    match self {
      IdentifierValueType::Algebraic { constructors } => {
        // Todo: Figure out the type of `constructors` and fix this.
        heap.cons(Value::Data(0), *constructors)
      }

      IdentifierValueType::Synonym { source_type } => {
        heap.cons(Value::Data(1), (*source_type).into())
      }

      IdentifierValueType::Abstract { basis } => {
        // Todo: Figure out the type of `basis` and fix this.
        heap.cons(Value::Data(2), *basis)
      }

      IdentifierValueType::PlaceHolder => {
        heap.cons(Value::Data(3), Combinator::Nil.into())
      }

      IdentifierValueType::Free => {
        heap.cons(Value::Data(4), Combinator::Nil.into())
      }
    } // end match on self
  }

  fn get(reference: Value, heap: &Heap) -> Result<IdentifierValueType, ()> {

    let type_cell : HeapCell = heap.expect(Tag::Cons, reference)?;
    let class_ = match type_cell.head.0 {

      0 => {
        IdentifierValueType::Algebraic{
          constructors: type_cell.tail.into()
        }
      }

      1 => {
        IdentifierValueType::Synonym {
          source_type: Type::from_isize(type_cell.tail.0).ok_or(())?
        }
      }

      2 => {
        IdentifierValueType::Abstract {
          basis: type_cell.tail.into()
        }
      }

      3 => {
        IdentifierValueType::PlaceHolder
      }

      4 => {
        IdentifierValueType::Free
      }

      _ => {
        eprintln!("malformed identifier type value");
        unreachable!();
      }

    };

    Ok(class_)
  }


}

