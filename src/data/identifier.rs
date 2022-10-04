/*!



 */


use saucepan::LineNumber;
use num_traits::{FromPrimitive, ToPrimitive};

use crate::data::heap::Heap;
use crate::data::tag::Tag;
use crate::data::{ATOM_LIMIT, Combinator, Value};
use crate::data::values::{HeapCell, RawValue};
use super::{
  Type,
  ValueRepresentationType
};

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Identifier<'t>{
  pub name         : &'t str, // `String` or `&str`?
  pub definition   : IdentifierDefinition<'t>,
  pub datatype     : Type,
  pub arity        : ValueRepresentationType,
  pub show_function: &'t str, // What is a show function?
  pub value        : IdentifierValueType
}

impl<'t> Identifier<'t> {
  pub fn compile(&self, heap: &mut Heap) -> Value {
    // Id: {
    //   hd:  cons(strcons(name,who),type)
    //   tl: value
    // }
    let who = self.definition.compile(heap);
    let name = heap.string(self.name);
    let mut id_head = heap.strcons(name, who);
    id_head = heap.cons(id_head, self.datatype.into());

    // Constructing the value is complicated. From Miranda:
    //
    //   The value field of type identifier takes one of the following forms:
    //   cons(cons(arity,showfn),cons(algebraic_t,constructors))
    //   cons(cons(arity,showfn),cons(synonym_t,rhs))
    //   cons(cons(arity,showfn),cons(abstract_t,basis))
    //   cons(cons(arity,showfn),cons(placeholder_t,NIL))
    //   cons(cons(arity,showfn),cons(free_t,NIL))
    let showfn = heap.string(self.show_function);
    let value_head = heap.cons(RawValue(self.arity).into(), showfn);
    let value_tail = self.value.compile(heap);
    let value = heap.cons(value_head, value_tail);

    heap.put_cell(
      HeapCell::new(Tag::Id, id_head, value)
    )
  }

  /// Retrieves the identifier information pointed to by the given reference.
  /// The argument must be a reference.
  pub fn get(reference: Value, heap: &Heap) -> Result<Identifier, ()> {
    // Id: {
    //   hd:  cons(strcons(name,who),type)
    //   tl: value
    // }
    let id_cell: HeapCell = heap.expect(Tag::Id, reference)?;

    let (arity, show_function, value_type): (ValueRepresentationType, &str, IdentifierValueType)
        = IdentifierValueType::get(id_cell.tail.into(), heap)?;

    let cons_cell    : HeapCell             = heap.expect(Tag::Cons, id_cell.head.into() )?;
    let strcons_cell : HeapCell             = heap.expect(Tag::StrCons, cons_cell.head.into())?;
    let id_definition: IdentifierDefinition = IdentifierDefinition::get(strcons_cell.tail.into(), heap)?;
    let datatype     : Type                 = Type::from_usize(cons_cell.tail.0).unwrap();
    let name         : &str                 = heap.resolve_string(strcons_cell.head)?;

    Ok(
      Identifier{
        name,
        definition: id_definition,
        datatype,
        arity,
        show_function,
        value: value_type
      }
    )
  }

}


#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum IdentifierDefinition<'t> {
  Undefined, // The identifier is completely undefined.

  DefinedAt{
    script_file: &'t str,   // `String` or `&str`?
    line       : LineNumber // Span?
  },

  Alias{
    script_file: &'t str,    // `String` or `&str`?
    line       : LineNumber, // Span?
    source     : &'t str,    // Old name
  }
}

impl<'t> IdentifierDefinition<'t> {
  /// Compiles to the `who` part of an ID
  pub fn compile(&self, heap: &mut Heap) -> Value {
    match self{

      IdentifierDefinition::Undefined => heap.NILL,

      IdentifierDefinition::DefinedAt {
        script_file,
        line
      } => {
        // hereinfo := `fileinfo(script,line_no)`
        let script = heap.string(script_file);
        let hereinfo = heap.file_info(script, RawValue(line.0 as usize).into());
        hereinfo
      }

      IdentifierDefinition::Alias {
        script_file,
        line,
        source
      } => {
        // `cons(aka,hereinfo)` for a name that has been aliased, where `aka`
        // is of the form `datapair(oldn,0)`, `oldn` being a string.
        // hereinfo := `fileinfo(script,line_no)`
        let source = heap.string(source);
        let aka = heap.data_pair(source, RawValue(0).into());
        let script = heap.string(script_file);
        let hereinfo = heap.file_info(script, RawValue(line.0 as usize).into());

        heap.cons(aka, hereinfo)
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
      let source  : &str     = heap.resolve_string(aka_cell.head)?;

      let (script_file, line): (&str, LineNumber) = IdentifierDefinition::get_here_info(who_cell.tail.into(), heap)?;

      Ok(IdentifierDefinition::Alias{
        script_file,
        line,
        source
      })
    } else if who_cell.tag == Tag::FileInfo {
      let (script_file, line): (&str, LineNumber) = IdentifierDefinition::get_here_info(reference, heap)?;

      Ok(IdentifierDefinition::DefinedAt {
        script_file,
        line
      })
    } else {
      println!("Failed to fetch who info: {:?}", reference);
      Err(()) // Malformed identifier, shouldn't happen.
    }
  }


  fn get_here_info(reference: Value, heap: &Heap) -> Result<(&str, LineNumber), ()> {
    let here_info_cell = heap.expect(Tag::FileInfo, reference)?;
    let line_number: LineNumber = here_info_cell.tail.0.into();
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
From Miranda:

  The value field of type identifier takes one of the following forms:
  cons(cons(arity,showfn),cons(algebraic_t,constructors))
  cons(cons(arity,showfn),cons(synonym_t,rhs))
  cons(cons(arity,showfn),cons(abstract_t,basis))
  cons(cons(arity,showfn),cons(placeholder_t,NIL))
  cons(cons(arity,showfn),cons(free_t,NIL))

  Note that:
    #define algebraic_t 0
    #define synonym_t 1
    #define abstract_t 2
    #define placeholder_t 3
    #define free_t 4
 */
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Primitive)]
#[repr(usize)]
pub enum IdentifierValueType{
  // todo: incorporate constructors, source, basis.
  Algebraic   = 0, //{ constructors: Vec<&'t str> },
  Synonym     = 1, //{ source:       &'t str      },
  Abstract    = 2, //{ basis:        &'t str      },
  PlaceHolder = 3,
  Free        = 4
}

impl IdentifierValueType {
  pub fn compile(&self, heap: &mut Heap) -> Value {
    // todo: This is incorrect. It ignores constructors, source, basis, etc.
    heap.cons(Value::Data(*self as ValueRepresentationType), heap.NILL)
  }

  /// This is not the inverse of compile! Returns `(arity, show_function, value_type)`.
  pub fn get(reference: Value, heap: &Heap) -> Result<(ValueRepresentationType, &str, IdentifierValueType), ()> {
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

     */
    let outer_cons_cell: HeapCell = heap.expect(Tag::Cons, reference)?;
    let inner_cons_cell: HeapCell = heap.expect(Tag::Cons, outer_cons_cell.head.into())?;
    let show_function  : &str     = heap.resolve_string(inner_cons_cell.tail)?;
    let arity          : ValueRepresentationType = inner_cons_cell.head.0;

    let type_cell : HeapCell            = heap.expect(Tag::Cons, outer_cons_cell.tail.into())?;
    let value_type: IdentifierValueType = IdentifierValueType::from_usize(type_cell.head.0).unwrap();
    // let info = type_cell.tail;

    Ok((arity, show_function, value_type))

  }

  /*
  fn get_identifier_value_unchecked<'t>(reference: ValueRepresentationType, heap: &Heap)
                                        -> (ValueRepresentationType, &'t str, IdentifierValueType)
  {
    let outer_cons_cell: HeapCell                = heap.data[reference - ATOM_LIMIT];
    let inner_cons_cell: HeapCell                = heap.data[outer_cons_cell.head.0 - ATOM_LIMIT];
    let arity          : ValueRepresentationType = inner_cons_cell.head.0;
    let type_cell      : HeapCell                = heap.data[outer_cons_cell.tail.0 - ATOM_LIMIT];
    let value_type     : IdentifierValueType     = IdentifierValueType::from_usize(type_cell.head.0).unwrap();
    let show_function  : &'t str                 = heap.strings[inner_cons_cell.tail.0].as_str();

    (arity, show_function, value_type)
  }
  */
}


#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}
