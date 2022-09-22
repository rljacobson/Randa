/*!

See the `values` module for more information about how data is encoded in a `HeapCell`.

 */

use num_traits::{FromPrimitive, ToPrimitive};
use saucepan::LineNumber;

use crate::data::{ATOM_LIMIT, Combinator, Identifier, IdentifierDefinition, IdentifierValueType, ValueRepresentationType};
use crate::data::tag::Tag;
use crate::data::types::Type;
use crate::data::values::{HeapCell, RawValue, Value};


type ValueOption = Result<Value, ()>;


#[derive(Clone, Default)]
pub struct Heap {
  data: Vec<HeapCell>,
  strings: Vec<String>
}

impl Heap {

  pub fn new() -> Self {
    Self::default()
  }


  // region Generic read/write functions

  /// Resolves a reference to the `HeapCell` it points to.
  pub fn resolve(&self, value: Value) -> Result<HeapCell, ()> {
    match value {

      Value::Reference(r) => {
        Ok(self.data[r])
      }

      _ => {
        println!("Failed to resolve: {:?}", value);
        Err(())
      }

    }
  }

  /// Resolves a reference to the `HeapCell` it points to without checking if the value is a reference.
  /// The only use for this version of `resolve` is to avoid unwrapping an `Option` in cases where you
  /// know the value type.
  pub fn resolve_unchecked(&self, value: Value) -> HeapCell {
    match value {

      Value::Reference(r) => {
        self.data[r]
      }

      _ => panic!("Attempted to dereference a non-reference value.")

    }
  }

  /// Pushes the given cell into the heap and returns a `Value::Reference` wrapping the
  /// index of the `HeapCell` in the heap.
  pub fn put_cell(&mut self, cell: HeapCell) -> Value {
    let idx = self.data.len();
    self.data.push(cell);
    Value::Reference(idx as ValueRepresentationType)
  }

  /// Same as `put_cell`, but creates the `HeapCell` as well.
  pub fn put(&mut self, tag: Tag, head: Value, tail: Value) -> Value {
    self.put_cell(HeapCell::new(tag, head, tail))
  }

  /// Dereference the given reference, and if the result has `tag`, returns the cell. If `reference` isn't a
  /// reference, or if the tag of the referent is not `tag`, returns `Err(())`.
  pub fn expect(&self, tag: Tag, reference: Value) -> Result<HeapCell, ()> {
    if let Value::Reference(idx) = reference {
      let found_cell = self.data[idx];
      if found_cell.tag == tag {
        return Ok(found_cell);
      } else {
        println!("Expected: {:?}\nFound: {:?}", tag, found_cell.tag);
      }
    }
    // Else:
    Err(())
  }

  /// If maybe_reference is a reference, dereference it, and check that its tag is `tag`. If so, return the head.
  /// Otherwise, return None.
  ///
  /// Because `expect_head` takes an `Option<Value>`, it is composable with itself.
  pub fn expect_head(&self, tag: Tag, reference: Value) -> ValueOption{
    if let Value::Reference(idx) = reference {
      let cell = self.data[idx];
      if cell.tag == tag {
        return Ok(cell.head.into());
      }
    }
    // Else:
    println!("Expected: {:?}\nFound: {:?}", tag, reference);
    Err(())
  }


  /// If maybe_reference is a reference, dereference it, and check that its tag is `tag`. If so, return the tail.
  /// Otherwise, return None.
  ///
  /// Because `expect_tail` takes an `Option<Value>`, it is composable with itself.
  pub fn expect_tail(&self, tag: Tag, reference: Value) -> ValueOption{
    if let Value::Reference(idx) = reference {
      let cell = self.data[idx];
      if cell.tag == tag {
        return Ok(cell.tail.into());
      }
    }
    // Else:
    println!("Expected: {:?}\nFound: {:?}", tag, reference);
    Err(())
  }

  /// Resolve the string cell to the string value.
  pub fn resolve_string(&self, value: RawValue) -> Result<&str, ()> {
    return Ok(self.strings[value.0].as_str());
  }

  // endregion


  // region Identifier name resolution functions

  /// Retrieves the identifier information pointed to by the given reference.
  /// The argument must be a reference.
  pub fn get_identifier(&self, reference: Value) -> Result<Identifier, ()> {
    // Id: {
    //   hd:  cons(strcons(name,who),type)
    //   tl: value
    // }
    let id_cell: HeapCell = self.expect(Tag::Id, reference)?;

    let (arity, show_function, value_type): (ValueRepresentationType, &str, IdentifierValueType)
        = self.get_identifier_value(id_cell.tail.into())?;

    let cons_cell    : HeapCell             = self.expect(Tag::Cons, id_cell.head.into() )?;
    let strcons_cell : HeapCell             = self.expect(Tag::StrCons, cons_cell.head.into())?;
    let id_definition: IdentifierDefinition = self.get_who_info(strcons_cell.tail.into())?;
    let datatype     : Type                 = Type::from_usize(cons_cell.tail.0).unwrap();
    let name         : &str                 = self.resolve_string(strcons_cell.head)?;

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

  pub fn get_identifier_unchecked(&self, reference: ValueRepresentationType) -> Identifier {
    // Id: {
    //   hd:  cons(strcons(name,who),type)
    //   tl: value
    // }
    let id_cell = self.data[reference];

    let (arity, show_function, value_type): (ValueRepresentationType, &str, IdentifierValueType)
        = self.get_identifier_value_unchecked(id_cell.tail.0);

    let cons_cell    = self.data[id_cell.head.0];
    let strcons_cell = self.data[cons_cell.head.0];

    let datatype     : Type                 = Type::from_usize(cons_cell.tail.0).unwrap();
    let name         : &str                 = self.strings[strcons_cell.head.0].as_str();
    let id_definition: IdentifierDefinition = self.get_who_info_unchecked(strcons_cell.tail.0);


    Identifier{
      name,
      definition: id_definition,
      datatype,
      arity,
      show_function,
      value: value_type
    }
  }


  pub fn put_identifier(&self, identifier: Identifier) -> Value {
    // Id: {
    //   hd:  cons(strcons(name,who),type)
    //   tl: value
    // }
    let id_head;   // cons(strcons(name,who),type)
    let here_info; // fileinfo(script,line_no)
    let who;       // cons(aka,hereinfo)
                   // hereinfo
                   // NIL
    let aka;       // datapair(oldn,0)
  }


  /**
  Reads the `who` field of an identifier off of the stack.

  The `who` field contains one of:

  * `NIL` (for a name that is totally undefined)
  * `hereinfo` for a name that has been defined or specified, where `hereinfo` is `fileinfo(script,line_no)`
  * `cons(aka,hereinfo)` for a name that has been aliased, where `aka`
       is of the form `datapair(oldn,0)`, `oldn` being a string.

   */
  fn get_who_info(&self, reference: Value) -> Result<IdentifierDefinition, ()> {
    if reference == Value::Combinator(Combinator::Nil){
      // It's not a reference but a Nil combinator, which means the variable is undefined.
      return Ok(IdentifierDefinition::Undefined);
    }

    let who_cell: HeapCell = self.resolve(reference)?;

    // Check if the alias info is cons'ed on to the here info cell.
    if who_cell.tag == Tag::Cons {
      let aka_cell: HeapCell = self.expect(Tag::DataPair, who_cell.head.into())?;
      let source  : &str     = self.resolve_string(aka_cell.head)?;

      let (script_file, line): (&str, LineNumber) = self.get_here_info(who_cell.tail.into())?;

      Ok(IdentifierDefinition::Alias{
        script_file,
        line,
        source
      })
    } else if who_cell.tag == Tag::FileInfo {
      let (script_file, line): (&str, LineNumber) = self.get_here_info(reference)?;

      Ok(IdentifierDefinition::DefinedAt {
        script_file,
        line
      })
    } else {
      println!("Failed to fetch who info: {:?}", reference);
      Err(()) // Malformed identifier, shouldn't happen.
    }
  }

  fn get_who_info_unchecked<'t>(&'t self, reference: ValueRepresentationType) -> IdentifierDefinition<'t> {
    if reference == Combinator::Nil as ValueRepresentationType {
      // It's not a reference but a Nil combinator, which means the variable is undefined.
      return IdentifierDefinition::Undefined;
    }

    let who_cell: HeapCell = self.data[reference - ATOM_LIMIT];

    // Check if the alias info is cons'ed on to the here info cell.
    if who_cell.tag == Tag::Cons {
      let aka_cell = self.data[who_cell.head.0 - ATOM_LIMIT];
      let (script_file, line): (&str, LineNumber) = self.get_here_info_unchecked(who_cell.tail.0);
      let source: &str = self.strings[aka_cell.head.0].as_str();

      IdentifierDefinition::Alias{
        script_file,
        line,
        source
      }
    } else if who_cell.tag == Tag::FileInfo {
      let (script_file, line): (&str, LineNumber) = self.get_here_info_unchecked(reference);

      IdentifierDefinition::DefinedAt {
        script_file,
        line
      }
    } else {
      IdentifierDefinition::Undefined // Malformed identifier, shouldn't happen.
    }
  }

  fn get_here_info<'t>(&'t self, reference: Value) -> Result<(&'t str, LineNumber), ()> {
    let here_info_cell = self.expect(Tag::FileInfo, reference)?;
    let line_number: LineNumber = here_info_cell.tail.0.into();
    let script_file = self.resolve_string(here_info_cell.head)?;

    Ok((script_file, line_number))
  }

  fn get_here_info_unchecked<'t>(&'t self, reference: ValueRepresentationType) -> (&'t str, LineNumber){
    let here_info_cell: HeapCell   = self.data[reference - ATOM_LIMIT];
    let script_file   : &str       = self.strings[here_info_cell.head.0].as_str();
    let line_number   : LineNumber = here_info_cell.tail.0.into();

    (script_file, line_number)
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

  */
  fn get_identifier_value<'t>(&'t self, reference: Value)
    -> Result<(ValueRepresentationType, &'t str, IdentifierValueType), ()>
  {
    let outer_cons_cell: HeapCell = self.expect(Tag::Cons, reference)?;
    let inner_cons_cell: HeapCell = self.expect(Tag::Cons, outer_cons_cell.head.into())?;
    let show_function  : &'t str  = self.resolve_string(inner_cons_cell.tail)?;
    let arity          : ValueRepresentationType = inner_cons_cell.head.0;

    let type_cell : HeapCell            = self.expect(Tag::Cons, outer_cons_cell.tail.into())?;
    let value_type: IdentifierValueType = IdentifierValueType::from_usize(type_cell.head.0).unwrap();
    // let info = type_cell.tail;

    Ok((arity, show_function, value_type))

  }


  fn get_identifier_value_unchecked<'t>(&'t self, reference: ValueRepresentationType)
    -> (ValueRepresentationType, &'t str, IdentifierValueType)
  {
    let outer_cons_cell: HeapCell                = self.data[reference - ATOM_LIMIT];
    let inner_cons_cell: HeapCell                = self.data[outer_cons_cell.head.0 - ATOM_LIMIT];
    let arity          : ValueRepresentationType = inner_cons_cell.head.0;
    let type_cell      : HeapCell                = self.data[outer_cons_cell.tail.0 - ATOM_LIMIT];
    let value_type     : IdentifierValueType     = IdentifierValueType::from_usize(type_cell.head.0).unwrap();
    let show_function  : &'t str                 = self.strings[inner_cons_cell.tail.0].as_str();

    (arity, show_function, value_type)
  }

  /* data abstractions for identifiers (see also sto_id() in data.c)
  #define get_id(x) ((char *)hd[hd[hd[x]]])
  #define id_who(x) tl[hd[hd[x]]]
  #define id_type(x) tl[hd[x]]
  #define id_val(x) tl[x]
  #define isconstructor(x) (tag[x]==ID&&isconstrname(get_id(x)))
  #define isvariable(x) (tag[x]==ID&&!isconstrname(get_id(x)))
  /* the who field contains NIL (for a name that is totally undefined)
  hereinfo for a name that has been defined or specified and
  cons(aka,hereinfo) for a name that has been aliased, where aka
  is of the form datapair(oldn,0) oldn being a string */
  char *getaka();
  /* returns true name of an identifier, even after aliasing (data.c) */
  */

  // endregion


  // region Heap cell creation convenience functions

  pub fn datapair(&mut self, x: Value, y: Value) -> Value {
    self.put_cell(
      HeapCell::new(Tag::DataPair, x, y)
    )
  }

  pub fn fileinfo(&mut self, x: Value, y: Value) -> Value {
    self.put_cell(
      HeapCell::new(Tag::FileInfo, x, y)
    )
  }

  pub fn constructor(&mut self, n: Value, x: Value) -> Value {
    self.put_cell(
      HeapCell::new(Tag::Constructor, n, x)
    )
  }

  pub fn strcons(&mut self, x: Value, y: Value) -> Value {
    self.put_cell(
      HeapCell::new(Tag::StrCons, x, y)
    )
  }

  pub fn cons(&mut self, x: Value, y: Value) -> Value {
    self.put_cell(
      HeapCell::new(Tag::Cons, x, y)
    )
  }

  pub fn lambda(&mut self, x: Value, y: Value) -> Value {
    self.put_cell(
      HeapCell::new(Tag::Lambda, x, y)
    )
  }

  pub fn let_(&mut self, x: Value, y: Value) -> Value {
    self.put_cell(
      HeapCell::new(Tag::Let, x, y)
    )
  }

  pub fn letrec(&mut self, x: Value, y: Value) -> Value {
    self.put_cell(
      HeapCell::new(Tag::LetRec, x, y)
    )
  }

  pub fn share(&mut self, x: Value, y: Value) -> Value {
    self.put_cell(
      HeapCell::new(Tag::Share, x, y)
    )
  }

  pub fn pair(&mut self, x: Value, y: Value) -> Value {
    self.put_cell(
      HeapCell::new(Tag::Pair, x, y)
    )
  }

  pub fn tcons(&mut self, x: Value, y: Value) -> Value {
    self.put_cell(
      HeapCell::new(Tag::TCons, x, y)
    )
  }

  pub fn tries(&mut self, x: Value, y: Value) -> Value {
    self.put_cell(
      HeapCell::new(Tag::Tries, x, y)
    )
  }

  pub fn label(&mut self, x: Value, y: Value) -> Value {
    self.put_cell(
      HeapCell::new(Tag::Label, x, y)
    )
  }

  pub fn show(&mut self, x: Value, y: Value) -> Value {
    self.put_cell(
      HeapCell::new(Tag::Show, x, y)
    )
  }

  pub fn readvals(&mut self, x: Value, y: Value) -> Value {
    self.put_cell(
      HeapCell::new(Tag::StartReadValues, x, y)
    )
  }

  pub fn apply(&mut self, x: Value, y: Value) -> Value {
    self.put_cell(
      HeapCell::new(Tag::Ap, x, y)
    )
  }

  pub fn apply2(&mut self, x: Value, y: Value, z: Value) -> Value {
    let f = self.apply(x, y);
    self.apply(f, z)
  }

  pub fn apply3(&mut self, w: Value, x: Value, y: Value, z: Value) -> Value {
    let f = self.apply2(w, x, y);
    self.apply(f, z)
  }

  // endregion

}


#[cfg(test)]
mod tests {
  use crate::data::types::Type;
  use super::*;

  #[test]
  fn round_trip_values() {
    let x        : Value    = Value::Data(42usize);
    let y        : Value    = Value::Data(43usize);
    let expected : HeapCell = HeapCell::new(Tag::DataPair, x, y);
    let mut heap : Heap     = Heap::new();
    let reference: Value    = heap.datapair(x, y);
    let result   : Result<HeapCell, ()> = heap.resolve(reference);

    assert_eq!(result.is_ok(),  true);
    assert_eq!(result.unwrap(), expected);
  }


  #[test]
  fn composition() {
    let mut heap : Heap     = Heap::new();
    heap.strings.push(String::from("noodles"));
    heap.strings.push(String::from("wontons"));
    heap.strings.push(String::from("salad.ml"));

    let x = heap.cons(
      RawValue(2usize).into(),
      RawValue(1usize).into() // "wontons"
    );
    let y = heap.cons(
      RawValue(IdentifierValueType::PlaceHolder as usize).into(),
      RawValue(Combinator::Nil as usize).into()
    );
    let value = heap.cons(
      x,
      y
    ); //cons(cons(arity,showfn),cons(placeholder_t,NIL))

    let aka = heap.datapair(Value::Data(0usize), Value::Data(0usize));   // Aliasing noodles.
    let y   = heap.fileinfo(Value::Data(2usize), Value::Data(328usize)); // salad.ml
    let who = heap.cons(aka, y);
    // cons(aka,hereinfo)
    // fileinfo(script,line_no)

    let x = heap.strcons(RawValue(0usize).into(), who);
    let id_head = heap.cons(x, Value::Data(Type::Number as usize));
    let  id = heap.put(Tag::Id,id_head,value); // cons(strcons(name,who),type)

    match heap.get_identifier(id) {
      Ok(ident) => {
        println!("Identifier:\n\t{:?}", ident);
        // ident
      },

      Err(()) => {
        println!("FAILURE!");
        assert!(false);
      }
    };

  }

}
