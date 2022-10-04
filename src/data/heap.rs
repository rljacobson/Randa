/*!

See the `values` module for more information about how data is encoded in a `HeapCell`.

 */

use crate::{
  compiler::Token,
  data::{
    tag::Tag,
    types::Type,
    values::{
      HeapCell,
      RawValue,
      Value
    },
    Combinator,
    Identifier,
    IdentifierValueType,
    ValueRepresentationType
  }
};


type ValueOption = Result<Value, ()>;


#[derive(Clone)]
pub struct Heap {
  data: Vec<HeapCell>,
  strings: Vec<String>,

  // References to constants
  pub NILL: Value,

  // Common compound types.
  // numeric_function_type : Value,
  // numeric_function2_type: Value,
  // boolean_function_type : Value,
  // boolean_function2_type: Value,
  // char_list_type        : Value,
  // string_function_type  : Value,
  // range_step_type       : Value,
  // range_step_until_type : Value,

  // Common constants
}

impl Default for Heap {
  fn default() -> Self {
    let mut heap = Heap {
      data: vec![],
      strings: vec![],
      NILL: Value::Data(0),
    };

    heap.setup_constants();

    heap
  }
}

impl Heap {

  pub fn new() -> Self {
    Self::default()
  }

  fn setup_constants(&mut self) {
    // Why have a reference to this instead of just using Combinator::Nil?
    // That's what Miranda does, but is there a reason?
    self.NILL = self.cons(Value::Token(Token::Constant), Combinator::Nil.into());

  }
  /*
  fn setup_standard_types(&mut self) {
    let numeric_function_type = self.arrow_type(Type::Number.into(), Type::Number.into());
    let numeric_function2_type = self.arrow_type(Type::Number.into(), numeric_function_type);
    let boolean_function_type = self.arrow_type(Type::Bool.into(), Type::Bool.into());
    let boolean_function2_type = self.arrow_type(Type::Bool.into(), boolean_function_type);
    let char_list_type = self.list_type(Type::Char.into());
    let string_function_type = self.arrow_type(char_list_type, char_list_type);

    // let tfnumnum   = tf(num_t   , num_t);
    let range_step_type = self.arrow2_type(
      Type::Number.into(),
      Type::Number.into(),
      self.list_type(Type::Number.into())
    );
    let range_step_until_type = self.arrow_type(Type::Number.into(), range_step_type);
  }
  */


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
    Identifier::get(reference, self)
  }

  /// Encodes the identifier on the heap.
  pub fn put_identifier(&mut self, identifier: Identifier) -> Value {
    identifier.compile(self)
  }


  /* data abstractions for identifiers (see also sto_id() in data.c)
  #define get_id(x) ((char *)hd[hd[hd[x]]])
  #define id_who(x) tl[hd[hd[x]]]
  #define id_type(x) tl[hd[x]]
  #define id_val(x) tl[x]
  #define isconstructor(x) (tag[x]==ID&&isconstrname(get_id(x)))
  #define isvariable(x) (tag[x]==ID&&!isconstrname(get_id(x)))
  */

  // endregion


  // region Heap cell creation convenience functions

  // todo: Do real string interning so multiple copies of the same string aren't being made.
  pub fn string(&mut self, text: &str) -> Value {
    let idx = self.strings.len();
    self.strings.push(text.to_string());
    self.put_cell(
      HeapCell::new(Tag::String, Value::Data(idx), RawValue(0).into())
    )
  }

  pub fn data_pair(&mut self, x: Value, y: Value) -> Value {
    self.put_cell(
      HeapCell::new(Tag::DataPair, x, y)
    )
  }

  pub fn file_info(&mut self, x: Value, y: Value) -> Value {
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


  // region Type creation and checking functions

  /// Is the type referenced by `reference` an Arrow type?
  pub fn is_arrow_type(&self, reference: Value) -> bool {
    self.is_type_auxiliary(reference, Type::Arrow)
  }

  /// Is the type referenced by `reference` a Comma type?
  pub fn is_comma_type(&self, reference: Value) -> bool {
    self.is_type_auxiliary(reference, Type::Comma)
  }

  // Code common to is_arrow_type and is_comma_type is factored out into this auxiliary function.
  fn is_type_auxiliary(&self, reference: Value, type_required: Type) -> bool {
    let type_cell: HeapCell =
      match self.expect(Tag::Ap, reference) {
        Ok(cell) => cell,
        Err(_) => return false,
      };

    // The cell referenced by the head of `type_cell`
    let head_cell: HeapCell =
      match self.expect(Tag::Ap, type_cell.head.into()) {
        Ok(cell) => cell,
        Err(_) => return false,
      };

    return head_cell.head == RawValue(type_required as ValueRepresentationType) ;
  }

  /// Is the type referenced by reference a list type?
  pub fn is_list_type(&self, reference: Value) -> bool {
    let type_cell: HeapCell =
      match self.expect(Tag::Ap, reference) {
        Ok(cell) => cell,
        Err(_) => return false,
      };

    return type_cell.head == RawValue(Type::List as ValueRepresentationType);
  }

  pub fn is_type_variable(&self, reference: Value) -> bool {
    self.expect(Tag::TypeVar, reference).is_ok()
  }

  /// Is the type referenced by reference a compound type? Note that this function returns true for
  /// list and arrow types.
  pub fn is_compound_type(&self, reference: Value) -> bool {
    self.expect(Tag::Ap, reference).is_ok()
  }

  pub fn is_bound_type(&self, reference: Value) -> bool {
    let type_cell: HeapCell =
        match self.expect(Tag::Ap, reference) {
          Ok(cell) => cell,
          Err(_) => return false,
        };

    return type_cell.head == RawValue(Type::Bind as ValueRepresentationType);
  }

  pub fn arrow_type(&mut self, arg1: Value, arg2: Value) -> Value {
    self.apply2(Type::Arrow.into(), arg1, arg2)
  }

  pub fn arrow2_type(&mut self, arg1: Value, arg2: Value, arg3: Value) -> Value {
    let composed: Value = self.arrow_type(arg2, arg3);
    self.arrow_type(arg1, composed)
  }

  pub fn arrow3_type(&mut self, arg1: Value, arg2: Value, arg3: Value, arg4: Value)
      -> Value {
    let composed: Value = self.arrow2_type(arg2, arg3, arg4);
    self.arrow_type(arg1, composed)
  }

  pub fn arrow4_type(&mut self, arg1: Value, arg2: Value, arg3: Value, arg4: Value, arg5: Value)
      -> Value {
    let composed: Value = self.arrow3_type(arg2, arg3, arg4, arg5);
    self.arrow_type(arg1, composed)
  }

  pub fn list_type(&mut self, arg: Value) -> Value {
    self.apply(Type::List.into(), arg)
  }

  pub fn pair_type(&mut self, arg1: Value, arg2: Value) -> Value{
    self.apply2(
      Type::Comma.into(),
      arg1,
      self.apply2(Type::Comma.into(), arg2, Type::Void.into())
    )
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
    let reference: Value    = heap.data_pair(x, y);
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

    let aka = heap.data_pair(Value::Data(0usize), Value::Data(0usize));   // Aliasing noodles.
    let y   = heap.file_info(Value::Data(2usize), Value::Data(328usize)); // salad.ml
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
