/*!

See the `values` module for more information about how data is encoded in a `HeapCell`.

Item representation within the heap:

 */


use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::ops::{Index, IndexMut};

use crate::{
  compiler::Token,
  data::{
    ATOM_LIMIT,
    tag::{
      Tag
    },
    types::Type,
    values::{
      RawValue,
      Value
    },
    Combinator,
    Identifier,
    ValueRepresentationType,
    IdentifierDefinition
  },
  constants::{
    INIT_SPACE,
  }
};
use crate::data::api::{HeapObjectProxy, IdentifierDefinition, IdentifierDefinitionData, IdentifierRecord};

type ValueResult = Result<Value, ()>;

// Todo: Give these a home. Seems like they should be combinators.
static GENERATOR: ValueRepresentationType = 0;
static GUARD    : ValueRepresentationType = 1;
static REPEAT   : ValueRepresentationType = 2;

/// The fundamental unit of data for data that lives in the heap.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct HeapCell {
  pub tag : Tag,
  pub head: RawValue,
  pub tail: RawValue,
}

impl HeapCell {
  pub fn new(tag: Tag, head: Value, tail: Value) -> HeapCell {
    // Trivial implementation, but nice to have in case we decide we need a nontrivial implementation in the future.
    HeapCell{
      tag,
      head: head.into(),
      tail: tail.into()
    }
  }

  pub fn new_raw(tag: Tag, head: RawValue, tail: RawValue) -> HeapCell {
    // Trivial implementation, but nice to have in case we decide we need a nontrivial implementation in the future.
    HeapCell{
      tag,
      head,
      tail
    }
  }

}


// #[derive(Clone)]
pub struct Heap {
  // Heap limits
  // ToDo: Should this be replaced by `self.heap.capacity`?
  /// This is SPACE in Miranda and is initialized as `SPACE = INIT_SPACE`. See also [`DEFAULT_SPACE`](DEFAULT_SPACE),
  /// [`SPACE_LIMIT`](SPACE_LIMIT), [`INIT_SPACE`](INIT_SPACE), and [`BIG_TOP`](BIG_TOP).
  heap_space: usize,

  data        : Vec<HeapCell>,
  strings     : Vec<String>,
  /// A map from an identifier name to (a reference to) its identifier structure on the heap.
  // Todo: Could we use the string's index in `heap.strings` for the symbol table key instead of having a second copy
  //       of the string?
  pub(crate) symbol_table: HashMap<String, Value>,

  /// The private environment is represented as a vector of references to items of the form `strcons(index, value)`,
  /// where `index` is the index of the item in `private_symbols`, and `value` is (a reference to) the item.
  pub(crate) private_symbols: Vec<ValueRepresentationType>,

  /// Special heap value NILL
  pub(crate) nill: Value

}

impl Debug for Heap {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "Heap{{size={}, strings={}}}", self.data.len(), self.strings.len())
  }
}

impl Default for Heap {
  /// Setup and initialization of [`Heap`](crate::data::heap::Heap) occurs in
  /// [`VM::default()`](crate::data::heap::Heap::default()),
  /// [`VM::setup_constants()`](crate::data::heap::Heap::setup_constants()), and
  /// [`VM::setup_standard_types()`](crate::data::heap::Heap::setup_standard_types()).
  fn default() -> Self {
    let mut heap = Heap {
      // Heap limits
      heap_space: INIT_SPACE, // SPACE = INIT_SPACE;

      data           : Vec::with_capacity(INIT_SPACE),
      strings        : vec![],
      symbol_table   : HashMap::new(),
      private_symbols: Vec::with_capacity(200), // This is the initial capacity in Miranda.

      nill: Value::Uninitialized
    };

    // Todo: Why have a reference to this instead of just using Combinator::Nil?
    //       That's what Miranda does, but is there a reason? To have it on the heap, but why?
    //       Possible answer: So it can be used in contexts that allow Combinator values that are not actually
    //       combinators.
    // Nill lives in `Heap` because some `Heap` functions use it.
    heap.nill = heap.cons(Value::Token(Token::Constant), Combinator::Nil.into());

    heap
  }
}


// region Index impls
impl Index<RawValue> for Heap {
  type Output = HeapCell;

  fn index(&self, index: RawValue) -> &Self::Output {
    // RawValues are the `index + ATOM_LIMIT`
    let idx = index.0 - ATOM_LIMIT;
    if idx >= 0 {
      &self.data[idx as usize ]
    } else {
      unreachable!("Tried to index into the heap with a RawValue that doesn't represent a reference.")
    }
  }
}

impl IndexMut<RawValue> for Heap{
  fn index_mut(&mut self, index: RawValue) -> &mut Self::Output {
    // RawValues are the `index + ATOM_LIMIT`
    let idx = index.0 - ATOM_LIMIT;
    if idx >= 0 {
      &mut self.data[idx as usize ]
    } else {
      unreachable!("Tried to index into the heap with a RawValue that doesn't represent a reference.")
    }
  }
}

impl Index<Value> for Heap {
  type Output = HeapCell;

  fn index(&self, value: Value) -> &Self::Output {
    if let Value::Reference(idx) = value{
      &self.data[idx as usize]
    } else {
      unreachable!("Tried to use a non-Value::Reference variant as a reference. This is a bug.")
    }
  }
}

impl IndexMut<Value> for Heap{
  fn index_mut(&mut self, value: Value) -> &mut Self::Output {
    if let Value::Reference(idx) = value{
      &mut self.data[idx as usize]
    } else {
      unreachable!("Tried to use a non-Value::Reference variant as a reference. This is a bug.")
    }
  }
}
// endregion

impl Heap {

  pub fn new() -> Self {
    Self::default()
  }

  /// Reports the `heap_space + ATOM_LIMIT`.
  fn top(&self) -> usize{
    // Todo: Should this just report `self.heap.capacity + ATOM_LIMIT`?
    self.heap_space + ATOM_LIMIT as usize
  }


  // region Generic read/write functions

  /// Extracts the type of an identifier represented on the stack. Assumes `value` is a reference to an identifier.
  pub(crate) fn id_type(&mut self, value: Value) -> &mut RawValue {
    self.tl_hd_mut(value)
  }

  /// Extracts the value of an identifier represented on the stack. Assumes `value` is a reference to an identifier.
  pub(crate) fn id_val(&mut self, value: Value) -> &mut RawValue {
    &mut self.index_mut(value).tail
  }

  /// A convenience method used by "privlib" and "stdlib", see below. It creates an identifier with the given name,
  /// value, and datatype, constructing the value according to whether the name is that of a constructor
  /// (capitalized) or not.
  pub(crate) fn predefine_identifier<T>(&mut self, name: &str, value: Value, data_type: T) -> IdentifierRecord
      where T: Into<Value>
  {
    let id_ref = IdentifierRecord::new(
      &mut self.heap,
      name.to_string(),
      IdentifierDefinition::undefined(),
      data_type.into(),
      None // Value to be filled in after we get a reference to this identifier.
    );

    let id_value = if is_capitalized(name) {
      self.constructor(value, id_ref.into())
    } else {
      value
    };

    self[id_ref].tail = id_value.into();

    id_ref
  }


  /// Registers `name` in the symbol table and strings list and creates a "bare bones"
  /// identifier structure on the heap for the given name, returning a reference to the heap object.
  pub fn make_empty_identifier(&mut self, name: &str) -> IdentifierRecord {
    // return self.make(ID, cons(strcons(p1, NIL), undef_t), UNDEF);
    IdentifierRecord::new(
      &mut self.heap,
      name.to_string(),
      IdentifierDefinition::from_ref(0.into()),
      Type::Undefined.into(),
      None
    )
  }


  /*
  fn make(&mut self, t: u8, x: u16, y: u16) -> u16 {
    let mut listp = 0;

    while self.data[listp].tag as i32 > 0 {
      listp += 1;
    }
    // Todo: It's not clear how any of this GC stuff will apply.

    // find next cell with zero or negative tag (=unwanted)
    if listp == self.top() {
      if self.heap_space != SPACE_LIMIT {
        if !compiling {
          self.heap_space = SPACE_LIMIT;
        } else if claims <= self.heap_space / 4 && nogcs > 1 {
          // during compilation we raise false ceiling whenever residency reaches 75% on 2 successive gc's
          static mut wait: u16 = 0;
          let sp = self.heap_space;
          if wait > 0 {
            wait -= 1;
          } else {
            self.heap_space += self.heap_space / 2;
            wait = 2;
            self.heap_space = 5000 * (1 + (self.heap_space - 1) / 5000); /* round upwards */
          }
          if self.heap_space > SPACE_LIMIT {
            self.heap_space = SPACE_LIMIT;
          }
          if atgc && self.heap_space > sp {
            println!("\n<<increase heap from {} to {}>>\n", sp, self.heap_space);
          }
        }
      }

      // If after raising the false space ceiling we still do not have a free cell, then run  the GC and try again.
      if listp == TOP {
        gc();

        if t > Tag::StrCons as TagRepresentationType {
          mark(x);
        }
        if t >= INT {
          mark(y);
        }
        return make(t, head, tail);
      }
    }
    claims += 1;
    tag[listp] = t;
    hd[listp] = x;
    tl[listp] = y;
    listp
  }
  */

  /// Returns a mutable reference to the RawValue in the head of the head of the provided reference.
  pub fn hd_hd_mut<T: Into<RawValue>>(&mut self, idx: T) -> &mut RawValue {
    let hd = self[idx.into()].head;
    &mut self[hd].head
  }

  /// Returns a copy of the RawValue in the head of the head of the provided reference.
  pub fn hd_hd<T: Into<RawValue>>(&self, idx: T) -> RawValue {
    let hd = self[idx.into()].head;
    self[hd].head
  }

  pub fn hd_tl_mut<T: Into<RawValue>>(&mut self, idx: T) -> &mut RawValue {
    let hd = self[idx.into()].tail;
    &mut self[hd].head
  }

  pub fn hd_tl<T: Into<RawValue>>(&self, idx: T) -> RawValue {
    let hd = self[idx.into()].tail;
    self[hd].head
  }

  pub fn tl_tl_mut<T: Into<RawValue>>(&mut self, idx: T) -> &mut RawValue {
    let tl = self[idx.into()].tail;
    &mut self[tl].tail
  }

  pub fn tl_tl<T: Into<RawValue>>(&self, idx: T) -> RawValue {
    let tl = self[idx.into()].tail;
    self[tl].tail
  }

  pub fn tl_hd_mut<T: Into<RawValue>>(&mut self, idx: T) -> &mut RawValue {
    let tl = self[idx.into()].head;
    &mut self[tl].tail
  }

  pub fn tl_hd<T: Into<RawValue>>(&self, idx: T) -> RawValue {
    let tl = self[idx.into()].head;
    self[tl].tail
  }

  /// Resolves a reference to the `HeapCell` it points to.
  pub fn resolve(&self, value: Value) -> Result<HeapCell, ()> {
    match value {

      Value::Reference(r) => {
        Ok(self.data[r as usize])
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
        self.data[r as usize]
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

  /// Same as `put_cell`, but creates the `HeapCell` as well. This is Miranda's `make`, roughly speaking.
  pub fn put(&mut self, tag: Tag, head: Value, tail: Value) -> Value {
    self.put_cell(HeapCell::new(tag, head, tail))
  }

  /// Dereference the given reference, and if the result has `tag`, returns the cell. If `reference` isn't a
  /// reference, or if the tag of the referent is not `tag`, returns `Err(())`.
  pub fn expect(&self, tag: Tag, reference: Value) -> Result<HeapCell, ()> {
    if let Value::Reference(idx) = reference {
      let found_cell = self.data[idx as usize];
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
  pub fn expect_head(&self, tag: Tag, reference: Value) -> ValueResult {
    if let Value::Reference(idx) = reference {
      let cell = self.data[idx as usize];
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
  pub fn expect_tail(&self, tag: Tag, reference: Value) -> ValueResult {
    if let Value::Reference(idx) = reference {
      let cell = self.data[idx as usize];
      if cell.tag == tag {
        return Ok(cell.tail.into());
      }
    }
    // Else:
    println!("Expected: {:?}\nFound: {:?}", tag, reference);
    Err(())
  }

  /// Resolve the string cell to the string value.
  pub fn resolve_string(&self, value: RawValue) -> Result<String, ()> {
    return Ok(self.strings[value.0 as usize].clone());
  }

  /// Create a new private symbol with `value` and return a reference to it.
  pub fn make_private_symbol(&mut self, value: Value) -> Value {
    let new_symbol = self.strcons(Value::Data(self.private_symbols.len() as ValueRepresentationType), value);
    self.private_symbols.push(new_symbol.into());

    new_symbol
  }

  /// If `n < private_symbols.len()`, returns `private_symbols[n]`. If `n >= private_symbols.len()`, creates new
  /// "empty" private symbols for all j with `private_symbols.len() <= j <= n` and returns `private_symbols[n]`.
  //  ToDo: This sounds dumb.
  pub fn get_nth_private_symbol(&mut self, n: usize) -> ValueRepresentationType {
    let length = self.private_symbols.len();
    if n >= length {
      for j in length..=n {
        let new_symbol = self.strcons(Value::Data(j as ValueRepresentationType), Combinator::Undef.into());
        self.private_symbols.push(new_symbol.into());
      }
    }

    self.private_symbols[n]
  }


  // endregion

  /* data abstractions for identifiers (see also sto_id() in data.c)
  #define get_id(x) ((char *)hd[hd[hd[x]]])
  #define id_who(x) tl[hd[hd[x]]]
  #define id_type(x) tl[hd[x]]
  #define id_val(x) tl[x]
  #define isconstructor(x) (tag[x]==ID&&isconstrname(get_id(x)))
  #define isvariable(x) (tag[x]==ID&&!isconstrname(get_id(x)))
  */


  // region Heap cell creation convenience functions

  /// Registers the string in the strings list, creates the string object on the heap, and places a reference to the
  /// heap string object into the symbol table, returning the heap reference.
  pub fn string<T: Into<String>>(&mut self, text: T) -> Value {
    let text: String = text.into();

    // Cannot use `Entry` API because we would need a second mutable borrow of self to create the ID on the heap.
    if let Some(value) = self.symbol_table.get(&text) {
      return *value;
    }

    let idx  = self.strings.len();
    let string_ref = self.put(Tag::String, Value::Data(idx as ValueRepresentationType), Value::None);

    self.symbol_table.insert(text.clone(), string_ref);
    self.strings.push(text);

    string_ref
  }

  pub fn identifier(&mut self, head: Value, tail: Value) -> Value {
    self.put(Tag::Id, head, tail)
  }

  pub fn data_pair(&mut self, head: Value, tail: Value) -> Value {
    self.put(Tag::DataPair, head, tail)
  }

  pub fn file_info(&mut self, head: Value, tail: Value) -> Value {
    self.put(Tag::FileInfo, head, tail)
  }

  pub fn constructor(&mut self, n: Value, x: Value) -> Value {
    self.put(Tag::Constructor, n, x)
  }

  pub fn strcons(&mut self, head: Value, tail: Value) -> Value {
    self.put(Tag::StrCons, head, tail)
  }

  pub fn cons(&mut self, head: Value, tail: Value) -> Value {
    self.put(Tag::Cons, head, tail)
  }

  pub fn lambda(&mut self, head: Value, tail: Value) -> Value {
    self.put(Tag::Lambda, head, tail)
  }

  pub fn let_(&mut self, head: Value, tail: Value) -> Value {
    self.put(Tag::Let, head, tail)
  }

  pub fn letrec(&mut self, head: Value, tail: Value) -> Value {
    self.put(Tag::LetRec, head, tail)
  }

  pub fn share(&mut self, head: Value, tail: Value) -> Value {
    self.put(Tag::Share, head, tail)
  }

  pub fn pair(&mut self, head: Value, tail: Value) -> Value {
    self.put(Tag::Pair, head, tail)
  }

  pub fn tcons(&mut self, head: Value, tail: Value) -> Value {
    self.put(Tag::TCons, head, tail)
  }

  pub fn tries(&mut self, head: Value, tail: Value) -> Value {
    self.put(Tag::Tries, head, tail)
  }

  pub fn label(&mut self, head: Value, tail: Value) -> Value {
    self.put(Tag::Label, head, tail)
  }

  pub fn show(&mut self, head: Value, tail: Value) -> Value {
    self.put(Tag::Show, head, tail)
  }

  pub fn readvals(&mut self, head: Value, tail: Value) -> Value {
    self.put(Tag::StartReadValues, head, tail)
  }

  pub fn apply(&mut self, head: Value, tail: Value) -> Value {
    self.put(Tag::Ap, head, tail)
  }

  pub fn apply2(&mut self, x: Value, y: Value, z: Value) -> Value {
    let f = self.apply(x, y);
    self.apply(f, z)
  }

  pub fn apply3(&mut self, w: Value, x: Value, y: Value, z: Value) -> Value {
    let f = self.apply2(w, x, y);
    self.apply(f, z)
  }

  /// Boxes a real number (an `f64`).
  pub fn real(&mut self, number: f64) -> Value {
    let bits = unsafe {
      std::mem::transmute::<f64, ValueRepresentationType>(number)
    };
    self.put(Tag::Double, Value::Data(bits), Value::None)
  }

  // endregion


  // region Type creation and checking functions

  /// Returns true iff the value is 1) an identifier that 2) is a constructor,
  /// which occurs iff 1) its tag is `Tag::Identifier` and 2) its name is capitalized.
  pub fn is_constructor(&self, reference: Value) -> bool {
    let identifier_cell: HeapCell =
        match self.expect(Tag::Id, reference) {
          Ok(cell) => cell,
          Err(_) => return false,
        };

    let strcons = self[identifier_cell.head].head;
    let name_ref = self[strcons].head;

    is_capitalized(self.strings[name_ref.0 as usize].as_str())
  }

  /// Is the type referenced by `reference` an Arrow type? Not only checks the type but also that the correct
  /// `HeapCell` `Tag` is present at each level of the composition.
  pub fn is_arrow_type(&self, reference: Value) -> bool {
    self.is_type_auxiliary(reference, Type::Arrow)
  }

  /// Is the type referenced by `reference` a Comma type?  Not only checks the type but also that the correct
  /// `HeapCell` `Tag` is present at each level of the composition.
  pub fn is_comma_type(&self, reference: Value) -> bool {
    self.is_type_auxiliary(reference, Type::Comma)
  }

  /// Code common to is_arrow_type and is_comma_type is factored out into this auxiliary function. Note that this
  /// function checks that correct tags are present.
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

  /// Is the type referenced by reference a list type? Not only checks the type but also that the correct
  /// `HeapCell` `Tag` is present at each level of the composition.
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
    let inner = self.apply2(Type::Comma.into(), arg2, Type::Void.into());
    self.apply2(
      Type::Comma.into(),
      arg1,
      inner,
    )
  }

  // endregion

}


// Todo: Find a better home for this function.
/// Returns true iff the first non-'$' character of word is uppercase.
pub(crate) fn is_capitalized(word: &str) -> bool {
  for c in word.chars() {
    if c == '$' {
      continue;
    }
    return c.is_uppercase();
  }
  // Consists only of '$'
  return false;
}




#[cfg(test)]
mod tests {
  use crate::data::api::{IdentifierValue, IdentifierValueType, IdentifierValueTypeData};
  use crate::data::IdentifierValueType;
  use crate::data::types::Type;
  use super::*;

  #[test]
  fn round_trip_values() {
    let x        : Value    = Value::Data(42);
    let y        : Value    = Value::Data(43);
    let expected : HeapCell = HeapCell::new(Tag::DataPair, head, tail);
    let mut heap : Heap     = Heap::new();
    let reference: Value    = heap.data_pair(x, y);
    let result   : Result<HeapCell, ()> = heap.resolve(reference);

    assert_eq!(result.is_ok(),  true);
    assert_eq!(result.unwrap(), expected);
  }


  #[test]
  fn composition() {
    let mut heap: Heap = Heap::new();
    heap.strings.push(String::from("noodles"));
    heap.strings.push(String::from("wontons"));
    heap.strings.push(String::from("salad.ml"));

    let x = heap.cons(
      RawValue(2).into(),
      RawValue(1).into() // "wontons"
    );

    let value_type = IdentifierValueType::new(&mut heap, IdentifierValueTypeData::PlaceHolder);

    let y = heap.cons(
      value_type.get_ref().into(),
      RawValue(Combinator::Nil as ValueRepresentationType).into()
    );
    let value = heap.cons(
      x,
      y
    ); //cons(cons(arity,showfn),cons(placeholder_t,NIL))

    let aka = heap.data_pair(Value::Data(0), Value::Data(0));   // Aliasing noodles.
    let y   = heap.file_info(Value::Data(2), Value::Data(328)); // salad.ml
    let who = heap.cons(aka, y);
    // cons(aka,hereinfo)
    // fileinfo(script,line_no)

    let x       = heap.strcons(RawValue(0).into(), who);
    let id_head = heap.cons(x, Value::Data(Type::Number as ValueRepresentationType));
    let id      = heap.put(Tag::Id, id_head, value); // cons(strcons(name,who),type)

    let id_record = IdentifierRecord::from_ref(id.into());

    match id_record.get_data(&heap) {
      Ok(ident) => {
        println!("Identifier:\n\t{}", ident.name);
        // ident
        assert!(true);
      },

      Err(_) => {
        println!("FAILURE!");
        assert!(false);
      }
    };

  }

}
