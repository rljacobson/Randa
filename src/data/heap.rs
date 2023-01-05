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
      Tag,
      TagRepresentationType
    },
    types::Type,
    values::{
      RawValue,
      Value
    },
    Combinator,
    Identifier,
    ValueRepresentationType,
    IdentifierDefinition,
    IdentifierValueType
  },
  constants::{
    DEFAULT_SPACE,
    SPACE_LIMIT,
    INIT_SPACE,
    BIG_TOP,
    DEFAULT_DICT_SPACE,
    DICT_SPACE,
  }
};

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
  pub(crate) symbol_table: HashMap<String, Value>,

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

      data     : Vec::with_capacity(INIT_SPACE),
      strings  : vec![],
      symbol_table: HashMap::new(),

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
    &self.data[index.0 as usize]
  }
}

impl IndexMut<RawValue> for Heap{
  fn index_mut(&mut self, index: RawValue) -> &mut Self::Output {
    &mut self.data[index.0 as usize]
  }
}

impl Index<Value> for Heap {
  type Output = HeapCell;

  fn index(&self, value: Value) -> &Self::Output {
    let index = RawValue::from_value(value);
    &self.data[index.0 as usize]
  }
}

impl IndexMut<Value> for Heap{
  fn index_mut(&mut self, value: Value) -> &mut Self::Output {
    let index = RawValue::from_value(value);
    &mut self.data[index.0 as usize]
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
    self.tl_hd_mut(Into::<RawValue>::into(value).0 as usize)
  }

  /// Extracts the value of an identifier represented on the stack. Assumes `value` is a reference to an identifier.
  pub(crate) fn id_val(&mut self, value: Value) -> &mut RawValue {
    &mut self.index_mut(value).tail
  }

  // This just  compiles the value arm of an identifier: `IdentifierValueType::compile(..)`. We therefore have no
  // use for it.
  // Usage: `make_typ(0, 0, IdentifierValueType::Synonym, Type::Number)`
  // pub fn make_type(&mut self, arity: i32, show_function: Value, class_: Value, info: Value) {
  //   cons(cons(arity,show_function), cons(class_,info))
  // }

  /// A convenience method used by "privlib" and "stdlib", see below. It creates an identifier with the given name,
  /// value, and datatype, constructing the value according to whether the name is that of a constructor
  /// (capitalized) or not.
  pub(crate) fn predefine_identifier(&mut self, name: &str, value: Value, datatype: Type) -> Value {
    let id_ref = Identifier {
      name: name.to_string(),
      definition: IdentifierDefinition::Undefined,
      datatype,
      value: None // To be filled in after we get a reference to this identifier, i.e. after compilation.
    }.compile(&mut self.heap);

    let id_value = if is_capitalized(name) {
      self.constructor(value, id_ref)
    } else {
      value
    };

    self[id_ref].tail = id_value.into();

    id_ref
  }


  /// Registers `name` in the symbol table and strings list (both via `compile()`) and creates a "bare bones"
  /// identifier structure on the heap for the given name, returning a reference to the heap object.
  pub fn make_empty_identifier(&mut self, name: &str) -> Value {
    Identifier{
      name: name.to_string(),
      definition: IdentifierDefinition::Undefined,
      datatype: Default::default(),
      value: None,
    }.compile(self)
    // return self.make(ID, cons(strcons(p1, NIL), undef_t), UNDEF);
  }


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
          /* during compilation we raise false ceiling whenever residency
             reaches 75% on 2 successive gc's */
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
        return make(t, x, y);
      }
    }
    claims += 1;
    tag[listp] = t;
    hd[listp] = x;
    tl[listp] = y;
    listp
  }


  /// Returns a mutable reference to the RawValue in the head of the head of the provided reference.
  pub fn hd_hd_mut<T: Into<usize>>(&mut self, idx: T) -> &mut RawValue {
    let hd = self.data[idx.into()].head;
    &mut self.data[hd.0 as usize].head
  }

  /// Returns a copy of the RawValue in the head of the head of the provided reference.
  pub fn hd_hd<T: Into<usize>>(&self, idx: T) -> RawValue {
    let hd = self.data[idx.into()].head;
    self.data[hd.0 as usize].head
  }

  pub fn hd_tl_mut<T: Into<usize>>(&mut self, idx: T) -> &mut RawValue {
    let hd = self.data[idx.into()].head;
    &mut self.data[hd.0 as usize].tail
  }

  pub fn hd_tl<T: Into<usize>>(&self, idx: T) -> RawValue {
    let hd = self.data[idx.into()].head;
    self.data[hd.0 as usize].tail
  }

  pub fn tl_tl_mut<T: Into<usize>>(&mut self, idx: T) -> &mut RawValue {
    let tl = self.data[idx.into()].tail;
    &mut self.data[tl.0 as usize].tail
  }

  pub fn tl_tl<T: Into<usize>>(&self, idx: T) -> RawValue {
    let tl = self.data[idx.into()].tail;
    self.data[tl.0 as usize].tail
  }

  pub fn tl_hd_mut<T: Into<usize>>(&mut self, idx: T) -> &mut RawValue {
    let tl = self.data[idx.into()].tail;
    &mut self.data[tl.0 as usize].head
  }

  pub fn tl_hd<T: Into<usize>>(&self, idx: T) -> RawValue {
    let tl = self.data[idx.into()].tail;
    self.data[tl.0 as usize].head
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

  /// Same as `put_cell`, but creates the `HeapCell` as well.
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

  // endregion


  // region Identifier name resolution functions

  /// Retrieves the identifier information pointed to by the given reference.
  /// The argument must be a reference.
  pub fn get_identifier(&self, reference: Value) -> Result<Identifier, ()> {
    // The `Identifier` knows how to encode/decode itself.
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
  /// Registers the string in the strings list, creates the string object on the heap, and places a reference to the
  /// heap string object into the symbol table, returning the heap reference.
  pub fn string<T: Into<String>>(&mut self, text: T) -> Value {
    let name = text.into();
    let idx  = self.strings.len();

    let string_ref = self.put(Tag::String, Value::Data(idx as ValueRepresentationType), Value::None);
    self.symbol_table[&name] = string_ref;
    self.strings.push(name);

    string_ref
  }

  pub fn data_pair(&mut self, x: Value, y: Value) -> Value {
    self.put(Tag::DataPair, x, y)
  }

  pub fn file_info(&mut self, x: Value, y: Value) -> Value {
    self.put(Tag::FileInfo, x, y)
  }

  pub fn constructor(&mut self, n: Value, x: Value) -> Value {
    self.put(Tag::Constructor, n, x)
  }

  pub fn strcons(&mut self, x: Value, y: Value) -> Value {
    self.put(Tag::StrCons, x, y)
  }

  pub fn cons(&mut self, x: Value, y: Value) -> Value {
    self.put(Tag::Cons, x, y)
  }

  pub fn lambda(&mut self, x: Value, y: Value) -> Value {
    self.put(Tag::Lambda, x, y)
  }

  pub fn let_(&mut self, x: Value, y: Value) -> Value {
    self.put(Tag::Let, x, y)
  }

  pub fn letrec(&mut self, x: Value, y: Value) -> Value {
    self.put(Tag::LetRec, x, y)
  }

  pub fn share(&mut self, x: Value, y: Value) -> Value {
    self.put(Tag::Share, x, y)
  }

  pub fn pair(&mut self, x: Value, y: Value) -> Value {
    self.put(Tag::Pair, x, y)
  }

  pub fn tcons(&mut self, x: Value, y: Value) -> Value {
    self.put(Tag::TCons, x, y)
  }

  pub fn tries(&mut self, x: Value, y: Value) -> Value {
    self.put(Tag::Tries, x, y)
  }

  pub fn label(&mut self, x: Value, y: Value) -> Value {
    self.put(Tag::Label, x, y)
  }

  pub fn show(&mut self, x: Value, y: Value) -> Value {
    self.put(Tag::Show, x, y)
  }

  pub fn readvals(&mut self, x: Value, y: Value) -> Value {
    self.put(Tag::StartReadValues, x, y)
  }

  pub fn apply(&mut self, x: Value, y: Value) -> Value {
    self.put(Tag::Ap, x, y)
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

  /// Returns true iff the value is 1) an identifier that 2) is a constructor,
  /// which occurs iff 1) its tag is `Tag::Identifier` and 2) its name is capitalized.
  pub fn is_constructor(&self, reference: Value) -> bool {
    let identifier_cell: HeapCell =
        match self.expect(Tag::Id, reference) {
          Ok(cell) => cell,
          Err(_) => return false,
        };

    let strcons = self.hd_hd(identifier_cell);
    let name_ref = self.data[strcons].head();

    is_capitalized(self.strings[name_ref].as_str())
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
  use crate::data::IdentifierValueType;
  use crate::data::types::Type;
  use super::*;

  #[test]
  fn round_trip_values() {
    let x        : Value    = Value::Data(42);
    let y        : Value    = Value::Data(43);
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
      RawValue(2).into(),
      RawValue(1).into() // "wontons"
    );
    let y = heap.cons(
      RawValue(IdentifierValueType::PlaceHolder as ValueRepresentationType).into(),
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

    let x = heap.strcons(RawValue(0).into(), who);
    let id_head = heap.cons(x, Value::Data(Type::Number as ValueRepresentationType));
    let  id = heap.put(Tag::Id,id_head,value); // cons(strcons(name,who),type)

    match heap.get_identifier(id) {
      Ok(ident) => {
        println!("Identifier:\n\t{}", ident.name);
        // ident
      },

      Err(()) => {
        println!("FAILURE!");
        assert!(false);
      }
    };

  }

}
