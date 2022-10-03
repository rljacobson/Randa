/*!

From Miranda:

  All Miranda values are of type word

  | 0..ATOM_LIMIT-1      | atoms, made up as follows                              |
  |:--------------------|:-------------------------------------------------------|
  | 0..255              | the Latin 1 character set (0..127 ascii)               |
  | 256..CMBASE-1       | lexical tokens (see rules.y)                           |
  | CMBASE..ATOM_LIMIT-1 | combinators and special values, e.g. NIL (see combs.h) |


  The first pointer value is `ATOM_LIMIT`. Values >= `ATOM_LIMIT` are indexes into the heap. The
  heap is held as three arrays: `tag[]`, `hd[]`, `tl[]`. The arrays `word *hd,*tl` are offset
  so they are indexed from `ATOM_LIMIT`. The array `char *tag` holds type info and is indexed
  from 0. The type info stored in `tag[0]..tag[ATOM_LIMIT-1]` are all 0, meaning ATOM. See
  `setupheap()` in data.c.


 */

use num_traits::{FromPrimitive, ToPrimitive};
use crate::compiler::Token;

use crate::data::{
  ATOM_LIMIT,
  COMBINATOR_BASE,
  TOKEN_BASE,
  Combinator,
  ValueRepresentationType,
};
use super::tag::Tag;
use super::token::Token;


pub type Heap = Vec<HeapCell>;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Default)]
pub struct RawValue(pub ValueRepresentationType);

impl RawValue {
  pub fn from_value(value: Value) -> RawValue {
    match value {

      Value::Char(c) => RawValue((c as u32) as ValueRepresentationType),

      Value::Token(token) => RawValue(token as ValueRepresentationType),

      Value::Combinator(combinator) => RawValue(combinator as ValueRepresentationType),

      Value::Reference(p) => RawValue(p + ATOM_LIMIT),

      Value::Data(d) => RawValue(d)

    }
  }
}


impl From<Value> for RawValue
{
  fn from(v: Value) -> Self {
    RawValue::from_value(v)
  }
}

impl<T> From<T> for RawValue
  where T: Into<ValueRepresentationType>
{
  fn from(c: T) -> Self {
    RawValue(Into::<ValueRepresentationType>::into(c))
  }
}


#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Value {
  Char(char),                         // 0..TOKEN_BASE-1               ==   0..255
  Token(Token),                       // TOKEN_BASE..COMBINATOR_BASE-1 == 256..305
  Combinator(Combinator),             // COMBINATOR_BASE..ATOM_LIMIT-1 == 306..446
  Reference(ValueRepresentationType), // Reference to another cell.
  Data(ValueRepresentationType)       // Uninterpreted data. Shouldn't use this, as there should be a `Value` variant
                                      // for everything.
}


impl From<RawValue> for Value {
  fn from(value: RawValue) -> Self {
    let v = value.0;

    if v < TOKEN_BASE {
      // It is possible to encode a non-ASCII char which then decodes as the wrong value type.
      // Make sure `Value::Char`'s only hold ASCII characters.
      Value::Char(
        // This must be kept in sync with the target of the type alias `ValueRepresentationType`.
        unsafe { char::from_u32(v as u32).unwrap() }
      )
    } else if v < COMBINATOR_BASE {
      Value::Token(
        Token::from_usize(v).unwrap()
      )
    } else if v < ATOM_LIMIT {
      Value::Combinator(
        Combinator::from_usize(v).unwrap()
      )
    } else {
      Value::Reference(v - ATOM_LIMIT)
    }
  }
}

impl From<char> for Value {
  fn from(c: char) -> Self {
    Value::Char(c)
  }
}

impl From<Combinator> for Value {
  fn from(combinator: Combinator) -> Value {
    Value::Combinator(combinator)
  }
}

impl From<Token> for Value {
  fn from(token: Token) -> Value {
    Value::Token(token)
  }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct HeapCell {
  pub tag: Tag,
  pub head: RawValue,
  pub tail: RawValue
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



#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}
