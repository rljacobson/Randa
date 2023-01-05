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

use num_traits::{FromPrimitive}; // For conversion from `i32` to `Token` or `Combinator`

use crate::{
  data::{
    ATOM_LIMIT,
    COMBINATOR_BASE,
    TOKEN_BASE,
    Combinator,
    ValueRepresentationType,
  },
  compiler::Token,
};


#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Default)]
pub struct RawValue(pub ValueRepresentationType);

impl RawValue {
  pub fn from_value(value: Value) -> RawValue {
    match value {

      Value::Tag(tag) => RawValue(tag),

      Value::Char(c) => RawValue((c as u32) as ValueRepresentationType),

      Value::Token(token) => RawValue(token as ValueRepresentationType),

      Value::Combinator(combinator) => RawValue(combinator as ValueRepresentationType),

      Value::Reference(p) => RawValue(p + ATOM_LIMIT),

      Value::Data(d) => RawValue(d),

      // The `RawValue`s for the variants used by the parser shouldn't be needed, as they not real Miranda values. We
      // recycle `Value::None` to use as a "zero" `Value` and make the rest errors. Note that like
      // `Value::Data`, `Value::None` cannot round-trip, because it collides with `Value::Char`.
      Value::None => RawValue(0),

      _ => unreachable!("Attempted to convert a parser-only value to a RawValue. This is a bug.")
    }
  }
}


impl From<Value> for RawValue {
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
  /// Required by parser.
  None,
  /// Required by parser.
  Uninitialized,
  /// Required by parser.
  Stolen,


  Tag(ValueRepresentationType),       // 0..23, distinguished from context.
  Char(char),                         // 0..TOKEN_BASE-1               ==   0..255
  /// Required by parser and used by compiler.
  /// Represents a token that is returned from a Lexer
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
        Token::from_isize(v).unwrap()
      )
    } else if v < ATOM_LIMIT {
      Value::Combinator(
        Combinator::from_isize(v).unwrap()
      )
    } else {
      Value::Reference(v - ATOM_LIMIT)
    }
  }
}

impl Default for Value {
  fn default() -> Self {
    Self::Stolen
  }
}

impl Value {
  /// Required method, parser expects it to be defined.
  ///
  /// Constructor for `LexicalValue::Token(token)` variant.
  pub(crate) fn from_token(value: Token) -> Self {
    Self::Token(value)
  }

  pub(crate) fn new_uninitialized() -> Self {
    Self::Uninitialized
  }

  pub(crate) fn is_uninitialized(&self) -> bool {
    matches!(self, Self::Uninitialized)
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
