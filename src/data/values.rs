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

use num_traits::FromPrimitive; // For conversion from `i32` to `Token` or `Combinator`

use crate::{
    compiler::{token::ParserLookahead, Token},
    data::{Combinator, ATOM_LIMIT, COMBINATOR_BASE, TOKEN_BASE},
};

pub type RawValue = isize;

impl From<Value> for RawValue {
    fn from(value: Value) -> Self {
        match value {
            Value::Tag(tag) => tag,
            Value::Char(c) => c as u32 as RawValue,
            Value::Token(token) => token as RawValue,
            Value::Combinator(combinator) => combinator as RawValue,
            Value::Reference(p) => p + ATOM_LIMIT,
            Value::Data(d) => d,

            // The raw values for the variants used by the parser shouldn't be needed, as they not real Miranda values.
            // We recycle `Value::None` to use as a "zero" `Value` and make the rest errors. Note that like
            // `Value::Data`, `Value::None` cannot round-trip, because it collides with `Value::Char`.
            Value::None => 0,

            _ => unreachable!(
                "Attempted to convert a parser-only value to a RawValue. This is a bug."
            ),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Default)]
pub enum Value {
    /// Required by parser.
    None,
    /// Required by parser.
    Uninitialized,
    /// Required by parser.
    #[default]
    Stolen,

    // ToDo: This should wrap a `crate::data::tag::Tag`.
    Tag(RawValue), // 0..23, distinguished from context.
    Char(char),    // 0..TOKEN_BASE-1               ==   0..255
    /// Required by parser and used by compiler.
    /// Represents a token returned from a Lexer.
    Token(Token), // TOKEN_BASE..COMBINATOR_BASE-1 == 256..305
    Combinator(Combinator), // COMBINATOR_BASE..ATOM_LIMIT-1 == 306..446
    // ToDo: We should probably create a `HeapReference` newtype.
    /// Reference to another cell.
    Reference(RawValue),
    Data(RawValue), // Uninterpreted data. Shouldn't use this, as there should be a `Value` variant
                    // for everything.
}

impl From<RawValue> for Value {
    /// Warning: Use this method with care! `Value::Data(n)` will never round trip! However, the `RawValue` r it
    /// encodes to will always be the same even under composition of `Value<-->RawValue` conversions.
    fn from(value: RawValue) -> Self {
        let v = value;

        if v < TOKEN_BASE {
            // It is possible to encode a non-ASCII char, which then decodes as the wrong value type.
            // Make sure `Value::Char`'s only hold ASCII characters.
            Value::Char(
                // This must be kept in sync with the target of `RawValue`.
                char::from_u32(v as u32).unwrap(),
            )
        } else if v < COMBINATOR_BASE {
            Value::Token(Token::from_isize(v).unwrap())
        } else if v < ATOM_LIMIT {
            Value::Combinator(Combinator::from_isize(v).unwrap())
        } else {
            Value::Reference(v - ATOM_LIMIT)
        }
    }
}

impl Value {
    /// Required method, parser expects it to be defined.
    ///
    /// Constructor for the parser skeleton's shifted-token semantic value. The invariant is that
    /// this forwards the semantic payload computed for the lexical token while leaving token id
    /// and location on the lookahead wrapper.
    pub(crate) fn from_token(value: ParserLookahead) -> Self {
        value.value
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
