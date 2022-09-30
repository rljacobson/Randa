/*!
  The data module collects things having to do with representing (encoding/decoding) _values_

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

pub mod combinator;
pub mod tag;
pub mod types;
mod identifier;
mod values;


use saucepan::LineNumber;

pub use crate::{
  data::{
    token::Token,
    combinator::Combinator,
    types::Type,
  }
};


/// The type used to represent tokens, combinators, pointers, Randa types...
// Miranda uses size of a pointer, 64-bits, which removes an indirection for pointer data.
// If the bit-width of `ValueRepresentationType` changes, synchronize it with
//    * `Heap::resolve_string()`
//    * instances of `IdentifierValueType::from_usize`
type ValueRepresentationType                              = usize;
pub const TOKEN_BASE            : ValueRepresentationType = 256; // There are 80 token values.
pub const COMBINATOR_BASE       : ValueRepresentationType = 336; // There are 141 combinators.
pub const ATOM_LIMIT            : ValueRepresentationType = COMBINATOR_BASE + 141; // = 477


#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Value {
  Char(ValueRepresentationType),
  // Token(Token),
  Combinator(Combinator),
  Pointer(ValueRepresentationType)
}

impl Value{
  pub fn discriminant(&self) -> ValueRepresentationType {

    match self {
      Value::Char(v)       => *v,
      // Value::Token(v)      => *v as ValueRepresentationType,
      Value::Combinator(v) => *v as ValueRepresentationType,
      Value::Pointer(v)    => *v,

    }

  }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct File<'t> {
  name      : &'t str,
  mtime     : &'t str, // type?
  sharable  : bool,   // repeated instances are shareable?
  definienda: u32     // type??
}


#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Identifier<'t>{
  name         : &'t str, // `String` or `&str`?
  definition   : IdentifierDefinition<'t>,
  datatype     : Type,
  arity        : ValueRepresentationType,
  show_function: &'t str, // What is a show function?
  value        : IdentifierValueType
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
    source     : &'t str,    // `String` or `&str`?
  }
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
  Algebraic   = 0, //{ constructors: Vec<&'t str> },
  Synonym     = 1, //{ source:       &'t str      },
  Abstract    = 2, //{ basis:        &'t str      },
  PlaceHolder = 3,
  Free        = 4
}
