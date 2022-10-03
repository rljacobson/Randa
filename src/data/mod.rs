/*!

The `data` module collects things having to do with representing (encoding/decoding) _values_. Everything is stored
on a heap. The heap holds `HeapCell`s, which have the form
  ┌─────┬───────────┬───────────┐
  │ Tag │ HeadValue │ TailValue │
  └─────┴───────────┴───────────┘
A `Tag` has representation `TagRepresentationType`, which will probably be a `u32`. The head and tail
values will have type `Value` (an enum) in client code, which is represented internally and on the
heap by a `ValueRepresentationType`, a `usize`. With very little effort, `ValueRepresentationType`
could probably be a `u32`, but this is not what Miranda does on 64-bit systems, so neither do we.

As is common in functional programming languages, Miranda treats code as data, storing what is essentially the AST
of the code on the heap with the rest of the data. In the context of code, a HeapCell can be thought of as a bytecode
consisting of an instruction stored in the `tag` and up to two arguments, one in `head` and one in `tail`. Larger
pieces of code are built up using composition, where references to nested instructions are stored in `head`/`tail`.

Miranda stores the addresses of C-style (null-terminated) strings as values when it needs to reference a string. We
instead intern strings and store the interned string's handle as values.

TODO: Strings should be reference counted.

Which variant of the enum `Value` a value of type `ValueRepresentationType` represents can be determined by which of
the ranges in the table below the value falls into. Thus, values do not require tags in their representation on the
heap. (There are some exceptions, see below.) The `Value` enum exists as a convenience and to enforce type safety
outside of the heap implementation details.

  From Miranda docs/source code:
  All Miranda values are of type word

  | 0..ATOM_LIMIT-1      | atoms, made up as follows                              |
  |:---------------------|:-------------------------------------------------------|
  | 0..255               | the Latin 1 character set (0..127 ascii)               |
  | 256..CMBASE-1        | lexical tokens (see rules.y)                           |
  | CMBASE..ATOM_LIMIT-1 | combinators and special values, e.g. NIL (see combs.h) |

  The first pointer value is `ATOM_LIMIT`. Values >= `ATOM_LIMIT` are indexes into the heap. The
  heap is held as three arrays: `tag[]`, `hd[]`, `tl[]`. The arrays `word *hd,*tl` are offset
  so they are indexed from `ATOM_LIMIT`. The array `char *tag` holds type info and is indexed
  from 0. The type info stored in `tag[0]..tag[ATOM_LIMIT-1]` are all 0, meaning ATOM. See
  `setupheap()` in data.c.

Our ranges differ from Miranda's ranges, because we use more lexical tokens where Miranda uses single characters
instead, e.g. `+`.

Unfortunately, there are some exceptions to the value range scheme. In some cases, a `HeapCell` may contain other
data in `head` or `tail` besides what is described by the value ranges above. For example, a `HeapCell` `cell` with
`cell.tag == Tag::Ap` can have either a reference or a `Type` in `cell.head`. So long as `Type` is encoded as a
number less than `ATOM_LIMIT`, references and `Type`s can be distinguished.

A better scheme would be to encode three tags in the `HeapCell::tag`. There is more than enough room to do so.
However, Miranda does it this way, so we do to.


*/

pub mod combinator;
pub mod tag;
pub mod types;
mod identifier;
mod values;
mod heap;


use saucepan::LineNumber;

pub use crate::{
  compiler::Token,
  data::{
    combinator::Combinator,
    types::Type,
    values::Value,
    identifier::*,
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
pub struct File<'t> {
  name      : &'t str,
  mtime     : &'t str, // type?
  sharable  : bool,   // repeated instances are shareable?
  definienda: u32     // type??
}
