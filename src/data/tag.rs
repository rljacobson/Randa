/*!

  Tags for tagged values.

*/

use num_traits::{FromPrimitive, ToPrimitive};

// Keep in sync with `#[repr(u32)]` for Tag.
pub type TagRepresentationType = u32;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug, Primitive)]
#[repr(u32)]
pub enum Tag {
  Atom            = 0,
  Double          = 1,
  DataPair        = 2,
  FileInfo        = 3, // `FileInfo` differs from `DataPair` in that `str&` in hd will be made
                       // relative to current directory on dump/undump
  TypeVar         = 4,
  Int             = 5,
  Constructor     = 6,
  StrCons         = 7,
  Id              = 8,
  Ap              = 9,
  Lambda          = 10,
  Cons            = 11,
  Tries           = 12,
  Label           = 13,
  Show            = 14,
  StartReadValues = 15,
  Let             = 16,
  LetRec          = 17,
  Share           = 18,
  Lexer           = 19,
  Pair            = 20,
  Unicode         = 21,
  TCons           = 22,
  // Miranda stores `char*`'s in the head of a `DataPair` (with 0 in the tail). Instead, we store an index into a
  // string vector, splitting a `usize` across `head` and `tail`.
  String          = 23,
}


#[cfg(test)]
mod tests {

  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }

}
