/*!

# Bytecode representation of Miranda code.

`w` means `sizeof(word)` in bytes. For 64-bit, `w=8`.

| internal heap object        | external file rep - char sequence         |                         |
| --------------------------- | ----------------------------------------- | ----------------------- |
| 0..127                      | `self`                                    |                         |
| 128..383                    | `CHAR_X (self-128)`                       |                         |
| 384..`ATOMLIMIT`-1          | `(self-256)`                              |                         |
| integer (-127..127)         | `SHORT_X <byte>`                          |                         |
| integer                     | `INT_X <4n bytes> (-1)`                   |                         |
| double                      | `DBL_X <8 bytes>`                         |                         |
| unicode_char                | `UNICODE_X <4 bytes>`                     |                         |
| `typevar`                   | `TVAR_X <byte>`                           |                         |
| `ap(x, y)`                  | `[x] [y] AP_X`                            |                         |
| `cons(x, y)`                | `[y] [x] CONS_X`                          |                         |
| `id` (=occurrence)          | `ID_X <string terminated by '\0'>`        |                         |
| `pname` (=occurrence)       | `PN_X <2 bytes>`                          |                         |
|                             | `PN1_X <4 bytes>`                         |                         |
| `datapair(string, 0)`       | `AKA_X <string...\0>`                     |                         |
| `fileinfo(script, line_no)`&nbsp;&nbsp; | `HERE_X <string...\0> <2 bytes>`     (**) |                         |
| `constructor(n, x)`         | `[x] CONSTRUCT_X <2 bytes>`               |                         |
| `readvals(h, t)`            | `[t] RV_X`                                |                         |
| definition                  | `[val] [type] [who] [id] DEF_X`           |                         |
|                             | `[val] [pname] DEF_X`                     |                         |
| definition-list             | `[definition*] DEF_X`                     |                         |
| filename                    | `<string terminated by '\0'>`             |                         |
| `mtime`                     | `<w bytes>`                               |                         |
| complete script             | `__WORDSIZE`                              |                         |
|                             | `XVERSION`                                |                         |
|                             | `[ [filename]`                            |                         |
|                             | &nbsp;&nbsp;`[mtime]`                     |                         |
|                             | &nbsp;&nbsp;`[shareable]`                 | (=0 or 1)               |
|                             | &nbsp;&nbsp;`[definition-list] ]+`        |                         |
|                             | `'\0'`                                    |                         |
|                             | `[definition-list]`                       | (algshfns)              |
|                             | `[ND] or [True]`                          | (see below)             |
|                             | `DEF_X`                                   |                         |
|                             | `[freeids]`                               |                         |
|                             | `DEF_X`                                   |                         |
|                             | `[definition-list]`                       | (internals)             |
| type-error script           | `__WORDSIZE`                              |                         |
|                             | `XVERSION`                                |                         |
|                             | `'\1'`                                    |                         |
|                             | `<w bytes>`                               | (=errline)              |
|                             | `…`                                       | (rest as normal script) |
| syntax-error script         | `__WORDSIZE`                              |                         |
|                             | `XVERSION`                                |                         |
|                             | `'\0'`                                    |                         |
|                             | `<w bytes>`                               | (=errline)              |
|                             | `[ [filename]`                            |                         |
|                             | &nbsp;&nbsp;`[mtime] ]+`                  |                         |


# Notes

First filename in dump must be that of `current_script` (ie the
   main source file).  All path names in dump are correct wrt the
   directory of the main source.

(**) empty string is abbreviation for current filename in `hereinfo`.
  `True` in `ND` position indicates an otherwise correct dump whose exports
  include type orphans.

*/

use std::fmt::{Display, Formatter};

use enum_primitive_derive::Primitive;

use crate::data::ATOM_LIMIT;

pub const X_BASE : isize = ATOM_LIMIT - 256;
pub const X_LIMIT: isize = X_BASE + 16;

pub static BYTECODE_NAMES: [&str; 16] = [
  "CHAR_X",
  "SHORT_X",
  "INT_X",
  "DBL_X",
  "ID_X",
  "AKA_X",
  "HERE_X",
  "CONSTRUCT_X",
  "RV_X",
  "PN_X",
  "PN1_X",
  "DEF_X",
  "AP_X",
  "CONS_X",
  "TVAR_X",
  "UNICODE_X"
];


#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Primitive)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum Bytecode {
  Char         = 221, // = X_BASE +  0, CHAR_X
  Short        = 222, // = X_BASE +  1, SHORT_X
  Integer      = 223, // = X_BASE +  2, INT_X
  Double       = 224, // = X_BASE +  3, DBL_X
  ID           = 225, // = X_BASE +  4, ID_X
  AKA          = 226, // = X_BASE +  5, AKA_X
  Here         = 227, // = X_BASE +  6, HERE_X
  Construct    = 228, // = X_BASE +  7, CONSTRUCT_X
  ReadVals     = 229, // = X_BASE +  8, RV_X
  PrivateName  = 230, // = X_BASE +  9, PN_X
  PrivateName1 = 231, // = X_BASE + 10, PN1_X
  Definition   = 232, // = X_BASE + 11, DEF_X
  Apply        = 233, // = X_BASE + 12, AP_X
  Cons         = 234, // = X_BASE + 13, CONS_X
  TypeVariable = 235, // = X_BASE + 14, TVAR_X
  Unicode      = 236, // = X_BASE + 15, UNICODE_X
}


impl Bytecode {
  pub fn code(&self) -> u8 {
    *self as u8
  }
}


impl Display for Bytecode {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "‹{}›", BYTECODE_NAMES[(self.code() as isize - X_BASE) as usize]) // "«{}»"
  }
}





#[cfg(test)]
mod tests {

  use num_traits::FromPrimitive;
  use super::*;

  #[test]
  fn codec_test(){
    // First decode.
    let bc: Vec<Bytecode> = (X_BASE..X_LIMIT).map(|n| Bytecode::from_isize(n).unwrap_or_else(| | {
      println!("Cannot convert {}", n);
      Bytecode::Char
    })).collect();
    for code in bc{
      println!("{}: {}", code, code.code()); // encode and print string,
    }
    // println!("{{{}}}", bc.join(", "));
  }

}
