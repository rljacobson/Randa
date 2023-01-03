/*!
There are mild impedance mismatches between libraries. Logos returns ranges as "spans". Saucepan can make a
`saucepan::Span` from a range for a `Source` using slice notation: `let span: Span = source[range];`. OTOH, so can
Logos: `text[range]` gives the token as a slice. The advantage of using Saucepan is that Saucepan keeps track of
multiple files.

Bison has distinct notions of token and value. A value wraps a token. Rules produce values. For us, values will be
AST nodes.

*/


pub mod token;
pub mod errors;
mod lexer;
mod parser;


use std::collections::HashMap;
pub use saucepan::Span;
pub use token::Token;
pub use lexer::Lexer;

pub type Loc = std::ops::Range<u32>;

// Todo: Why only these keywords? This list is from steer.c, line ~1582.
pub static KEYWORDS: HashMap<&str, u8> = HashMap::from([
  ("abstype", 21),
  ("div", 8),
  ("if", 15),
  ("mod", 8),
  ("otherwise", 15),
  ("readvals", 31),
  ("show", 23),
  ("type", 22),
  ("where", 15),
  ("with", 21)
]);
