/*!



 */


mod lexer;
mod parser;
pub mod token;
pub mod errors;
mod value;
mod token_wrapper;

use logos::Logos;

use saucepan::Source;
pub use token::Token;
use errors::LexError;
use crate::data::{
  TOKEN_BASE,
  COMBINATOR_BASE,
  ATOM_LIMIT
};

pub type Loc = std::ops::Range<u32>;



#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}
