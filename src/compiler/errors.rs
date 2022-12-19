/*!

This module needs some work.

*/

use std::fmt::{Display, Formatter};
use std::error::Error;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum LexerError {
  EOF,
  BlankErr,
  UnknownError(Rc<dyn Error>)
}

impl LexerError {



}

impl Display for LexerError {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      LexerError::EOF => {
        write!(f, "encountered EOF\n")
      }

      LexerError::BlankErr => {
        write!(f, "formal text not delimited by blank line\n")
      }

      LexerError::UnknownError(e) => {
        write!(f, "unknown error: {}", e)
      }
    }
  }
}


impl Error for LexerError {

}


pub fn emit_error(e: LexerError) {
  println!("Error: {}", e);
}
