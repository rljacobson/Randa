use std::fmt::{Display, Formatter};
use std::error::Error;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum LexError{
  EOF,
  BlankErr,
  UnknownError(Rc<dyn Error>)
}

impl LexError {



}

impl Display for LexError {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      LexError::EOF => {
        write!(f, "encountered EOF\n")
      }

      LexError::BlankErr => {
        write!(f, "formal text not delimited by blank line\n")
      }

      LexError::UnknownError(e) => {
        write!(f, "unknown error: {}", e)
      }
    }
  }
}


impl Error for LexError {

}


pub fn emit_error(e: LexError) {
  println!("Error: {}", e);
}
