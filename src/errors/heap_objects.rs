/*!

Errors related to the manipulation of objects on the heap. These errors are used in `data::api`.

 */


use std::error::Error;
use std::fmt::{Display, Formatter};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum HeapObjectError {
  Malformed,
}

impl Display for HeapObjectError {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      HeapObjectError::Malformed => {
        write!(f, "heap object is malformed")
      }
    } // end match self
  }
}

impl Error for HeapObjectError {

}



#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}
