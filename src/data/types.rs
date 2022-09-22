/*!

The representation of types.




 */

#[derive(Copy, Clone, PartialOrd, Eq, PartialEq, Hash, Debug, Default, Primitive)]
pub enum Type {
  #[default]
  Undefined = 0,  // Or Atom
  Bool      = 1,
  Number    = 2,
  Char      = 3,
  List      = 4,
  Comma     = 5,
  Arrow     = 6,
  Void      = 7,
  Wrong     = 8,
  Bind      = 9,
  Type      = 10, // User defined type
  Strict    = 11,
  Alias     = 12,
  New       = 13,
}




#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}
