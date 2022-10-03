/*!

The representation of types.

NOTES:
  user defined types are represented by Miranda identifiers (of type "type"),
  generic types (e.g. "**") by Miranda numbers, and compound types are
  built up with AP nodes, e.g. "a->b" is represented by 'ap2(arrow_t,a,b)'
  Applying bind_t to a type variable, thus: ap(bind_t,tv), indicates that
  it is not to be instantiated. Applying strict_t to a type represents the
  '!' operator of algebraic type definitions.

 */

use crate::data::{Value, ValueRepresentationType};

#[derive(Copy, Clone, PartialOrd, Eq, PartialEq, Hash, Debug, Default, Primitive)]
#[repr(usize)]
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


impl From<Type> for Value {
  fn from(type_value: Type) -> Self {
    Value::Data(type_value as ValueRepresentationType)
  }
}
