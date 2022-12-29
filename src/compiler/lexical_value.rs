/*!

This module provides the `LexicalValue` type required by the Bison parser. The `LexicalValue` enum is what is returned by
parser actions. They may wrap tokens or whatever user-defined values the parser author wants. The generated parser
code can "unwrap" to the contained type by the parser author specifying `$<VariantName>1` (where `1` stands in for
whatever token number is being referenced) by calling `VariantName::from(value) -> InnerType`. Implement this with
modules, one for each unwrappable type.

todo: Merge with `crate::data::values::LexicalValue`.


 */

// use super::Token;
use crate::compiler::token::Token;
use crate::data::Combinator;
// use crate::data::Combinator;

/// Enum that represents all kinds of values that can be returned
/// from parser derivations.
///
/// This values has to be in a single enum, because LALR parsers
/// have a stack, and it's better for it to be heterogeneous.
#[derive(Clone, Debug)]
pub enum LexicalValue {
  /// Required variant, parser expects it to be defined
  None,
  /// Required variant, parser expects it to be defined
  Uninitialized,
  /// Required variant, parser expects it to be defined
  Stolen,

  /// Required variant, parser expects it to be defined.
  /// Represents a token that is returned from a Lexer
  Token(Token),

  /// Represents a number
  Combinator(Combinator),
}

impl Default for LexicalValue {
  fn default() -> Self {
    Self::Stolen
  }
}

impl LexicalValue {
  /// Required method, parser expects it to be defined.
  ///
  /// Constructor for `LexicalValue::Token(token)` variant.
  pub(crate) fn from_token(value: Token) -> Self {
    Self::Token(value)
  }

  pub(crate) fn new_uninitialized() -> Self {
    Self::Uninitialized
  }

  pub(crate) fn is_uninitialized(&self) -> bool {
    matches!(self, Self::Uninitialized)
  }
}

/*
macro_rules! impl_value_unwrapper {
  ($variant:ident, $inner_ty:ty) => {
    #[allow(non_snake_case)]
    pub mod $variant {
      use super::LexicalValue;

      pub(crate) fn from(value: LexicalValue) -> $inner_ty {
        match value {
          LexicalValue::$variant(out) => out,
          other => panic!("wrong type, expected {}, got {:?}", stringify!($variant), other),
        }
      }
    }
  }
}


impl_value_unwrapper!(Token, Token);
impl_value_unwrapper!(Combinator, Combinator);
*/
