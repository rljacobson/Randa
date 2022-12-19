/*!

A minimal wrapper around a Logos lexer to satisfy the Bison parser interface.

 */

use std::ops::Range;
use logos::{Logos, Lexer as LogosLexer, SpannedIter};
use saucepan::{Source, Span};


use super::errors::LexerError;
use super::Token;


pub struct Lexer<'t> {
  token_iterator: SpannedIter<'t, Token>,
  source: &'t Source<'t, 't>,
  span: Range<usize>, //Span<'t, 't>,
  token: Token,
}


impl<'t> Lexer<'t> {
  pub fn new(source: &'t Source<'t, 't>) -> Lexer<'t> {
    // A filler value for span.
    //let span = source.slice(0..0);
    let token_iterator = Token::lexer(source.text()).spanned();
    Lexer{
      token_iterator, //: Token::lexer(source.text()).spanned(),
      source,
      span: 0..0,
      token: Token::EmptySymbol
    }
  }


  pub fn yylex(&mut self) -> Result<Token, LexerError> {
    let (token, span) =
      match self.token_iterator.next() {

        Some((t, s)) => (t, s),

        None => {
          let n = self.source.len();
          (Token::EOF, (n-1..n))
        }
      };

    self.span = span;
    self.token = token;

    Ok(token)
  }


  /// Returns the last token.
  pub fn token(&self) -> Token {
    self.token
  }


  /// Returns the span of the last token.
  pub fn span(&self) -> Span {
    self.source.slice(self.span.clone())
  }

}



#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}
