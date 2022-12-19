#![allow(ambiguous_associated_items)]
#![feature(arbitrary_enum_discriminant)]
#![feature(pattern)]

#[macro_use]
extern crate enum_primitive_derive;
extern crate num_traits;


mod data;
mod compiler;
mod vm;

fn main() {
  /*
  There are mild impedance mismatches between libraries. Logos returns ranges as "spans". Saucepan can make a
  `saucepan::Span` from a range for a `Source` using slice notation: `let span: Span = source[range];`. OTOH, so can
  Logos: `text[range]` gives the token as a slice. The advantage of using Saucepan is that Saucepan keeps track of
  multiple files.

  Bison has distinct notions of token and value. A value wraps a token. Rules produce values. For us, values will be
  AST nodes.
  */
}


#[cfg(test)]
mod tests {
  use logos::{Logos, Lexer as LogosLexer};

  use saucepan::Source;
  use crate::compiler::{Token, Lexer};
  use crate::compiler::errors::LexerError;

  #[test]
  /// Tests the Logos lexer attached to the `Token` enum. Does not test anything in `lex.rs` at all.
  fn lexer_test() {
    let path = "examples/hanoi.m";

    println!("Starting...");

    let file_contents = std::fs::read_to_string(path).unwrap();
    let source = Source::new(path, file_contents.as_str());

    let mut lexer = Lexer::new(&source);

    loop {
      match lexer.yylex() {

        Ok(Token::Error) => {
          println!("Error Token.");
          break;
        }

        Ok(Token::EOF) => {
          println!("No next token.");
          break;
        }

        Ok(token) => {
          println!("{}", token);
        }

        Err(e) => {
          println!("ERROR: {}", e);
          break;
        }

      }
    }
  }
}
