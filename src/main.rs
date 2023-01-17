#![allow(ambiguous_associated_items)]
#![allow(dead_code)]
#![feature(arbitrary_enum_discriminant)]
#![feature(pattern)]


mod errors;
mod data;
mod compiler;
mod vm;
mod options;
mod constants;
mod algorithms;
mod bytecode_parser;


use crate::vm::VM;

fn main() {

  let mut vm: VM = VM::new();

  vm.announce();

  // println!("\n{}", vm.options);
}

/*
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
*/
