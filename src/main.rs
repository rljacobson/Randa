#![feature(arbitrary_enum_discriminant)]
#![feature(pattern)]

#[macro_use]
extern crate enum_primitive_derive;
extern crate num_traits;

mod lex;
mod errors;
// mod driver;
mod data;


use lex::{
  Lexer,
  Source
};
use crate::data::Token;
use crate::errors::LexError;

fn main() {

  let path = "examples/hanoi.m";

  println!("Starting...");

  let file_contents = std::fs::read_to_string(path).unwrap();
  let source = Source::new(path, file_contents.as_str());

  let mut lexer: Lexer = Lexer::from_span(source.source_span());

  match lexer.yylex() {
    Ok(token) => {
      println!("{}", token);
    }

    Err(e) => {
      println!("{}", e);
    }
  }




}
