/*
 *   Copyright (c) 2022
 *   All rights reserved.
 */
/*!

Randa Lexer, a mediator between the Bison parser and the Logos lexer, which is attacked to the `Token` enum. The
Lexer struct has-a Logos lexer and holds state for context sensitive lexing.

*/
#![allow(dead_code)]

use std::{
  collections::HashSet,
  str::pattern::Pattern
};

use lazy_static::lazy_static;
use saucepan::{ColumnNumber, Location, Slice, Span};
pub use saucepan::Source;
use saucepan::span::nom_impls::InputTake;

use crate::{
  compiler::{
    errors::{
      LexError,
      // emit_error
    }
  }
};
use crate::compiler::Token;


// todo: Figure out how to handle MIRALIB.
#[allow(unused_variables)]
pub static MIRALIB: &str = "./miralib/";


#[allow(unused_variables)]
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug, Default)]
pub struct Options {
  echoing  : bool,
  listing  : bool,
  verbosity: bool,
  magic    : bool,
  lit_main : bool, // flags "literate" comment convention
  literate : bool,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum LexMode {
  None           = 0,
  LexRules       = 1,
  LexDefinitions = 2,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum BNFMode {
  None = 0,
  Rule = 1,
  Def  = 2,
}

#[allow(unused_variables)]
#[derive(Debug)]
pub struct Lexer<'n, 't>{
  input: Span<'n, 't>,  // The input that has not yet been lexed.

  command_mode: bool,
  options     : Options,
  lex_mode    : LexMode,
  bnf_mode    : BNFMode,


  // Margin
  margin_stack: Vec<ColumnNumber>,
  left_margin : ColumnNumber,      // used to enforce the offside rule

  // Collections
  identifiers  : HashSet<&'t str>,
  private_names: HashSet<&'t str>,

  // ToDo: Why does the lexer need a heap?
  // Miranda heap.
  // heap: Heap,
}

impl<'n, 't> Lexer<'n, 't> {

  pub fn from_span(input: Span<'n, 't>) -> Self {
    Self {
      input,

      command_mode : false,
      options      : Options::default(),
      lex_mode     : LexMode::None,
      bnf_mode     : BNFMode::None,
      margin_stack : vec![],
      left_margin  : ColumnNumber(1),
      identifiers  : Default::default(),
      private_names: Default::default(),
      // heap         : Default::default(),
    }
  }

  fn set_left_margin(&mut self){
    self.margin_stack.push(self.left_margin);
    if self.left_margin < self.column(){
      self.left_margin = self.column();
    }
  }


  fn unset_left_margin(&mut self) {
    if self.margin_stack.is_empty() { return; }
    self.left_margin = self.margin_stack.pop().unwrap();
  }


  /// Gives the current column number of the input.
  fn column(&self) -> ColumnNumber {
    self.input.column().unwrap()
  }


  /// Checks if the current input starts with the given string.
  fn starts_with<'p, P: Pattern<'p>>(&'p self, pattern: P) -> bool
  // where 'n:'p, 't:'p
  {
    self.input.fragment().starts_with(pattern)
  }


  /// Removes n bytes from the input.
  fn chop(&mut self, n: usize){
    self.input = self.input.slice(n..);
  }

  /*
    Called by the parser to get the next token.
  */

  pub fn yylex(&mut self) -> Result<Token, LexError>{

    // Context sensitive syntax errors are handled where they are encountered, not here,
    // contrary to lex.c.

    self.eat_hashbang();
    self.eat_whitespace();
    if self.starts_with('\n') {
      // We can only see a newline here if we're in command mode, in which case we treat it like EOF.
      return Err(LexError::EOF);
    }

    // Check offside condition.
    if self.column() < self.left_margin {
      if self.starts_with('=')
         && (
              self.margin_stack.is_empty()
              || self.column() >= *self.margin_stack.last().unwrap()
            )
      {
        self.chop(1);

        return Ok(Token::ElseEqual);
      } else {
        return Ok(Token::Offside);
      }
    }
    // Offside condition corner case.
    if self.starts_with(';') {
      self.chop(1);
      self.eat_whitespace();

      if self.starts_with('=')
          && (
        self.margin_stack.is_empty()
            || self.column() >= *self.margin_stack.last().unwrap()
      )
      {
        self.chop(1);

        return Ok(Token::ElseEqual);
      } else {
        return Ok(Token::Semicolon);
      }
    }

    // Identifiers
    if let Some(id_span) = self.parse_identifier(false){
      if self.lex_mode == LexMode::LexRules {
        self.eat_whitespace();

        // store the name as an identifier.
        self.identifiers.insert(id_span.fragment());

        // Store the identifier on the stack.
        yylval = name();

        if self.starts_with('=') {
          self.identifiers.insert(id_span.fragment());
          return Ok(Token::LexDef);
        } else {
          if is_constructor() {
            CNAME
          } else {
            NAME
          }
        }

      }
    }






    Ok(Token::Begin)
  }


  /// Eats a hashbang magic line.
  fn eat_hashbang(&mut self) {
    if self.starts_with("#!") {
      let (_, rest) = self.input.split_at_position_complete::<_, ()>(|c| c == '\n' || c == '\r').unwrap();
      self.input = rest;
    }
  }

  /// Parses identifiers
  fn parse_identifier(&mut self, underlined: bool) -> Option<Span<'t, 't>> {
    lazy_static! {
      static ref IDENTIFIER_PATTERN: Regex = Regex::new("\\A[a-zA-Z][a-zA-Z0-9_']*").unwrap();
      static ref UNDERLINED_IDENTIFIER_PATTERN: Regex = Regex::new("\\A_[a-zA-Z0-9_']*").unwrap();
    }

    let matches = match underlined {
      true => UNDERLINED_IDENTIFIER_PATTERN.find(self.input.fragment()),
      false => IDENTIFIER_PATTERN.find(self.input.fragment()),
    };

    match matches {

      Some(result) => {
        let (identifier, rest) = self.input.take_split(result.end());
        self.input = rest;
        Some(identifier)
      }

      None => {
        None
      }

    }


  }


  /// Eats spaces and tabs and, if we are not in command mode, newlines.
  /// Also eats comments.
  fn eat_whitespace(&mut self) {
    let mut rest = self.input;

    loop {
      // Eat whitespace
      let (_, rhs) = rest.split_at_position_complete::<_, ()>(
        |c| {
          !(
            c == ' '
                || (c == '\n' && !self.command_mode)
                || c == '\t'
          )
        }
      ).unwrap(); // Complete version is infallible.

      rest = rhs;

      // Eat EOL comments.
      if rest.fragment().starts_with("||"){
        let (_, rhs) = rhs.split_at_position_complete::<_, ()>(|c| c!='\n').unwrap();
        rest = rhs;
        // We must loop again in case there is more whitespace or another comment line.
      } else {
        break;
      }
    }

    self.input = rest;
  }

}


mod utilities {
  use crate::errors::LexError;
  use super::*;

  /// Makes file path, turning `<thing>` into `/path/to/miralib/thing`, stripping quotes, and
  /// appending extension if the boolean argument is true.
  fn make_file_path(input: &str, append_extension: bool) -> String {
    let mut path =
        if input.starts_with('<') && input.ends_with('>') {
          // The form `<libname>` expands to `/path/to/miralib/libname.m`.
          format!("{}/{}", MIRALIB, &input[1..input.len() - 1])
        } else if input.starts_with('"') && input.ends_with('"') {
          // A quoted path is "expanded" to just the path without quotes.
          input[1..input.len() - 1].to_string()
        } else {
          input.to_string()
        };

    if append_extension && !path.ends_with(".m") {
      // The input can end in `.x` or `.`, which need to be pruned first.
      if path.ends_with(".x") {
        path = path[0..path.len() - 2].to_string();
      } else if path.ends_with('.') {
        path = path[0..path.len() - 1].to_string();
      }

      path.push_str(".m");
    }

    // todo: What is the point of `dic`?
    // dic.push(path);

    return path;
  }

  /// Is the filename that of a literate program?
  fn lit_name(name: &str) -> bool {
    name.ends_with(".lit.m")
  }


  /// Trims whitespace up to newline, with error message if no newline found.
  fn strip_ws(input: &str) -> Result<&str, LexError> {
    let result =
        input.strip_prefix(|c| c == ' ' || c == '\t').unwrap_or(input);

    if !result.starts_with('\n') {
      // emit_error(LexError::BlankErr);
      Err(LexError::BlankErr)
    } else {
      Ok(result)
    }
  }

}
