mod lexical;
mod bytecode;
mod heap_objects;

use std::error::Error;
use std::process::exit;


use console::{style, Term};

pub use lexical::LexError;
pub use bytecode::BytecodeError;

pub fn emit_error(e: &dyn Error) {
  let term = Term::stderr();
  // term.write_fmt(format_args!("{} {}\n", style("Error:").red(), e)).ok();
  term.write_line(format!("{} {}", style("Error:").red(), e).as_str()).ok();
}

pub fn fatal_error(msg: &str) -> ! {
  let term = Term::stderr();
  term.write_line(format!("{} {}", style("Fatal Error: []").red(), msg).as_str()).ok();
  exit(1);
}
