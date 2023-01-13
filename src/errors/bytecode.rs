/*!

Errors associated with loadfile, unload, dump, undump, etc.

 */

use std::fmt::{Display, Formatter};

use crate::data::Value;

/// This is Miranda's `BAD_DUMP`. Most of these errors are produced in `VM::load_file()`.
#[derive(Debug, Eq, PartialEq)]
pub enum BytecodeError {
  NameClash(Value),     // BAD_DUMP = -2, Holds a cons list of clashing aliases, Miranda's CLASHES
  ArchitectureMismatch, // BAD_DUMP = -1 "(unrecognised dump format)"
  WrongBytecodeVersion, // BAD_DUMP = -1 "(unrecognised dump format)"
  WrongSourceFile,      // BAD_DUMP = 1 "(wrong source file)"
  UnexpectedEOF,        // BAD_DUMP = 2
  MalformedDef,         // BAD_DUMP = 3 "badly formed def in dump\n"
                        // BAD_DUMP = 4  should unsetids, possibly not an error?
}

impl Display for BytecodeError {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      BytecodeError::ArchitectureMismatch => {
        write!(f, "The binary file was compiled on an incompatible architecture.")
      }
      BytecodeError::WrongBytecodeVersion => {
        write!(f, "Wrong bytecode version.")
      }
      BytecodeError::NameClash(clashes) => {
        write!(f, "Aliasing causes name clashes: {:?}", clashes)
      }
      BytecodeError::UnexpectedEOF => {
        write!(f, "File ended unexpectedly.")
      }
      BytecodeError::WrongSourceFile => {
        write!(f, "Wrong source file.")
      }
      BytecodeError::MalformedDef => {
        write!(f, "Malformed definition encountered.")
      }
    }
  }
}
