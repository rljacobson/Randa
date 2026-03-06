/*!

Errors associated with loadfile, unload, dump, undump, etc.

 */

use std::io;
use thiserror::Error;

/// This is Miranda's `BAD_DUMP`. Most of these errors are produced in `VM::load_file()`.
#[derive(Debug, Error)]
pub enum BytecodeError {
    #[error("Aliasing causes name clashes")]
    NameClash, // BAD_DUMP = -2, Holds a cons list of clashing aliases, Miranda's CLASHES
    #[error("The binary file was compiled on an incompatible architecture.")]
    ArchitectureMismatch, // BAD_DUMP = -1 "(unrecognised dump format)"
    #[error("Wrong bytecode version.")]
    WrongBytecodeVersion, // BAD_DUMP = -1 "(unrecognised dump format)"
    #[error("Wrong source file.")]
    WrongSourceFile, // BAD_DUMP = 1 "(wrong source file)"
    #[error("File ended unexpectedly.")]
    UnexpectedEOF {
        // BAD_DUMP = 2
        #[source]
        source: Option<io::Error>,
    },
    #[error("Malformed definition encountered.")]
    MalformedDef, // BAD_DUMP = 3 "badly formed def in dump\n"
    // BAD_DUMP = 4  should unsetids, possibly not an error?
    #[error("A nonexistent symbol was referenced.")]
    SymbolNotFound, // References a symbol that doesn't exist.
}

impl BytecodeError {
    pub fn unexpected_eof() -> Self {
        Self::UnexpectedEOF { source: None }
    }

    pub fn unexpected_eof_with_source(source: io::Error) -> Self {
        Self::UnexpectedEOF {
            source: Some(source),
        }
    }
}
