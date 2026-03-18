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
    #[error("Source file not found: {path}")]
    MissingSourceFile { path: String },
    #[error("Unable to open source file: {path}")]
    UnreadableSourceFile {
        path: String,
        #[source]
        source: io::Error,
    },
    #[error("Source parsing is not yet integrated for: {path}")]
    ParserIntegrationDeferred { path: String },
    #[error("Source contains syntax errors: {path}")]
    SyntaxErrorInSource { path: String },
    #[error("Malformed export file list entry")]
    MalformedExportFileList,
    #[error("Illegal fileid in export list (not included in script): {path}")]
    ExportFileNotIncludedInScript { path: String },
    #[error("Illegal fileid in export list (ambiguous): {path}")]
    ExportFileAmbiguous { path: String },
    #[error("Typecheck phase found undefined names ({count})")]
    TypecheckUndefinedNames { count: usize },
    #[error("Typecheck phase found invalid %free bindings ({count})")]
    TypecheckInvalidFreeBindings { count: usize },
    #[error("Typecheck phase found missing %free bindings ({count})")]
    TypecheckMissingFreeBindings { count: usize },
    #[error("Typecheck phase found undefined typenames ({count})")]
    TypecheckUndefinedTypeNames { count: usize },
    #[error("Typecheck phase found non-typename references in type expressions ({count})")]
    TypecheckNonTypeIdentifiersInTypeExpr { count: usize },
    #[error("Typecheck phase found typename arity mismatches ({count})")]
    TypecheckTypeArityMismatch { count: usize },
    #[error("Typecheck phase found abstract typenames without bindings ({count})")]
    TypecheckUnboundAbstractTypeNames { count: usize },
    #[error("Typecheck phase found specified but not defined names ({count})")]
    TypecheckSpecifiedButNotDefined { count: usize },
    #[error("Typecheck phase found typenames used as identifiers ({count})")]
    TypecheckTypeNamesUsedAsIdentifiers { count: usize },
    #[error("Export closure blocked by undefined names")]
    ExportClosureBlockedByUndefinedNames,
    #[error("Codegen phase requires at least one loaded file")]
    CodegenWithoutLoadedFiles,
    #[error("Initialization load contains unresolved errors")]
    InitializationLoadContainsErrors,
    #[error("Unable to write dump file: {path}")]
    DumpWriteFailed {
        path: String,
        #[source]
        source: io::Error,
    },
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
