use std::error::Error;
use std::io;
use std::process::exit;

use console::{style, Term};
use thiserror::Error;

/// Reports dump/bytecode deserialization failures from `src/bytecode_parser.rs` and `src/vm/bytecode.rs`.
#[derive(Debug, Error)]
pub enum BytecodeDecodeError {
    #[error("The binary file was compiled on an incompatible architecture.")]
    ArchitectureMismatch,
    #[error("Wrong bytecode version.")]
    WrongBytecodeVersion,
    #[error("Wrong source file.")]
    WrongSourceFile,
    #[error("File ended unexpectedly.")]
    UnexpectedEof {
        #[source]
        source: Option<io::Error>,
    },
    #[error("Malformed definition encountered.")]
    MalformedDefinition,
}

impl BytecodeDecodeError {
    pub fn unexpected_eof() -> Self {
        Self::UnexpectedEof { source: None }
    }

    pub fn unexpected_eof_with_source(source: io::Error) -> Self {
        Self::UnexpectedEof {
            source: Some(source),
        }
    }
}

/// Reports alias-install conflicts from `src/vm/aliases.rs` during dump loading.
#[derive(Debug, Error)]
pub enum AliasInstallError {
    #[error("Aliasing causes name clashes")]
    DestinationNameClash,
}

/// Reports source file lookup and read failures from `src/vm/load.rs`.
#[derive(Debug, Error)]
pub enum SourceInputError {
    #[error("Source file not found: {path}")]
    MissingFile { path: String },
    #[error("Unable to open source file: {path}")]
    UnreadableFile {
        path: String,
        #[source]
        source: io::Error,
    },
}

/// Reports source-form classification and syntax failures in the load pipeline.
#[derive(Debug, Error)]
pub enum SourceParseError {
    #[error("Source parsing is not yet integrated for: {path}")]
    UnsupportedTopLevelForm { path: String },
    #[error("Source contains syntax errors: {path}")]
    SyntaxErrorsPresent { path: String },
}

/// Reports `%export` validation and export-closure failures from `src/vm/load.rs`.
#[derive(Debug, Error)]
pub enum ExportValidationError {
    #[error("Malformed export file list entry")]
    MalformedPathList,
    #[error("Illegal fileid in export list (not included in script): {path}")]
    PathNotIncludedInScript { path: String },
    #[error("Illegal fileid in export list (ambiguous): {path}")]
    AmbiguousPathRequest { path: String },
    #[error("Name in export list is not defined in this script or its included files: {name}")]
    UndefinedExportedIdentifier { name: String },
    #[error("Export closure blocked by undefined names")]
    BlockedByUndefinedNames,
}

/// Reports `%include` materialization and modifier-application failures from `src/vm/load.rs`.
#[derive(Debug, Error)]
pub enum IncludeDirectiveError {
    #[error("Included source contains syntax errors: {path}")]
    SyntaxErrorsPresent { path: String },
    #[error("Include modifier references a name not defined in included file: {name}")]
    ModifierTargetNotFound { name: String },
    #[error("Illegal include suppression of typename: {name}")]
    IllegalTypeNameSuppression { name: String },
    #[error("Included graph still needs typenames not visible after modifier application: {names:?}")]
    MissingVisibleTypeNames { names: Vec<String> },
    #[error("Include rename target already defined: {name}")]
    RenameDestinationClash { name: String },
    #[error("Included file defines repeated non-synonym types in one scope: {path} ({names:?})")]
    RepeatedTypeClash { path: String, names: Vec<String> },
}

/// Reports semantic and type failures from the typecheck boundary in `src/vm/typecheck.rs`.
#[derive(Debug, Error)]
pub enum TypecheckError {
    #[error("Typecheck phase found undefined names ({count})")]
    UndefinedNames { count: usize },
    #[error("Typecheck phase found invalid %free bindings ({count})")]
    InvalidFreeBindings { count: usize },
    #[error("Typecheck phase found missing %free bindings ({count})")]
    MissingFreeBindings { count: usize },
    #[error("Typecheck phase found undefined typenames ({count})")]
    UndefinedTypeNames { count: usize },
    #[error("Typecheck phase found non-typename references in type expressions ({count})")]
    NonTypeIdentifiersInTypeExpr { count: usize },
    #[error("Typecheck phase found typename arity mismatches ({count})")]
    TypeArityMismatch { count: usize },
    #[error("Typecheck phase found abstract typenames without bindings ({count})")]
    UnboundAbstractTypeNames { count: usize },
    #[error("Typecheck phase found specified but not defined names ({count})")]
    SpecifiedButNotDefined { count: usize },
    #[error("Typecheck phase found typenames used as identifiers ({count})")]
    TypeNamesUsedAsIdentifiers { count: usize },
    #[error("Typecheck phase found undeclared constructors in formals ({count})")]
    UndeclaredConstructorsInFormals { count: usize },
    #[error("Typecheck phase found constructor arity mismatches in formals ({count})")]
    ConstructorArityMismatchInFormals { count: usize },
    #[error("Typecheck phase found non-canonical binary plus patterns in formals ({count})")]
    NonCanonicalPlusPatternsInFormals { count: usize },
    #[error("Typecheck phase found unary minus patterns in formals ({count})")]
    UnaryMinusPatternsInFormals { count: usize },
    #[error("Typecheck phase found malformed plus applications in formals ({count})")]
    MalformedPlusApplicationsInFormals { count: usize },
    #[error("Typecheck phase found malformed minus applications in formals ({count})")]
    MalformedMinusApplicationsInFormals { count: usize },
    #[error("Typecheck phase found invalid canonical successor patterns in formals ({count})")]
    InvalidSuccessorPatternsInFormals { count: usize },
    #[error("Typecheck phase found value-headed applications in formals ({count})")]
    ValueHeadApplicationsInFormals { count: usize },
    #[error("Typecheck phase found non-identifier application heads in formals ({count})")]
    NonIdentifierApplicationHeadsInFormals { count: usize },
}

/// Reports codegen precondition failures from the codegen boundary in `src/vm/codegen.rs`.
#[derive(Debug, Error)]
pub enum CodegenError {
    #[error("Codegen phase requires at least one loaded file")]
    NoLoadedFiles,
    #[error("Initialization load contains unresolved errors")]
    InitializationBlockedByUnresolvedNames,
}

/// Reports dump file creation, write, sync, and rename failures from `src/vm/load.rs`.
#[derive(Debug, Error)]
pub enum DumpWriteError {
    #[error("Unable to write dump file: {path}")]
    WriteFailed {
        path: String,
        #[source]
        source: io::Error,
    },
}

/// Wraps dump-load subsystem failures at the `VM::load_script` orchestration boundary.
#[derive(Debug, Error)]
pub enum LoadScriptError {
    #[error(transparent)]
    Decode(#[from] BytecodeDecodeError),
    #[error(transparent)]
    AliasInstall(#[from] AliasInstallError),
}

/// Wraps source-load subsystem failures at the `VM::load_file` orchestration boundary.
#[derive(Debug, Error)]
pub enum LoadFileError {
    #[error(transparent)]
    SourceInput(#[from] SourceInputError),
    #[error(transparent)]
    Parse(#[from] SourceParseError),
    #[error(transparent)]
    IncludeDirective(#[from] IncludeDirectiveError),
    #[error(transparent)]
    ExportValidation(#[from] ExportValidationError),
    #[error(transparent)]
    Typecheck(#[from] TypecheckError),
    #[error(transparent)]
    Codegen(#[from] CodegenError),
    #[error(transparent)]
    DumpWrite(#[from] DumpWriteError),
}

/// Wraps source-load and dump-load failures at the startup boundary used by `src/main.rs`.
#[derive(Debug, Error)]
pub enum StartupLoadError {
    #[error(transparent)]
    LoadFile(#[from] LoadFileError),
    #[error(transparent)]
    LoadScript(#[from] LoadScriptError),
}

pub fn emit_error(e: &dyn Error) {
    let term = Term::stderr();
    // term.write_fmt(format_args!("{} {}\n", style("Error:").red(), e)).ok();
    term.write_line(format!("{} {}", style("Error:").red(), e).as_str())
        .ok();
}

pub fn fatal_error(msg: &str) -> ! {
    let term = Term::stderr();
    term.write_line(format!("{} {}", style("Fatal Error: []").red(), msg).as_str())
        .ok();
    exit(1);
}
