/*!

Setup and orchestration of built-ins, compilation, etc. Because everything lives in the heap,
it's tempting to put all of this in `Heap`. We separate concerns by factoring out non-generic heap
manipulation, environment initialization, and compilation from generic heap functions, accessors, etc.

 */
mod aliases;
mod bytecode;
mod diagnostics;
mod init;
mod load;
mod state_reset;
mod ui;

use std::{
    fs::File,
    io::{BufRead, BufReader, Read},
    time::{SystemTime, UNIX_EPOCH},
};

use console::{pad_str, style, Alignment, Term};
use num_traits::FromPrimitive;

use crate::bytecode_parser::{
    get_i16_le, get_machine_word, get_u16_le, get_word_f64, get_word_raw_value,
    parse_filename_modified_time, parse_string,
};
use crate::compiler::bytecode::Bytecode;
use crate::compiler::ParserDiagnostic;
use crate::data::api::IdentifierDefinitionData;
use crate::{
    compiler::Token,
    constants::{DEFAULT_SPACE, WORD_SIZE, XVERSION},
    data::{
        api::{
            AliasEntry, ConsList, ConstructorRef, DataPair, FileInfoRef, FileRecord,
            HeapObjectProxy, IdentifierCoreData, IdentifierCoreRef, IdentifierDefinitionRef,
            IdentifierRecordRef, IdentifierValueData, IdentifierValueRef, IdentifierValueTypeKind,
            OpenFile, PrivateNameRef, TypeIdentifierValueParts,
        },
        path::*,
        Combinator, Heap, RawValue, Tag, Type, Value,
    },
    errors::{fatal_error, BytecodeError},
    options::{make_version_string, setup_argument_parser, Options},
};

const NIL: Value = Value::Combinator(Combinator::Nil);

pub enum ActivityReset {
    Collecting,
    Loading { blank_error: bool, unlink: bool },
    Making { make_status: bool },
}

/// Holds the state of the current session, including the heap and compilation state.
///
/// Setup and initialization of [`Heap`](crate::data::heap::Heap) occurs in
/// [`VM::default()`](crate::data::heap::Heap::default()),
/// [`VM::setup_constants()`](crate::data::heap::Heap::setup_constants()), and
/// [`VM::setup_standard_types()`](crate::data::heap::Heap::setup_standard_types()).
pub struct VM {
    // region State that doesn't live on the heap
    /// The `options` function should be produced by the `setup_argument_parser()` function.
    pub(crate) options: Options,

    terminal: Option<Term>,

    /// There are two types of Miranda process: "compiling" (the main process) and subsidiary processes
    /// launched for each evaluation. The `compiling  flag tells us which kind of process we are in.
    // Todo: How is this state thread local?
    compiling: bool,
    initializing: bool,
    making: bool,
    loading: bool,

    activity: ActivityReset,
    command_mode: bool,

    sorted: bool, // Are the file definitions sorted?
    in_export_list: bool,
    in_semantic_redeclaration: bool,
    rv_script: bool,    // Flags readvals in use (for garbage collector)
    unused_types: bool, // There are orphaned types (Miranda's TORPHANS)

    in_file: Option<Box<dyn BufRead>>,

    /// Tells editor where to find error: `errline` contains location of 1st error in main script, `errs` is `hereinfo`
    /// of up to one error in `%insert` script (each is 0 if not set). Some errors can set both.
    error_line: usize,
    errs: Vec<RawValue>, // Todo: What is this holding? It is `char[24]` in Miranda.
    parser_diagnostics: Vec<ParserDiagnostic>,

    last_expression: Value, // A reference to the last expression evaluated.
    private_symbol_base_index: usize, // The index into the private_symbol stack marking the beginning of the current file's private symbols.
    include_depth: u32, // How many `%include`'s deep is the current file. (Miranda's `ideep`.)

    // endregion
    heap: Heap,

    // region Flags and other state that lives on the heap.

    // list of currently open-for-input files, of form `cons(strcons(stream,<ptr to element of 'files'>),...)`
    file_queue: ConsList<OpenFile>, // Miranda's `file_q`
    // ToDo: Why is this on the heap? Can we just use a vector?
    prefix_stack: ConsList, // Paths are made relative to current file. If we descend into another file, need to record
    // prefix.
    col_fn: Value,
    embargoes: Value,
    eprodnts: Value,
    exportfiles: Value,
    exports: Value,
    fnts: Value,       // `fnts` is flag indicating %bnf in use. Treated as Value?
    aliases: ConsList, // Not `IdentifierRecordRef`, because `hold` is not an `Identifier`.
    free_ids: ConsList<IdentifierRecordRef>,
    // Miranda's DETROP, list of illegal `%free` actual bindings from `bindparams`.
    // Elements are either an identifier (`name not %free`) or `cons(id, datapair(fa, ta))`
    // for wrong-kind/wrong-arity `==` bindings.
    detritus_parameter_bindings: ConsList,
    // Miranda's MISSING, list of missing `%free` formal bindings from `bindparams`.
    // Elements are formal original-name datapairs.
    missing_parameter_bindings: ConsList,
    // Miranda's FBS, stack/list of formal-binding sets introduced by parameterized includes.
    // `bindparams` pushes raw formals list entries; include/type phases may later attach hereinfo.
    free_binding_sets: ConsList,
    internals: ConsList<IdentifierRecordRef>, // list of names not exported, used by fix/unfixexports
    idsused: ConsList<IdentifierRecordRef>,
    clashes: ConsList<IdentifierRecordRef>,
    suppressed: ConsList<IdentifierValueRef>, // list of `-id` aliases successfully obeyed (raw targets; may be non-ID)
    suppressed_t: ConsList<IdentifierRecordRef>, // list of -typename aliases (illegal just now)
    ihlist: Value,
    includees: ConsList<FileRecord>,
    lasth: Value,
    last_name: Value,
    lexdefs: Value,
    nonterminals: Value,
    ntmap: Value,
    ntspecmap: Value,

    /// A cons list of identifiers in the primitive environment.
    primitive_environment: ConsList<IdentifierRecordRef>,

    /**
    The cons list `files` is also called the environment in the Miranda source code.

    From Miranda:

      > `files` is a cons list of elements, each of which is of the form
      >   `cons(cons(fileinfo(filename,mtime),share),definienda)`
      > where `share` (=0,1) says if repeated instances are shareable. Current script at
      > the front followed by subsidiary files due to `%insert` and `%include` elements due
      > to `%insert` have `self.nill` `definienda` (they are attributed to the inserting script).

    The "definienda" is itself a cons list of items (types, identifiers, etc.) that are defined in the current file.
    (A definiendum is a term that is being defined or clarified. The plural form of definiendum is definienda.)
     */
    files: ConsList<FileRecord>,
    old_files: ConsList<FileRecord>, // most recent set of sources, in case of interrupted or failed compilation
    // list of a list of files to be unloaded if `mkincludes` is interrupted (Miranda's ld_stuff).
    mkinclude_files: ConsList<ConsList<FileRecord>>,

    sui_generis_constructors: ConsList, // user defined sui-generis constructors (Miranda's SGC)
    type_abstractions: ConsList,        // cons list of abstype declarations (Miranda's TABSTR)
    undefined_names: ConsList<IdentifierRecordRef>, // undefined names used in script (Miranda's ND)
    new_type_names: ConsList<IdentifierRecordRef>, // newly declared type names in current code unit (Miranda's `newtyps`)
    algebraic_show_functions: ConsList, // show functions of algebraic types in scope (Miranda's `algshfns`)
    special_show_forms: ConsList, // all occurrences of special forms (show) encountered during type check

    // `spec_location` is a list of `cons(id,hereinfo)` giving location of `spec` for ids both defined and specified.
    // Needed to locate errs in `meta_tcheck`, `abstr_mcheck`.
    spec_location: ConsList,

    // endregion

    // region Constants
    common_stdin: Value,
    common_stdinb: Value,
    concat: IdentifierRecordRef,
    cook_stdin: Value,
    diagonalise: IdentifierRecordRef,
    main_id: IdentifierRecordRef,
    message: IdentifierRecordRef,
    showabstract: IdentifierRecordRef,
    showbool: IdentifierRecordRef,
    showchar: IdentifierRecordRef,
    showfunction: IdentifierRecordRef,
    showlist: IdentifierRecordRef,
    shownum1: IdentifierRecordRef,
    showpair: IdentifierRecordRef,
    showparen: IdentifierRecordRef,
    showstring: IdentifierRecordRef,
    showvoid: IdentifierRecordRef,
    showwhat: IdentifierRecordRef,
    stdout: Value,

    // These might be constants
    indent_fn: IdentifierRecordRef,
    listdiff_fn: IdentifierRecordRef,
    outdent_fn: IdentifierRecordRef,

    // Common compound types.
    numeric_function_type: Value,
    numeric_function2_type: Value,
    boolean_function_type: Value,
    boolean_function2_type: Value,
    char_list_type: Value,
    string_function_type: Value,
    range_step_type: Value,
    range_step_until_type: Value,

    pub(crate) nill: Value,
    void_: IdentifierRecordRef,

    // This is test-only instrumentation that captures load_file phase order so we can verify orchestration
    // branches and sequencing while parser/typecheck/codegen internals are still deferred.
    #[cfg(test)]
    last_load_phase_trace: Vec<&'static str>,
    // endregion
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum ParsePhaseStatus {
    Parsed,
    SyntaxError,
}

#[derive(Copy, Clone, Debug)]
struct ParsePhaseOutcome {
    status: ParsePhaseStatus,
    files: ConsList<FileRecord>,
}

impl Default for VM {
    fn default() -> Self {
        VM::build(setup_argument_parser())
    }
}

#[cfg(test)]
mod tests;

impl VM {
    /// Setup and initialization of [`Heap`](crate::data::heap::Heap) occurs in
    /// [`VM::build()`](crate::vm::VM::build()),
    /// [`VM::setup_constants()`](crate::vm::VM::setup_constants()), and
    /// [`VM::setup_standard_types()`](crate::vm::VM::setup_standard_types()).
    fn build(options: Options) -> Self {
        let mut vm = VM {
            // region State that doesn't live on the heap
            options,

            terminal: None,

            compiling: true,
            initializing: true, // Checked in `undump(..)`
            making: false,
            loading: false,

            activity: ActivityReset::Collecting, // Arbitrary value.
            command_mode: false,
            sorted: false,
            // type check
            in_export_list: false,
            in_semantic_redeclaration: false,
            rv_script: false,    // Flags readvals in use (for garbage collector)
            unused_types: false, // There are orphaned types
            in_file: None,
            error_line: 0,
            errs: vec![],
            parser_diagnostics: vec![],
            last_expression: Value::None, // A reference to the last expression evaluated.
            private_symbol_base_index: 0,
            include_depth: 0,
            // endregion
            heap: Heap::default(),

            // region Flags and other state that lives on the heap.
            file_queue: ConsList::EMPTY,
            prefix_stack: ConsList::EMPTY,
            listdiff_fn: IdentifierRecordRef::UNINITIALIZED,
            indent_fn: IdentifierRecordRef::UNINITIALIZED,
            outdent_fn: IdentifierRecordRef::UNINITIALIZED,

            // Common compound types.
            numeric_function_type: Value::Uninitialized,
            numeric_function2_type: Value::Uninitialized,
            boolean_function_type: Value::Uninitialized,
            boolean_function2_type: Value::Uninitialized,
            char_list_type: Value::Uninitialized,
            string_function_type: Value::Uninitialized,
            range_step_type: Value::Uninitialized,
            range_step_until_type: Value::Uninitialized,

            nill: Value::Uninitialized,
            void_: IdentifierRecordRef::UNINITIALIZED,

            primitive_environment: ConsList::EMPTY,

            col_fn: Value::None,
            embargoes: NIL,
            eprodnts: NIL,
            exportfiles: NIL,
            exports: NIL,
            fnts: NIL,
            aliases: ConsList::EMPTY,
            free_ids: ConsList::EMPTY,
            detritus_parameter_bindings: ConsList::EMPTY,
            missing_parameter_bindings: ConsList::EMPTY,
            free_binding_sets: ConsList::EMPTY,
            internals: ConsList::EMPTY,
            idsused: ConsList::EMPTY,
            clashes: ConsList::EMPTY,
            suppressed: ConsList::EMPTY,
            suppressed_t: ConsList::EMPTY,
            ihlist: Value::None,
            includees: ConsList::EMPTY,
            lasth: Value::None,
            last_name: Value::None,
            lexdefs: NIL,
            nonterminals: NIL,
            ntmap: NIL,
            ntspecmap: NIL,

            sui_generis_constructors: ConsList::EMPTY, // user defined sui-generis constructors (Miranda's SGC)
            type_abstractions: ConsList::EMPTY, // cons list of abstype declarations (Miranda's TABSTR)
            undefined_names: ConsList::EMPTY,   // undefined names used in script (Miranda's ND)
            new_type_names: ConsList::EMPTY, // newly declared type names in current code unit (Miranda's `newtyps`)
            algebraic_show_functions: ConsList::EMPTY, // show functions of algebraic types in scope (Miranda's `algshfns`)
            special_show_forms: ConsList::EMPTY, // all occurrences of special forms (show) encountered during type check
            spec_location: ConsList::EMPTY, // list giving location of `spec` for ids both defined and specified.

            files: ConsList::EMPTY, // The cons list files is also called the environment
            old_files: ConsList::EMPTY, // most recent set of sources, in case of interrupted or failed compilation
            mkinclude_files: ConsList::EMPTY, // list of files to be unloaded if `mkincludes` is interrupted (Miranda's ld_stuff).

            // endregion

            // region Constants set to `Value::Uninitialized`/`IdentifierRecordRef::UNINITIALIZED`
            common_stdin: Value::Uninitialized,
            common_stdinb: Value::Uninitialized,
            concat: IdentifierRecordRef::UNINITIALIZED,
            cook_stdin: Value::Uninitialized,
            diagonalise: IdentifierRecordRef::UNINITIALIZED,
            main_id: IdentifierRecordRef::UNINITIALIZED,
            message: IdentifierRecordRef::UNINITIALIZED,
            showabstract: IdentifierRecordRef::UNINITIALIZED,
            showbool: IdentifierRecordRef::UNINITIALIZED,
            showchar: IdentifierRecordRef::UNINITIALIZED,
            showfunction: IdentifierRecordRef::UNINITIALIZED,
            showlist: IdentifierRecordRef::UNINITIALIZED,
            shownum1: IdentifierRecordRef::UNINITIALIZED,
            showpair: IdentifierRecordRef::UNINITIALIZED,
            showparen: IdentifierRecordRef::UNINITIALIZED,
            showstring: IdentifierRecordRef::UNINITIALIZED,
            showvoid: IdentifierRecordRef::UNINITIALIZED,
            showwhat: IdentifierRecordRef::UNINITIALIZED,
            stdout: Value::Uninitialized,
            #[cfg(test)]
            last_load_phase_trace: vec![],
            // endregion
        };

        vm.mira_setup();
        if vm.options.verbose {
            vm.terminal = Some(Term::stdout());
        }
        if !vm.options.make_file_list.is_empty() {
            vm.making = true;
        }

        vm
    }

    pub fn new() -> Self {
        Self::default()
    }

    #[cfg(test)]
    pub(crate) fn new_for_tests() -> Self {
        Self::build(Options::default())
    }

    pub(crate) fn run_startup(&mut self) -> Result<(), BytecodeError> {
        self.initializing = false;
        let script = self.options.script.clone();
        self.undump(&script)
    }
}
