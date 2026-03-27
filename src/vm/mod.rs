/*!

Setup and orchestration of built-ins, compilation, etc. Because everything lives in the heap,
it's tempting to put all of this in `Heap`. We separate concerns by factoring out non-generic heap
manipulation, environment initialization, and compilation from generic heap functions, accessors, etc.

 */
mod aliases;
mod bytecode;
mod codegen;
mod diagnostics;
mod init;
mod load;
mod state_reset;
mod typecheck;
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
use crate::compiler::{ParserDiagnostic, ParserTopLevelScriptPayload};
use crate::data::api::IdentifierDefinitionData;
use crate::{
    compiler::Token,
    constants::{DEFAULT_SPACE, WORD_SIZE, XVERSION},
    data::{
        api::{
            AliasEntry, ConsList, ConstructorRef, DataPair, FileInfoRef, FileRecord,
            FreeFormalBindingRef, HeapObjectProxy, IdentifierCoreData, IdentifierCoreRef,
            IdentifierDefinitionRef, IdentifierRecordRef, IdentifierValueData, IdentifierValueRef,
            IdentifierValueTypeKind, OpenFile, PrivateNameRef, TypeIdentifierValueParts,
        },
        path::*,
        Combinator, Heap, RawValue, Tag, Type, Value,
    },
    errors::{
        fatal_error, AliasInstallError, BytecodeDecodeError, CodegenError, DumpWriteError,
        ExportValidationError, IncludeDirectiveError, LoadFileError, LoadScriptError,
        SourceInputError, SourceParseError, StartupLoadError, TypecheckError,
    },
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
/// [`VM::build()`](crate::vm::VM::build()),
/// [`VM::setup_constants()`](crate::vm::VM::setup_constants()), and
/// [`VM::setup_standard_types()`](crate::vm::VM::setup_standard_types()).
pub struct VM {
    // region State that doesn't live on the heap
    /// The `options` field should be produced by the `setup_argument_parser()` function.
    pub(crate) options: Options,

    terminal: Option<Term>,

    /// There are two types of Miranda process: "compiling" (the main process) and subsidiary
    /// processes launched for each evaluation. The `compiling` flag tells us which kind of
    /// process we are in.
    // Todo: How is this state thread local?
    compiling: bool,
    initializing: bool,
    making: bool,
    loading: bool,

    activity: ActivityReset,
    command_mode: bool,

    /// Are the file definitions sorted?
    sorted: bool,
    /// True while dump visibility rewriting is processing an explicit `%export` list.
    export_list_is_active: bool,
    in_semantic_redeclaration: bool,
    /// True when readvals state is live and must be preserved for GC/rooting.
    readvals_script_is_active: bool,
    /// There are orphaned types (Miranda's `TORPHANS`).
    unused_types: bool,

    in_file: Option<Box<dyn BufRead>>,

    /// Tells the editor where to find syntax/load errors:
    /// `error_line` records the first main-script line, while `error_locations`
    /// stores heap-backed secondary locations such as `%insert`-origin errors.
    error_line: usize,
    error_locations: Vec<RawValue>,
    parser_diagnostics: Vec<ParserDiagnostic>,

    /// A reference to the last expression evaluated.
    last_expression: Value,
    /// The index into the private_symbol stack marking the beginning of the current file's private symbols.
    private_symbol_base_index: usize,
    /// How many `%include`'s deep is the current file. (Miranda's `ideep`.)
    include_depth: u32,

    // endregion
    
    heap: Heap,

    // region Flags and other state that lives on the heap.

    /// List of currently open-for-input files, of the form
    /// `cons(strcons(stream,<ptr to element of 'files'>),...)`.
    /// Miranda's `file_q`.
    file_queue: ConsList<OpenFile>,
    // TODO: Why is this on the heap? Can we just use a vector?
    /// Paths are made relative to current file. If we descend into another file, need to record prefix.
    prefix_stack: ConsList,
    bnf_column_function: Value,
    export_embargoes: Value,
    empty_production_nonterminals: Value,
    export_paths: Value,
    exported_identifiers: Value,
    /// Miranda's `fnts`, a heap-backed `%bnf`-enabled flag.
    bnf_enabled: Value,
    /// Not `IdentifierRecordRef`, because `hold` is not an `Identifier`.
    aliases: ConsList,
    /// Miranda `%free` formals, each `cons(id, cons(datapair(original_name, 0), type))`.
    free_identifiers: ConsList<FreeFormalBindingRef>,
    /// Miranda's `DETROP`, the list of illegal `%free` actual bindings from `bindparams`.
    /// Elements are either an identifier (`name not %free`) or `cons(id, datapair(fa, ta))`
    /// for wrong-kind or wrong-arity `==` bindings.
    detritus_parameter_bindings: ConsList<Value>,
    /// Miranda's `MISSING`, the list of missing `%free` formal bindings from `bindparams`.
    /// Elements are formal original-name datapairs.
    missing_parameter_bindings: ConsList<Value>,
    /// Miranda's `FBS`, the stack/list of formal-binding sets introduced by parameterized includes.
    /// `bindparams` pushes formal-list references as heap values; include/type phases may later attach hereinfo.
    free_binding_sets: ConsList<ConsList<FreeFormalBindingRef>>,
    /// List of names not exported, used by `fixexports`/`unfixexports`.
    internals: ConsList<IdentifierRecordRef>,
    used_identifiers: ConsList<IdentifierRecordRef>,
    clashes: ConsList<IdentifierRecordRef>,
    /// List of `-id` aliases successfully obeyed (raw targets; may be non-ID).
    suppressed: ConsList<IdentifierValueRef>,
    /// List of `-typename` aliases, currently illegal.
    suppressed_type_aliases: ConsList<IdentifierRecordRef>,
    inherited_attributes: Value,
    included_files: ConsList<FileRecord>,
    last_diagnostic_location: Value,
    last_identifier: Value,
    lex_rule_definitions: Value,
    nonterminals: Value,
    nonterminal_map: Value,
    nonterminal_specification_map: Value,

    /// A cons list of identifiers in the primitive environment.
    primitive_environment: ConsList<IdentifierRecordRef>,

    /// The cons list `files` is also called the environment in the Miranda source code.
    ///
    /// From Miranda:
    ///
    /// `files` is a cons list of elements, each of which is of the form
    /// `cons(cons(fileinfo(filename,mtime),share),definienda)`
    /// where `share` (=0,1) says if repeated instances are shareable. Current script at
    /// the front followed by subsidiary files due to `%insert` and `%include`; elements due
    /// to `%insert` have `self.nill` `definienda` (they are attributed to the inserting script).
    ///
    /// The "definienda" is itself a cons list of items (types, identifiers, etc.) that are defined
    /// in the current file. (A definiendum is a term that is being defined or clarified. The plural
    /// form of definiendum is definienda.)
    files: ConsList<FileRecord>,
    /// Most recent set of sources, in case of interrupted or failed compilation.
    old_files: ConsList<FileRecord>,
    /// List of file groups to unload if `mkincludes` is interrupted. Miranda's `ld_stuff`.
    include_rollback_files: ConsList<ConsList<FileRecord>>,

    /// User-defined sui-generis constructors. Miranda's `SGC`.
    sui_generis_constructors: ConsList,
    /// Cons list of `abstype` declarations. Miranda's `TABSTR`.
    type_abstractions: ConsList,
    /// Undefined names used in the script. Miranda's `ND`.
    undefined_names: ConsList<IdentifierRecordRef>,
    /// Newly declared type names in the current code unit. Miranda's `newtyps`.
    new_type_names: ConsList<IdentifierRecordRef>,
    /// Show functions of algebraic types in scope. Miranda's `algshfns`.
    algebraic_show_functions: ConsList,
    /// All occurrences of special forms (`show`) encountered during type check.
    special_show_forms: ConsList,

    /// `spec_location` is a list of `cons(id,hereinfo)` giving the location of `spec`
    /// for IDs both defined and specified. Needed to locate `error_locations` in
    /// `meta_tcheck` and `abstr_mcheck`.
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
    bool_show_function: IdentifierRecordRef,
    char_show_function: IdentifierRecordRef,
    function_show_function: IdentifierRecordRef,
    list_show_function: IdentifierRecordRef,
    internal_number_show_function: IdentifierRecordRef,
    pair_show_function: IdentifierRecordRef,
    paren_show_function: IdentifierRecordRef,
    string_show_function: IdentifierRecordRef,
    void_show_function: IdentifierRecordRef,
    showwhat: IdentifierRecordRef,
    stdout: Value,

    // These might be constants.
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

    /// Test-only instrumentation that captures `load_file` phase order so we can verify
    /// orchestration branches and sequencing while parser/typecheck/codegen internals are
    /// still deferred.
    #[cfg(test)]
    last_load_phase_trace: Vec<&'static str>,
    // endregion
}

#[derive(Clone, Debug)]
struct ParsePhaseOutcome {
    parsed_without_error: bool,
    files: ConsList<FileRecord>,
    top_level_payload: Option<ParserTopLevelScriptPayload>,
}

impl Default for VM {
    fn default() -> Self {
        VM::build(Options::default())
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
            export_list_is_active: false,
            in_semantic_redeclaration: false,
            readvals_script_is_active: false, // Flags readvals in use (for garbage collector)
            unused_types: false,              // There are orphaned types
            in_file: None,
            error_line: 0,
            error_locations: vec![],
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

            bnf_column_function: Value::None,
            export_embargoes: NIL,
            empty_production_nonterminals: NIL,
            export_paths: NIL,
            exported_identifiers: NIL,
            bnf_enabled: NIL,
            aliases: ConsList::EMPTY,
            free_identifiers: ConsList::EMPTY,
            detritus_parameter_bindings: ConsList::EMPTY,
            missing_parameter_bindings: ConsList::EMPTY,
            free_binding_sets: ConsList::EMPTY,
            internals: ConsList::EMPTY,
            used_identifiers: ConsList::EMPTY,
            clashes: ConsList::EMPTY,
            suppressed: ConsList::EMPTY,
            suppressed_type_aliases: ConsList::EMPTY,
            inherited_attributes: Value::None,
            included_files: ConsList::EMPTY,
            last_diagnostic_location: Value::None,
            last_identifier: Value::None,
            lex_rule_definitions: NIL,
            nonterminals: NIL,
            nonterminal_map: NIL,
            nonterminal_specification_map: NIL,

            sui_generis_constructors: ConsList::EMPTY, // user defined sui-generis constructors (Miranda's SGC)
            type_abstractions: ConsList::EMPTY, // cons list of abstype declarations (Miranda's TABSTR)
            undefined_names: ConsList::EMPTY,   // undefined names used in script (Miranda's ND)
            new_type_names: ConsList::EMPTY, // newly declared type names in current code unit (Miranda's `newtyps`)
            algebraic_show_functions: ConsList::EMPTY, // show functions of algebraic types in scope (Miranda's `algshfns`)
            special_show_forms: ConsList::EMPTY, // all occurrences of special forms (show) encountered during type check
            spec_location: ConsList::EMPTY, // list giving location of `spec` for ids both defined and specified.

            files: ConsList::EMPTY, // The cons list files is also called the environment
            old_files: ConsList::EMPTY, // most recent set of sources, in case of interrupted or failed compilation
            include_rollback_files: ConsList::EMPTY, // list of files to be unloaded if `mkincludes` is interrupted (Miranda's ld_stuff).

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
            bool_show_function: IdentifierRecordRef::UNINITIALIZED,
            char_show_function: IdentifierRecordRef::UNINITIALIZED,
            function_show_function: IdentifierRecordRef::UNINITIALIZED,
            list_show_function: IdentifierRecordRef::UNINITIALIZED,
            internal_number_show_function: IdentifierRecordRef::UNINITIALIZED,
            pair_show_function: IdentifierRecordRef::UNINITIALIZED,
            paren_show_function: IdentifierRecordRef::UNINITIALIZED,
            string_show_function: IdentifierRecordRef::UNINITIALIZED,
            void_show_function: IdentifierRecordRef::UNINITIALIZED,
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

    /// Constructs a VM with default options instead of parsing process command-line arguments.
    /// This exists so programmatic callers and integration tests can drive source loading without inheriting the current process argv.
    /// The invariant is that VM initialization matches `Options::default()`.
    pub fn new() -> Self {
        Self::default()
    }

    /// Constructs a VM from the process command-line arguments.
    /// This exists so the binary entrypoint keeps Miranda's command-line startup behavior separate from direct programmatic construction.
    /// The invariant is that command-line flags are parsed exactly once at VM startup.
    pub fn from_command_line() -> Self {
        Self::build(setup_argument_parser())
    }

    /// Returns the VM options used to initialize this session.
    /// This exists so the binary can report startup configuration without reaching into VM internals through the library boundary.
    /// The invariant is that the returned reference reflects the exact options captured when the VM was constructed.
    pub fn options(&self) -> &Options {
        &self.options
    }

    pub fn run_startup(&mut self) -> Result<(), StartupLoadError> {
        self.initializing = false;
        let script = self.options.script.clone();
        self.undump(&script)
    }
}
