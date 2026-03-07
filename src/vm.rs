/*!

Setup and orchestration of built-ins, compilation, etc. Because everything lives in the heap,
it's tempting to put all of this in `Heap`. We separate concerns by factoring out non-generic heap
manipulation, environment initialization, and compilation from generic heap functions, accessors, etc.

 */

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
use crate::data::api::IdentifierDefinitionData;
use crate::{
    compiler::Token,
    constants::{DEFAULT_SPACE, WORD_SIZE, XVERSION},
    data::{
        api::{
            AliasEntry, ConsList, DataPair, FileInfoRef, FileRecord, HeapObjectProxy,
            IdentifierCoreData, IdentifierCoreRef, IdentifierDefinitionRef, IdentifierRecordRef,
            IdentifierValueData, IdentifierValueRef, IdentifierValueTypeData,
            IdentifierValueTypeKind, IdentifierValueTypeRef, OpenFile,
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
    internals: ConsList<IdentifierRecordRef>, // list of names not exported, used by fix/unfixexports
    idsused: ConsList<IdentifierRecordRef>,
    clashes: ConsList<IdentifierRecordRef>,
    suppressed: ConsList<IdentifierRecordRef>, // list of `-id` aliases successfully obeyed, used for error reporting
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

    /// Loads the object file for `source_path` if it exists and is modified after `source_path`. Otherwise calls
    /// `load_file` for `source_path`.
    fn undump(&mut self, source_path: &str) -> Result<(), BytecodeError> {
        // This does not guarantee that the path ends in ".m" after the if block.
        if !source_path.ends_with(".m") && !self.initializing {
            // Except for prelude, only .m files have dumps.
            // Todo: Then should not there not be a not on initializing?
            return self.load_file(source_path);
        }

        // Change "source.m" into "source.x".
        // The if statement above does not guarantee that the path ends in ".m".
        // Include the dot before the extension to avoid a case like "them" --> "thex".
        let binary_path: String = format!(
            "{}{}",
            source_path.strip_suffix(".m").unwrap_or(source_path),
            ".x"
        );

        // region Check Failure to Load Bytecode
        // If we cannot load the binary file for the source file, then we have to do the entire `loadfile(..)`.
        if let Ok(source_metadata) = std::fs::metadata(source_path) {
            // Source file exists.
            let source_modified_time: SystemTime = source_metadata.modified().unwrap_or_else(
                // If we can't determine the time the source was modified, be conservative and assume it
                // was just modified.
                |_| SystemTime::now(),
            );

            if let Ok(binary_metadata) = std::fs::metadata(source_path) {
                // binary file exists.
                let binary_modified_time = binary_metadata
                    .modified()
                    // If we can't determine the time the binary was modified, be conservative and assume it
                    // was in the 1970s.
                    .unwrap_or_else(|_| SystemTime::UNIX_EPOCH);

                if binary_modified_time < source_modified_time {
                    // Can't use the binary.
                    return self.load_file(source_path);
                }
            }
        } else {
            // Source file does not exist. Delete binary if it exists.
            std::fs::remove_file(binary_path).ok();
            // Can't use the binary.
            return self.load_file(source_path);
        }

        // Lastly, if we cannot open the binary, we have to do the full `load_file`.
        let in_file: File = match File::open(&binary_path) {
            Ok(f) => f,
            Err(..) => {
                return self.load_file(source_path);
            }
        };

        // endregion

        // You have to `unload` before you can `load`.
        // Todo: Implement unload
        self.unload();

        #[cfg(feature = "debug")]
        if !self.initialising {
            println!("undumping from {}", binary_path);
        }

        // If this is the main script…
        let is_main_script: bool = !self.initializing && !self.making;
        if is_main_script {
            // Todo: Figure out how these signal handlers work.
            //       Probably use https://docs.rs/crossbeam-channel/latest/crossbeam_channel/
            // Can't take interrupt during load_script, so disable them.
            // sigflag = 0,
            // oldsig = signal(SIGINT, (sighandler) sigdefer);
        }

        let load_result = self.load_script(
            &in_file,
            source_path.to_string(),
            ConsList::EMPTY,
            NIL,
            is_main_script,
        );
        drop(in_file); // Close the file.

        // Restore interrupt handler
        if is_main_script {
            // Restore interrupt handler
            // Todo: Figure out how these signal handlers work.
            // (void) signal(SIGINT, oldsig);
        }
        // Todo: Once signal handlers are implemented, see if we actually need sigflag.
        /*
        if sigflag {
          // If a signal fired during loading, we handle it now.
          sigflag = 0;
          (*oldsig)(); // Take deferred interrupt.
        }
        */

        // Handle errors
        let dump_error_encountered: bool = // the result of the following match
    match load_result {

      Ok(files) => { self.files = files; false }

      Err(BytecodeError::ArchitectureMismatch)
      | Err(BytecodeError::WrongBytecodeVersion)
        => {
        std::fs::remove_file(&binary_path).ok();
        self.unload();
        true
      }

      Err(BytecodeError::NameClash) => {
        if self.include_depth == 0 {
          let sorted = self.alfasort(self.clashes);
          println!("Cannot load {} due to name clashes: {}", binary_path, self.printlist(sorted));
        }
        self.unload();
        self.loading = false;
        return load_result.map(|_| ());
      }

      Err(_bytecode_error) => {
        std::fs::remove_file(&binary_path).ok();
        self.unload();
        // Since we store clashes in the error, there is no equivalent for us. This might change when we figure out GC.
        // CLASHES = self.nill;
        // stackp = dstack;
        true
      }

    }; // end match on load_result

        #[cfg(feature = "debug")]
        if !self.initialising {
            println!(
                "{} undumped, success={}",
                binary_path, dump_error_encountered
            );
        }

        // If any of the files were modified during the dump, reload them.
        if dump_error_encountered || self.source_update_check() {
            self.load_file(source_path)?;
        } else if self.initializing {
            // Nested if causes us to break out of outer if block
            if !self.undefined_names.is_empty() || self.files.is_empty() {
                // There is an error in the dump of the prelude.
                // Todo: These errors should be propagated from their source instead of divined from the tea leaves of the flags.
                fatal_error(format!("panic: {} contains errors\n", &binary_path).as_str());
            }
        } else if self.options.verbose
            || self.options.magic
            || !self.options.make_exports.is_empty()
        {
            if self.files.is_empty() {
                println!("{} contains syntax error", source_path);
            } else if !self.undefined_names.is_empty() {
                println!("{} contains undefined names or type errors", source_path);
            } else if !self.making && !self.options.magic {
                println!("{}", source_path);
            }
        }

        if !self.files.is_empty() && !self.making && !self.initializing {
            self.unfix_exports_partial();
        }
        self.loading = false;

        Ok(())
    }

    fn load_file(&mut self, source_path: &str) -> Result<(), BytecodeError> {
        #[cfg(test)]
        self.last_load_phase_trace.clear();
        self.record_load_phase("begin");

        // Keep the previous source graph available while replacing current load state.
        self.old_files = self.files;
        self.unload();
        self.reset_load_phase_state();
        self.loading = true;

        // Ensure `loading` is reset on every return path.
        let result = (|| {
            let source_path = self.normalize_source_path_for_load(source_path);

            let metadata = match std::fs::metadata(&source_path) {
                Ok(metadata) => metadata,
                Err(err) if err.kind() == std::io::ErrorKind::NotFound => {
                    if self.initializing || self.making {
                        self.record_load_phase("missing-source-error");
                        return Err(BytecodeError::MissingSourceFile {
                            path: source_path.clone(),
                        });
                    }

                    self.record_load_phase("missing-source-allowed");
                    self.files = self.empty_environment_for_source(&source_path, UNIX_EPOCH);
                    self.old_files = self.files;
                    return Ok(());
                }
                Err(err) => {
                    self.record_load_phase("source-metadata-error");
                    return Err(BytecodeError::UnreadableSourceFile {
                        path: source_path.clone(),
                        source: err,
                    });
                }
            };

            let source_file =
                File::open(&source_path).map_err(|source| BytecodeError::UnreadableSourceFile {
                    path: source_path.clone(),
                    source,
                })?;

            // Keep the most recently targeted source graph available if compilation fails.
            let modified_time = metadata.modified().unwrap_or_else(|_| SystemTime::now());
            self.old_files = self.empty_environment_for_source(&source_path, modified_time);

            let is_main_script = !self.initializing && !self.making;
            self.record_load_phase("parse");
            let parse_outcome =
                self.parse_source_script(&source_file, &source_path, is_main_script)?;

            if parse_outcome.status == ParsePhaseStatus::SyntaxError {
                self.record_load_phase("syntax-fallback");
                self.files = parse_outcome.files;
                self.apply_syntax_error_fallback(&source_path)?;
                return Ok(());
            }

            self.files = parse_outcome.files;
            self.record_load_phase("exportfile-checks");
            self.validate_exportfile_bindings_partial()?;
            self.record_load_phase("include-expansion");
            self.run_mkincludes_phase()?;
            self.record_load_phase("typecheck");
            self.run_checktypes_phase()?;
            self.record_load_phase("export-closure");
            self.run_export_closure_phase_partial()?;
            self.record_load_phase("bereaved-warnings");
            self.emit_bereaved_warnings_partial();
            self.record_load_phase("unused-diagnostics");
            self.emit_unused_definition_diagnostics_partial();
            self.record_load_phase("codegen");
            self.run_codegen_phase()?;
            self.record_load_phase("dump-visibility");
            self.run_dump_visibility_phase(&source_path)?;

            self.record_load_phase("success-postlude");
            self.run_success_postlude_phase();
            Ok(())
        })();

        self.loading = false;
        result
    }

    // This is test-only instrumentation that captures load_file phase order so we can verify orchestration
    // branches and sequencing while parser/typecheck/codegen internals are still deferred.
    #[cfg(test)]
    fn record_load_phase(&mut self, phase: &'static str) {
        self.last_load_phase_trace.push(phase);
    }

    #[cfg(not(test))]
    fn record_load_phase(&mut self, _phase: &'static str) {}

    fn normalize_source_path_for_load(&self, source_path: &str) -> String {
        let mut normalized_path = source_path.to_string();

        if !normalized_path.ends_with(".m") && !self.initializing {
            if normalized_path.ends_with(".x") {
                normalized_path.truncate(normalized_path.len() - 2);
            }

            if normalized_path.ends_with('.') {
                normalized_path.pop();
            }

            normalized_path.push_str(".m");
        }

        normalized_path
    }

    fn reset_load_phase_state(&mut self) {
        // These parse/load accumulators are rebuilt by each load attempt.
        // Reset them eagerly to avoid carrying stale state across retries.
        self.embargoes = NIL;
        self.exportfiles = NIL;
        self.exports = NIL;
        self.eprodnts = NIL;
    }

    fn empty_environment_for_source(
        &mut self,
        source_path: &str,
        modified_time: SystemTime,
    ) -> ConsList<FileRecord> {
        let source_record = FileRecord::new(
            &mut self.heap,
            source_path.to_string(),
            modified_time,
            false,
            ConsList::EMPTY,
        );

        ConsList::new(&mut self.heap, source_record)
    }

    fn apply_syntax_error_fallback(&mut self, source_path: &str) -> Result<(), BytecodeError> {
        if self.initializing {
            return Err(BytecodeError::SyntaxErrorInSource {
                path: source_path.to_string(),
            });
        }

        self.old_files = self.files;
        self.record_load_phase("syntax-unload");
        self.unload();

        self.record_load_phase("syntax-dump-decision");
        if source_path.ends_with(".m") {
            self.maybe_write_syntax_dump(source_path)?;
        }

        self.record_load_phase("syntax-state-reset");
        self.reset_syntax_error_state_for_load();

        Ok(())
    }

    /// Applies success-branch finalization after all load phases succeed.
    ///
    /// C parity target: final success-branch housekeeping before returning from `loadfile`.
    /// This is concrete state update logic, not a deferred boundary.
    fn run_success_postlude_phase(&mut self) {
        self.sorted = true;
        self.reset_syntax_error_state_for_load();
        self.old_files = self.files;
    }

    fn reset_syntax_error_state_for_load(&mut self) {
        self.error_line = 0;
        self.errs.clear();
    }

    /// Validates `%export` file-id bindings against the currently tracked include set.
    ///
    /// C parity target: `loadfile` exportfiles/pathname checks in `steer.c`.
    /// Current concrete behavior:
    /// - ignores `PLUS` markers (the "include current script" sentinel),
    /// - requires each pathname entry to resolve to exactly one includee,
    /// - returns typed errors for malformed, missing, or ambiguous bindings.
    ///
    /// This implementation is intentionally partial: it covers binding-shape and
    /// include-membership constraints, while parser-driven export semantics remain
    /// deferred to parser/type integration.
    fn validate_exportfile_bindings_partial(&mut self) -> Result<(), BytecodeError> {
        if self.exportfiles == NIL {
            return Ok(());
        }

        let mut exportfiles: ConsList<RawValue> = ConsList::from_ref(self.exportfiles.into());
        while let Some(entry) = exportfiles.pop_raw(&self.heap) {
            if entry == Combinator::Plus.into() {
                continue;
            }

            // In this list shape, non-PLUS entries are expected to be heap strings.
            let path = self
                .heap
                .resolve_string(entry.into())
                .map_err(|_| BytecodeError::MalformedExportFileList)?;
            let mut includee_matches = 0usize;
            let mut includees = self.includees;
            while let Some(includee) = includees.pop(&self.heap) {
                if includee.get_file_name(&self.heap) == path {
                    includee_matches += 1;
                }
            }

            if includee_matches == 0 {
                return Err(BytecodeError::ExportFileNotIncludedInScript { path });
            }

            if includee_matches > 1 {
                return Err(BytecodeError::ExportFileAmbiguous { path });
            }
        }

        Ok(())
    }

    /// Applies the include-expansion phase for the current load cycle.
    ///
    /// C parity target: `files=append1(files,mkincludes(includees)),includees=NIL; ld_stuff=NIL;`
    /// from `loadfile`.
    /// Current concrete behavior:
    /// - appends currently tracked `includees` into `files` in list order,
    /// - clears `includees` after append,
    /// - clears `mkinclude_files` bookkeeping for interrupted include loads.
    ///
    /// Real `%include` discovery/compilation remains deferred to parser/typecheck integration.
    fn run_mkincludes_phase(&mut self) -> Result<(), BytecodeError> {
        if self.includees.is_empty() {
            self.mkinclude_files = ConsList::EMPTY;
            return Ok(());
        }

        let mut includees = self.includees;
        while let Some(includee) = includees.pop(&self.heap) {
            self.files.append(&mut self.heap, includee);
        }

        self.includees = ConsList::EMPTY;
        self.mkinclude_files = ConsList::EMPTY;
        Ok(())
    }

    /// Executes the typecheck gate for the current load cycle.
    ///
    /// C parity target: `if(!SYNERR) ... checktypes();`
    /// Current concrete behavior:
    /// - if undefined names are already present, fail the phase with a typed error,
    /// - otherwise succeed without invoking the real typechecker subsystem.
    fn run_checktypes_phase(&mut self) -> Result<(), BytecodeError> {
        let undefined_count = self.undefined_names.len(&self.heap);
        if undefined_count > 0 {
            return Err(BytecodeError::TypecheckUndefinedNames {
                count: undefined_count,
            });
        }

        Ok(())
    }

    /// Applies export-closure gating after typecheck.
    ///
    /// C parity target: export-list checks gated by `ND`/undefined names.
    /// Current concrete behavior:
    /// - if there is no export list, this phase is a no-op,
    /// - if undefined names are present, drop exports and return a typed blocking error,
    /// - otherwise keep exports unchanged and continue.
    ///
    /// This implementation is intentionally partial: deeper closure/dependency
    /// analysis (for example `deps`-style traversal) remains deferred.
    fn run_export_closure_phase_partial(&mut self) -> Result<(), BytecodeError> {
        if self.exports == NIL {
            return Ok(());
        }

        if !self.undefined_names.is_empty() {
            self.exports = NIL;
            return Err(BytecodeError::ExportClosureBlockedByUndefinedNames);
        }

        Ok(())
    }

    /// Emits post-export warnings for potentially bereaved/orphaned type information.
    ///
    /// C parity target: the `bereaved` warning block in `loadfile`.
    /// Current concrete behavior:
    /// - when exports are active and orphan risk was flagged (`unused_types`),
    ///   emit a warning in verbose/make contexts,
    /// - do not fail the load path for warning-only conditions.
    ///
    /// This implementation is intentionally partial: full bereaved analysis depends
    /// on deferred type/export internals.
    fn emit_bereaved_warnings_partial(&mut self) {
        if self.exports != NIL && self.unused_types && (self.options.verbose || self.making) {
            println!("warning, export list may be incomplete - missing typenames");
        }
    }

    /// Emits diagnostics for parser-collected unused definitions/nonterminals.
    ///
    /// C parity target: the `detrop` warning block in `loadfile`.
    /// Current concrete behavior:
    /// - if deferred unused-definition diagnostics are present (`eprodnts != NIL`),
    ///   emit a warning in verbose/make contexts,
    /// - clear the deferred diagnostics marker once processed for this load cycle.
    ///
    /// This implementation is intentionally partial: full production of these
    /// diagnostics remains deferred to parser/type integration.
    fn emit_unused_definition_diagnostics_partial(&mut self) {
        if self.eprodnts != Value::from(NIL) && (self.options.verbose || self.making) {
            println!("warning, script contains deferred unused-definition diagnostics");
        }

        self.eprodnts = NIL;
    }

    /// Executes the codegen gate across loaded file definitions.
    ///
    /// C parity target: codegen loop and `initialising && ND!=NIL` panic branch.
    /// Current concrete behavior:
    /// - fail if no files are available for code generation,
    /// - fail during initialization if unresolved names remain,
    /// - otherwise succeed without invoking the real codegen subsystem.
    fn run_codegen_phase(&mut self) -> Result<(), BytecodeError> {
        if self.files.is_empty() {
            return Err(BytecodeError::CodegenWithoutLoadedFiles);
        }

        if self.initializing && !self.undefined_names.is_empty() {
            return Err(BytecodeError::InitializationLoadContainsErrors);
        }

        Ok(())
    }

    /// Applies dump-writing and export-visibility orchestration for the load cycle.
    ///
    /// C parity target: `fixexports(); makedump(); unfixexports();` on success paths.
    /// Current concrete behavior:
    /// - run phase only for initialization or normal `.m` sources,
    /// - execute `fixexports`/`makedump` F2 boundaries in order,
    /// - execute `unfix_exports_partial` as the current F6 unwind subset.
    ///
    /// This keeps orchestration concrete while deferring full dump serialization
    /// and deep export-visibility rewriting semantics.
    fn run_dump_visibility_phase(&mut self, source_path: &str) -> Result<(), BytecodeError> {
        // C parity: initialization always writes a dump; otherwise only "normal" scripts
        // (ending in `.m`) run through fixexports/makedump/unfixexports.
        let should_run_dump_visibility = self.initializing || source_path.ends_with(".m");
        if !should_run_dump_visibility {
            return Ok(());
        }

        self.record_load_phase("dump-fixexports");
        self.run_fixexports_boundary();
        self.record_load_phase("dump-write");
        self.run_makedump_boundary(source_path)?;
        self.record_load_phase("dump-unfixexports");
        self.unfix_exports_partial();

        Ok(())
    }

    /// `fixexports` visibility boundary.
    ///
    /// C parity target: `fixexports();` before dump serialization.
    /// Current concrete behavior:
    /// - marks export-list processing mode when exports are present,
    /// - leaves real visibility rewriting (including full internal-name handling)
    ///   deferred.
    ///
    /// This is an F2 deferred boundary that should collapse into direct concrete
    /// visibility logic when export-visibility parity work lands.
    fn run_fixexports_boundary(&mut self) {
        if self.exports != NIL {
            self.in_export_list = true;
        }
    }

    /// Deferred success/syntax dump-write boundary.
    ///
    /// C parity target: `makedump();` after visibility fixups.
    /// Current concrete behavior:
    /// - keeps dump write decision concrete in orchestration,
    /// - intentionally preserves `source_path` in the boundary contract for
    ///   future serializer output/diagnostic context,
    /// - defers actual dump serialization to later integration.
    fn run_makedump_boundary(&mut self, _source_path: &str) -> Result<(), BytecodeError> {
        Ok(())
    }

    /// Captures syntax-dump boundary bookkeeping for syntax-error load exits.
    ///
    /// C parity target: `if(normal(t)&&SYNERR!=2)makedump();` on syntax fallback.
    /// Current concrete behavior:
    /// - only applies to normal `.m` sources,
    /// - ensures `old_files` carries at least one source anchor record so
    ///   syntax-error state remains attributable,
    /// - routes through the same `makedump` boundary used by success-path dump flow,
    ///   while leaving actual dump serialization deferred.
    fn maybe_write_syntax_dump(&mut self, source_path: &str) -> Result<(), BytecodeError> {
        if !source_path.ends_with(".m") {
            return Ok(());
        }

        if self.old_files.is_empty() {
            self.old_files = self.empty_environment_for_source(source_path, UNIX_EPOCH);
        }

        self.record_load_phase("dump-write");
        self.run_makedump_boundary(source_path)?;

        Ok(())
    }

    /// Parser/openfile load boundary.
    ///
    /// C parity target: parser/openfile segment of `loadfile` (`yyparse`-driven source parse).
    /// Current concrete behavior:
    /// - reads source bytes and creates a placeholder single-file environment,
    /// - recognizes explicit test markers (`RANDA_PARSE_OK`, `RANDA_PARSE_SYNTAX_ERROR`)
    ///   to drive phase orchestration deterministically,
    /// - returns typed deferral error for all non-marker inputs.
    ///
    /// This is an F2 deferred boundary. Full parser invocation, semantic actions,
    /// and `%include`/`%export` side effects remain deferred to parser integration,
    /// where this boundary should be removed or replaced by direct parser wiring.
    fn parse_source_script(
        &mut self,
        source_file: &File,
        source_path: &str,
        _is_main_script: bool,
    ) -> Result<ParsePhaseOutcome, BytecodeError> {
        let mut source_text = String::new();
        let mut reader = BufReader::new(source_file);
        reader.read_to_string(&mut source_text).map_err(|source| {
            BytecodeError::UnreadableSourceFile {
                path: source_path.to_string(),
                source,
            }
        })?;

        let placeholder_files = self.empty_environment_for_source(source_path, SystemTime::now());
        if source_text.contains("RANDA_PARSE_OK") {
            return Ok(ParsePhaseOutcome {
                status: ParsePhaseStatus::Parsed,
                files: placeholder_files,
            });
        }

        if source_text.contains("RANDA_PARSE_SYNTAX_ERROR") {
            return Ok(ParsePhaseOutcome {
                status: ParsePhaseStatus::SyntaxError,
                files: placeholder_files,
            });
        }

        Err(BytecodeError::ParserIntegrationDeferred {
            path: source_path.to_string(),
        })
    }

    fn alfasort(
        &mut self,
        mut items: ConsList<IdentifierRecordRef>,
    ) -> ConsList<IdentifierRecordRef> {
        let mut identifiers: Vec<IdentifierRecordRef> = vec![];
        while let Some(identifier) = items.pop(&self.heap) {
            identifiers.push(identifier);
        }

        identifiers.sort_by(|left, right| {
            let left_name = self
                .identifier_name_for_diagnostics(*left)
                .unwrap_or_default();
            let right_name = self
                .identifier_name_for_diagnostics(*right)
                .unwrap_or_default();
            left_name
                .cmp(&right_name)
                .then(left.get_ref().cmp(&right.get_ref()))
        });

        let mut sorted = ConsList::EMPTY;
        for identifier in identifiers {
            sorted.append(&mut self.heap, identifier);
        }

        sorted
    }

    fn printlist(&self, mut items: ConsList<IdentifierRecordRef>) -> String {
        let mut names: Vec<String> = vec![];
        while let Some(identifier) = items.pop(&self.heap) {
            let name = self
                .identifier_name_for_diagnostics(identifier)
                .unwrap_or_else(|| "<invalid-id>".to_string());
            names.push(name);
        }

        names.join(", ")
    }

    fn source_update_check(&self) -> bool {
        let mut loaded_files = self.files;
        while let Some(file_record) = loaded_files.pop(&self.heap) {
            let path = file_record.get_file_name(&self.heap);
            let dumped_modified_time = file_record.get_last_modified(&self.heap);

            let source_modified_time = match std::fs::metadata(&path).and_then(|m| m.modified()) {
                Ok(modified_time) => modified_time,
                Err(_) => return true,
            };

            if source_modified_time > dumped_modified_time {
                return true;
            }
        }

        false
    }

    /// `unfixexports` state restoration used by undump and load-tail paths.
    ///
    /// Current concrete behavior:
    /// - exits export-list processing mode,
    /// - clears deferred `internals` bookkeeping.
    ///
    /// This is intentionally not the full C `unfixexports` algorithm.
    /// Deeper name-restore/publicise semantics remain deferred to export-visibility
    /// parity work.
    fn unfix_exports_partial(&mut self) {
        self.in_export_list = false;
        self.internals = ConsList::EMPTY;
    }

    fn identifier_name_for_diagnostics(&self, identifier: IdentifierRecordRef) -> Option<String> {
        Some(identifier.get_name(&self.heap))
    }

    fn hdsort(&mut self, _params: Value) -> Value {
        unimplemented!()
    }

    fn bindparams(&mut self, _formal: Value, _actual: Value) {
        unimplemented!()
    }

    fn prefix(&self) -> Result<String, ()> {
        let str_ref = self.prefix_stack.value_head(&self.heap).ok_or(())?;
        self.heap.resolve_string(str_ref)
    }

    /// Replace aliases with their referent
    fn obey_aliases(&mut self, aliases: ConsList) -> Result<(), BytecodeError> {
        if !aliases.is_empty() {
            // For each `old' install diversion to `new', i.e. make the `old` identifier an alias for the `new` identifier.
            // If alias is of form `-old`, `new' is a `pname` (private name).

            // ToDo: The uppercase `ALIASES` is a global in Miranda. We aren't mutating `aliases`, so the value of
            //       `ALIASES` *should* just be the value we passed into `load_script`. Is that enough, or do we need to
            //       make a new member `VM::aliases`? Unfortunately, I think we need a new member.
            self.aliases = aliases;
            let mut alias_iterator = aliases; // Not technically an iterator.

            while !alias_iterator.is_empty() {
                // IdentifierRecordRef looks like this:
                //   cons(
                //     cons(strcons(name,who),type),
                //     cons(cons(arity,showfn),cons(free_t,NIL))
                //   )
                // Entries in `aliases` look like this:
                //   cons(new_id, old_id)
                let alias_ref: RawValue = alias_iterator.pop_value(&self.heap).unwrap().into();
                let alias_entry = AliasEntry::from_ref(alias_ref);
                // In Miranda, `new` may be either:
                // - an identifier (`Tag::Id`), or
                // - a private name/pname target (not `Tag::Id`).
                // We always install the diversion old -> new, regardless of which case this is.
                let new_target = alias_entry.get_new_target(&self.heap);
                let old: IdentifierRecordRef = alias_entry.get_old_identifier_record(&self.heap);
                let hold = IdentifierCoreRef::from_old_identifier(&mut self.heap, old);

                old.set_type(&mut self.heap, Type::Alias.into());
                // We make old an alias of new.
                old.set_value(&mut self.heap, new_target);

                // Name-clash detection only applies when `new` is actually an identifier.
                if let Some(new_id) = alias_entry.get_new_identifier_record(&self.heap) {
                    let new_datatype = new_id.get_datatype(&self.heap);
                    if (new_datatype != Value::from(Type::Undefined)
                        || new_id.get_value(&self.heap).is_some())
                        && new_datatype != Value::from(Type::Alias)
                    {
                        // Insert new into self.clashes such that self.clashes remains in ascending address order.
                        // Todo: Why order these?
                        // Todo: Why is this a clash? Isn't it just an alias?
                        self.clashes.insert_ordered(&mut self.heap, new_id); //add1(&mut self.heap, new, &mut self.clashes);
                    }
                }

                // Replace new_ref with hold
                // Todo: But `hold` isn't an `IdentifierRecordRef`...? The alias is replaced with the info required to undo the
                //       aliasing. The aliasing is undone in the event of an error. But in that case we are potentially
                //       overwriting a previous `hold` with a "new" `hold`.
                alias_entry.set_hold(&mut self.heap, hold);
            }

            if !self.clashes.is_empty() {
                // Todo: I don't think `unalias` is necessary here.
                self.unalias(aliases);
                // return (NIL);
                return Err(BytecodeError::NameClash); // BAD_DUMP = -2;
            }

            // The following block was inserted to deal with the pathological case that the destination of an alias (not
            // part of a cyclic alias) has a direct definition in the file and the aliasee is missing from the file - this is
            // both name clash and missing aliasee, but without fix the two errors cancel each other out and are unreported
            let mut alias_iterator = aliases; // Not technically an iterator.
            while !alias_iterator.is_empty() {
                let alias_ref: RawValue = alias_iterator.pop_value(&self.heap).unwrap().into();
                let alias_entry = AliasEntry::from_ref(alias_ref);
                let old = alias_entry.get_old_identifier_record(&self.heap);
                let new_target = old.get_value(&self.heap).expect(
                    "Old identifier is missing alias destination during obey_aliases FIX1.",
                );

                // Miranda FIX1:
                //   if(tag[ch=id_val(old)]==ID)
                //   if(id_type(ch)!=alias_t)
                //      id_type(ch)=new_t;
                //
                // Interpretation: mark the destination identifier as `new` when it is an ID and not
                // already an alias. This ensures subsequent missing-aliasee reporting is preserved in
                // the pathological clash+missing case described above.
                if self.heap[new_target.get_ref()].tag == Tag::Id {
                    let new_id = IdentifierRecordRef::from_ref(new_target.get_ref());
                    if new_id.get_datatype(&self.heap) != Type::Alias.into() {
                        new_id.set_type(&mut self.heap, Type::New.into());
                    }
                }
            } // FIX1
        }

        Ok(())
    }

    /// Gets the file name for the topmost file in the `vm.files` cons list, which _should_ be the current file.
    fn current_file(&self) -> String {
        assert!(!self.files.is_empty());
        let f = self.files.head(&self.heap).unwrap();

        f.get_file_name(&self.heap)
    }

    /// Loads a compiled script from `in_file` for the `source_file`.
    fn load_script(
        &mut self,
        in_file: &File,
        mut source_file: String,
        aliases: ConsList, // List of cons(new_id, old_id)
        parameters: Value,
        is_main_script: bool,
    ) -> Result<ConsList<FileRecord>, BytecodeError> {
        /*
        extern word nextpn,ND,errline,algshfns,internals,freeids,includees,SGC;
        extern char *dicp, *dicq;
        word ch,files=NIL;
        TORPHANS=BAD_DUMP=0;
        CLASHES=NIL;
        */
        // This holds the return value.
        let mut files: ConsList<FileRecord> = ConsList::EMPTY;
        self.clashes = ConsList::EMPTY;

        if let Ok(prefix) = self.prefix() {
            make_relative_path(&mut source_file, &prefix);
        }

        let mut file_bytes = vec![];
        {
            // Scope of f_reader
            let mut f_reader = BufReader::new(in_file);
            let bytes_read = f_reader
                .read_to_end(&mut file_bytes)
                .map_err(BytecodeError::unexpected_eof_with_source)?;
            if bytes_read < 16 {
                return Err(BytecodeError::unexpected_eof());
            }
        }
        // An iterator over the bytes of the file.
        let mut byte_iter = file_bytes.iter().peekable();

        // Parse the machine word size
        // First byte: `__WORDSIZE` (64 bits in my case, so `__WORDSIZE == 64 == 0x40`.)
        if *byte_iter.next().unwrap() as usize != WORD_SIZE {
            return Err(BytecodeError::ArchitectureMismatch);
        }

        // Parse the bytecode version
        // Second byte: `XVERSION`, the bytecode version. (Latest Miranda` == 83 == 0x53`)
        if *byte_iter.next().unwrap() as i32 != XVERSION {
            return Err(BytecodeError::WrongBytecodeVersion);
        }

        // Todo: Why is this even here? It doesn't depend on the contents of the file!
        self.obey_aliases(aliases)?;

        // PNBASE = nextpn; // base for relocation of internal names in dump
        self.private_symbol_base_index = self.heap.private_symbols.len();
        // I think the idea is that the indices of the names in the serialized binary bytecode assume an empty private
        // symbol table, and that may not be the case if the current file is, say, included in another file. So indices
        // in the bytecode are actually _relative_ to the index of the first private symbol of the current file, and
        // adding private_name_base gives the absolute index in the vector of private names.

        // ToDo: Is this RE-setting the values of `suppressed*`? Might they be non-empty? Neither appear to be added to
        //       in this function.
        self.suppressed = ConsList::EMPTY; // SUPPRESSED  // list of `-id' aliases successfully obeyed
        self.suppressed_t = ConsList::EMPTY; // TSUPPRESSED // list of -typename aliases (illegal just now)

        // The following parses the rest of the binary file. See
        // (Serialized Binary Representation.md)[../Serialized Binary Representation.md]
        // for more details. The first two bytes containing the word size and bytecode version have already been parsed.

        // Each pass through the while loop parses a single
        // [ [filename]
        //   [mtime]
        //   [shareable]
        //   [definition-list] ]
        // block. (Note: This is skipped over in the case of a syntax-error script.)
        while let Some(ch) = byte_iter.next() {
            if *ch == 0 {
                if !aliases.is_empty() {
                    self.unalias(aliases);
                }
                return Ok(files);
            }

            // If we encounter a `ch==1` *before* we ever even see a file, it is because it is the magic number for a
            // type-error script.
            // Todo: It is awkward to have this inside the loop.
            if *ch == 1 && files.is_empty() {
                // The next w bytes (8 bytes) give the line number of the error.
                let error_line_prefetch: usize =
                    get_machine_word(&mut byte_iter.by_ref().copied())?;
                if is_main_script {
                    // But only save it if this is the main script.
                    self.error_line = error_line_prefetch;
                }
                // Todo: What if there are multiple type errors?
            }

            // Parse the file name and modified time

            let (mut filename, modified_time) = parse_filename_modified_time(&mut byte_iter)?;
            if let Ok(prefix) = self.prefix() {
                make_absolute_path(&mut filename, &prefix);
            }

            // Parse sharable bit
            let sharable: bool = *byte_iter.next().unwrap_or(&0u8) == 1;

            #[cfg(feature = "debug")]
            println("loading: {}({})", filename, _modified_time);

            if files.is_empty() {
                // Is this the right dump file?
                if filename != source_file {
                    // I don't think unalias is needed here.
                    if !aliases.is_empty() {
                        self.unalias(aliases);
                    }
                    return Err(BytecodeError::WrongSourceFile);
                }
            }

            // Add a new `FileRecord` to `files`
            {
                // Scope of `new_defs`, `file_record`

                // Warning: `load_defs` side effects id's in namebuckets, cannot  be  undone  by
                //          `unload`  until  attached  to  global `files', so interrupts are disabled during
                //          `load_script` - see steer.c
                // For big dumps this may be too coarse - FIX
                let new_defs =
                    ConsList::from_ref(self.load_defs(&mut byte_iter.by_ref().copied())?.into());
                let file_record =
                    FileRecord::new(&mut self.heap, filename, modified_time, sharable, new_defs);
                files.push(&mut self.heap, file_record);
            }
        }

        // Dump of syntax error state
        // If we got here, we broke out of the while because we encountered `ch==0`. If we did so before we ever found a
        // file, it is because the `0` is the magic number for a syntax error script.
        if files.is_empty() {
            // The next w bytes (8 bytes) give the line number of the error.
            {
                // Scope of `error_line_prefetch`
                let error_line_prefetch: usize =
                    get_machine_word(&mut byte_iter.by_ref().copied())?;
                if is_main_script {
                    // But only save it if this is the main script.
                    self.error_line = error_line_prefetch;
                }
            }
            // Todo: What if there are multiple type errors?

            while let Some(_ch) = byte_iter.next() {
                // Parse filename and modified time.
                let (mut filename, modified_time) = parse_filename_modified_time(&mut byte_iter)?;
                if let Ok(prefix) = self.prefix() {
                    make_absolute_path(&mut filename, &prefix);
                }

                if self.old_files.is_empty() {
                    // Is this the right dump file?
                    if filename != source_file {
                        // I don't think unalias is needed here.
                        if !aliases.is_empty() {
                            self.unalias(aliases);
                        }
                        return Err(BytecodeError::WrongSourceFile);
                    }
                }

                // Note: This mirrors the block inside the while-loop, except it adds to `old_files`, sets `sharable` to false,
                // and has empty `defienda`.
                {
                    // Scope of `file_record`
                    // Add a new `FileRecord` to `old_files`
                    // Todo: WTF is `old_files`? Why are we using it instead of `files`? Why are we setting its `sharable` to
                    //       `false`, `defienda` to `NIL`? It is the most recent set of sources, in case of interrupted or failed
                    //       compilation. Why its definitions and sharable flag are not saved is a mystery.
                    let file_record = FileRecord::new(
                        &mut self.heap,
                        filename,
                        modified_time,
                        false,
                        ConsList::EMPTY,
                    );
                    self.old_files.push(&mut self.heap, file_record);
                }
            }

            if !aliases.is_empty() {
                self.unalias(aliases)
            }

            return Ok(files); // Equivalent to `return Ok(Nil);`
        }

        // Parse algebraic show functions
        {
            // scope of new_defs
            let new_defs: RawValue = self.load_defs(&mut byte_iter.by_ref().copied())?.into();
            self.algebraic_show_functions
                .append(&mut self.heap, new_defs);
        }

        // Parse [ND] or [True] (Are there type orphans?)
        if self.load_defs(&mut byte_iter.by_ref().copied())? == Combinator::True.into() {
            self.undefined_names = ConsList::EMPTY;
            self.unused_types = true;
        }

        // Parse DEF_X
        // Parse sui generis constructors
        // Todo: This does not appear in the binary description
        {
            let new_defs: RawValue = self.load_defs(&mut byte_iter.by_ref().copied())?.into();
            self.sui_generis_constructors
                .append(&mut self.heap, new_defs);
        }

        // Parse DEF_X
        // Parse free_ids
        if is_main_script || self.includees.is_empty() {
            self.free_ids =
                ConsList::from_ref(self.load_defs(&mut byte_iter.by_ref().copied())?.into());
        } else {
            let defs = self.load_defs(&mut byte_iter.by_ref().copied())?;
            let sorted_parameters = self.hdsort(parameters);
            self.bindparams(defs, sorted_parameters);
        }

        // Housekeeping
        // Todo: Do we unalias unconditionally?
        if !aliases.is_empty() {
            self.unalias(aliases)
        }

        // Parse DEF_X
        // Parse internals
        if is_main_script {
            self.internals =
                ConsList::from_ref(self.load_defs(&mut byte_iter.by_ref().copied())?.into());
        }

        Ok(files.reversed(&mut self.heap))
    }

    /// Remove old to new diversions installed in `obey_aliases`. (Miranda's `unscramble()`.)
    fn unalias(&mut self, aliases: ConsList) {
        // `aliases` contains alias entries created during load, each conceptually `cons(new, old)`.
        // During `obey_aliases`, we mutate each alias entry so `head` becomes a temporary hold payload:
        //
        //   alias_entry = cons(hold, old)
        //   hold       = cons(new, cons(old_type, old_value))
        //
        // where `old_who`/`old_type`/`old_value` are the pre-alias fields of `old`.
        //
        // This first pass mirrors C `unscramble`'s first loop:
        // 1) restore each `old` identifier from `hold`,
        // 2) write `new` back into `hold.head` (so second pass can inspect alias destination).
        let mut cursor = aliases;

        while let Some(alias_ref_value) = cursor.pop_value(&self.heap) {
            let alias_ref: RawValue = alias_ref_value.into();
            let alias_entry = AliasEntry::from_ref(alias_ref);
            let old = alias_entry.get_old_identifier_record(&self.heap);
            let hold = alias_entry
                .get_hold(&self.heap)
                .expect("Alias entry is missing hold payload during unalias.");
            // During alias installation, `old.value` was overwritten with the alias destination `new`.
            // That value can be either an ID reference or a pname/non-ID payload.
            let new_target: IdentifierValueRef = match old.get_value(&self.heap) {
                Some(v) => v,
                _ => {
                    panic! {"Impossible value found in aliases."}
                }
            };

            // C: `hd[hd[aliases]] = new`.
            // In our representation this means writing `new` into `hold.head`.
            // We do this before restoring `old` so pass 2 sees the final destination target.
            alias_entry.set_hold_value(&mut self.heap, new_target);

            let restored_core = hold.get_data(&self.heap);
            let restored_value = restored_core
                .value
                .expect("Impossible value found in aliases.");

            // id_who(old)=hd[hold];
            old.set_definition(&mut self.heap, restored_core.definition);
            // id_type(old)=hd[tl[hold]];
            old.set_type(&mut self.heap, restored_core.datatype);
            // id_val(old)=tl[tl[hold]];
            old.set_value(&mut self.heap, restored_value);
        } // end iter over `aliases`

        // Second pass mirrors C `unscramble`'s ALIASES loop.
        // We rebuild `self.aliases` to contain only "missing aliasees" (`old` identifiers whose destinations
        // cannot be resolved after rollback and suppression/clash handling).
        // This list is consumed later by error-reporting and follow-on load logic.
        let mut missing_aliases: ConsList = ConsList::EMPTY;

        cursor = self.aliases;
        while !cursor.is_empty() {
            let alias_ref: RawValue = cursor.pop_value(&self.heap).unwrap().into();
            let alias_entry = AliasEntry::from_ref(alias_ref);
            let old_id = alias_entry.get_old_identifier_record(&self.heap);
            let new_target = alias_entry.get_new_target(&self.heap);

            if let Some(new_id) = alias_entry.get_new_identifier_record(&self.heap) {
                // FIX1
                if new_id.get_type(&self.heap) == Type::New.into() {
                    new_id.set_type(&mut self.heap, Type::Undefined.into());
                }

                if new_id.get_type(&self.heap) == Type::Undefined.into() {
                    missing_aliases.push(&mut self.heap, old_id.get_ref());
                } else if !self.clashes.contains(&self.heap, new_id) {
                    let new_def: IdentifierDefinitionRef = new_id.get_definition(&self.heap);
                    let new_def_data: IdentifierDefinitionData = new_def.get_data(&self.heap);

                    // Install aka info in new
                    match new_def_data {
                        IdentifierDefinitionData::Alias { .. } => { /* pass */ }

                        // If it's not an alias
                        _ => {
                            // C: id_who(new) = cons(datapair(get_id(old), 0), id_who(new));
                            // The constructor encapsulates this alias-metadata shape.
                            let aliased_definition = IdentifierDefinitionRef::from_alias_source(
                                &mut self.heap,
                                old_id,
                                new_def,
                            );
                            new_id.set_definition(&mut self.heap, aliased_definition);
                        }
                    }
                }
            } else {
                // C: `if(tag[new] != ID) { if(!member(SUPPRESSED,new)) missing_aliases=cons(old,...); continue; }`
                //
                // Interpretation:
                // - `new` is not an identifier object (typically a pname/private-name target), so we cannot
                //   attach aka/who metadata to it.
                // - If this non-ID target was not intentionally suppressed, the missing aliasee is `old`.
                //
                // `self.suppressed` currently stores these non-ID targets using `IdentifierRecordRef` wrappers,
                // so membership checks are by raw reference equality on the wrapped `RawValue`.
                let new_target_as_id = IdentifierRecordRef::from_ref(new_target.get_ref());
                if !self.suppressed.contains(&self.heap, new_target_as_id) {
                    missing_aliases.push(&mut self.heap, old_id.get_ref())
                }
                continue;
            }
        }

        // Transmits info about missing aliasees
        self.aliases = missing_aliases;
    }

    fn println_centered(&self, text: &str) {
        if let Some(term) = &self.terminal {
            let (_terminal_height, terminal_width) = term.size();
            // println!("width: {}", terminal_width);
            let centered = pad_str(text, terminal_width as usize, Alignment::Center, Some("…"));

            term.write_line(&*centered).ok();
        }
    }

    pub fn announce(&mut self) {
        if let Some(term) = &mut self.terminal {
            term.set_title(format!(
                "Randa {}",
                make_version_string(self.options.version)
            ));

            // let title1 = "T h e   M i r a n d a   S y s t e m";
            self.println_centered(
                format!(
                    "{}   {}   {}",
                    style("T h e").blue(),
                    style("R a n d a").red(),
                    style("S y s t e m").blue()
                )
                .as_str(),
            );
            print!("\n\n"); // two lines
            self.println_centered(
                format!(
                    "Version {} last revised {}",
                    make_version_string(self.options.version),
                    self.options.build_date
                )
                .as_str(),
            );
            print!("\n\n"); // two lines
            self.println_centered(
                format!(
                    "{} Copyright 2022-2026 Robert Jacobson, BSD License",
                    style("Randa").red()
                )
                .as_str(),
            );
            // print!("\n"); // one line
            self.println_centered(
                "The Original Miranda System Copyright Research Software Ltd 1985-2020",
            );
            // print!("\n\n"); // two lines
            self.println_centered("Original Miranda System: http://miranda.org.uk");
            print!("\n\n\n"); // three lines

            if self.options.space_limit != DEFAULT_SPACE {
                println!("({} cells)\n", self.options.space_limit);
            }
            if !self.options.strict_if {
                println!("(-nostrictif : deprecated!)\n");
                // printf("\t\t\t\t%dbit platform\n",__WORDSIZE);  temporary/
            }

            /*
            // Only relevant once .mirarc persists version info.

            if old_version < 1999 {
                // pre-release two
                println!("WARNING:");
                println!("a new release of Miranda has been installed since you last used");
                println!("the system - please read the `CHANGES` section of the /man pages !!!");
                println!();
            } else if version > old_version {
                println!("a new version of Miranda has been installed since you last");
                println!("used the system - see under `CHANGES` in the /man pages");
                println!();
            }

            if version < old_version {
                println!("warning - this is an older version of Miranda than the one");
                println!("you last used on this machine!!");
                println!();
            }

            if let Some(rc_error) = rc_error {
                println!("warning: \"{}\" contained bad data (ignored)", rc_error);
            }
            */
        }
    }

    fn mira_setup(&mut self) {
        self.setup_constants();
        self.setup_standard_types();

        // reset_pns(); // Setup private namespace. We use a vector that requires no setup.
        // Enters the primitive identifiers into the primitive environment. Called by "mira_setup".
        self.primlib()
    }

    /// Most of Miranda's `mira_setup()` is here.
    ///
    /// Setup and initialization of [`Heap`](crate::data::heap::Heap) occurs in
    /// [`Heap::default()`](crate::data::heap::Heap::default()),
    /// [`Heap::setup_constants()`](crate::data::heap::Heap::setup_constants()), and
    /// [`Heap::setup_standard_types()`](crate::data::heap::Heap::setup_standard_types()).
    fn setup_constants(&mut self) {
        // Referenced below
        // Nil lives in `Heap` (`Heap::nill`) because some `Heap` functions use it
        self.void_ = self.heap.make_empty_identifier("()");
        IdentifierRecordRef::new(
            &mut self.heap,
            "()".to_string(),
            IdentifierDefinitionRef::undefined(),
            Type::Void.into(),
            None,
        );
        // *self.heap.id_type(self.void_) = Type::Void as RawValue;
        // *self.heap.id_val(self.void_)  = self.heap.constructor(0, self.void_).into();
        let value: Value = self.heap.constructor_ref(0, self.void_.into());
        self.void_
            .set_value(&mut self.heap, IdentifierValueRef::from_ref(value.into()));

        self.common_stdin = self.heap.apply_ref(Combinator::Read.into(), Value::from(0));
        self.common_stdinb = self
            .heap
            .apply_ref(Combinator::ReadBin.into(), Value::from(0));
        let readvals = self.heap.readvals_ref(0.into(), 0.into());
        self.cook_stdin = self.heap.apply_ref(
            readvals,
            // Todo: Should Offside be a combinator?
            Token::Offside.into(),
        );
        self.concat = self.heap.make_empty_identifier("concat");
        self.diagonalise = self.heap.make_empty_identifier("diagonalise");
        self.indent_fn = self.heap.make_empty_identifier("indent");
        self.listdiff_fn = self.heap.make_empty_identifier("listdiff");
        self.main_id = self.heap.make_empty_identifier("main"); // Miranda: change to magic scripts 19.11.2013
        self.message = self.heap.make_empty_identifier("sys_message");
        self.outdent_fn = self.heap.make_empty_identifier("outdent");
        self.showabstract = self.heap.make_empty_identifier("showabstract");
        self.showbool = self.heap.make_empty_identifier("showbool");
        self.showchar = self.heap.make_empty_identifier("showchar");
        self.showfunction = self.heap.make_empty_identifier("showfunction");
        self.showlist = self.heap.make_empty_identifier("showlist");
        self.shownum1 = self.heap.make_empty_identifier("shownum1");
        self.showpair = self.heap.make_empty_identifier("showpair");
        self.showparen = self.heap.make_empty_identifier("showparen");
        self.showstring = self.heap.make_empty_identifier("showstring");
        self.showvoid = self.heap.make_empty_identifier("showvoid");
        self.showwhat = self.heap.make_empty_identifier("showwhat");
        let stdout_ = self.heap.string("Stdout");
        self.stdout = self.heap.constructor_ref(0, stdout_.into());
    }

    /// This is tsetup() in Miranda.
    ///
    /// Setup and initialization of [`Heap`](crate::data::heap::Heap) occurs in
    /// [`Heap::default()`](crate::data::heap::Heap::default()),
    /// [`Heap::setup_constants()`](crate::data::heap::Heap::setup_constants()), and
    /// [`Heap::setup_standard_types()`](crate::data::heap::Heap::setup_standard_types()).
    fn setup_standard_types(&mut self) {
        self.numeric_function_type = self
            .heap
            .arrow_type_ref(Type::Number.into(), Type::Number.into());
        self.numeric_function2_type = self
            .heap
            .arrow_type_ref(Type::Number.into(), self.numeric_function_type);
        self.boolean_function_type = self
            .heap
            .arrow_type_ref(Type::Bool.into(), Type::Bool.into());
        self.boolean_function2_type = self
            .heap
            .arrow_type_ref(Type::Bool.into(), self.boolean_function_type);
        self.char_list_type = self.heap.list_type_ref(Type::Char.into());
        self.string_function_type = self
            .heap
            .arrow_type_ref(self.char_list_type, self.char_list_type);

        // tfnumnum is identical to self.numeric_function_type
        // let tfnumnum   = tf(num_t   , num_t);

        let number_list_type: Value = self.heap.list_type_ref(Type::Number.into());
        self.range_step_type =
            self.heap
                .arrow2_type_ref(Type::Number.into(), Type::Number.into(), number_list_type);

        self.range_step_until_type = self
            .heap
            .arrow_type_ref(Type::Number.into(), self.range_step_type);
    }

    /// The primdef function just creates an identifier on the heap and appends it to the primitive environment.
    fn primitive_synonym_definition(&mut self, name: &str, type_: Type) {
        // self.primdef("num"  , make_typ(0, 0, IdentifierValueType::Synonym, Type::Number), Type::Type);
        let h_id_value_type = IdentifierValueTypeRef::new(
            &mut self.heap,
            IdentifierValueTypeData::Synonym { source_type: type_ },
        );
        let h_id_value_data = IdentifierValueData::Typed {
            arity: 0,
            show_function: Value::None,
            value_type: h_id_value_type,
        };
        let h_id_value = IdentifierValueRef::new(&mut self.heap, h_id_value_data);
        let h_id = IdentifierRecordRef::new(
            &mut self.heap,
            name.to_string(),
            IdentifierDefinitionRef::undefined(),
            Type::Type.into(),
            Some(h_id_value),
        );

        self.primitive_environment.push(&mut self.heap, h_id);
    }

    /// The analog of `primitive_synonym_definition` for the constants `True` and `False`
    fn primitive_bool_definition(&mut self, name: &str, tv: RawValue) {
        // self.primdef("True" , Value::Data(1), Type::Bool); // accessible only to 'finger'

        let h_value_data = IdentifierValueData::Arbitrary(Value::Data(tv));
        let h_value = IdentifierValueRef::new(&mut self.heap, h_value_data);
        let h_bool_constant = IdentifierRecordRef::new(
            &mut self.heap,
            name.to_string(),
            IdentifierDefinitionRef::undefined(),
            Type::Bool.into(),
            Some(h_value),
        );

        self.primitive_environment
            .push(&mut self.heap, h_bool_constant);
    }

    /// Enters the primitive identifiers into the primitive environment, sets up predefined ids not referred to by
    /// `parser.y`. Called by [VM::mira_setup()].
    fn primlib(&mut self) {
        self.primitive_synonym_definition("num", Type::Number);
        self.primitive_synonym_definition("char", Type::Char);
        self.primitive_synonym_definition("bool", Type::Bool);

        self.primitive_bool_definition("True", 1); // accessible only to 'finger'
        self.primitive_bool_definition("False", 0); // likewise - FIX LATER

        // The preceding code is the equivalent of the following.
        // self.primdef("num"  , make_typ(0, 0, IdentifierValueType::Synonym, Type::Number), Type::Type);
        // self.primdef("char" , make_typ(0, 0, IdentifierValueType::Synonym, Type::Char)  , Type::Type);
        // self.primdef("bool" , make_typ(0, 0, IdentifierValueType::Synonym, Type::Bool)  , Type::Type);
        // self.primdef("True" , Value::Data(1), Type::Bool);
        // self.primdef("False", Value::Data(0), Type::Bool);
    }

    // / Reset all variables used by the compiler.
    // ToDo: This method is unnecessary. You'd just drop the VM and make a new instance.
    /*
    fn reset_state(&mut self){
    if self.command_mode {
      while c != '\n' && c != EOF {
        c = getc(s_in);
      }
    }  /* no echo */
    // Close all files in file queue and empty the queue.
    self.file_queue.clear();
    // fclose((FILE *)hd[hd[fileq]]);
    // fileq = tl[fileq];

    insertdepth = -1;
    self.in_file = Box::new(stdin());
    echostack = idsused = prefixstack = litstack = linostack = vergstack = margstack = self.nill;
    prefix = 0;
    prefixbase[0] = '\0';
    echoing = verbosity & listing;
    brct = inbnf = sreds = inlex = inexplist = commandmode = lverge = col = lmargin = 0;
    atnl = 1;
    rv_script = 0;
    algshfns = newtyps = showchain = SGC = TABSTRS = self.nill;
    c = ' ';
    line_no = 0;
    litmain = literate = 0;
    }
    */

    /// Adds the item (type, identifier, etc.) to the environment, i.e. cons it onto the definienda of the first
    /// item in the `files` cons list.
    fn add_to_environment(&mut self, item: IdentifierRecordRef) {
        // The thread of pointers goes like this:
        //     self.files == cons(first, rest);
        //     head(self.files) == first
        //     first == cons(cons(fileinfo(filename, mtime), share), definienda)
        //     tail( first  ) == definienda
        if let Some(current_file) = self.files.head(&self.heap) {
            current_file.push_item(&mut self.heap, item.into());
        }
    }

    /// A convenience method used by `privlib(..)` and `stdlib(..)`, see below. It creates an identifier
    /// with the given name, value, and datatype, constructing the value according to whether the name is
    /// that of a constructor (capitalized) or not, and then it adds the identifier to the environment.
    fn predefine_identifier<T>(&mut self, name: &str, value: Value, datatype: T)
    where
        T: Into<Value>,
    {
        let id_ref = self.heap.predefine_identifier(name, value, datatype);

        self.add_to_environment(id_ref);
    }

    // Todo: Why aren't privlib, primlib, and stdlib combined into one and/or all called at once?
    // Todo: Is it ok that some of these are identical to the ones in `stdlib(..)`?
    ///  Adds some internally defined identifiers to the environment. Called when compiling `<prelude>`.
    fn privlib(&mut self) {
        self.predefine_identifier("offside", Token::Offside.into(), self.char_list_type); // Used by `indent' in prelude
        self.predefine_identifier("changetype", Combinator::I.into(), Type::Wrong); // Type::Wrong to prevent being typechecked
        self.predefine_identifier("first", Combinator::Hd.into(), Type::Wrong);
        self.predefine_identifier("rest", Combinator::Tl.into(), Type::Wrong);
        // The following added to make prelude compilable without `stdenv`
        self.predefine_identifier("code", Combinator::Code.into(), Type::Undefined);
        let concat = self.heap.apply2(
            Combinator::FoldR.into(),
            Combinator::Append.into(),
            Combinator::Nil.into_value(),
        );
        self.predefine_identifier("concat", concat.into(), Type::Undefined);
        self.predefine_identifier("decode", Combinator::Decode.into(), Type::Undefined);
        self.predefine_identifier("drop", Combinator::Drop.into(), Type::Undefined);
        self.predefine_identifier("error", Combinator::Error_.into(), Type::Undefined);
        self.predefine_identifier("filter", Combinator::Filter.into(), Type::Undefined);
        self.predefine_identifier("foldr", Combinator::FoldR.into(), Type::Undefined);
        self.predefine_identifier("hd", Combinator::Hd.into(), Type::Undefined);
        self.predefine_identifier("map", Combinator::Map.into(), Type::Undefined);
        self.predefine_identifier("shownum", Combinator::ShowNum.into(), Type::Undefined);
        self.predefine_identifier("take", Combinator::Take.into(), Type::Undefined);
        self.predefine_identifier("tl", Combinator::Tl.into(), Type::Undefined);
    }

    /// Called when compiling `<stdenv>`. Adds some internally defined identifiers to the environment
    fn stdlib(&mut self) {
        self.predefine_identifier("arctan", Combinator::Arctan_Fn.into(), Type::Undefined);
        self.predefine_identifier("code", Combinator::Code.into(), Type::Undefined);
        self.predefine_identifier("cos", Combinator::Cos_Fn.into(), Type::Undefined);
        self.predefine_identifier("decode", Combinator::Decode.into(), Type::Undefined);
        self.predefine_identifier("drop", Combinator::Drop.into(), Type::Undefined);
        self.predefine_identifier("entier", Combinator::Entier_Fn.into(), Type::Undefined);
        self.predefine_identifier("error", Combinator::Error_.into(), Type::Undefined);
        self.predefine_identifier("exp", Combinator::Exp_Fn.into(), Type::Undefined);
        self.predefine_identifier("filemode", Combinator::FileMode.into(), Type::Undefined);
        self.predefine_identifier("filestat", Combinator::FileStat.into(), Type::Undefined); // Added Feb 91
        self.predefine_identifier("foldl", Combinator::FoldL.into(), Type::Undefined);
        self.predefine_identifier("foldl1", Combinator::FoldL1.into(), Type::Undefined); // New at release 2
        let huge_num = self.heap.real_ref(f64::MAX);
        // Note: Miranda says, "`hugenum' is the largest fractional number that can exist in this implementation (should
        // be around 1e308 for IEEE standard 64 bit floating point." We use the exact value provided by `f64::MAX`.
        self.predefine_identifier("hugenum", huge_num, Type::Undefined);
        self.predefine_identifier("last", Combinator::ListLast.into(), Type::Undefined);
        self.predefine_identifier("foldr", Combinator::FoldR.into(), Type::Undefined);
        self.predefine_identifier("force", Combinator::Force.into(), Type::Undefined);
        self.predefine_identifier("getenv", Combinator::GetEnv.into(), Type::Undefined);
        self.predefine_identifier("integer", Combinator::Integer.into(), Type::Undefined);
        self.predefine_identifier("log", Combinator::Log_Fn.into(), Type::Undefined);
        self.predefine_identifier("log10", Combinator::Log10_Fn.into(), Type::Undefined); // New at release 2
        self.predefine_identifier("merge", Combinator::Merge.into(), Type::Undefined); // New at release 2
        self.predefine_identifier("numval", Combinator::NumVal.into(), Type::Undefined);
        self.predefine_identifier("read", Combinator::StartRead.into(), Type::Undefined);
        self.predefine_identifier("readb", Combinator::StartReadBin.into(), Type::Undefined);
        self.predefine_identifier("seq", Combinator::Seq.into(), Type::Undefined);
        self.predefine_identifier("shownum", Combinator::ShowNum.into(), Type::Undefined);
        self.predefine_identifier("showhex", Combinator::ShowHex.into(), Type::Undefined);
        self.predefine_identifier("showoct", Combinator::ShowOct.into(), Type::Undefined);
        self.predefine_identifier("showfloat", Combinator::ShowFloat.into(), Type::Undefined); // New at release 2
        self.predefine_identifier("showscaled", Combinator::ShowScaled.into(), Type::Undefined); // New at release 2
        self.predefine_identifier("sin", Combinator::Sin_Fn.into(), Type::Undefined);
        self.predefine_identifier("sqrt", Combinator::Sqrt_Fn.into(), Type::Undefined);
        self.predefine_identifier("system", Combinator::Exec.into(), Type::Undefined); // New at release 2
        self.predefine_identifier("take", Combinator::Take.into(), Type::Undefined);
        let tiny_num = self.heap.real_ref(f64::MIN_POSITIVE);
        // Note: This is likely different from Miranda's `mktiny()`. Miranda says, "`tinynum' is
        // the smallest positive fractional number that can be distinguished from zero in this
        // implementation (should be around 1e-324 for IEEE standard 64 bit floating point)."
        // We instead use the exact value provided by `f64::MIN_POSITIVE`.
        self.predefine_identifier("tinynum", tiny_num, Type::Undefined); // New at release 2
        self.predefine_identifier("zip2", Combinator::Zip.into(), Type::Undefined);
        // New at release 2
    }

    /// Clear out current script in preparation for reloading
    fn unload(&mut self) {
        self.sorted = false;
        self.spec_location = ConsList::EMPTY;
        self.rv_script = false;
        self.algebraic_show_functions = ConsList::EMPTY;
        self.heap.private_symbols.clear();

        // Todo: Make `unsetids` take a `ConsList`.
        self.unset_ids(self.new_type_names);
        self.new_type_names = ConsList::EMPTY;

        self.unset_ids(self.free_ids);
        self.free_ids = ConsList::EMPTY;

        self.sui_generis_constructors = ConsList::EMPTY;
        self.includees = ConsList::EMPTY;
        self.type_abstractions = ConsList::EMPTY;
        self.undefined_names = ConsList::EMPTY;

        self.unset_ids(self.internals);
        self.internals = ConsList::EMPTY;

        while !self.files.is_empty() {
            // let files: ConsList   = ConsList::from_ref(self.files.into());
            // Guaranteed to unwrap because self.files != ConsList::EMPTY.
            let file: FileRecord = self.files.pop(&self.heap).unwrap();

            let definienda = file.get_definienda(&self.heap);
            if !definienda.is_empty() {
                self.unset_ids(ConsList::<IdentifierRecordRef>::from_ref(
                    definienda.get_ref(),
                ))
                // unsetids(fil_defs(hd[files]));
            }
            file.clear_definienda(&mut self.heap); // fil_defs(hd[files]) = NIL;
        }

        // Remember `self.mkinclude_files` is a nested list of lists.
        while !self.mkinclude_files.is_empty() {
            let mut file_list = self.mkinclude_files.pop(&self.heap).unwrap();

            while !file_list.is_empty() {
                let file: FileRecord = file_list.pop(&self.heap).unwrap();
                let definienda: ConsList = file.get_definienda(&self.heap);

                if !definienda.is_empty() {
                    // Todo: Miranda checks that the item has Tag::Id and just continues if not.
                    //       Do we expect everything in `id_list` to be an identifier?
                    self.unset_ids(ConsList::<IdentifierRecordRef>::from_ref(
                        definienda.get_ref(),
                    ))
                    // unsetids(fil_defs(hd[files]));
                }
            }
        }
    }

    fn unset_ids(&mut self, mut id_list: ConsList<IdentifierRecordRef>) {
        while !id_list.is_empty() {
            // Todo: Miranda checks that the item has Tag::Id and just continues if not.
            //       Do we expect everything in `id_list` to be an identifier?
            let id_record: IdentifierRecordRef = id_list.pop(&mut self.heap).unwrap();
            id_record.unset_id(&mut self.heap);
            // should we remove from namebucket ?
        }
    }

    /// load a sequence of definitions terminated by `DEF_X`, or a single object terminated
    /// by `DEF_X`, from the byte stream `byte_iter`.
    ///
    /// The type of the returned value has to be an opaque type, because any serializable object can be returned.
    // Todo: Some calls to `load_defs()` assume that a cons list, or even a `ConsList<IdentifierRecordRef>`, is returned,
    //       but it's clear that there are times when the return value is not a cons list.
    //       The code in Miranda for this is a complete and total mess.
    fn load_defs(
        &mut self,
        byte_iter: &mut dyn Iterator<Item = u8>,
    ) -> Result<Value, BytecodeError> {
        // Holds a list of definitions in cases where multiple definitions are read.
        let mut defs: ConsList = ConsList::EMPTY;
        // Holds the components of an item that have been read so far. When all of the components have been read, the
        // item will be created using `item_stack`.
        let mut item_stack: Vec<RawValue> = Vec::new();

        while let Some(ch) = byte_iter.next() {
            // Decode the byte
            let code = if let Some(code) = Bytecode::from_u8(ch) {
                code
            } else {
                // It is a RawValue.
                let v = if ch > 127 {
                    ch as RawValue + 256
                } else {
                    ch as RawValue
                };
                item_stack.push(v.into());
                continue;
            };

            match code {
                Bytecode::Char => {
                    let v = next(byte_iter)?;
                    item_stack.push(v as RawValue + 128);
                }

                Bytecode::TypeVariable => {
                    let v = next(byte_iter)?;
                    let type_var = self
                        .heap
                        .type_var_ref(Value::None.into(), (v as RawValue).into());
                    item_stack.push(type_var.into());
                }

                // Todo: Do we want to support "small" integers? Yes for now, for Miranda compatibility.
                Bytecode::Short => {
                    let mut v = next(byte_iter)?;
                    if (v & 128u8) != 0 {
                        v = v | (!127u8);
                    }
                    item_stack.push(self.heap.small_int_ref(v as RawValue).into());
                }

                Bytecode::Integer => {
                    // Allow the very first value to be -1.
                    let mut v: RawValue = get_word_raw_value(byte_iter)?;
                    let int_list: RawValue = self.heap.integer_ref(v).into();

                    item_stack.push(int_list);

                    // The list of ints is constructed from the head to the tail, the opposite from if we used `push`.
                    // `cursor_ref` points to the cell whose tail is the next insertion point.
                    let mut cursor_ref: RawValue = int_list;
                    v = get_word_raw_value(byte_iter)?;

                    while v != -1 {
                        // Construct a boxed integer and store it in the tail of the previous boxed integer
                        let new_cell_ref: RawValue = self.heap.integer_ref(v).into();
                        self.heap[cursor_ref].tail = new_cell_ref;
                        // Read the next integer from the byte iterator
                        v = get_word_raw_value(byte_iter)?;
                        // Update the cursor to point to the tail of the newly constructed boxed integer
                        cursor_ref = new_cell_ref;
                    }
                }

                Bytecode::Double => {
                    let real_number = get_word_f64(byte_iter)?;

                    // Todo: We convert isize-->f64 and then f64-->isize. This is stupid.
                    item_stack.push(self.heap.real_ref(real_number).into());
                }

                Bytecode::Unicode => {
                    let v = get_word_raw_value(byte_iter)?;

                    item_stack.push(self.heap.unicode_ref(v).into());
                }

                // Reads in the index of a private symbol. Differs from `Bytecode::PrtivateName1` in that it only reads two
                // bytes.
                Bytecode::PrivateName => {
                    let mut v: usize = get_u16_le(byte_iter)? as usize;

                    // See the notes for `private_symbol_base_index` in `vm.load_script()`.
                    // Turn relative index into absolute index.
                    v += self.private_symbol_base_index;

                    let ps = self.heap.get_nth_private_symbol_ref(v);
                    item_stack.push(ps.into());

                    // Todo: This is an index into `vm.private_names`?
                    /* Miranda source code is:
                    ch = getc(f);
                    ch = PNBASE + (ch | (getc(f) << 8));
                    *stackp++ = ch < nextpn ? pnvec[ch] : sto_pn(ch);
                    // efficiency hack for *stackp++ = sto_pn(ch);
                    */
                }

                // This differs from Bytecode::PrivateName in that is reads `MACHINE_WORD_SIZE` bytes.
                Bytecode::PrivateName1 => {
                    let mut v = get_machine_word(byte_iter)?;
                    // See the notes  for `private_symbol_base_index` in `vm.load_script()`.
                    v += self.private_symbol_base_index;

                    let ps = self.heap.get_nth_private_symbol_ref(v);
                    item_stack.push(ps.into());
                }

                Bytecode::Construct => {
                    let v: i16 = get_i16_le(byte_iter)?;
                    // Wrap the top value on the stack with the constructor.
                    if item_stack.is_empty() {
                        item_stack.push(Combinator::Nil.into());
                    }
                    let last_value = item_stack.last_mut().unwrap();
                    *last_value = self.heap.constructor_ref(v, (*last_value).into()).into();

                    let new_value = self.heap.constructor_ref(v, (*last_value).into());
                    item_stack.push(new_value.into());
                }

                Bytecode::ReadVals => {
                    let previous_value = item_stack.pop().unwrap_or(Combinator::Nil.into());
                    let new_value = self
                        .heap
                        .start_read_vals_ref(Value::None.into(), previous_value.into());
                    item_stack.push(new_value.into());

                    self.rv_script = true;
                }

                Bytecode::ID => {
                    let name: String = parse_string(byte_iter)?;

                    match self.heap.symbol_table.get(name.as_str()) {
                        Some(id_ref) => {
                            let id = IdentifierRecordRef::from_ref((*id_ref).into());
                            let id_type = id.get_type(&self.heap);

                            if id_type == Type::New.into() {
                                // A name collision
                                self.clashes.insert_ordered(&mut self.heap, id);
                            } else if id_type == Type::Alias.into() {
                                // Follow the alias.
                                match id.get_value(&self.heap) {
                                    None => return Err(BytecodeError::MalformedDef),
                                    Some(id_value) => {
                                        item_stack.push(id_value.get_ref());
                                    }
                                }
                            } else {
                                // Can this happen?
                                item_stack.push((*id_ref).into());
                            }
                        }

                        None => {
                            // Create the ID
                            let id = self.heap.make_empty_identifier(name.as_str());
                            item_stack.push(id.get_ref())
                        }
                    }
                }

                Bytecode::AKA => {
                    let name = parse_string(byte_iter)?;
                    let id_ref = *self
                        .heap
                        .symbol_table
                        .get(name.as_str())
                        .ok_or(BytecodeError::SymbolNotFound)?;
                    let pair = DataPair::new(&mut self.heap, id_ref.into(), Value::None.into());

                    item_stack.push(pair.get_ref());
                }

                Bytecode::Here => {
                    let first = next(byte_iter)?;
                    let file_path: String = if first == 0 {
                        // `first == 0` is shorthand for "current file".
                        self.current_file()
                    } else if first != b'/' {
                        // Transform path into absolute path.
                        let prefix_ref = self.prefix_stack.value_head(&self.heap).unwrap();
                        let prefix = self.heap.resolve_string(prefix_ref).unwrap();

                        format!(
                            "{}{}{}",
                            prefix,
                            char::from(first),
                            parse_string(byte_iter)?
                        )
                    } else {
                        format!("/{}", parse_string(byte_iter)?)
                    };
                    let file_id = match self.heap.symbol_table.get(file_path.as_str()) {
                        Some(value) => value.clone(),
                        None => {
                            // Todo: What is the right thing to do here?
                            self.heap
                                .make_empty_identifier(file_path.as_str())
                                .get_ref()
                                .into()
                        }
                    };
                    let line_number = get_u16_le(byte_iter)? as usize;
                    let file_info = FileInfoRef::new(
                        &mut self.heap,
                        file_id.into(),
                        (line_number as RawValue).into(),
                    );

                    item_stack.push(file_info.get_ref());
                }

                Bytecode::Definition => {
                    // This function (`load_defs`) loads a sequence of definitions terminated by DEF_X, or a single object
                    // terminated by DEF_X. When we encounter `DEF_X`, we need to construct the item whose components have
                    // accumulated in `item_stack`. Also, in some cases `DEF_X` does not signal to stop loading definitions. A
                    // summary of the uses of `DEF_X`:
                    //
                    // | Case | Use                                   | Form                            |
                    // |------|:--------------------------------------|:--------------------------------|
                    // | 1    | Terminate a list of definitions       | `[definition*] DEF_X`           |
                    // | 2    | Terminate a single identifier record  | `[val] [type] [who] [id] DEF_X` |
                    // | 3    | Terminate a private name              | `[val] [pname] DEF_X`           |
                    // | 4    | Precedes list of free IDs             | `DEF_X [freeids]`               |
                    // | 5    | Precedes definition list of internals | `DEF_X [definition-list]`       |

                    // The different cases can be partially distinguished by the number of items already read into `defs`.
                    match item_stack.len() {
                        0 => {
                            // Case 1: Definition list delimiter, signals the end of a definition list.
                            return Ok(defs.reversed(&mut self.heap).into());
                        }

                        1 => {
                            // Case 2: Object delimiter, signals the end of an identifier record.
                            let item: RawValue = item_stack.pop().unwrap();
                            return Ok(item.into());
                        }

                        2 => {
                            // Case 3: Private name delimiter, signals end of a private name.
                            let item = item_stack.pop().unwrap();
                            self.heap[item].tail = item_stack.pop().unwrap();
                            defs.push(&mut self.heap, item);
                        }

                        4 => {
                            let mut top_item = *item_stack.last().unwrap();

                            if self.heap[top_item].tag != Tag::Id {
                                if top_item == NIL.into() {
                                    // FIX1
                                    item_stack.clear();
                                    continue;
                                }

                                // An ID aliased to a pname.
                                // A pname has the form `strcons(index_in_private_names_vector, value)`,
                                // where value (I think) is an Identifier Record.

                                item_stack.pop(); // Value already in top_item
                                                  // let top_item = item_stack.pop().unwrap();

                                // Todo: Check that `top_item` is a reference to an `IdentifierRecordRef`.
                                //       But it is gauranteed not to be an `IdentifierRecordRef`...
                                let new_id = IdentifierRecordRef::from_ref(top_item);
                                self.suppressed.push(&mut self.heap, new_id);

                                // Todo: Why are we throwing away the who field?
                                item_stack.pop(); // who field

                                let mut private_aka = {
                                    // Scope of temporary
                                    let top_item = item_stack.last().unwrap();
                                    if self.heap[*top_item].tag == Tag::Cons {
                                        self.heap[*top_item].head
                                    } else {
                                        NIL.into()
                                    }
                                };

                                // Todo: Why are we throwing away the type field?
                                let new_id_type = item_stack.pop().unwrap(); // type

                                // The value of the private name
                                let new_id_value =
                                    IdentifierValueRef::from_ref(item_stack.pop().unwrap());
                                new_id.set_value(&mut self.heap, new_id_value);

                                // let new_id_value_data = new_id_value.get_data(&self.heap);
                                if let IdentifierValueData::Typed { value_type, .. } =
                                    new_id_value.get_data(&self.heap)
                                {
                                    if new_id_type == Type::Type.into()
                                        && value_type.get_identifier_value_type_kind(&self.heap)
                                            != IdentifierValueTypeKind::Synonym
                                    {
                                        // Suppressed typename
                                        // Reverse assoc in ALIASES
                                        let mut aliases = self.aliases;
                                        let mut id: Option<IdentifierRecordRef> = None;
                                        while let Some(alias_value) = aliases.pop_value(&self.heap)
                                        {
                                            let alias: RawValue = alias_value.into();
                                            let alias_entry = AliasEntry::from_ref(alias);
                                            let inner =
                                                alias_entry.get_old_identifier_record(&self.heap); // a temporary
                                            id = Some(inner);
                                            // It's not clear if "get_value" is meant to be a get_value or if it is just a `tl[ref]`
                                            // operation.
                                            if let Some(found_val) = inner.get_value(&self.heap) {
                                                if matches!(
                                                    found_val.get_data(&self.heap),
                                                    IdentifierValueData::Arbitrary(v)
                                                        if v == Value::from(top_item)
                                                ) {
                                                    break;
                                                }
                                            }
                                            id = None;
                                        }
                                        if let Some(found_id) = id {
                                            // Surely must hold ??
                                            self.suppressed_t.push(&mut self.heap, found_id);
                                        }
                                    } else if matches!(
                                        new_id.get_value(&self.heap),
                                        Some(v)
                                            if matches!(
                                                v.get_data(&self.heap),
                                                IdentifierValueData::Undefined
                                            )
                                    ) {
                                        // Special kludge for undefined names, necessary only if we allow names specified
                                        // but not defined to be %included.
                                        if private_aka == Combinator::NIL.into() {
                                            // Reverse assoc in ALIASES
                                            let mut aliases = self.aliases;
                                            let mut id: Option<IdentifierRecordRef> = None;
                                            while let Some(alias_value) =
                                                aliases.pop_value(&self.heap)
                                            {
                                                let alias: RawValue = alias_value.into();
                                                let alias_entry = AliasEntry::from_ref(alias);
                                                let inner = alias_entry
                                                    .get_old_identifier_record(&self.heap); // a temporary
                                                id = Some(inner);
                                                // It's not clear if "get_value" is meant to be a get_value or if it is just a `tl[ref]`
                                                // operation.
                                                if let Some(found_val) = inner.get_value(&self.heap)
                                                {
                                                    if matches!(
                                                        found_val.get_data(&self.heap),
                                                        IdentifierValueData::Arbitrary(v)
                                                            if v == Value::from(top_item)
                                                    ) {
                                                        break;
                                                    }
                                                }
                                                id = None;
                                            }
                                            if let Some(found_id) = id {
                                                // Todo: Untangle what Miranda is doing here. What's the difference between `id_val` and `get_id`?
                                                private_aka = DataPair::new(
                                                    &mut self.heap,
                                                    found_id.get_ref().into(),
                                                    0.into(),
                                                )
                                                .get_ref();
                                            }
                                        }
                                        // this will generate sensible error message
                                        // see reduction rule for DATAPAIR
                                        let file_info_ref: RawValue = {
                                            let current_file_ref =
                                                self.heap.string(self.current_file());
                                            FileInfoRef::new(
                                                &mut self.heap,
                                                current_file_ref.into(),
                                                0.into(),
                                            )
                                            .get_ref()
                                        };
                                        let applied_value: RawValue = self
                                            .heap
                                            .apply_ref(
                                                private_aka.into(),
                                                IdentifierValueRef::from_ref(file_info_ref).into(),
                                            )
                                            .into();
                                        new_id.set_value(
                                            &mut self.heap,
                                            IdentifierValueRef::from_ref(applied_value),
                                        );
                                    }
                                    defs.push(&mut self.heap, top_item.into());
                                    continue;
                                }
                            }

                            // Previous if gaurantees top_item is an identifier.
                            // Todo: does it?
                            top_item = *item_stack.last().unwrap();
                            let new_id = IdentifierRecordRef::from_ref(top_item);
                            let new_id_type = new_id.get_type(&self.heap);
                            // The id's type will be an immediate value (not a reference) in the cases in the if condition below.
                            // Likewise for the id's value.
                            if new_id_type != Type::New.into()
                                && (new_id_type != Type::Undefined.into()
                                    || new_id.get_value_field(&self.heap)
                                        != Combinator::Undef.into())
                            {
                                if new_id_type == Type::Alias.into() {
                                    // cyclic aliasing
                                    let mut aliases = self.aliases;
                                    let mut alias: RawValue = NIL.into();
                                    let mut id: Option<IdentifierRecordRef> = None;
                                    while !aliases.is_empty() {
                                        alias = aliases.pop_value(&self.heap).unwrap().into();
                                        let alias_entry = AliasEntry::from_ref(alias);
                                        id =
                                            Some(alias_entry.get_old_identifier_record(&self.heap));

                                        if alias_entry.old_identifier_matches(
                                            &self.heap,
                                            IdentifierRecordRef::from_ref(top_item),
                                        ) {
                                            break;
                                        }

                                        id = None;
                                    }
                                    if id.is_none() {
                                        // Todo: Make infrastructure for nonfatal errors.
                                        eprintln!(
                                            "impossible event in cyclic alias ({:?})",
                                            new_id.get_name(&self.heap)
                                        );
                                        item_stack.clear();
                                        continue;
                                    }
                                    defs.push(&mut self.heap, top_item);

                                    // Manipulating the alias, not an id.
                                    let alias_entry = AliasEntry::from_ref(alias);
                                    let definition = IdentifierDefinitionRef::from_ref(
                                        item_stack.pop().unwrap(),
                                    );
                                    let datatype: Value = item_stack.pop().unwrap().into();
                                    let value_ref = item_stack.pop().unwrap();
                                    let value = if value_ref == Combinator::Nil.into() {
                                        None
                                    } else {
                                        Some(IdentifierValueRef::from_ref(value_ref))
                                    };
                                    let restored_core = IdentifierCoreData {
                                        definition,
                                        datatype,
                                        value,
                                    };
                                    alias_entry.set_cyclic_hold_data(&mut self.heap, restored_core);
                                    continue;
                                }

                                self.clashes.insert_ordered(&mut self.heap, new_id);
                                item_stack.clear();
                            } else {
                                defs.push(&mut self.heap, top_item);

                                #[cfg(feature = "debug")]
                                println!("{} undumped", new_id.get_name(&self.heap));

                                item_stack.pop(); // top_item
                                                  // who
                                new_id.set_definition(
                                    &mut self.heap,
                                    IdentifierDefinitionRef::from_ref(item_stack.pop().unwrap()),
                                );
                                // type
                                new_id.set_type(&mut self.heap, item_stack.pop().unwrap().into());
                                // value
                                new_id.set_value(
                                    &mut self.heap,
                                    IdentifierValueRef::from_ref(item_stack.pop().unwrap()),
                                );
                            }
                        }

                        _ => {
                            // Todo: Miranda also returns the defs
                            return Err(BytecodeError::MalformedDef);
                        }
                    } // end math on item_stack.len()
                } // end Bytecode::Definitions match branch

                Bytecode::Apply => {
                    // Miranda does not check length of item_stack
                    let top_item = item_stack.pop().unwrap();
                    let next_item = item_stack.pop().unwrap();
                    if next_item == Combinator::Read.into() && top_item == 0 {
                        item_stack.push(self.common_stdin.into());
                    } else if next_item == Combinator::ReadBin.into() && top_item == 0 {
                        item_stack.push(self.common_stdinb.into());
                    } else {
                        item_stack.push(
                            self.heap
                                .apply_ref(next_item.into(), top_item.into())
                                .into(),
                        );
                    }
                } // end Bytecode::Apply branch

                Bytecode::Cons => {
                    let head = item_stack.pop().unwrap();
                    let tail = item_stack.pop().unwrap();
                    item_stack.push(self.heap.cons_ref(head.into(), tail.into()).into());
                }
            } // end match on bytecode value
        }

        // Miranda returns `defs`, too.
        return Err(BytecodeError::MalformedDef); // Miranda: "should unsetids"
    }
}

/// Convenience function that returns the next byte or `BytecodeError::UnexpectedEOF`.
fn next(byte_iter: &mut dyn Iterator<Item = u8>) -> Result<u8, BytecodeError> {
    match byte_iter.next() {
        Some(ch) => Ok(ch),

        None => Err(BytecodeError::unexpected_eof()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn unique_test_path(file_name: &str) -> PathBuf {
        let mut base = std::env::temp_dir();
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos();
        base.push(format!("randa-vm-tests-{}-{}", std::process::id(), nanos));
        std::fs::create_dir_all(&base).expect("failed to create temp test directory");
        base.push(file_name);
        base
    }

    #[test]
    fn load_file_missing_script_is_allowed_outside_initialization() {
        let mut vm = VM::new_for_tests();
        vm.initializing = false;
        let source_path = unique_test_path("missing_script.m");
        let source_path_str = source_path.to_string_lossy().to_string();

        let result = vm.load_file(&source_path_str);

        assert!(result.is_ok());
        assert!(!vm.loading);
        assert!(!vm.files.is_empty());
        assert!(!vm.old_files.is_empty());
    }

    #[test]
    fn load_file_missing_script_errors_during_initialization() {
        let mut vm = VM::new_for_tests();
        vm.initializing = true;
        let source_path = unique_test_path("missing_prelude.m");
        let source_path_str = source_path.to_string_lossy().to_string();

        let result = vm.load_file(&source_path_str);

        assert!(matches!(
            result,
            Err(BytecodeError::MissingSourceFile { path }) if path == source_path_str
        ));
        assert!(!vm.loading);
    }

    #[test]
    fn undump_propagates_load_file_error_after_bad_dump() {
        let mut vm = VM::new_for_tests();
        vm.initializing = false;

        let source_path = unique_test_path("script.m");
        std::fs::write(&source_path, "-- test source\n").expect("failed to write source test file");

        let binary_path = source_path.with_extension("x");
        std::fs::write(&binary_path, [0_u8, 0_u8, 0_u8, 0_u8])
            .expect("failed to write binary test file");

        let source_path_str = source_path.to_string_lossy().to_string();
        let result = vm.undump(&source_path_str);

        assert!(matches!(
            result,
            Err(BytecodeError::ParserIntegrationDeferred { path }) if path == source_path_str
        ));
        assert!(!vm.loading);
    }

    #[test]
    fn undump_falls_back_to_load_file_after_wrong_bytecode_version() {
        let mut vm = VM::new_for_tests();
        vm.initializing = false;

        let source_path = unique_test_path("script_version_mismatch.m");
        std::fs::write(&source_path, "-- test source\n").expect("failed to write source test file");

        let binary_path = source_path.with_extension("x");
        let mut bad_dump = vec![0_u8; 16];
        bad_dump[0] = WORD_SIZE as u8;
        bad_dump[1] = (XVERSION as u8).wrapping_add(1);
        std::fs::write(&binary_path, bad_dump).expect("failed to write binary test file");

        let source_path_str = source_path.to_string_lossy().to_string();
        let result = vm.undump(&source_path_str);

        assert!(matches!(
            result,
            Err(BytecodeError::ParserIntegrationDeferred { path }) if path == source_path_str
        ));
        assert!(!vm.loading);
    }

    #[test]
    fn load_file_parser_deferred_keeps_attempted_source_in_old_files() {
        let mut vm = VM::new_for_tests();
        vm.initializing = false;

        vm.files = vm.empty_environment_for_source("prior_source.m", UNIX_EPOCH);

        let source_path = unique_test_path("parser_deferred.m");
        std::fs::write(&source_path, "-- no parse marker\n")
            .expect("failed to write source test file");
        let source_path_str = source_path.to_string_lossy().to_string();

        let result = vm.load_file(&source_path_str);

        assert!(matches!(
            result,
            Err(BytecodeError::ParserIntegrationDeferred { path }) if path == source_path_str
        ));
        assert_eq!(vm.last_load_phase_trace, vec!["begin", "parse"]);
        assert!(!vm.loading);
        assert!(vm.files.is_empty());
        assert!(!vm.old_files.is_empty());

        let old_anchor = vm
            .old_files
            .head(&vm.heap)
            .expect("missing old_files anchor");
        assert_eq!(old_anchor.get_file_name(&vm.heap), source_path_str);
    }

    #[test]
    fn load_file_missing_script_errors_during_make_mode() {
        let mut vm = VM::new_for_tests();
        vm.initializing = false;
        vm.making = true;
        let source_path = unique_test_path("missing_make_target.m");
        let source_path_str = source_path.to_string_lossy().to_string();

        let result = vm.load_file(&source_path_str);

        assert!(matches!(
            result,
            Err(BytecodeError::MissingSourceFile { path }) if path == source_path_str
        ));
        assert_eq!(
            vm.last_load_phase_trace,
            vec!["begin", "missing-source-error"]
        );
        assert!(!vm.loading);
    }

    #[test]
    fn load_file_normalizes_non_m_source_paths() {
        let mut vm = VM::new_for_tests();
        vm.initializing = false;

        let source_path_no_ext = unique_test_path("normalize_target");
        let source_path_with_m = source_path_no_ext.with_extension("m");
        std::fs::write(&source_path_with_m, "-- test source\n")
            .expect("failed to write source test file");

        let requested_path = source_path_no_ext.to_string_lossy().to_string();
        let normalized_path = source_path_with_m.to_string_lossy().to_string();
        let result = vm.load_file(&requested_path);

        assert!(matches!(
            result,
            Err(BytecodeError::ParserIntegrationDeferred { path }) if path == normalized_path
        ));
        assert!(!vm.loading);
    }

    #[test]
    fn load_file_runs_placeholder_phase_pipeline_when_parse_is_marked_ok() {
        let mut vm = VM::new_for_tests();
        vm.initializing = false;

        let source_path = unique_test_path("phase_pipeline.m");
        std::fs::write(&source_path, "RANDA_PARSE_OK\n").expect("failed to write source test file");
        let source_path_str = source_path.to_string_lossy().to_string();

        let result = vm.load_file(&source_path_str);

        assert!(result.is_ok());
        assert_eq!(
            vm.last_load_phase_trace,
            vec![
                "begin",
                "parse",
                "exportfile-checks",
                "include-expansion",
                "typecheck",
                "export-closure",
                "bereaved-warnings",
                "unused-diagnostics",
                "codegen",
                "dump-visibility",
                "dump-fixexports",
                "dump-write",
                "dump-unfixexports",
                "success-postlude",
            ]
        );
        assert!(!vm.loading);
        assert!(vm.sorted);
    }

    #[test]
    fn load_file_uses_syntax_fallback_when_parse_marks_syntax_error() {
        let mut vm = VM::new_for_tests();
        vm.initializing = false;

        let source_path = unique_test_path("syntax_fallback.m");
        std::fs::write(&source_path, "RANDA_PARSE_SYNTAX_ERROR\n")
            .expect("failed to write source test file");
        let source_path_str = source_path.to_string_lossy().to_string();

        let result = vm.load_file(&source_path_str);

        assert!(result.is_ok());
        assert_eq!(
            vm.last_load_phase_trace,
            vec![
                "begin",
                "parse",
                "syntax-fallback",
                "syntax-unload",
                "syntax-dump-decision",
                "dump-write",
                "syntax-state-reset",
            ]
        );
        assert!(vm.files.is_empty());
        assert!(!vm.old_files.is_empty());
        assert!(!vm.loading);
    }

    #[test]
    fn load_file_reports_syntax_error_during_initialization() {
        let mut vm = VM::new_for_tests();
        vm.initializing = true;

        let source_path = unique_test_path("init_syntax.m");
        std::fs::write(&source_path, "RANDA_PARSE_SYNTAX_ERROR\n")
            .expect("failed to write source test file");
        let source_path_str = source_path.to_string_lossy().to_string();

        let result = vm.load_file(&source_path_str);

        assert!(matches!(
            result,
            Err(BytecodeError::SyntaxErrorInSource { path }) if path == source_path_str
        ));
        assert_eq!(
            vm.last_load_phase_trace,
            vec!["begin", "parse", "syntax-fallback"]
        );
        assert!(!vm.loading);
    }

    #[test]
    fn success_postlude_phase_resets_syntax_editor_state() {
        let mut vm = VM::new_for_tests();
        vm.initializing = false;
        vm.sorted = false;
        vm.error_line = 42;
        vm.errs = vec![Combinator::True.into()];

        let source_path = unique_test_path("success_postlude_reset.m");
        std::fs::write(&source_path, "RANDA_PARSE_OK\n").expect("failed to write source test file");
        let source_path_str = source_path.to_string_lossy().to_string();

        let result = vm.load_file(&source_path_str);

        assert!(result.is_ok());
        assert!(vm.sorted);
        assert_eq!(vm.error_line, 0);
        assert!(vm.errs.is_empty());
        assert!(!vm.old_files.is_empty());
    }

    #[test]
    fn syntax_fallback_resets_syntax_editor_state_outside_initialization() {
        let mut vm = VM::new_for_tests();
        vm.initializing = false;
        vm.error_line = 17;
        vm.errs = vec![Combinator::False.into()];
        vm.includees = vm.empty_environment_for_source("stale_includee.m", UNIX_EPOCH);

        let source_path = unique_test_path("syntax_reset.m");
        std::fs::write(&source_path, "RANDA_PARSE_SYNTAX_ERROR\n")
            .expect("failed to write source test file");
        let source_path_str = source_path.to_string_lossy().to_string();

        let result = vm.load_file(&source_path_str);

        assert!(result.is_ok());
        assert_eq!(vm.error_line, 0);
        assert!(vm.errs.is_empty());
        assert!(vm.files.is_empty());
        assert!(vm.includees.is_empty());
        assert!(!vm.old_files.is_empty());
    }

    #[test]
    fn exportfile_phase_errors_when_path_is_not_in_includees() {
        let mut vm = VM::new_for_tests();

        let path = unique_test_path("exports_only.m")
            .to_string_lossy()
            .to_string();
        let path_ref = vm.heap.string(path.clone());
        vm.exportfiles = vm.heap.cons_ref(Value::Reference(path_ref), NIL).into();
        vm.includees = ConsList::EMPTY;

        let result = vm.validate_exportfile_bindings_partial();

        assert!(matches!(
            result,
            Err(BytecodeError::ExportFileNotIncludedInScript { path: p }) if p == path
        ));
    }

    #[test]
    fn exportfile_phase_errors_when_path_binding_is_ambiguous() {
        let mut vm = VM::new_for_tests();

        let path = unique_test_path("ambiguous.m")
            .to_string_lossy()
            .to_string();
        let path_ref = vm.heap.string(path.clone());
        vm.exportfiles = vm.heap.cons_ref(Value::Reference(path_ref), NIL).into();

        let first = FileRecord::new(
            &mut vm.heap,
            path.clone(),
            UNIX_EPOCH,
            false,
            ConsList::EMPTY,
        );
        let second = FileRecord::new(
            &mut vm.heap,
            path.clone(),
            UNIX_EPOCH,
            false,
            ConsList::EMPTY,
        );
        vm.includees = ConsList::new(&mut vm.heap, first);
        vm.includees.push(&mut vm.heap, second);

        let result = vm.validate_exportfile_bindings_partial();

        assert!(matches!(
            result,
            Err(BytecodeError::ExportFileAmbiguous { path: p }) if p == path
        ));
    }

    #[test]
    fn exportfile_phase_accepts_unique_included_binding() {
        let mut vm = VM::new_for_tests();

        let path = unique_test_path("included_once.m")
            .to_string_lossy()
            .to_string();
        let path_ref = vm.heap.string(path.clone());
        vm.exportfiles = vm.heap.cons_ref(Value::Reference(path_ref), NIL).into();

        let includee = FileRecord::new(&mut vm.heap, path, UNIX_EPOCH, false, ConsList::EMPTY);
        vm.includees = ConsList::new(&mut vm.heap, includee);

        let result = vm.validate_exportfile_bindings_partial();

        assert!(result.is_ok());
    }

    #[test]
    fn include_expansion_phase_appends_includees_and_clears_bookkeeping() {
        let mut vm = VM::new_for_tests();

        let main_file = FileRecord::new(
            &mut vm.heap,
            unique_test_path("main.m").to_string_lossy().to_string(),
            UNIX_EPOCH,
            false,
            ConsList::EMPTY,
        );
        vm.files = ConsList::new(&mut vm.heap, main_file);

        let include_a = FileRecord::new(
            &mut vm.heap,
            unique_test_path("include_a.m")
                .to_string_lossy()
                .to_string(),
            UNIX_EPOCH,
            false,
            ConsList::EMPTY,
        );
        let include_b = FileRecord::new(
            &mut vm.heap,
            unique_test_path("include_b.m")
                .to_string_lossy()
                .to_string(),
            UNIX_EPOCH,
            false,
            ConsList::EMPTY,
        );
        vm.includees = ConsList::new(&mut vm.heap, include_a);
        vm.includees.append(&mut vm.heap, include_b);
        vm.mkinclude_files = ConsList::new(&mut vm.heap, vm.includees);

        let result = vm.run_mkincludes_phase();

        assert!(result.is_ok());
        assert!(vm.includees.is_empty());
        assert!(vm.mkinclude_files.is_empty());
        assert_eq!(vm.files.len(&vm.heap), 3);
    }

    #[test]
    fn include_expansion_phase_is_noop_when_includees_empty() {
        let mut vm = VM::new_for_tests();
        vm.files = ConsList::EMPTY;
        vm.includees = ConsList::EMPTY;
        let include_stub = FileRecord::new(
            &mut vm.heap,
            unique_test_path("ld_stuff.m").to_string_lossy().to_string(),
            UNIX_EPOCH,
            false,
            ConsList::EMPTY,
        );
        let include_list = ConsList::new(&mut vm.heap, include_stub);
        vm.mkinclude_files = ConsList::new(&mut vm.heap, include_list);

        let result = vm.run_mkincludes_phase();

        assert!(result.is_ok());
        assert!(vm.files.is_empty());
        assert!(vm.includees.is_empty());
        assert!(vm.mkinclude_files.is_empty());
    }

    #[test]
    fn typecheck_phase_succeeds_when_no_undefined_names() {
        let mut vm = VM::new_for_tests();
        vm.undefined_names = ConsList::EMPTY;

        let result = vm.run_checktypes_phase();

        assert!(result.is_ok());
    }

    #[test]
    fn typecheck_phase_fails_when_undefined_names_present() {
        let mut vm = VM::new_for_tests();
        let missing_id = vm.heap.make_empty_identifier("missing_name");
        vm.undefined_names = ConsList::new(&mut vm.heap, missing_id);

        let result = vm.run_checktypes_phase();

        assert!(matches!(
            result,
            Err(BytecodeError::TypecheckUndefinedNames { count: 1 })
        ));
    }

    #[test]
    fn export_closure_phase_is_noop_without_exports() {
        let mut vm = VM::new_for_tests();
        vm.exports = NIL;

        let result = vm.run_export_closure_phase_partial();

        assert!(result.is_ok());
    }

    #[test]
    fn export_closure_phase_clears_exports_when_undefined_names_present() {
        let mut vm = VM::new_for_tests();
        let export_id = vm.heap.make_empty_identifier("exported_name");
        vm.exports = vm.heap.cons_ref(export_id.into(), NIL).into();
        let missing_id = vm.heap.make_empty_identifier("missing_name");
        vm.undefined_names = ConsList::new(&mut vm.heap, missing_id);

        let result = vm.run_export_closure_phase_partial();

        assert!(matches!(
            result,
            Err(BytecodeError::ExportClosureBlockedByUndefinedNames)
        ));
        assert_eq!(vm.exports, NIL);
    }

    #[test]
    fn export_closure_phase_keeps_exports_when_no_undefined_names() {
        let mut vm = VM::new_for_tests();
        let export_id = vm.heap.make_empty_identifier("exported_name");
        vm.exports = vm.heap.cons_ref(export_id.into(), NIL).into();
        vm.undefined_names = ConsList::EMPTY;

        let result = vm.run_export_closure_phase_partial();

        assert!(result.is_ok());
        assert_ne!(vm.exports, NIL);
    }

    #[test]
    fn bereaved_warning_phase_is_noop_without_risk_flag() {
        let mut vm = VM::new_for_tests();
        vm.exports = NIL;
        vm.unused_types = false;

        vm.emit_bereaved_warnings_partial();
        assert!(!vm.unused_types);
    }

    #[test]
    fn bereaved_warning_phase_returns_ok_when_risk_flagged() {
        let mut vm = VM::new_for_tests();
        let export_id = vm.heap.make_empty_identifier("exported_name");
        vm.exports = vm.heap.cons_ref(export_id.into(), NIL).into();
        vm.unused_types = true;

        vm.emit_bereaved_warnings_partial();
        assert!(vm.unused_types);
        assert_ne!(vm.exports, NIL);
    }

    #[test]
    fn unused_diagnostics_phase_clears_deferred_marker() {
        let mut vm = VM::new_for_tests();
        vm.eprodnts = Combinator::True.into();

        vm.emit_unused_definition_diagnostics_partial();
        assert_eq!(vm.eprodnts, Value::from(NIL));
    }

    #[test]
    fn unused_diagnostics_phase_is_stable_when_marker_absent() {
        let mut vm = VM::new_for_tests();
        vm.eprodnts = Value::from(NIL);

        vm.emit_unused_definition_diagnostics_partial();
        assert_eq!(vm.eprodnts, Value::from(NIL));
    }

    #[test]
    fn codegen_phase_fails_without_loaded_files() {
        let mut vm = VM::new_for_tests();
        vm.files = ConsList::EMPTY;

        let result = vm.run_codegen_phase();

        assert!(matches!(
            result,
            Err(BytecodeError::CodegenWithoutLoadedFiles)
        ));
    }

    #[test]
    fn codegen_phase_fails_during_initialization_with_unresolved_names() {
        let mut vm = VM::new_for_tests();
        vm.initializing = true;
        let source_file = FileRecord::new(
            &mut vm.heap,
            unique_test_path("codegen_init.m")
                .to_string_lossy()
                .to_string(),
            UNIX_EPOCH,
            false,
            ConsList::EMPTY,
        );
        vm.files = ConsList::new(&mut vm.heap, source_file);
        let missing_id = vm.heap.make_empty_identifier("missing_name");
        vm.undefined_names = ConsList::new(&mut vm.heap, missing_id);

        let result = vm.run_codegen_phase();

        assert!(matches!(
            result,
            Err(BytecodeError::InitializationLoadContainsErrors)
        ));
    }

    #[test]
    fn codegen_phase_succeeds_with_loaded_files_and_no_init_errors() {
        let mut vm = VM::new_for_tests();
        vm.initializing = false;
        let source_file = FileRecord::new(
            &mut vm.heap,
            unique_test_path("codegen_ok.m")
                .to_string_lossy()
                .to_string(),
            UNIX_EPOCH,
            false,
            ConsList::EMPTY,
        );
        vm.files = ConsList::new(&mut vm.heap, source_file);
        vm.undefined_names = ConsList::EMPTY;

        let result = vm.run_codegen_phase();

        assert!(result.is_ok());
    }

    #[test]
    fn dump_visibility_phase_runs_for_normal_m_source() {
        let mut vm = VM::new_for_tests();
        vm.initializing = false;
        let internal = vm.heap.make_empty_identifier("internal_name");
        vm.internals = ConsList::new(&mut vm.heap, internal);

        let source_path = unique_test_path("normal_source.m")
            .to_string_lossy()
            .to_string();
        let result = vm.run_dump_visibility_phase(&source_path);

        assert!(result.is_ok());
        assert!(vm.internals.is_empty());
    }

    #[test]
    fn dump_visibility_phase_skips_non_m_source_when_not_initializing() {
        let mut vm = VM::new_for_tests();
        vm.initializing = false;
        let internal = vm.heap.make_empty_identifier("internal_name");
        vm.internals = ConsList::new(&mut vm.heap, internal);

        let source_path = unique_test_path("binary_source.x")
            .to_string_lossy()
            .to_string();
        let result = vm.run_dump_visibility_phase(&source_path);

        assert!(result.is_ok());
        assert!(!vm.internals.is_empty());
    }

    #[test]
    fn dump_visibility_phase_runs_during_initialization_even_without_m_extension() {
        let mut vm = VM::new_for_tests();
        vm.initializing = true;
        let internal = vm.heap.make_empty_identifier("internal_name");
        vm.internals = ConsList::new(&mut vm.heap, internal);

        let source_path = unique_test_path("prelude_source")
            .to_string_lossy()
            .to_string();
        let result = vm.run_dump_visibility_phase(&source_path);

        assert!(result.is_ok());
        assert!(vm.internals.is_empty());
    }

    #[test]
    fn syntax_dump_records_source_anchor_for_m_scripts() {
        let mut vm = VM::new_for_tests();
        vm.old_files = ConsList::EMPTY;
        vm.last_load_phase_trace.clear();

        let source_path = unique_test_path("syntax_dump_anchor.m")
            .to_string_lossy()
            .to_string();
        let result = vm.maybe_write_syntax_dump(&source_path);

        assert!(result.is_ok());
        assert!(!vm.old_files.is_empty());
        assert_eq!(vm.last_load_phase_trace, vec!["dump-write"]);
    }

    #[test]
    fn syntax_dump_is_noop_for_non_m_sources() {
        let mut vm = VM::new_for_tests();
        vm.old_files = ConsList::EMPTY;
        vm.last_load_phase_trace.clear();

        let source_path = unique_test_path("syntax_dump_anchor.x")
            .to_string_lossy()
            .to_string();
        let result = vm.maybe_write_syntax_dump(&source_path);

        assert!(result.is_ok());
        assert!(vm.old_files.is_empty());
        assert!(vm.last_load_phase_trace.is_empty());
    }

    #[test]
    fn alfasort_is_deterministic_for_diagnostic_identifier_lists() {
        let mut vm = VM::new_for_tests();

        let zeta = IdentifierRecordRef::new(
            &mut vm.heap,
            "zeta".to_string(),
            IdentifierDefinitionRef::undefined(),
            Type::Undefined.into(),
            None,
        );
        let alpha = IdentifierRecordRef::new(
            &mut vm.heap,
            "alpha".to_string(),
            IdentifierDefinitionRef::undefined(),
            Type::Undefined.into(),
            None,
        );
        let mu = IdentifierRecordRef::new(
            &mut vm.heap,
            "mu".to_string(),
            IdentifierDefinitionRef::undefined(),
            Type::Undefined.into(),
            None,
        );

        let mut unsorted = ConsList::new(&mut vm.heap, zeta);
        unsorted.push(&mut vm.heap, alpha);
        unsorted.push(&mut vm.heap, mu);

        let sorted_once = vm.alfasort(unsorted);
        let sorted_twice = vm.alfasort(unsorted);
        let rendered_once = vm.printlist(sorted_once);
        let rendered_twice = vm.printlist(sorted_twice);

        assert_eq!(rendered_once, rendered_twice);
        assert_eq!(rendered_once.split(", ").count(), 3);
    }

    #[test]
    fn printlist_formats_identifier_names_for_diagnostics() {
        let mut vm = VM::new_for_tests();

        let alpha = IdentifierRecordRef::new(
            &mut vm.heap,
            "alpha".to_string(),
            IdentifierDefinitionRef::undefined(),
            Type::Undefined.into(),
            None,
        );
        let beta = IdentifierRecordRef::new(
            &mut vm.heap,
            "beta".to_string(),
            IdentifierDefinitionRef::undefined(),
            Type::Undefined.into(),
            None,
        );
        let gamma = IdentifierRecordRef::new(
            &mut vm.heap,
            "gamma".to_string(),
            IdentifierDefinitionRef::undefined(),
            Type::Undefined.into(),
            None,
        );

        let mut names = ConsList::new(&mut vm.heap, alpha);
        names.append(&mut vm.heap, beta);
        names.append(&mut vm.heap, gamma);

        let rendered = vm.printlist(names);

        let rendered_parts: Vec<&str> = rendered.split(", ").collect();
        assert_eq!(rendered_parts.len(), 3);
        assert!(rendered_parts.iter().all(|part| !part.is_empty()));
    }

    #[test]
    fn source_update_check_detects_when_loaded_source_is_newer_than_dump_timestamp() {
        let mut vm = VM::new_for_tests();

        let source_path = unique_test_path("source_update_check_newer.m");
        std::fs::write(&source_path, "-- source\n").expect("failed to write source test file");
        let source_path_str = source_path.to_string_lossy().to_string();

        let stale_dump_timestamp = UNIX_EPOCH;
        let file_record = FileRecord::new(
            &mut vm.heap,
            source_path_str,
            stale_dump_timestamp,
            false,
            ConsList::EMPTY,
        );
        vm.files = ConsList::new(&mut vm.heap, file_record);

        assert!(vm.source_update_check());
    }

    #[test]
    fn source_update_check_is_false_when_dump_timestamp_is_not_older_than_source() {
        let mut vm = VM::new_for_tests();

        let source_path = unique_test_path("source_update_check_current.m");
        std::fs::write(&source_path, "-- source\n").expect("failed to write source test file");
        let source_path_str = source_path.to_string_lossy().to_string();

        let future_dump_timestamp = SystemTime::now() + std::time::Duration::from_secs(60);
        let file_record = FileRecord::new(
            &mut vm.heap,
            source_path_str,
            future_dump_timestamp,
            false,
            ConsList::EMPTY,
        );
        vm.files = ConsList::new(&mut vm.heap, file_record);

        assert!(!vm.source_update_check());
    }

    #[test]
    fn unfix_exports_partial_clears_export_processing_state() {
        let mut vm = VM::new_for_tests();

        vm.in_export_list = true;
        let internal = vm.heap.make_empty_identifier("internal_name");
        vm.internals = ConsList::new(&mut vm.heap, internal);

        vm.unfix_exports_partial();

        assert!(!vm.in_export_list);
        assert!(vm.internals.is_empty());
    }
}
