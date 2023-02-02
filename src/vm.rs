/*!

Setup and orchestration of built-ins, compilation, etc. Because everything lives in the heap,
it's tempting to put all of this in `Heap`. We separate concerns by factoring out non-generic heap
manipulation, environment initialization, and compilation from generic heap functions, accessors, etc.

 */

use std::{
  fs::File,
  io::{
    BufRead,
    BufReader,
    Read
  },
  process::exit,
  time::SystemTime
};
use std::iter::Peekable;
use std::slice::Iter;
use std::time::Duration;


use console::{Alignment, pad_str, style, Term};
use num_traits::FromPrimitive;

use crate::{
  compiler::Token,
  data::{
    Combinator,
    Heap,
    path::*,
    Identifier,
    IdentifierDefinition,
    IdentifierValue,
    IdentifierValueType,
    Tag,
    Type,
    Value,
    RawValue,
    api::{
      HeapObjectProxy,
      ConsList,
      FileRecord,
      IdentifierDefinition,
      IdentifierRecord,
      IdentifierValue,
      IdentifierValueData,
      IdentifierValueType,
      IdentifierValueTypeData,
      IdentifierValueTypeDataSpecifier,
      OpenFile
    },
    ValueRepresentationType
  },
  constants::{DEFAULT_SPACE, WORD_SIZE, XVERSION},
  options::{
    make_version_string,
    Options,
    setup_argument_parser
  },
  errors::{
    BytecodeError,
    emit_error,
    fatal_error
  },
  algorithms::get_machine_word
};
use crate::bytecode_parser::{get_machine_word, parse_filename_modified_time, parse_string, get_number, get_number_16bit, parse_line_number};
use crate::compiler::bytecode::Bytecode;
use crate::data::api::IdentifierDefinitionData;
use crate::data::HeapCell;


const NIL: Value = Value::Combinator(Combinator::Nil);

pub enum ActivityReset {
  Collecting,
  Loading {
    blank_error: bool,
    unlink: bool,
  },
  Making {
    make_status: bool
  },
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
  compiling   : bool,
  initializing: bool,
  making      : bool,
  loading     : bool,

  activity                : ActivityReset,
  command_mode            : bool,

  sorted                   : bool, // Are the file definitions sorted?
  in_export_list           : bool,
  in_semantic_redeclaration: bool,
  rv_script                : bool, // Flags readvals in use (for garbage collector)
  unused_types             : bool, // There are orphaned types (Miranda's TORPHANS)


  in_file                  : Option<Box<dyn BufRead>>,

  /// Tells editor where to find error: `errline` contains location of 1st error in main script, `errs` is `hereinfo`
  /// of up to one error in `%insert` script (each is 0 if not set). Some errors can set both.
  error_line: usize,
  errs      : Vec<ValueRepresentationType>, // Todo: What is this holding? It is `char[24]` in Miranda.

  last_expression          : Value,       // A reference to the last expression evaluated.

  // endregion

  heap: Heap,

  // region Flags and other state that lives on the heap.

  // list of currently open-for-input files, of form `cons(strcons(stream,<ptr to element of 'files'>),...)`
  file_queue  : ConsList<OpenFile>, // Miranda's `file_q`
  // ToDo: Why is this on the heap? Can we just use a vector?
  prefix_stack: ConsList, // Paths are made relative to current file. If we descend into another file, need to record
                          // prefix.
  col_fn      : Value,
  embargoes   : Value,
  eprodnts    : Value,
  exportfiles : Value,
  exports     : Value,
  fnts        : Value, // `fnts` is flag indicating %bnf in use. Treated as Value?
  aliases     : ConsList, // Not `IdentifierRecord`, because `hold` is not an `Identifier`.
  free_ids    : ConsList<IdentifierRecord>,
  internals   : ConsList<IdentifierRecord>, // list of names not exported, used by fix/unfixexports
  idsused     : ConsList<IdentifierRecord>,
  clashes     : ConsList<IdentifierRecord>,
  suppressed  : ConsList<IdentifierRecord>, // list of `-id' aliases successfully obeyed, used for error reporting
  suppressed_t: ConsList<IdentifierRecord>, // list of -typename aliases (illegal just now)
  ihlist      : Value,
  includees   : ConsList<FileRecord>,
  lasth       : Value,
  last_name    : Value,
  lexdefs     : Value,
  nonterminals: Value,
  ntmap       : Value,
  ntspecmap   : Value,


  /// A cons list of identifiers in the primitive environment.
  primitive_environment: ConsList<IdentifierRecord>,


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
  files          : ConsList<FileRecord>,
  old_files      : ConsList<FileRecord>, // most recent set of sources, in case of interrupted or failed compilation
  // list of a list of files to be unloaded if `mkincludes` is interrupted (Miranda's ld_stuff).
  mkinclude_files: ConsList<ConsList<FileRecord>>,

  sui_generis_constructors: ConsList, // user defined sui-generis constructors (Miranda's SGC)
  type_abstractions       : ConsList, // cons list of abstype declarations (Miranda's TABSTR)
  undefined_names         : ConsList<IdentifierRecord>, // undefined names used in script (Miranda's ND)
  new_type_names          : ConsList<IdentifierRecord>, // newly declared type names in current code unit (Miranda's `newtyps`)
  algebraic_show_functions: ConsList, // show functions of algebraic types in scope (Miranda's `algshfns`)
  special_show_forms      : ConsList, // all occurrences of special forms (show) encountered during type check

  // `spec_location` is a list of `cons(id,hereinfo)` giving location of `spec` for ids both defined and specified.
  // Needed to locate errs in `meta_tcheck`, `abstr_mcheck`.
  spec_location: ConsList,

  // endregion

  // region Constants
  common_stdin : Value,
  common_stdinb: Value,
  concat       : IdentifierRecord,
  cook_stdin   : Value,
  diagonalise  : IdentifierRecord,
  main_id      : IdentifierRecord,
  message      : IdentifierRecord,
  showabstract : IdentifierRecord,
  showbool     : IdentifierRecord,
  showchar     : IdentifierRecord,
  showfunction : IdentifierRecord,
  showlist     : IdentifierRecord,
  shownum1     : IdentifierRecord,
  showpair     : IdentifierRecord,
  showparen    : IdentifierRecord,
  showstring   : IdentifierRecord,
  showvoid     : IdentifierRecord,
  showwhat     : IdentifierRecord,
  stdout       : Value,

  // These might be constants
  indent_fn  : IdentifierRecord,
  listdiff_fn: IdentifierRecord,
  outdent_fn : IdentifierRecord,

  // Common compound types.
  numeric_function_type : Value,
  numeric_function2_type: Value,
  boolean_function_type : Value,
  boolean_function2_type: Value,
  char_list_type        : Value,
  string_function_type  : Value,
  range_step_type       : Value,
  range_step_until_type : Value,

  pub(crate) nill: Value,
  void_: IdentifierRecord,


  // endregion
}

// pub fn reset(activity_reset: ActivityReset)-

impl Default for VM {
  /// Setup and initialization of [`Heap`](Heap) occurs in
  /// [`VM::default()`](Heap::default()),
  /// [`VM::setup_constants()`](crate::data::heap::Heap::setup_constants()), and
  /// [`VM::setup_standard_types()`](crate::data::heap::Heap::setup_standard_types()).
  fn default() -> Self {
    let mut vm = VM {
      // region State that doesn't live on the heap
      options                 : setup_argument_parser(),

      terminal                : None,

      compiling   : true,
      initializing: true,  // Checked in `undump(..)`
      making      : false,
      loading     : false,

      activity                : ActivityReset::Collecting, // Arbitrary value.
      command_mode            : false,
      sorted                  : false,
      // type check
      in_export_list           : false,
      in_semantic_redeclaration: false,
      rv_script                : false, // Flags readvals in use (for garbage collector)
      unused_types             : false, // There are orphaned types

      in_file                  : None,
      error_line               : 0,
      errs                     : vec![],
      last_expression          : Value::None, // A reference to the last expression evaluated.
      // endregion

      heap: Heap::default(),

      // region Flags and other state that lives on the heap.

      file_queue  : ConsList::EMPTY,
      prefix_stack: ConsList::EMPTY,
      listdiff_fn : IdentifierRecord::UNINITIALIZED,
      indent_fn   : IdentifierRecord::UNINITIALIZED,
      outdent_fn  : IdentifierRecord::UNINITIALIZED,

      // Common compound types.
      numeric_function_type : Value::Uninitialized,
      numeric_function2_type: Value::Uninitialized,
      boolean_function_type : Value::Uninitialized,
      boolean_function2_type: Value::Uninitialized,
      char_list_type        : Value::Uninitialized,
      string_function_type  : Value::Uninitialized,
      range_step_type       : Value::Uninitialized,
      range_step_until_type : Value::Uninitialized,

      nill : Value::Uninitialized,
      void_: IdentifierRecord::UNINITIALIZED,

      primitive_environment: ConsList::EMPTY,


      col_fn      : Value::None,
      embargoes   : NIL,
      eprodnts    : NIL,
      exportfiles : NIL,
      exports     : NIL,
      fnts        : NIL,
      aliases     : ConsList::EMPTY,
      free_ids    : ConsList::EMPTY,
      internals   : ConsList::EMPTY,
      idsused     : ConsList::EMPTY,
      clashes     : ConsList::EMPTY,
      suppressed  : ConsList::EMPTY,
      suppressed_t: ConsList::EMPTY,
      ihlist      : Value::None,
      includees   : ConsList::EMPTY,
      lasth       : Value::None,
      last_name   : Value::None,
      lexdefs     : NIL,
      nonterminals: NIL,
      ntmap       : NIL,
      ntspecmap   : NIL,

      sui_generis_constructors: ConsList::EMPTY, // user defined sui-generis constructors (Miranda's SGC)
      type_abstractions       : ConsList::EMPTY, // cons list of abstype declarations (Miranda's TABSTR)
      undefined_names         : ConsList::EMPTY, // undefined names used in script (Miranda's ND)
      new_type_names          : ConsList::EMPTY, // newly declared type names in current code unit (Miranda's `newtyps`)
      algebraic_show_functions: ConsList::EMPTY, // show functions of algebraic types in scope (Miranda's `algshfns`)
      special_show_forms      : ConsList::EMPTY, // all occurrences of special forms (show) encountered during type check
      spec_location           : ConsList::EMPTY, // list giving location of `spec` for ids both defined and specified.

      files          : ConsList::EMPTY, // The cons list files is also called the environment
      old_files      : ConsList::EMPTY, // most recent set of sources, in case of interrupted or failed compilation
      mkinclude_files: ConsList::EMPTY, // list of files to be unloaded if `mkincludes` is interrupted (Miranda's ld_stuff).

      // endregion

      // region Constants set to `Value::Uninitialized`/`IdentifierRecord::UNINITIALIZED`
      common_stdin : Value::Uninitialized,
      common_stdinb: Value::Uninitialized,
      concat       : IdentifierRecord::UNINITIALIZED,
      cook_stdin   : Value::Uninitialized,
      diagonalise  : IdentifierRecord::UNINITIALIZED,
      main_id      : IdentifierRecord::UNINITIALIZED,
      message      : IdentifierRecord::UNINITIALIZED,
      showabstract : IdentifierRecord::UNINITIALIZED,
      showbool     : IdentifierRecord::UNINITIALIZED,
      showchar     : IdentifierRecord::UNINITIALIZED,
      showfunction : IdentifierRecord::UNINITIALIZED,
      showlist     : IdentifierRecord::UNINITIALIZED,
      shownum1     : IdentifierRecord::UNINITIALIZED,
      showpair     : IdentifierRecord::UNINITIALIZED,
      showparen    : IdentifierRecord::UNINITIALIZED,
      showstring   : IdentifierRecord::UNINITIALIZED,
      showvoid     : IdentifierRecord::UNINITIALIZED,
      showwhat     : IdentifierRecord::UNINITIALIZED,
      stdout       : Value::Uninitialized,
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
}

impl VM {
  pub fn new() -> Self {
    Self::default()
  }

  /// Loads the object file for `source_path` if it exists and is modified after `source_path`. Otherwise calls
  /// `load_file` for `source_path`.
  fn undump(&mut self, source_path: &str) -> Result<(), BytecodeError>{
    // This does not guarantee that the path ends in ".m" after the if block.
    if !source_path.ends_with(".m") && !self.initializing {
      // Except for prelude, only .m files have dumps.
      // Todo: Then should not there not be a not on initializing?
      return self.load_file(source_path);
    }

    // Change "source.m" into "source.x".
    // The if statement above does not guarantee that the path ends in ".m".
    // Include the dot before the extension to avoid a case like "them" --> "thex".
    let binary_path: String = format!("{}{}", source_path.strip_suffix(".m").unwrap_or(source_path), ".x");


    // region Check Failure to Load Bytecode
    // If we cannot load the binary file for the source file, then we have to do the entire `loadfile(..)`.
    if let Ok(source_metadata) = std::fs::metadata(source_path) {
      // Source file exists.
      let source_modified_time: SystemTime
          = source_metadata.modified()
                           .unwrap_or_else(
                             // If we can't determine the time the source was modified, be conservative and assume it
                             // was just modified.
                             |_| SystemTime::now()
                           );

      if let Ok(binary_metadata) = std::fs::metadata(source_path) {
        // binary file exists.
        let binary_modified_time
            = binary_metadata.modified()
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
      Ok(f) => {
        f
      }
      Err(..) => {
        // Todo: Implement load_file
        return self.load_file(source_path);
      }
    };

    // endregion

    // You have to `unload` before you can `load`.
    // Todo: Implement unload
    self.unload();

    #[cfg(feature = "DEBUG")]
    if !self.initialising {
      println!("undumping from {}", binary_path);
    }

    // If this is the main script…
    let is_main_script: bool = !self.initialising && !self.making ;
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
      is_main_script
    );
    drop(in_file); // Close the file.

    // Restore interrupt handler
    if is_main_script {
      // Restore interrupt handler
      // Todo: Figure out how these signal handlers work.
      // (void) signal(SIGINT, oldsig);
    }
    // Todo: Once signal handlers are implemented, see if we actually need sigflag.
    if sigflag {
      // If a signal fired during loading, we handle it now.
      sigflag = 0;
      (*oldsig)(); // Take deferred interrupt.
    }

    // Handle errors
    let dump_error_encountered: bool = // the result of the following match
    match load_result {

      Ok(files) => { self.files = files; false }

      Err(bytecode_error@BytecodeError::ArchitectureMismatch)
      | Err(bytecode_error@BytecodeError::WrongBytecodeVersion)
        => {
        std::fs::remove_file(binary_path).ok();
        self.unload();
        emit_error(&bytecode_error);
        true
      }

      Err(BytecodeError::NameClash) => {
        if ideep == 0 {
          let sorted = self.alfasort(self.clashes);
          println!("Cannot load {} due to name clashes: {}", binary_path, self.printlist(sorted));
        }
        self.unload();
        self.loading = false;
        return load_result.map(|_| ());
      }

      Err(bytecode_error) => {
        std::fs::remove_file(binary_path).ok();
        self.unload();
        // Since we store clashes in the error, there is no equivalent for us. This might change when we figure out GC.
        // CLASHES = self.nill;
        // stackp = dstack;
        emit_error(&bytecode_error);
        true
      }

    }; // end match on load_result

    #[cfg(feature = "DEBUG")]
    if !self.initialising {
      println!("{} undumped, success={}", binary_path, dump_error_encountered);
    }

    if dump_error_encountered || self.source_update_check() {
      self.load_file(source_path);
    }
    else if self.initialising && (self.not_defined!=NIL || self.files == Combinator::Nil.into()){
      // There is an error in the dump of the prelude.
      // Todo: These errors should be propagated from their source instead of divined from the tea leaves of the flags.
      fatal_error("Cannot read the prelude dump.");
    }
    else if self.options.verbose || self.options.magic || !self.options.make_exports.is_empty() {
      if self.files == NIL {
        println!("{} contains syntax error", source_path);
      }
      else if self.not_defined!=NIL {
        println!("{} contains undefined names or type errors", source_path);
      }
      else if !self.making && !self.options.magic {
        println!(source_path); // added && !magic 26.11.2019
      }
    }

    if self.files != NIL && !self.making & !self.initialising {
      self.unfix_exports();
    }
    loading = 0;

    Ok(())
  }

  fn prefix(&self) -> Result<String, ()> {
    let str_ref = self.prefix_stack.head(&self.heap).ok_or_else(())?;
    self.heap.resolve_string(str_ref.into())
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
      let mut alias_iterator   = aliases; // Not technically an iterator.

      while !alias_iterator.is_empty() {
        // IdentifierRecord looks like this:
        //   cons(
        //     cons(strcons(name,who),type),
        //     cons(cons(arity,showfn),cons(free_t,NIL))
        //   )
        // Entries in `aliases` look like this:
        //   cons(new_id, old_id)
        let mut alias = alias_iterator.head_unchecked(heap);
        let new_ref   = self.heap[alias].head;
        let old       = IdentifierRecord::from_ref( self.heap[alias].tail );


        let hold = { // scope of temporaries
          // cons(  id_who(old), cons(  id_type(old), id_val(old))  );
          let old_who  = old.get_definition(&self.heap).unwrap();
          let old_type = old.get_datatype(&self.heap).unwrap();
          let old_val  = old.get_value(&self.heap).unwrap();
          let tl       = self.heap.cons(old_type.into(), old_val.into());
          self.heap.cons( old_who.into(), tl);
        };

        old.set_type(&mut self.heap, Type::Alias.into());
        // We make old an alias of new.
        old.set_val(&mut self.heap, new_ref);

        // Todo: This check suggests that `new` might not be an Identifier? Correct, `alias` is replaced by
        //       `cons( old_who, cons(old_type, old_val) )`, the data required to undo the aliasing.
        if self.heap[new_ref].tag == Tag::Id {
          let new = IdentifierRecord::from_ref(new_ref);
          let new_datatype = new.get_datatype(&self.heap);
          if (new_datatype != Type::Undefined || new.get_value(&self.heap).unwrap().is_some())
              && new_datatype != Type::Alias
          {
            // Insert new into self.clashes such that self.clashes remains in ascending address order.
            // Todo: Why order these?
            // Todo: Why is this a clash? Isn't it just an alias?
            self.clashes.insert_ordered(&mut self.heap, new); //add1(&mut self.heap, new, &mut self.clashes);
          }
        }

        // Replace new_ref with hold
        // Todo: But `hold` isn't an `IdentifierRecord`...? The alias is replaced with the info required to undo the
        //       aliasing. The aliasing is undone in the event of an error. But in that case we are potentially
        //       overwriting a previous `hold` with a "new" `hold`.
        self.heap[alias].head = hold;
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
        let alias = alias_iterator.pop_unchecked(&mut self.heap);
        let  ch = self.heap[alias].tail;
        if self.heap[ch].tag == Tag::Id {
          let ch = IdentifierRecord::from_ref(ch);
          if ch.get_datatype(&self.heap) != Type::Alias {
            ch.set_type(&mut self.heap, Type::New.into());
          }
        }
      } // FIX1
    }

    Ok(())

  }

  /// Gets the file name for the topmost file in the `vm.files` cons list, which _should_ be the current file. This
  fn current_file(&self) -> String {
    assert!(!self.files.is_empty());
    let f = self.files.head(&self.heap).unwrap();

    f.get_file_name(&self.heap)
  }

  /// Loads a compiled script from `in_file` for the `source_file`.
  fn load_script(
    &mut self,
    in_file        : &File,
    mut source_file: String,
    mut aliases    : ConsList, // List of cons(new_id, old_id)
    parameters     : Value,    // Todo: `parameters` never used?
    is_main_script : bool
  ) -> Result<ConsList<FileRecord>, BytecodeError>
  {
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
    { // Scope of f_reader
      let mut f_reader = BufReader::new(in_file);
      let bytes_read = f_reader.read_to_end(&mut file_bytes)
                                .map_err(|e| BytecodeError::UnexpectedEOF)?;
      if bytes_read < 16 {
        return Err(BytecodeError::UnexpectedEOF);
      }
    }
    // An iterator over the bytes of the file.
    let mut byte_iter: Peekable<Iter<u8>> = file_bytes.iter().peekable();

    // Parse the machine word size
    // First byte: `__WORDSIZE` (64 bits in my case, so `__WORDSIZE == 64 == 0x40`.)
    if byte_iter.next().unwrap() != WORD_SIZE {
      return Err(BytecodeError::ArchitectureMismatch);
    }

    // Parse the bytecode version
    // Second byte: `XVERSION`, the bytecode version. (Latest Miranda` == 83 == 0x53`)
    if byte_iter.next().unwrap() != XVERSION {
      return Err(BytecodeError::WrongBytecodeVersion);
    }

    // Todo: Why is this even here? It doesn't depend on the contents of the file!
    self.obey_aliases(aliases)?;

    // PNBASE = nextpn; // base for relocation of internal names in dump
    let private_symbol_base_index = self.heap.private_symbols.len();
    // I think the idea is that the indices of the names in the serialized binary bytecode assume an empty private
    // symbol table, and that may not be the case if the current file is, say, included in another file. So indices
    // in the bytecode are actually _relative_ to the index of the first private symbol of the current file, and
    // adding private_name_base gives the absolute index in the vector of private names.

    // ToDo: Is this RE-setting the values of `suppressed*`? Might they be non-empty? Neither appear to be added to
    //       in this function.
    self.suppressed:   ConsList = ConsList::EMPTY; // SUPPRESSED  // list of `-id' aliases successfully obeyed
    self.suppressed_t: ConsList = ConsList::EMPTY; // TSUPPRESSED // list of -typename aliases (illegal just now)

    let mut bad_dump: bool = false; // Flags an error condition

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
      if ch == 0 || bad_dump {
        if !bad_dump {
          // But we need to also return `files`?
          // I *think* the returned `files` isn't used if there is an error.
          return Err(BytecodeError::UnexpectedEOF)
        }
        if !aliases.is_empty() {
          self.unalias(aliases);
        }
        return Ok(files);
      }

      // If we encounter a `ch==1` *before* we ever even see a file, it is because it is the magic number for a
      // type-error script.
      // Todo: It is awkward to have this inside the loop.
      if ch == 1 && files.is_empty() {
        // The next w bytes (8 bytes) give the line number of the error.
        let error_line_prefetch: usize = get_machine_word(&mut byte_iter)?;
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
      let sharable: bool = byte_iter.next().unwrap_or(&0u8) == 1;

      #[cfg(feature = "DEBUG")]
      println("loading: {}({})", filename, _modified_time);

      if files.is_empty() {
        // Is this the right dump file?
        if filename != source_file {
          // I don't think unalias is needed here.
          if aliases != NIL {
            self.unalias(aliases);
          }
          return Err(BytecodeError::WrongSourceFile);
        }
      }

      // Add a new `FileRecord` to `files`
      { // Scope of `new_defs`, `file_record`

        // Warning: `load_defs` side effects id's in namebuckets, cannot  be  undone  by
        //          `unload`  until  attached  to  global `files', so interrupts are disabled during
        //          `load_script` - see steer.c
        // For big dumps this may be too coarse - FIX
        let new_defs = self.load_defs(&mut byte_iter)?;
        let file_record = FileRecord::new(&mut self.heap, filename, modified_time, sharable, new_defs);
        files.push(&mut self.heap, file_record);
      }

    }


    // Dump of syntax error state
    // If we got here, we broke out of the while because we encountered `ch==0`. If we did so before we ever found a
    // file, it is because the `0` is the magic number for a syntax error script.
    if files.is_empty() {
      // The next w bytes (8 bytes) give the line number of the error.
      { // Scope of `error_line_prefetch`
        let error_line_prefetch: usize = get_machine_word(&mut byte_iter)?;
        if is_main_script {
          // But only save it if this is the main script.
          self.error_line = error_line_prefetch;
        }
      }
      // Todo: What if there are multiple type errors?

      while let Some(_ch) = byte_iter.next(){


        // Parse filename and modified time.
        let (filename, modified_time) = parse_filename_modified_time(&mut byte_iter);
        if let Ok(prefix) = self.prefix() {
          make_absolute_path(&mut filename, &prefix);
        }

        if old_files.is_empty() {
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
        { // Scope of `file_record`
          // Add a new `FileRecord` to `old_files`
          // Todo: WTF is `old_files`? Why are we using it instead of `files`? Why are we setting its `sharable` to
          //       `false`, `defienda` to `NIL`? It is the most recent set of sources, in case of interrupted or failed
          //       compilation. Why its definitions and sharable flag are not saved is a mystery.
          let file_record = FileRecord::new(&mut self.heap, filename, modified_time, false, ConsList::EMPTY);
          self.old_files.push(&mut self.heap, file_record);
        }
      }

      if !aliases.is_empty() {
        self.unalias(aliases)
      }

      return Ok(files); // Equivalent to `return Ok(Nil);`
    }

    // Parse algebraic show functions
    { // scope of new_defs
      let new_defs = self.load_defs(&mut byte_iter)?;
      self.algebraic_show_functions.append(&mut self.heap, new_defs.get_ref());
    }

    // Parse [ND] or [True] (Are there type orphans?)
    if self.load_defs(&mut byte_iter)? == Combinator::True {
      self.undefined_names = ConsList::EMPTY;
      self.unused_types = true;
    }

    // Parse DEF_X
    // Parse sui generis constructors
    // Todo: This does not appear in the binary description
    {
      let new_defs = self.load_defs(&mut byte_iter)?;
      self.sui_generis_constructors.append(&mut self.heap, new_defs.get_ref());
    }

    // Parse DEF_X
    // Parse free_ids
    if is_main_script || self.includees.is_empty() {
      self.free_ids = self.load_defs(&mut byte_iter )?;
    }
    else {
      // Todo: Implement bindparams, hdsort
      bindparams(self.load_defs(f), hdsort(params)); // the only use of params
    }

    // Housekeeping
    // Todo: Do we unalias unconditionally?
    if !aliases.is_empty() {
      self.unalias(aliases)
    }

    // Parse DEF_X
    // Parse internals
    if is_main_script {
      let new_defs = self.load_defs(&mut byte_iter)?;
      self.internals = new_defs;
    }

    Ok(files.reversed(&mut self.heap))
  }

  /// Remove old to new diversions installed in `obey_aliases`. (Miranda's `unscramble()`.)
  fn unalias(&mut self, aliases: ConsList) {
    let mut cursor = aliases;

    while !cursor.is_empty() {
      let old = IdentifierRecord::from_ref(self.heap.tl_hd(aliases.get_ref()));
      let mut hold = IdentifierRecord::from_ref(self.heap.hd_hd(old));
      let new_value: IdentifierValue = match old.get_value(&self.heap) {
        Ok(Some(v)) => {
          v
        },
        _ => {
          panic!{"Impossible value found in aliases."}
        }
      };

      // Put `new_value` (which is the _value_ of `old`) where `hold` used to be. For missing check, see below.
      *self.heap.hd_hd_mut(aliases.get_ref()) = new_value.get_ref();

      let hold_data = hold.get_data(&self.heap).expect("Impossible value found in aliases.");
      // id_who(old)=hd[hold]; hold=tl[hold];
      old.set_definiton(&mut self.heap, hold_data.definition);
      // id_type(old)=hd[hold];
      old.set_type(&mut self.heap, hold_data.datatype);
      { // id_val(old)=tl[hold];
        let v: IdentifierValue = hold_data.value.expect("Impossible value found in aliases.");
        old.set_value(&mut self.heap, v);
      }

      cursor = cursor.rest_unchecked(&self.heap);
    } // end iter over `aliases`

    // Now adjust self.aliases

    // This accumulates missing aliases. We will replace `self.aliases` with `missing_aliases` at the end.
    let mut missing_aliases: ConsList = ConsList::EMPTY;

    cursor = self.aliases;
    while !cursor.is_empty() {
      let alias: RawValue = cursor.pop_unchecked(&mut self.heap);
      let new_ref = self.heap[alias].head;
      let old_ref = self.heap[alias].tail;

      if self.heap[new_ref].tag != Tag::Id {
        // aka stuff irrelevant to pnames
        // Todo: This is wrong by definition, because we only get here if tag != Tag:Id.
        let new_id = IdentifierRecord.from_ref(new_ref);
        if !self.suppressed.contains(&self.heap, new_id) {
          missing_aliases.push(&mut self.heap, new_id)
        }
        continue;
      }

      // We know `new_ref` points to an `IdentifierRecord` at this point.
      let new_id: IdentifierRecord = IdentifierRecord::from_ref(new_ref);

      // FIX1
      if  new_id.get_type(&self.heap) == Type::New.into() {
        new_id.set_type(&mut self.heap, Type::Undefined.into());
      }

      if new_id.get_type(&self.heap) == Type::Undefined.into() {
        // Todo: Do we know old_ref is an `IdentifierRecord`?
        let old_id = IdentifierRecord::from_ref(old_ref);
        missing_aliases.push(&mut self.heap, old_id.get_ref());
      }
      else if !self.clashes.contains(&self.heap, new_id) {
        let new_def     : IdentifierDefinition     = new_id.get_definition(&self.heap).unwrap();
        let new_def_data: IdentifierDefinitionData = new_def.get_data(&self.heap).unwrap();

        // Install aka info in new
        match new_def_data {

          IdentifierDefinitionData::Alias {..} => {/* pass */}

          // If it's not an alias
          _ => {
            // Todo: This is a complete mess.
            let old_tmp = self.heap.hd_hd(old_ref);
            let old_id = self.heap[old_tmp].head;
            // Todo: This does not look like the correct format for the who field.
            // id_who(new) = cons(datapair(get_id(old), 0), id_who(new));
            let datapair = self.heap.data_pair(old_id.into(), Value::None);
            let new_who = self.heap.cons(datapair, new_def.get_ref().into());
            new_id.set_definiton(
              &mut self.heap,
              IdentifierDefinition::from_ref(new_who.into())
            );
          }

        }
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
      term.set_title(format!("Randa {}", make_version_string(self.options.version)));

      // let title1 = "T h e   M i r a n d a   S y s t e m";
      self.println_centered(format!(
        "{}   {}   {}",
        style("T h e").blue(),
        style("R a n d a").red(),
        style("S y s t e m").blue()
      ).as_str());
      print!("\n\n"); // two lines
      self.println_centered(
        format!(
          "Version {} last revised {}",
          make_version_string(self.options.version),
          self.options.build_date
        ).as_str()
      );
      print!("\n\n"); // two lines
      self.println_centered(
        format!(
          "{} Copyright 2022-2023 Robert Jacobson, BSD License",
          style("Randa").red()
        ).as_str()
      );
      // print!("\n"); // one line
      self.println_centered("The Original Miranda System Copyright Research Software Ltd 1985-2020");
      // print!("\n\n"); // two lines
      self.println_centered("Original Miranda System: http://miranda.org.uk");
      print!("\n\n\n"); // three lines

      if self.options.space_limit != DEFAULT_SPACE {
        format!("({} cells)\n", self.options.space_limit);
      }
      if !self.options.strict_if {
        format!("(-nostrictif : deprecated!)\n");
        // printf("\t\t\t\t%dbit platform\n",__WORDSIZE);  temporary/
      }

      /*
      // Only relevant once .mirarc persists version info.

        if (oldversion < 1999) /* pre-release two */ {
            printf("\
            WARNING:\n\
            a new release of Miranda has been installed since you last used\n\
            the system - please read the `CHANGES' section of the /man pages !!!\n\n");
        } else if (version > oldversion) {
            printf("a new version of Miranda has been installed since you last\n"),
                    printf("used the system - see under `CHANGES' in the /man pages\n\n");
        }
        if (version < oldversion) {
            printf("warning - this is an older version of Miranda than the one\n"),
                    printf("you last used on this machine!!\n\n");
        }
        if (rc_error) {
            printf("warning: \"%s\" contained bad data (ignored)\n", rc_error);
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
    IdentifierRecord::new(
      &mut self.heap,
      "()".to_string(),
      IdentifierDefinition::undefined(),
      Type::Void.into(),
      None
    );
    // *self.heap.id_type(self.void_) = RawValue(Type::Void as isize);
    // *self.heap.id_val(self.void_)  = self.heap.constructor(RawValue(0).into(), self.void_).into();
    let value = self.heap.constructor(RawValue(0).into(), self.void_.into()).into();
    self.void_.set_value(&mut self.heap, IdentifierValue::from_ref(value.into()));

    self.common_stdin  = self.heap.apply(Combinator::Read.into()   , RawValue(0).into());
    self.common_stdinb = self.heap.apply(Combinator::ReadBin.into(), RawValue(0).into());
    let readvals       = self.heap.readvals(
      RawValue(0).into(),
      RawValue(0).into(),
    );
    self.cook_stdin = self.heap.apply(
      readvals,
      // Todo: Should Offside be a combinator?
      Token::Offside.into(),
    );
    self.concat       = self.heap.make_empty_identifier("concat");
    self.diagonalise  = self.heap.make_empty_identifier("diagonalise");
    self.indent_fn    = self.heap.make_empty_identifier("indent");
    self.listdiff_fn  = self.heap.make_empty_identifier("listdiff");
    self.main_id      = self.heap.make_empty_identifier("main");                    // Miranda: change to magic scripts 19.11.2013
    self.message      = self.heap.make_empty_identifier("sys_message");
    self.outdent_fn   = self.heap.make_empty_identifier("outdent");
    self.showabstract = self.heap.make_empty_identifier("showabstract");
    self.showbool     = self.heap.make_empty_identifier("showbool");
    self.showchar     = self.heap.make_empty_identifier("showchar");
    self.showfunction = self.heap.make_empty_identifier("showfunction");
    self.showlist     = self.heap.make_empty_identifier("showlist");
    self.shownum1     = self.heap.make_empty_identifier("shownum1");
    self.showpair     = self.heap.make_empty_identifier("showpair");
    self.showparen    = self.heap.make_empty_identifier("showparen");
    self.showstring   = self.heap.make_empty_identifier("showstring");
    self.showvoid     = self.heap.make_empty_identifier("showvoid");
    self.showwhat     = self.heap.make_empty_identifier("showwhat");
    let stdout_       = self.heap.string("Stdout");
    self.stdout = self.heap.constructor(RawValue(0).into(), stdout_);
  }

  /// This is tsetup() in Miranda.
  ///
  /// Setup and initialization of [`Heap`](crate::data::heap::Heap) occurs in
  /// [`Heap::default()`](crate::data::heap::Heap::default()),
  /// [`Heap::setup_constants()`](crate::data::heap::Heap::setup_constants()), and
  /// [`Heap::setup_standard_types()`](crate::data::heap::Heap::setup_standard_types()).
  fn setup_standard_types(&mut self) {
    self.numeric_function_type  = self.heap.arrow_type(Type::Number.into(), Type::Number.into());
    self.numeric_function2_type = self.heap.arrow_type(Type::Number.into(), self.numeric_function_type);
    self.boolean_function_type  = self.heap.arrow_type(Type::Bool.into()  , Type::Bool.into());
    self.boolean_function2_type = self.heap.arrow_type(Type::Bool.into()  , self.boolean_function_type);
    self.char_list_type         = self.heap.list_type(Type::Char.into());
    self.string_function_type   = self.heap.arrow_type(self.char_list_type, self.char_list_type);

    // tfnumnum is identical to self.numeric_function_type
    // let tfnumnum   = tf(num_t   , num_t);

    let number_list_type = self.heap.list_type(Type::Number.into());
    self.range_step_type = self.heap.arrow2_type(
      Type::Number.into(),
      Type::Number.into(),
      number_list_type,
    );

    self.range_step_until_type = self.heap.arrow_type(Type::Number.into(), self.range_step_type);
  }


  /// The primdef function just creates an identifier on the heap and appends it to the primitive environment.
  fn primitive_synonym_definition(&mut self, name: &str, type_: Type) {
    // self.primdef("num"  , make_typ(0, 0, IdentifierValueType::Synonym, Type::Number), Type::Type);
    let h_id_value_type = IdentifierValueType::new(
      &mut self.heap,
      IdentifierValueTypeData::Synonym {
        source_type: type_
      }
    );
    let h_id_value_data = IdentifierValueData::Typed {
      arity: 0,
      show_function: Value::None,
      value_type: h_id_value_type
    };
    let h_id_value = IdentifierValue::new(&mut self.heap, h_id_value_data);
    let h_id = IdentifierRecord::new(
      &mut self.heap,
      name.to_string(),
      IdentifierDefinition::Undefined,
      Type::Type.into(),
      Some(h_id_value)
    );

    self.primitive_environment.push(&mut self.heap, h_id);
  }


  /// The analog of `primitive_synonym_definition` for the constants `True` and `False`
  fn primitive_bool_definition(&mut self, name: &str, tv: ValueRepresentationType) {
    // self.primdef("True" , Value::Data(1), Type::Bool); // accessible only to 'finger'

    let h_value_data = IdentifierValueData::Arbitrary(Value::Data(tv));
    let h_value = IdentifierValue::new(&mut self.heap, h_value_data);
    let h_bool_constant = IdentifierRecord::new(
      &mut self.heap,
      name.to_string(),
      IdentifierDefinition::Undefined,
      Type::Bool.into(),
      Some(h_value)
    );

    self.primitive_environment.push(&mut self.heap, h_bool_constant);
  }

  /// Enters the primitive identifiers into the primitive environment, sets up predefined ids not referred to by
  /// `parser.y`. Called by [VM::mira_setup()].
  fn primlib(&mut self) {
    self.primitive_synonym_definition("num" , Type::Number);
    self.primitive_synonym_definition("char", Type::Char);
    self.primitive_synonym_definition("bool", Type::Bool);

    self.primitive_bool_definition("True" , 1); // accessible only to 'finger'
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
  fn add_to_environment(&mut self, item: IdentifierRecord) {
    // The thread of pointers goes like this:
    //     self.files == cons(first, rest);
    //     head(self.files) == first
    //     first == cons(cons(fileinfo(filename, mtime), share), definienda)
    //     tail( first  ) == definienda
    if !self.files.is_empty(){
      let rest = self.heap.tl_hd(self.files.get_ref());
      *self.heap.tl_hd_mut(self.files) = self.heap.cons(item.into(), rest.into()).into();
    }
  }

  /// A convenience method used by `privlib(..)` and `stdlib(..)`, see below. It creates an identifier
  /// with the given name, value, and datatype, constructing the value according to whether the name is
  /// that of a constructor (capitalized) or not, and then it adds the identifier to the environment.
  fn predefine_identifier<T>(&mut self, name: &str, value: Value, datatype: T)
      where T: Into<Value>
  {
    let id_ref = self.heap.predefine_identifier(name, value, datatype);

    self.add_to_environment(id_ref);
  }

  // Todo: Why aren't privlib, primlib, and stdlib combined into one and/or all called at once?
  // Todo: Is it ok that some of these are identical to the ones in `stdlib(..)`?
  ///  Adds some internally defined identifiers to the environment. Called when compiling `<prelude>`.
  fn privlib(&mut self) {
    self.predefine_identifier("offside"   , Token::Offside.into(), self.char_list_type); // Used by `indent' in prelude
    self.predefine_identifier("changetype", Combinator::I.into() , Type::Wrong);         // Type::Wrong to prevent being typechecked
    self.predefine_identifier("first"     , Combinator::Hd.into(), Type::Wrong);
    self.predefine_identifier("rest"      , Combinator::Tl.into(), Type::Wrong);
    // The following added to make prelude compilable without `stdenv`
    self.predefine_identifier("code"   , Combinator::Code.into()   , Type::Undefined);
    let concat = self.heap.apply2(Combinator::FoldR.into(), Combinator::Append.into(), Combinator::Nil.into_value());
    self.predefine_identifier("concat" , concat                    , Type::Undefined);
    self.predefine_identifier("decode" , Combinator::Decode.into() , Type::Undefined);
    self.predefine_identifier("drop"   , Combinator::Drop.into()   , Type::Undefined);
    self.predefine_identifier("error"  , Combinator::Error_.into() , Type::Undefined);
    self.predefine_identifier("filter" , Combinator::Filter.into() , Type::Undefined);
    self.predefine_identifier("foldr"  , Combinator::FoldR.into()  , Type::Undefined);
    self.predefine_identifier("hd"     , Combinator::Hd.into()     , Type::Undefined);
    self.predefine_identifier("map"    , Combinator::Map.into()    , Type::Undefined);
    self.predefine_identifier("shownum", Combinator::ShowNum.into(), Type::Undefined);
    self.predefine_identifier("take"   , Combinator::Take.into()   , Type::Undefined);
    self.predefine_identifier("tl"     , Combinator::Tl.into()     , Type::Undefined);
  }

  /// Called when compiling `<stdenv>`. Adds some internally defined identifiers to the environment
  fn stdlib(&mut self) {
    self.predefine_identifier("arctan"  , Combinator::Arctan_Fn.into(), Type::Undefined);
    self.predefine_identifier("code"    , Combinator::Code.into()     , Type::Undefined);
    self.predefine_identifier("cos"     , Combinator::Cos_Fn.into()   , Type::Undefined);
    self.predefine_identifier("decode"  , Combinator::Decode.into()   , Type::Undefined);
    self.predefine_identifier("drop"    , Combinator::Drop.into()     , Type::Undefined);
    self.predefine_identifier("entier"  , Combinator::Entier_Fn.into(), Type::Undefined);
    self.predefine_identifier("error"   , Combinator::Error_.into()   , Type::Undefined);
    self.predefine_identifier("exp"     , Combinator::Exp_Fn.into()   , Type::Undefined);
    self.predefine_identifier("filemode", Combinator::FileMode.into() , Type::Undefined);
    self.predefine_identifier("filestat", Combinator::FileStat.into() , Type::Undefined); // Added Feb 91
    self.predefine_identifier("foldl"   , Combinator::FoldL.into()    , Type::Undefined);
    self.predefine_identifier("foldl1"  , Combinator::FoldL1.into()   , Type::Undefined); // New at release 2
    let huge_num = self.heap.real(f64::MAX);
    // Note: Miranda says, "`hugenum' is the largest fractional number that can exist in this implementation (should
    // be around 1e308 for IEEE standard 64 bit floating point." We use the exact value provided by `f64::MAX`.
    self.predefine_identifier("hugenum"   , huge_num                       , Type::Undefined);
    self.predefine_identifier("last"      , Combinator::ListLast.into()    , Type::Undefined);
    self.predefine_identifier("foldr"     , Combinator::FoldR.into()       , Type::Undefined);
    self.predefine_identifier("force"     , Combinator::Force.into()       , Type::Undefined);
    self.predefine_identifier("getenv"    , Combinator::GetEnv.into()      , Type::Undefined);
    self.predefine_identifier("integer"   , Combinator::Integer.into()     , Type::Undefined);
    self.predefine_identifier("log"       , Combinator::Log_Fn.into()      , Type::Undefined);
    self.predefine_identifier("log10"     , Combinator::Log10_Fn.into()    , Type::Undefined); // New at release 2
    self.predefine_identifier("merge"     , Combinator::Merge.into()       , Type::Undefined); // New at release 2
    self.predefine_identifier("numval"    , Combinator::NumVal.into()      , Type::Undefined);
    self.predefine_identifier("read"      , Combinator::StartRead.into()   , Type::Undefined);
    self.predefine_identifier("readb"     , Combinator::StartReadBin.into(), Type::Undefined);
    self.predefine_identifier("seq"       , Combinator::Seq.into()         , Type::Undefined);
    self.predefine_identifier("shownum"   , Combinator::ShowNum.into()     , Type::Undefined);
    self.predefine_identifier("showhex"   , Combinator::ShowHex.into()     , Type::Undefined);
    self.predefine_identifier("showoct"   , Combinator::ShowOct.into()     , Type::Undefined);
    self.predefine_identifier("showfloat" , Combinator::ShowFloat.into()   , Type::Undefined); // New at release 2
    self.predefine_identifier("showscaled", Combinator::ShowScaled.into()  , Type::Undefined); // New at release 2
    self.predefine_identifier("sin"       , Combinator::Sin_Fn.into()      , Type::Undefined);
    self.predefine_identifier("sqrt"      , Combinator::Sqrt_Fn.into()     , Type::Undefined);
    self.predefine_identifier("system"    , Combinator::Exec.into()        , Type::Undefined); // New at release 2
    self.predefine_identifier("take"      , Combinator::Take.into()        , Type::Undefined);
    let tiny_num = self.heap.real(f64::MIN_POSITIVE);
    // Note: This is likely different from Miranda's `mktiny()`. Miranda says, "`tinynum' is
    // the smallest positive fractional number that can be distinguished from zero in this
    // implementation (should be around 1e-324 for IEEE standard 64 bit floating point)."
    // We instead use the exact value provided by `f64::MIN_POSITIVE`.
    self.predefine_identifier("tinynum", tiny_num              , Type::Undefined); // New at release 2
    self.predefine_identifier("zip2"   , Combinator::Zip.into(), Type::Undefined); // New at release 2
  }


  /// Clear out current script in preparation for reloading
  fn unload(&mut self) {

    self.sorted        = false;
    self.spec_location = ConsList::EMPTY;
    self.rv_script     = false;
    self.algebraic_show_functions  = ConsList::EMPTY;
    self.heap.private_symbols.clear();

    // Todo: Make `unsetids` take a `ConsList`.
    self.unset_ids(self.new_type_names);
    self.new_type_names = ConsList::EMPTY;

    self.unset_ids(self.free_ids);
    self.free_ids = ConsList::EMPTY;

    self.sui_generis_constructors = ConsList::EMPTY;
    self.includees                = ConsList::EMPTY;
    self.type_abstractions        = ConsList::EMPTY;
    self.undefined_names          = ConsList::EMPTY;

    self.unset_ids(self.internals);
    self.internals = ConsList::EMPTY;

    while !self.files.is_empty() {
      // let files: ConsList   = ConsList::from_ref(self.files.into());
      // Guaranteed to unwrap because self.files != ConsList::EMPTY.
      let file : FileRecord = self.files.pop(&self.heap).unwrap();

      let definienda = file.get_definienda(&self.heap);
      if !definienda.is_empty() {
        self.unset_ids(definienda.get_ref().into()) // unsetids(fil_defs(hd[files]));
      }
      file.clear_definienda(&mut self.heap); // fil_defs(hd[files]) = NIL;
    }

    // Remember `self.mkinclude_files` is a nested list of lists.
    while !self.mkinclude_files.is_empty(){
      let file_list = self.mkinclude_files.pop(&self.heap).unwrap();
      let mut file_list = ConsList::<ConsList<FileRecord>>::from_ref(file_list);

      while !file_list.is_empty() {
        let file      : FileRecord = file_list.pop(&self.heap).unwrap();
        let definienda: ConsList   = file.get_definienda(&self.heap);

        if !definienda.is_empty() {
          // Todo: Miranda checks that the item has Tag::Id and just continues if not.
          //       Do we expect everything in `id_list` to be an identifier?
          self.unset_ids(definienda.get_ref().into()) // unsetids(fil_defs(hd[files]));
        }
      }
    }

  }

  fn unset_ids(&mut self, mut id_list: ConsList<IdentifierRecord>) {
    while !id_list.is_empty() {
      // Todo: Miranda checks that the item has Tag::Id and just continues if not.
      //       Do we expect everything in `id_list` to be an identifier?
      let id_record: IdentifierRecord = id_list.pop(&mut self.heap).unwrap();
      id_record.unset_id(&mut self.heap);
      // should we remove from namebucket ?
    }
  }


  /// load a sequence of definitions terminated by `DEF_X`, or a single object terminated
  /// by `DEF_X`, from the byte stream `byte_iter`.
  ///
  /// The type of the returned value has to be an opaque type, because any serializable object can be returned.
  // Todo: Some calls to `load_defs()` assume that a cons list, or even a `ConsList<IdentifierRecord>`, is returned,
  //       but it's clear that there are times when the return value is not a cons list.
  fn load_defs(
    &mut self,
    byte_iter: &mut dyn Iterator<Item=u8>
  ) -> Result<ConsList<IdentifierRecord>, BytecodeError>
  {
    // Holds a list of definitions in cases where multiple definitions are read.
    let mut defs: ConsList = ConsList::EMPTY;
    // Holds the components of an item that have been read so far. When all of the components have been read, the
    // item will be created using `item_stack`.
    let mut item_stack: Vec<RawValue> = Vec::new();

    while let Some(ch) = byte_iter.next() {
      // Decode the byte
      let code = if let Some(code) = Bytecode::from_u8(ch)  {
        code
      } else {
        // It is a RawValue.
        let v = if  ch > 127 {
          ch as ValueRepresentationType + 256
        } else {
          ch as ValueRepresentationType
        };
        item_stack.push(v.into());
        continue;
      };

      match code {

        Bytecode::Char => {
          let v = next(byte_iter)?;
          item_stack.push((v as ValueRepresentationType + 128).into());
        }

        Bytecode::TypeVariable => {
          let v = next(byte_iter)?;
          let type_var = self.heap.type_var(Value::None, (v as ValueRepresentationType).into());
          item_stack.push(type_var.into());
        }

        // Todo: Do we want to support "small" integers? Yes for now, for Miranda compatibility.
        Bytecode::Short => {
          let mut v = next(byte_iter)?;
          if v & 128 {
            v = v | (!127);
          }
          item_stack.push(self.heap.small_int((v as ValueRepresentationType)).into());
        }

        Bytecode::Integer => {
          // Allow the very first value to be -1.
          let mut v: ValueRepresentationType = get_number::<ValueRepresentationType>(byte_iter)?;
          let mut int_list: RawValue = self.heap.integer(v).into();

          item_stack.push(int_list);

          // The list of ints is constructed from the head to the tail, the opposite from if we used `push`.
          // The `cursor` always points to the "tail", the next empty slot the next boxed integer will go.
          let mut cursor: &mut RawValue = &mut self.heap[int_list].tail;
          v = get_number::<ValueRepresentationType>(byte_iter)?;

          while v != -1 {
            // Construct a boxed integer and store it in the tail of the previous boxed integer
            *cursor = self.heap.integer(v);
            // Read the next integer from the byte iterator
            v = get_number::<ValueRepresentationType>(byte_iter)?;
            // Update the cursor to point to the tail of the newly constructed boxed integer
            cursor = &mut self.heap[*cursor].tail;
          }
        }

        Bytecode::Double => {
          let real_number = get_number::<f64>(byte_iter)?;

          // Todo: We convert isize-->f64 and then f64-->isize. This is stupid.
          item_stack.push(self.heap.real(real_number).into());
        }

        Bytecode::Unicode => {
          let v = get_number::<ValueRepresentationType>(byte_iter)?;

          item_stack.push(self.heap.unicode(v).into());
        }

        // Reads in the index of a private symbol. Differs from `Bytecode::PrtivateName1` in that it only reads two
        // bytes.
        Bytecode::PrivateName => {
          let mut v: usize = get_number_16bit::<u16>(byte_iter)? as usize;

          // See the notes for `private_symbol_base_index` in `vm.load_script()`.
          v += private_symbol_base_index;

          let ps = self.heap.get_nth_private_symbol(v);
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
          let mut v = get_number::<usize>(byte_iter)?;
          // See the notes  for `private_symbol_base_index` in `vm.load_script()`.
          v += private_symbol_base_index;

          let ps = self.heap.get_nth_private_symbol(v);
          item_stack.push(ps.into());
        }

        Bytecode::Construct => {
          let v: i16 = get_number_16bit::<i16>(byte_iter)?;
          // Wrap the top value on the stack with the constructor.
          if item_stack.is_empty() {
            item_stack.push(Combinator::Nil.into());
          }
          let last_value = item_stack.last().unwrap();
          *last_value = self.heap.constructor(v, (*last_value));
          
          let new_value = self.heap.constructor(v, last_value);
          item_stack.push(new_value.into());
        }

        Bytecode::ReadVals => {
          let previous_value = item_stack.pop_unchecked(&mut self.heap);
          let new_value = self.heap.start_read_vals(Value::None, previous_value.into());
          item_stack.push(new_value.into());

          self.rv_script = true;
        }

        Bytecode::ID => {
          let name: String = parse_string(byte_iter)?;

          match self.heap.symbol_table.get(name.as_str()) {
            Some(id_ref) => {
              let id = IdentifierRecord::from_ref(id_ref.into());
              let id_type = id.get_type(&mut self.heap);

              if id_type == Type::New.into()  {
                // A name collision
                self.clashes.insert_ordered(&mut self.heap, id);
              }  else if id_type == Type::Alias.into() {
                // Follow the alias.
                match id.get_value(&self.heap)? {
                  None => { return Err(BytecodeError::MalformedDef) }
                  Some(id_value) => {
                    item_stack.push(id_value.get_ref());
                  }
                }
              } else {
                // Can this happen?
                item_stack.push(id_ref.into());
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
          let id = self.heap.symbol_table.get(name.as_str()).ok_or(BytecodeError::SymbolNotFound)?;
          let pair = self.heap.data_pair(*id, Value::None);

          item_stack.push(pair.into());
        }

        Bytecode::Here => {
          let ch = byte_iter.peek();
          let file_path: String = // the value of the following if block
              if ch == 0 {
                // ch==0 is shorthand for "current file"
                byte_iter.next(); // Consumed peeked value.
                self.current_file()
              }
              else if ch != b'/' {
                // Transform path into absolute path.
                let prefix_ref = self.prefix_stack.head(&self.heap);
                let mut prefix = self.heap.resolve_string(prefix_ref.into()).unwrap();

                format!("{}{}", prefix, parse_string(byte_iter)?)
              }
              else {
                parse_string(byte_iter)?
              };
          let file_id = match self.heap.symbol_table.get(file_path.as_str()) {
            Some(value) => value.clone(),
            None => {
              // Todo: What is the right thing to do here?
              self.heap.make_empty_identifier(file_path.as_str()).get_ref().into()
            }
          };
          let line_number = parse_line_number(byte_iter)?;
          let file_info = self.heap.file_info(file_id, line_number.into());

          item_stack.push(file_info.into());
        }

        Bytecode::Definitions => {
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
              return  Ok(defs.reversed(heap));
            }

            1 => {
              // Case 2: Object delimiter, signals the end of an identifier record.
              let item: RawValue = item_stack.pop().unwrap();
              return Ok(item);
            }

            2 => {
              // Case 3: Private name delimiter, signals end of a private name.
              let item = item_stack.pop().unwrap();
              self.heap[item].tail = item_stack.pop().unwrap();
              defs.push(&mut self.heap, item);
            }

            4 => {
              let top_item = *item_stack.last().unwrap();

              if self.heap[top_item].tag != Tag::Id {
                if top_item == NIL.into() { // FIX1
                  item_stack.clear();
                  continue;
                }

                // An ID aliased to a pname.
                // A pname has the form `strcons(index_in_private_names_vector, value)`,
                // where value (I think) is an Identifier Record.

                item_stack.pop(); // Value already in top_item
                // let top_item = item_stack.pop().unwrap();

                // Todo: Check that `top_item` is a reference to an `IdentifierRecord`.
                //       But it is gauranteed not to be an `IdentifierRecord`
                let new_id = IdentifierRecord::from_ref(top_item);
                self.suppressed.push(&mut self.heap, new_id);

                // Todo: Why are we throwing away the who field?
                item_stack.pop(); // who field

                let private_aka = { // Scope of temporary
                  let top_item = item_stack.last().unwrap();
                  if self.heap[top_item].tag == Tag::Cons {
                    self.heap[top_item].head
                  } else {
                    NIL
                  }
                };

                // Todo: Why are we throwing away the type field?
                item_stack.pop(); // lose type

                // The value of the private name
                let new_id_value = IdentifierValue::from_ref(item_stack.pop().unwrap());
                new_id.set_value(&mut self.heap, new_id_value);

                // let new_id_value_data = new_id_value.get_data(&self.heap);
                if let IdentifierValueData::Typed { value_type, .. } = new_id_value.get_data(&self.heap) {
                  if item_stack[1] == Type::Type.into() &&
                    value_type.get_numeric_type_specifier(&self.heap) != Ok(IdentifierValueTypeDataSpecifier::Synonym) {
                      // Suppressed typename
                      // Reverse assoc in ALIASES
                      let mut aliases = self.aliases;
                      let mut alias: RawValue = NIL.into();
                      let mut id: Option<IdentifierRecord> = None;
                      while !aliases.is_empty() {
                        alias = aliases.pop_unchecked(&self.heap);
                        id = Some(IdentifierRecord::from_ref(self.heap[alias].tail ));
                        // It's not clear if "get_value" is meant to be a get_value or if it is just a `tl[ref]` operation.
                        if id.get_value(&self.heap) == Ok(Some(top_item)) {
                          break;
                        }
                        id = None;
                      }
                      if let Some(found_id) = id {
                        // Surely must hold ??
                        self.suppressed_t.push(&mut self.heap, found_id);
                      }
                    }
                    else if new_id.get_value(&self.heap) == IdentifierValueData::Undefined {
                      // Special kludge for undefined names, necessary only if we allow names specified
				              // but not defined to be %included.
                      if private_aka == Combinator::NIL.into() {
                        // Reverse assoc in ALIASES
                        let mut aliases = self.aliases;
                        let mut alias: RawValue = NIL.into();
                        let mut id: Option<IdentifierRecord> = None;
                        while !aliases.is_empty() {
                          alias = aliases.pop_unchecked(&self.heap);
                          id = Some(IdentifierRecord::from_ref(self.heap[alias].tail ));
                          // It's not clear if "get_value" is meant to be a get_value or if it is just a `tl[ref]` operation.
                          if id.get_value(&self.heap) == Ok(Some(top_item)) {
                            break;
                          }
                          id = None;
                        }
                        if let Some(found_id) = id {
                          // Todo: Untangle what Miranda is doing here. What's the difference between `id_val` and `get_id`?
                          private_aka = self.heap.data_pair(found_id, 0);
                        }
                      }
                      // this will generate sensible error message
				              // see reduction rule for DATAPAIR
                      new_id.set_value(&mut self.heap, self.heap.apply(private_aka, self.heap.file_info(self.current_file(), 0)));
                    }
                    defs.push(&mut self.heap, top_item);
                    continue;
                }
              }

              if id_type(item_stack.last().unwrap()) != Type::New.into() &&
                  (id_type(item_stack.last().unwrap()) != Type::Undefined
                  || id_val(item_stack.last().unwrap()) != Combinator::Undef)
              {
                //stuff

              }
            }
          }

        }

        Bytecode::Apply => {}

        Bytecode::Cons => {}

      }
    }


    ConsList::default()
  }






}




/// Convenience function that returns the next byte or `BytecodeError::UnexpectedEOF`.
fn next(byte_iter: &mut dyn Iterator<Item=u8>) -> Result<u8, BytecodeError> {
  match byte_iter.next() {
    Some(ch) => Ok(ch),

    None => {
      Err(BytecodeError::UnexpectedEOF)
    }
  }
}
