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


use console::{Alignment, pad_str, style, Term};

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
  constants::DEFAULT_SPACE,
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
};
use crate::constants::XVERSION;
use crate::data::Tag;


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

  sorted                   : bool,        // Are the file definitions sorted?
  in_export_list           : bool,
  in_semantic_redeclaration: bool,
  rv_script                : bool,        // flags readvals in use (for garbage collector)
  in_file                  : Option<Box<dyn BufRead>>,
  last_expression          : Value,       // A reference to the last expression evaluated.
  last_mame                : Option<&'static str>,

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
  free_ids    : ConsList<IdentifierRecord>,
  internals   : ConsList<IdentifierRecord>, // list of names not exported, used by fix/unfixexports
  idsused     : ConsList<IdentifierRecord>,
  clashes     : ConsList<IdentifierRecord>,
  ihlist      : Value,
  includees   : ConsList,
  lasth       : Value,
  lastname    : Value,
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
  files                   : ConsList<FileRecord>,
  sui_generis_constructors: ConsList, // user defined sui-generis constructors (Miranda's SGC)
  type_abstractions       : ConsList, // cons list of abstype declarations (Miranda's TABSTR)
  undefined_names         : ConsList<IdentifierRecord>, // undefined names used in script (Miranda's ND)
  new_type_names          : ConsList<IdentifierRecord>, // newly declared type names in current code unit (Miranda's `newtyps`)
  algebraic_show_functions: ConsList, // show functions of algebraic types in scope (Miranda's `algshfns`)
  special_show_forms      : ConsList, // all occurrences of special forms (show) encountered during type check

  // list of a list of files to be unloaded if `mkincludes` is interrupted (Miranda's ld_stuff).
  mkinclude_files         : ConsList<ConsList<FileRecord>>,

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

      compiling               : true,
      initializing            : true,                      // Checked in `undump(..)`
      making                  : false,
      loading                 : false,

      activity                : ActivityReset::Collecting, // Arbitrary value.
      command_mode            : false,
      sorted                  : false,
      // type check
      in_export_list           : false,
      in_semantic_redeclaration: false,
      rv_script                : false,       // Flags readvals in use (for garbage collector)
      in_file                  : None,
      last_expression          : Value::None, // A reference to the last expression evaluated.
      last_mame                : None,
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
      free_ids    : ConsList::EMPTY,
      internals   : ConsList::EMPTY,
      idsused     : ConsList::EMPTY,
      clashes     : ConsList::EMPTY,
      ihlist      : Value::None,
      includees   : ConsList::EMPTY,
      lasth       : Value::None,
      lastname    : Value::None,
      lexdefs     : NIL,
      nonterminals: NIL,
      ntmap       : NIL,
      ntspecmap   : NIL,

      files                   : ConsList::EMPTY,
      sui_generis_constructors: ConsList::EMPTY, // user defined sui-generis constructors (Miranda's SGC)
      type_abstractions       : ConsList::EMPTY, // cons list of abstype declarations (Miranda's TABSTR)
      undefined_names         : ConsList::EMPTY, // undefined names used in script (Miranda's ND)
      new_type_names          : ConsList::EMPTY, // newly declared type names in current code unit (Miranda's `newtyps`)
      algebraic_show_functions: ConsList::EMPTY, // show functions of algebraic types in scope (Miranda's `algshfns`)
      special_show_forms      : ConsList::EMPTY, // all occurrences of special forms (show) encountered during type check
      mkinclude_files         : ConsList::EMPTY, // list of files to be unloaded if `mkincludes` is interrupted (Miranda's ld_stuff).
      spec_location           : ConsList::EMPTY, // list giving location of `spec` for ids both defined and specified.

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

      Err(BytecodeError::NameClash(clashes)) => {
        if ideep == 0 {
          let sorted = self.alfasort(clashes);
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
        println!(source_path); // added &&!magic 26.11.2019
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

  /// Loads a compiled script from `in_file` for the `source_file`.
  fn load_script(
    &mut self,
    in_file        : &File,
    mut source_file: String,
    mut aliases    : ConsList, // List of cons(new_id, old_id)
    parameters     : Value,
    is_main_script: bool
  ) -> Result<ConsList<FileRecord>, BytecodeError>
  {
    /*
    extern word nextpn,ND,errline,algshfns,internals,freeids,includees,SGC;
    extern char *dicp, *dicq;
    word ch,files=NIL;
    TORPHANS=BAD_DUMP=0;
    CLASHES=NIL;
    */
    { // scope of prefix
      let prefix = self.prefix().unwrap_or_else(|_| "".to_string());
      make_relative_path(&mut source_file, &prefix);
    }

    self.files = ConsList::EMPTY;

    let mut file_bytes = vec![];
    { // Scope of f_reader
      let mut f_reader = BufReader::new(in_file);
      let bytes_read = f_reader.read_to_end(&mut file_bytes)
                                .map_err(|e| BytecodeError::UnexpectedEOF)?;
      if bytes_read < 2 {
        return Err(BytecodeError::UnexpectedEOF);
      }
    }
    let mut byte = file_bytes.iter();
    if byte.next().unwrap() != WORD_SIZE {
      return Err(BytecodeError::ArchitectureMismatch);
    }
    if byte.next().unwrap() != XVERSION {
      return Err(BytecodeError::WrongBytecodeVersion);
    }

    if !aliases.is_empty() {
      // For each `old' install diversion to `new'.
      // If alias is of form `-old`, `new' is a `pname` (private name).

      let mut original_aliases = aliases;
      let mut alias_iterator   = aliases; // Not technically an iterator.

      while !alias_iterator.is_empty() {
        // cons(
        //   cons(strcons(name,who),type),
        //   cons(cons(arity,showfn),cons(free_t,NIL))
        // )
        let mut a   = alias_iterator.pop(&mut self.heap);
        let old     = IdentifierRecord::from_ref( self.tl_hd(a) );
        let new_ref = self.hd_hd(a);


        let hold = { // scope of temporaries
          // cons(  id_who(old), cons(  id_type(old), id_val(old))  );
          let old_who  = old.get_definition(&self.heap).unwrap();
          let old_type = old.get_datatype(&self.heap).unwrap();
          let old_val  = old.get_value(&self.heap).unwrap();
          let tl       = self.heap.cons(old_type.into(), old_val.into());
          self.heap.cons( old_who.into(), tl);
        };

        old.set_type(&mut self.heap, Type::Alias);
        old.set_val(&mut self.heap, new_ref);

        if self.heap[new_ref].tag == Tag::Id {
          let new = IdentifierRecord::from_ref(new_ref);
          let new_datatype = new.get_datatype(&self.heap);
          if (new_datatype != Type::Undefined || new.get_value(&self.heap).unwrap().is_some())
              && new_datatype != Type::Alias
          {
            // Insert new into self.clashes such that self.clashes remains in ascending address order.
            // Todo: Why order these?
            self.clashes = add1(new, &mut self.clashes);
          }
        }

      }
    }



    Err(BytecodeError::ArchitectureMismatch)
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
    self.void_.set_value(&mut self.heap, IdentifierValueData::Arbitrary(value));

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

}


