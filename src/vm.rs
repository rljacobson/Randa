/*!

Setup and orchestration of built-ins, compilation, etc. Because everything lives in the heap,
it's tempting to put all of this in `Heap`. We separate concerns by factoring out non-generic heap
manipulation, environment initialization, and compilation from generic heap functions, accessors, etc.

*/

use std::{
  fs::File,
  io::BufRead
};

use crate::{
  compiler::Token,
  data::{
    heap::{is_constructor, is_capitalized},
    Combinator,
    Heap,
    Identifier,
    IdentifierDefinition,
    IdentifierValue,
    IdentifierValueType,
    Type,
    Value,
    ValueRepresentationType,
    values::RawValue
  }
};
use crate::options::{Options, setup_argument_parser};


pub enum ActivityReset {
  Collecting,
  Loading{
    blank_error: bool,
    unlink     : bool
  },
  Making{
    make_status: bool
  }
}

/// Holds the state of the current session, including the heap and compilation state.
///
/// Setup and initialization of [`Heap`](crate::data::heap::Heap) occurs in
/// [`VM::default()`](crate::data::heap::Heap::default()),
/// [`VM::setup_constants()`](crate::data::heap::Heap::setup_constants()), and
/// [`VM::setup_standard_types()`](crate::data::heap::Heap::setup_standard_types()).
pub struct VM {
  // Flags and other state that do not live on the heap.

  // The `options` function should be produced by the `setup_argument_parser()` function.
  options: Options,

  /// There are two types of Miranda process: "compiling" (the main process) and subsidiary processes
  /// launched for each evaluation. The `compiling  flag tells us which kind of process we are in.
  // Todo: How is this state thread local?
  compiling: bool,

  activity                 : ActivityReset,
  command_mode             : bool,
  abstype_declarations     : Vec<String>,
  sui_generis_constructors : Vec<String>,  // user defined sui-generis constructors
  new_type_names           : Vec<String>,  // newly declared type names in current code unit
  algebraic_show_functions : Vec<String>,  // show functions of algebraic types in scope
  special_show_forms       : Vec<String>,  // all occurrences of special forms (show) encountered during
                                           // type check
  in_export_list           : bool,
  in_semantic_redeclaration: bool,
  rv_script                : bool,         // flags readvals in use (for garbage collector)
  file_queue               : Vec<File>,
  in_file                  : Option<Box<dyn BufRead>>,
  last_expression          : Value,        // A reference to the last expression evaluated.
  last_mame                : Option<&'static str>,

  heap                     : Heap,

  // region Flags and other state that lives on the heap.

  /**
  The cons list `files` is also called the environment in the Miranda source code.

  From Miranda:

    > `files` is a cons list of elements, each of which is of the form
    >   `cons(cons(fileinfo(filename,mtime),share),definienda)`
    > where `share` (=0,1) says if repeated instances are shareable. Current script at
    > the front followed by subsidiary files due to `%insert` and `%include` elements due
    > to `%insert` have `NIL` `definienda` (they are attributed to the inserting script).

  The "definienda" is itself a cons list of items (types, identifiers, etc.) that are defined in the current file.
  (A definiendum is a term that is being defined or clarified. The plural form of definiendum is definienda.)
  */
  files       : Value,
  col_fn      : Value,
  embargoes   : Value,
  eprodnts    : Value,
  exportfiles : Value,
  exports     : Value,
  fnts        : Value, // fnts is flag indicating %bnf in use. Treated as Value?
  freeids     : Value,
  idsused     : Value,
  ihlist      : Value,
  includees   : Value,
  lasth       : Value,
  lastname    : Value,
  lexdefs     : Value,
  nonterminals: Value,
  ntmap       : Value,
  ntspecmap   : Value,

  // endregion

  // region Constants
  common_stdin : Value,
  common_stdinb: Value,
  concat       : Value,
  cook_stdin   : Value,
  diagonalise  : Value,
  main_id      : Value,
  message      : Value,
  showabstract : Value,
  showbool     : Value,
  showchar     : Value,
  showfunction : Value,
  showlist     : Value,
  shownum1     : Value,
  showpair     : Value,
  showparen    : Value,
  showstring   : Value,
  showvoid     : Value,
  showwhat     : Value,
  standardout  : Value,

  // These might be constants
  listdiff_fn: Value,
  indent_fn  : Value,
  outdent_fn : Value,

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
  void_          : Value,

  /// A cons list of identifiers in the primitive environment.
  primitive_environment: Value,

  // endregion

}

// pub fn reset(activity_reset: ActivityReset)-

impl Default for VM {
  /// Setup and initialization of [`Heap`](crate::data::heap::Heap) occurs in
  /// [`VM::default()`](crate::data::heap::Heap::default()),
  /// [`VM::setup_constants()`](crate::data::heap::Heap::setup_constants()), and
  /// [`VM::setup_standard_types()`](crate::data::heap::Heap::setup_standard_types()).
  fn default() -> Self {
    let mut vm = VM{
      options                  : setup_argument_parser(),
      compiling                : true,
      activity                 : ActivityReset::Collecting, // Arbitrary value.
      command_mode             : false,
      abstype_declarations     : vec![],
      sui_generis_constructors : vec![],  // User defined sui-generis constructors
      new_type_names           : vec![],  // Newly declared type names in current code unit
      algebraic_show_functions : vec![],  // Show functions of algebraic types in scope
      special_show_forms       : vec![],  // All occurrences of special forms (show) encountered during
      // type check
      in_export_list           : false,
      in_semantic_redeclaration: false,
      rv_script                : false,   // Flags readvals in use (for garbage collector)
      file_queue               : vec![],
      in_file                  : None,
      last_expression          : Value::None, // A reference to the last expression evaluated.
      last_mame                : None,


      heap: Heap::default(),


      listdiff_fn: Value::Uninitialized,
      indent_fn  : Value::Uninitialized,
      outdent_fn : Value::Uninitialized,

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
      void_: Value::Uninitialized,

      primitive_environment: Combinator::Nil.into(),

      files       : Combinator::Nil.into(),
      col_fn      : Value::None,
      embargoes   : Combinator::Nil.into(),
      eprodnts    : Combinator::Nil.into(),
      exportfiles : Combinator::Nil.into(),
      exports     : Combinator::Nil.into(),
      fnts        : Combinator::Nil.into(),
      freeids     : Combinator::Nil.into(),
      idsused     : Combinator::Nil.into(),
      ihlist      : Value::None,
      includees   : Combinator::Nil.into(),
      lasth       : Value::None,
      lastname    : Value::None,
      lexdefs     : Combinator::Nil.into(),
      nonterminals: Combinator::Nil.into(),
      ntmap       : Combinator::Nil.into(),
      ntspecmap   : Combinator::Nil.into(),

      common_stdin : Value::Uninitialized,
      common_stdinb: Value::Uninitialized,
      concat       : Value::Uninitialized,
      cook_stdin   : Value::Uninitialized,
      diagonalise  : Value::Uninitialized,
      main_id      : Value::Uninitialized,
      message      : Value::Uninitialized,
      showabstract : Value::Uninitialized,
      showbool     : Value::Uninitialized,
      showchar     : Value::Uninitialized,
      showfunction : Value::Uninitialized,
      showlist     : Value::Uninitialized,
      shownum1     : Value::Uninitialized,
      showpair     : Value::Uninitialized,
      showparen    : Value::Uninitialized,
      showstring   : Value::Uninitialized,
      showvoid     : Value::Uninitialized,
      showwhat     : Value::Uninitialized,
      standardout  : Value::Uninitialized,

    };

    vm.mira_setup();
    vm
  }

}

impl VM {

  fn mira_setup(&mut self){
    self.setup_constants();
    self.setup_standard_types();

    // reset_pns(); // Setup private namespace
    // Enters the primitive identifiers into the primitive environment. Called by "mira_setup".
    // primlib()
  }

  /// Most of Miranda's `mira_setup()` is here.
  ///
  /// Setup and initialization of [`Heap`](crate::data::heap::Heap) occurs in
  /// [`Heap::default()`](crate::data::heap::Heap::default()),
  /// [`Heap::setup_constants()`](crate::data::heap::Heap::setup_constants()), and
  /// [`Heap::setup_standard_types()`](crate::data::heap::Heap::setup_standard_types()).
  fn setup_constants(&mut self) {
    // Referenced below
    // Nill lives in `Heap` (`Heap::nill`) because some `Heap` functions use it.
    self.void_     = self.heap.make_empty_identifier("()");

    *self.heap.id_type(self.void_) = RawValue(Type::Void as isize);
    *self.heap.id_val(self.void_)  = self.heap.constructor(RawValue(0).into(), self.void_).into();

    self.common_stdin  = self.heap.apply(Combinator::Read.into(), RawValue(0).into());
    self.common_stdinb = self.heap.apply(Combinator::ReadBin.into(), RawValue(0).into());
    self.cook_stdin    = self.heap.apply(
      self.heap.readvals(
        RawValue(0).into(),
        RawValue(0).into()
      ),
      // Todo: Should Offside be a combinator?
      Token::Offside.into()
    );
    self.concat        = self.heap.make_empty_identifier("concat");
    self.diagonalise   = self.heap.make_empty_identifier("diagonalise");
    self.indent_fn     = self.heap.make_empty_identifier("indent");
    self.listdiff_fn   = self.heap.make_empty_identifier("listdiff");
    self.main_id       = self.heap.make_empty_identifier("main"); // Miranda: change to magic scripts 19.11.2013
    self.message       = self.heap.make_empty_identifier("sys_message");
    self.outdent_fn    = self.heap.make_empty_identifier("outdent");
    self.showabstract  = self.heap.make_empty_identifier("showabstract");
    self.showbool      = self.heap.make_empty_identifier("showbool");
    self.showchar      = self.heap.make_empty_identifier("showchar");
    self.showfunction  = self.heap.make_empty_identifier("showfunction");
    self.showlist      = self.heap.make_empty_identifier("showlist");
    self.shownum1      = self.heap.make_empty_identifier("shownum1");
    self.showpair      = self.heap.make_empty_identifier("showpair");
    self.showparen     = self.heap.make_empty_identifier("showparen");
    self.showstring    = self.heap.make_empty_identifier("showstring");
    self.showvoid      = self.heap.make_empty_identifier("showvoid");
    self.showwhat      = self.heap.make_empty_identifier("showwhat");
    self.standardout   = self.heap.constructor(RawValue(0).into(), self.heap.string("Stdout"));

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

    self.range_step_type = self.heap.arrow2_type(
      Type::Number.into(),
      Type::Number.into(),
      self.heap.list_type(Type::Number.into())
    );

    self.range_step_until_type = self.heap.arrow_type(Type::Number.into(), self.range_step_type);
  }

  /*
  // The primdef function just creates an identifier on the heap. We can do this more declaratively, although with
  // slightly more copy+pasted boilerplate. This function would be used by` primlib()`.
  // fn primdef(&mut self, n: &str, v: Value, t: Type) {
  //   let x: Value                = self.heap.make_id(n);
  //   self.primitive_environment  = self.heap.cons(x, self.primitive_environment);
  //   *self.heap.id_val(x)  = v.into();
  //   *self.heap.id_type(x) = RawValue(t as ValueRepresentationType);
  // }
  */

  /// Enters the primitive identifiers into the primitive environment, sets up predefined ids, not referred to by
  /// `parser.y`. Called by [VM::mira_setup()].
  fn primlib(&mut self) {
    let num_id =
        Identifier{
          name: "num".to_string(),
          definition: IdentifierDefinition::Undefined,
          datatype: Type::Type,
          value: Some(IdentifierValue::Typed{
            arity: 0,
            show_function: Value::None,
            value_type: IdentifierValueType::Synonym {
              source_type: Type::Number
            },
          }),
        }.compile(&mut self.heap);

    self.primitive_environment  = self.heap.cons(num_id, self.primitive_environment);


    let char_id =
        Identifier{
          name: "char".to_string(),
          definition: IdentifierDefinition::Undefined,
          datatype: Type::Type,
          value: Some(IdentifierValue::Typed{
            arity: 0,
            show_function: Value::None,
            value_type: IdentifierValueType::Synonym {
              source_type: Type::Char
            },
          }),
        }.compile(&mut self.heap);

    self.primitive_environment  = self.heap.cons(char_id, self.primitive_environment);


    let bool_id =
        Identifier{
          name: "bool".to_string(),
          definition: IdentifierDefinition::Undefined,
          datatype: Type::Type,
          value: Some(IdentifierValue::Typed{
            arity: 0,
            show_function: Value::None,
            value_type: IdentifierValueType::Synonym {
              source_type: Type::Bool
            },
          })
        }.compile(&mut self.heap);

    self.primitive_environment  = self.heap.cons(bool_id, self.primitive_environment);


    let true_id =
        Identifier {
          name: "True".to_string(),
          definition: IdentifierDefinition::Undefined,
          datatype: Type::Bool,
          value: Some(IdentifierValue::Arbitrary(Value::Data(1))),
        }.compile(&mut self.heap);

    self.primitive_environment  = self.heap.cons(true_id, self.primitive_environment);


    let false_id =
        Identifier {
          name: "False".to_string(),
          definition: IdentifierDefinition::Undefined,
          datatype: Type::Bool,
          value: Some(IdentifierValue::Arbitrary(Value::Data(0))),
        }.compile(&mut self.heap);

    self.primitive_environment  = self.heap.cons(false_id, self.primitive_environment);

    // The preceding code is the equivalent of the following.
    // self.primdef("num"  , make_typ(0, 0, IdentifierValueType::Synonym, Type::Number), Type::Type);
    // self.primdef("char" , make_typ(0, 0, IdentifierValueType::Synonym, Type::Char)  , Type::Type);
    // self.primdef("bool" , make_typ(0, 0, IdentifierValueType::Synonym, Type::Bool)  , Type::Type);
    // self.primdef("True" , Value::Data(1), Type::Bool); // accessible only to 'finger'
    // self.primdef("False", Value::Data(0), Type::Bool); // likewise - FIX LATER
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
    echostack = idsused = prefixstack = litstack = linostack = vergstack = margstack = NIL;
    prefix = 0;
    prefixbase[0] = '\0';
    echoing = verbosity & listing;
    brct = inbnf = sreds = inlex = inexplist = commandmode = lverge = col = lmargin = 0;
    atnl = 1;
    rv_script = 0;
    algshfns = newtyps = showchain = SGC = TABSTRS = NIL;
    c = ' ';
    line_no = 0;
    litmain = literate = 0;
  }
  */


  /// Adds the item (type, identifier, etc.) to the environment, i.e. cons it onto the definienda of the first
  /// item in the `files` cons list.
  fn add_to_environment(&mut self, item: Value) {
    // The thread of pointers goes like this:
    //     self.files == cons(first, rest);
    //     head(self.files) == first
    //     first == cons(cons(fileinfo(filename, mtime), share), definienda)
    //     tail( first  ) == definienda

    let rest = self.heap.tl_hd(self.files);
    *self.heap.tl_hd_mut(self.files) = self.heap.cons(item, rest.into()).into();
  }

  /// A convenience method used by `privlib(..)` and `stdlib(..)`, see below. It creates an identifier
  /// with the given name, value, and datatype, constructing the value according to whether the name is
  /// that of a constructor (capitalized) or not, and then it adds the identifier to the environment.
  fn predefine_identifier(&mut self, name: &str, value: Value, datatype: Type) {
    let id_ref = self.heap.predefine_identifier(name, value, datatype);

    self.add_to_environment(id_ref);
  }

  // Todo: Why aren't privlib, primlib, and stdlib combined into one and/or all called at once?
  // Todo: Is it ok that some of these are identical to the ones in `stdlib(..)`?
  ///  Adds some internally defined identifiers to the environment. Called when compiling `<prelude>`.
  fn privlib(&mut self) {
    self.predefine_identifier("offside", Token::Offside.into(), self.char_list_type.into()); // Used by `indent' in prelude
    self.predefine_identifier("changetype", Combinator::I.into(), Type::Wrong); // Type::Wrong to prevent being typechecked
    self.predefine_identifier("first", Combinator::Hd.into(), Type::Wrong);
    self.predefine_identifier("rest", Combinator::Tl.into(), Type::Wrong);
    // The following added to make prelude compilable without `stdenv`
    self.predefine_identifier("code", Combinator::Code.into(), Type::Undefined);
    self.predefine_identifier("concat", ap2(Combinator::Foldr.into(), Combinator::Append, Combinator::Nil), Type::Undefined);
    self.predefine_identifier("decode", Combinator::Decode.into(), Type::Undefined);
    self.predefine_identifier("drop", Combinator::Drop.into(), Type::Undefined);
    self.predefine_identifier("error", Combinator::Error.into(), Type::Undefined);
    self.predefine_identifier("filter", Combinator::Filter.into(), Type::Undefined);
    self.predefine_identifier("foldr", Combinator::FoldR.into(), Type::Undefined);
    self.predefine_identifier("hd", Combinator::Hd.into(), Type::Undefined);
    self.predefine_identifier("map", Combinator::Map.into(), Type::Undefined);
    self.predefine_identifier("shownum", Combinator::Shownum.into(), Type::Undefined);
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
    // Todo: Implement `Heap::store_double()`
    self.predefine_identifier("hugenum", sto_dbl(f64::MAX), Type::Undefined);
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
    self.predefine_identifier("tinynum", sto_dbl(f64::MIN_POSITIVE), Type::Undefined); // New at release 2
    self.predefine_identifier("zip2", Combinator::Zip.into(), Type::Undefined); // New at release 2
  }

}


