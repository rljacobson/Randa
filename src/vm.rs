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

  // region Flags and other state

  /**
  From Miranda:

  > `files` is a cons list of elements, each of which is of the form
  >      `cons(cons(fileinfo(filename,mtime),share),definienda)`
  > where `share` (=0,1) says if repeated instances are shareable. Current script at
  > the front followed by subsidiary files due to `%insert` and `%include` elements due
  >to `%insert` have `NIL` `definienda` (they are attributed to the inserting script).

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

      activity                 : ActivityReset::Collecting, // Arbitrary value.
      command_mode             : false,
      abstype_declarations     : vec![],
      sui_generis_constructors : vec![],  // user defined sui-generis constructors
      new_type_names           : vec![],  // newly declared type names in current code unit
      algebraic_show_functions : vec![],  // show functions of algebraic types in scope
      special_show_forms       : vec![],  // all occurrences of special forms (show) encountered during
      // type check
      in_export_list           : false,
      in_semantic_redeclaration: false,
      rv_script                : false,   // flags readvals in use (for garbage collector)
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
    self.void_     = self.heap.make_id("()");

    *self.heap.id_type(self.void_) = RawValue(Type::Void as isize);
    *self.heap.id_val(self.void_)  = self.heap.constructor(RawValue(0).into(), self.void_).into();

    self.common_stdin  = self.heap.apply(Combinator::Read.into(), RawValue(0).into());
    self.common_stdinb = self.heap.apply(Combinator::ReadBin.into(), RawValue(0).into());
    self.cook_stdin    = self.heap.apply(
      self.heap.readvals(
        RawValue(0).into(),
        RawValue(0).into()
      ),
      Token::Offside.into()
    );
    self.concat        = self.heap.make_id("concat");
    self.diagonalise   = self.heap.make_id("diagonalise");
    self.indent_fn     = self.heap.make_id("indent");
    self.listdiff_fn   = self.heap.make_id("listdiff");
    self.main_id       = self.heap.make_id("main"); // Miranda: change to magic scripts 19.11.2013
    self.message       = self.heap.make_id("sys_message");
    self.outdent_fn    = self.heap.make_id("outdent");
    self.showabstract  = self.heap.make_id("showabstract");
    self.showbool      = self.heap.make_id("showbool");
    self.showchar      = self.heap.make_id("showchar");
    self.showfunction  = self.heap.make_id("showfunction");
    self.showlist      = self.heap.make_id("showlist");
    self.shownum1      = self.heap.make_id("shownum1");
    self.showpair      = self.heap.make_id("showpair");
    self.showparen     = self.heap.make_id("showparen");
    self.showstring    = self.heap.make_id("showstring");
    self.showvoid      = self.heap.make_id("showvoid");
    self.showwhat      = self.heap.make_id("showwhat");
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

  // Used by "primlib", see below
  // fn primdef(&mut self, n    : &str                  , v: Value, t: Type) {
  //   let x: Value                = self.heap.make_id(n);
  //   self.primitive_environment  = self.heap.cons(x, self.primitive_environment);
  //   *self.heap.id_val(x)  = v.into();
  //   *self.heap.id_type(x) = RawValue(t as ValueRepresentationType);
  // }

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




}

/*



//  used by "privlib" and "stdlib", see below
fn predef(n: &str, v: ValueRepresentationType, t: ValueRepresentationType) {
  word x;
  x= make_id(n);
  addtoenv(x);
  id_val(x)= isconstructor(x)?constructor(v,x):v;
  id_type(x)=t;
}

// called when compiling <prelude>, adds some
// internally defined identifiers to the environment
fn privlib() {
  // extern word ltchar;
  predef("offside",OFFSIDE,ltchar);  /* used by `indent' in prelude */
  predef("changetype",I,wrong_t); /* wrong_t to prevent being typechecked */
  predef("first",HD,wrong_t);
  predef("rest",TL,wrong_t);
  // the following added to make prelude compilable without stdenv
  predef("code",CODE,undef_t);
  predef("concat",ap2(FOLDR,APPEND,NIL),undef_t);
  predef("decode",DECODE,undef_t);
  predef("drop",DROP,undef_t);
  predef("error",ERROR,undef_t);
  predef("filter",FILTER,undef_t);
  predef("foldr",FOLDR,undef_t);
  predef("hd",HD,undef_t);
  predef("map",MAP,undef_t);
  predef("shownum",SHOWNUM,undef_t);
  predef("take",TAKE,undef_t);
  predef("tl",TL,undef_t);
}

// called when compiling <stdenv>, adds some
// internally defined identifiers to the environment
fn stdlib() {
  predef("arctan",ARCTAN_FN,undef_t);
  predef("code",CODE,undef_t);
  predef("cos",COS_FN,undef_t);
  predef("decode",DECODE,undef_t);
  predef("drop",DROP,undef_t);
  predef("entier",ENTIER_FN,undef_t);
  predef("error",ERROR,undef_t);
  predef("exp",EXP_FN,undef_t);
  predef("filemode",FILEMODE,undef_t);
  predef("filestat",FILESTAT,undef_t);  /* added Feb 91 */
  predef("foldl",FOLDL,undef_t);
  predef("foldl1",FOLDL1,undef_t);  /* new at release 2 */
  predef("hugenum",sto_dbl(DBL_MAX),undef_t);
  predef("last",LIST_LAST,undef_t);
  predef("foldr",FOLDR,undef_t);
  predef("force",FORCE,undef_t);
  predef("getenv",GETENV,undef_t);
  predef("integer",INTEGER,undef_t);
  predef("log",LOG_FN,undef_t);
  predef("log10",LOG10_FN,undef_t); /* new at release 2 */
  predef("merge",MERGE,undef_t); /* new at release 2 */
  predef("numval",NUMVAL,undef_t);
  predef("read",STARTREAD,undef_t);
  predef("readb",STARTREADBIN,undef_t);
  predef("seq",SEQ,undef_t);
  predef("shownum",SHOWNUM,undef_t);
  predef("showhex",SHOWHEX,undef_t);
  predef("showoct",SHOWOCT,undef_t);
  predef("showfloat",SHOWFLOAT,undef_t); /* new at release 2 */
  predef("showscaled",SHOWSCALED,undef_t); /* new at release 2 */
  predef("sin",SIN_FN,undef_t);
  predef("sqrt",SQRT_FN,undef_t);
  predef("system",EXEC,undef_t); /* new at release 2 */
  predef("take",TAKE,undef_t);
  predef("tinynum",mktiny(),undef_t); /* new at release 2 */
  predef("zip2",ZIP,undef_t); /* new at release 2 */
}

*/
