/*
 *   Copyright (c) 2022
 *   All rights reserved.
 */
/*

Setup and orchestration of built-ins, etc. Contains utility functions that perform the work.



*/
use std::fs::File;
use std::io::{stdin, BufRead};




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

/// Holds the state of the .. what? Parser? Lexer? Both?
pub struct VM {
  activity                 : ActivityReset,
  command_mode             : bool,
  abstype_declarations     : Vec<String>,
  sui_generis_constructors : Vec<String>,   // user defined sui-generis constructors
  new_type_names           : Vec<String>,   // newly declared type  names in current code unit
  algebraic_show_functions : Vec<String>,   // show functions of algebraic  types in scope
  special_show_forms       : Vec<String>,   // all occurrences of special forms (show) encountered during
                                            // type check
  in_export_list           : bool,
  in_semantic_redeclaration: bool,
  rv_script                : bool,          // flags readvals in use (for garbage collector)
  ids_used                 : Vec<String>,   // Identifiers
  file_queue               : Vec<File>,
  in_file: Box<dyn BufRead>,
  last_expression: Value, // A reference to the last expression evaluated.
  last_mame: Option<&str>,
  heap: Heap,
}

// pub fn reset(activity_reset: ActivityReset)-

impl Driver {

  /// Reset all variables used by the compiler.
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

pub fn mira_setup(mut heap: Heap) {
  // extern word common_stdin,common_stdinb,cook_stdin;
  // setupheap(); // Allocate heap
  // tsetup(); // Setup common types
  // reset_pns(); // Setup private namespace
  // bigsetup(); // Setup big number support
  // common_stdin= ap(READ,0);      // STDIN
  // common_stdinb= ap(READBIN,0);

  // ??
  // cook_stdin=ap(readvals(0,0),OFFSIDE);


  /// Common types referred to in Parser.y.
  nill= cons(CONST,NIL);
  Void=make_id("()");
  id_type(Void)=void_t;
  id_val(Void)=constructor(0,Void);
  message=make_id("sys_message");
  main_id=make_id("main");       /* change to magic scripts 19.11.2013 */
  concat=make_id("concat");
  diagonalise=make_id("diagonalise");
  standardout=constructor(0,"Stdout");
  indent_fn=make_id("indent");
  outdent_fn=make_id("outdent");
  listdiff_fn=make_id("listdiff");
  shownum1=make_id("shownum1");
  showbool=make_id("showbool");
  showchar=make_id("showchar");
  showlist=make_id("showlist");
  showstring=make_id("showstring");
  showparen=make_id("showparen");
  showpair=make_id("showpair");
  showvoid=make_id("showvoid");
  showfunction=make_id("showfunction");
  showabstract=make_id("showabstract");
  showwhat=make_id("showwhat");
  primlib();
}

// used by "primlib", see below
fn primdef(n: &str, v: ValueRepresentationType, t: ValueRepresentationType) {
  word x;
  x= make_id(n);
  primenv=cons(x,primenv);
  id_val(x)= v;
  id_type(x)=t;
}

//  used by "privlib" and "stdlib", see below
fn predef(n: &str, v: ValueRepresentationType, t: ValueRepresentationType) {
  word x;
  x= make_id(n);
  addtoenv(x);
  id_val(x)= isconstructor(x)?constructor(v,x):v;
  id_type(x)=t;
}

// called by "mira_setup", this routine enters
// the primitive identifiers into the primitive environment
// sets up predefined ids, not referred to by rules.y
fn primlib() {
  primdef("num",make_typ(0,0,synonym_t,num_t),type_t);
  primdef("char",make_typ(0,0,synonym_t,char_t),type_t);
  primdef("bool",make_typ(0,0,synonym_t,bool_t),type_t);
  primdef("True",1,bool_t); /* accessible only to 'finger' */
  primdef("False",0,bool_t); /* likewise - FIX LATER */
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
