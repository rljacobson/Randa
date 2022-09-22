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
pub struct Driver {
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
  rv_script                : bool,          //  flags readvals in use (for garbage collector)
  ids_used                 : Vec<String>,   // Identifiers
  file_queue               : Vec<File>,


  in_file: Box<dyn BufRead>,
}

// pub fn reset(activity_reset: ActivityReset)-

impl Driver {

  fn reset_state(&mut self){
    if self.command_mode {
      while c != '\n' && c != EOF {
        c = getc(s_in);
      }
    }  /* no echo */
    while !self.file_queue.is_empty(){
      let file = self.file_queue.pop();

    }
      fclose((FILE *)hd[hd[fileq]]);
    fileq = tl[fileq];

    insertdepth = -1;
    s_in = stdin();
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

}
