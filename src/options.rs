/*!

Parse command line arguments and environment variables and set internal flags and values.

The values for `space_limit`, `dict_space`, `version`, and `editor` are persisted in the ".mirarc" file, which is a
space delimited value file of the form HDVE: heap space, dict size, version, editor.


*/

// Remaining Miranda functionality missing from Randa relevant to this file:
//    Todo: Persist `space_limit`, `dict_space`, `version`, and `editor` in the `.mirarc` file.
//    Todo: Figure out what `mirahdr`/`lmirahdr` is.



use std::{
  process::exit,
  io::{BufRead, BufReader},
  fs::OpenOptions,
  fmt::{Display, Formatter},
  fs::File
};

use atty::Stream;
use gag::Redirect;
// use dirs::data_local_dir;

use argparse::{ArgumentParser, Store, StoreTrue, StoreFalse, List as StoreList, Print};

use crate::constants::{COMPILER_HOST_TARGET, BUILD_DATE, VERSION, XVERSION, DICT_SPACE, SPACE_LIMIT, LOG_FILE_PATH};

// use crate::data::heap::{DICT_SPACE, SPACE_LIMIT};



pub type List = Vec<String>;

pub struct Options {

  pub(crate) version             : i32,          // Compatible Miranda version X 1000 (see above), `2066`
  pub(crate) xversion            : i32,          // Bytecode version (see above), `84`
  pub(crate) build_date          : &'static str, // "Dec 11 2022"
  pub(crate) compiler_host_target: &'static str, // "x86_64 Darwin 22.1.0"

  pub(crate) no_stdenv       : bool,   // Do not load standard environment
  pub(crate) at_count        : bool,   // Print stats after evaluation
  pub(crate) listing         : bool,   // Print scripts to the screen during compilation
  pub(crate) strict_if       : bool,   // Deny deprecated elision of "if" after guard comma
  pub(crate) at_gc           : bool,   // Print GC stats
  pub(crate) at_object       : bool,   // Makes `?identifier(s)` shows combinator code
  pub(crate) miralib         : String, // Location of miralib directory
  pub(crate) dict_space      : usize,  // Size of dictionary in bytes (ignored)
  pub(crate) space_limit     : usize,  // Size of heap in cells
  pub(crate) editor          : String, // Text editor program
  pub(crate) verbose         : bool,   // Interpreter provides prompt and other text
  pub(crate) magic           : bool,   // Script will start with UNIX magic string
  pub(crate) manual_only     : bool,   // Only show the manual
  pub(crate) exec            : bool,   // Run script as standalone program
  pub(crate) stderr_redirect : Option<Redirect<File>>, // Log errors to file (e.g. with `-exec2`)
  pub(crate) make_file_list  : List,   // Files to check if bytecode is up-to-date
  pub(crate) make_exports    : List,   // Print identifiers of given source files
  pub(crate) make_sources    : List,   // Print files on which the given source files directly or indirectly depend
  pub(crate) use_utf8        : bool,   // Assume a UTF-8 locale
  pub(crate) recheck_mira    : bool,   // Monitor changes to relevant source file
  pub(crate) shell           : String, // Shell to use for `!` escapes.
  pub(crate) mira_prompt     : String, // String for session prompt
  pub(crate) viewer          : String, // Program used to display pages of the online manual
  pub(crate) return_to_menu  : bool,   // Return to menu without printing prompt
  pub(crate) menu_viewer     : String, // Program used to display manual contents pages
  pub(crate) script          : String, // File of Miranda definitions
}

impl Default for Options {
  fn default() -> Self {
    Options {

      version       : VERSION,    // Compatible Miranda version X 1000 (see above), 2066.
      xversion      : XVERSION,   // Bytecode version (see above), "84"
      build_date    : BUILD_DATE, // "Dec 11 2022"
      compiler_host_target: COMPILER_HOST_TARGET, // "x86_64 Darwin 22.1.0"

      no_stdenv     : false,
      at_count      : false,
      listing       : false,
      strict_if     : true,
      at_gc         : false,
      at_object     : false,
      miralib       : std::env::var("MIRALIB").unwrap_or(String::new()),
      dict_space    : DICT_SPACE,
      space_limit   : SPACE_LIMIT,
      editor        : std::env::var("EDITOR").unwrap_or("vi".to_string()),
      verbose       : atty::is(Stream::Stdout), // Set if stdout is a terminal (not, e.g., piped to a file).
      magic         : false, // A combination of the other parameters.
      manual_only   : false,
      exec          : false,
      stderr_redirect: None, // Same as `exec` but redirects output to a log file.
      make_file_list: vec![],
      make_exports  : vec![],
      make_sources  : vec![],
      use_utf8      : detect_utf8(),
      recheck_mira  : false,
      shell         : "/bin/sh".to_string(),  // ToDo: Make a static for this somewhere.
      mira_prompt   : "Miranda ".to_string(), // ToDo: Make a static for this somewhere.
      viewer        : "more -d ".to_string(), // ToDo: Make a static for this somewhere.
      return_to_menu: false,                  // ToDo: Make a static for this somewhere.
      menu_viewer   : "more -d ".to_string(), // ToDo: Make a static for this somewhere.
      script        : "script.m".to_string(),
    }
  }
}

impl Display for Options {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    let make_file_list = self.make_file_list.join(", ");
    let make_exports   = self.make_exports.join(", ");
    let make_sources   = self.make_sources.join(", ");

    let no_stdenv      = &self.no_stdenv;
    let at_count       = &self.at_count;
    let listing        = &self.listing;
    let strict_if      = &self.strict_if;
    let at_gc          = &self.at_gc;
    let at_object      = &self.at_object;
    let miralib        = &self.miralib;
    let dict_space     = &self.dict_space;
    let space_limit    = &self.space_limit;
    let editor         = &self.editor;
    let verbose        = &self.verbose;
    let magic          = &self.magic;
    let manual_only    = &self.manual_only;
    let exec           = &self.exec;
    let log_errors     = &self.stderr_redirect.is_some();
    let use_utf8       = &self.use_utf8;
    let recheck_mira   = &self.recheck_mira;
    let shell          = &self.shell;
    let mira_prompt    = &self.mira_prompt;
    let viewer         = &self.viewer;
    let return_to_menu = &self.return_to_menu;
    let menu_viewer    = &self.menu_viewer;
    let script         = &self.script;

    write!(f, "Options {{
  no_stdenv     : {no_stdenv}
  at_count      : {at_count}
  listing       : {listing}
  strict_if     : {strict_if}
  at_gc         : {at_gc}
  at_object     : {at_object}
  miralib       : {miralib}
  dict_space    : {dict_space}
  space_limit   : {space_limit}
  editor        : {editor}
  verbose       : {verbose}
  magic         : {magic}
  manual_only   : {manual_only}
  exec          : {exec}
  log_errors    : {log_errors}
  make_file_list: {make_file_list}
  make_exports  : {make_exports}
  make_sources  : {make_sources}
  use_utf8      : {use_utf8}
  recheck_mira  : {recheck_mira}
  shell         : {shell}
  mira_prompt   : {mira_prompt}
  viewer        : {viewer}
  return_to_menu: {return_to_menu}
  menu_viewer   : {menu_viewer}
  script        : {script}
}}")

  }
}


/// Returns true iff `m` is directory with version containing our version number. If the directory contains a
/// different version, we record it in `found_versions`.
fn check_version(m: &str, version: i32, found_versions: &mut Vec<(String, i32)>) -> bool {
  let path = format!("{}/version", m);
  let f = match File::open(&path) {
    Ok(file) => file,
    Err(_)   => return false,
  };

  let mut reader = BufReader::new(f);
  let mut line: String = String::new();

  if let Err(_) = reader.read_line(&mut line) {
    return false;
  }

  let v1 = match line.parse() {
    Ok(v)  => v,
    Err(_) => return false,
  };

  #[allow(unused_parens)]
  let r = (v1 == version);
  if !r {
    // We found an rc, but it's the wrong version.
    found_versions.push((m.to_string(), v1));
  }

  return r;
}

fn find_miralib() -> String {
  let mut miralib = String::new();
  let mut found_versions = Vec::new();
  let version = VERSION;

  // Note the search order
  if check_version("/usr/lib/miralib", version, &mut found_versions) {
    miralib = "/usr/lib/miralib".to_string();
  } else if check_version("/usr/local/lib/miralib", version, &mut found_versions) {
    miralib = "/usr/local/lib/miralib".to_string();
  } else if check_version("miralib", version, &mut found_versions) {
    miralib = "miralib".to_string();
  } else {
    eprintln!("fatal error: miralib version {} not found", make_version_string(version));
    if !found_versions.is_empty() {
      println!("found");
    }
    for (v, loc) in found_versions {
      eprintln!("\tversion {} at: {}", v, loc);
    }
    // exit(1);
  }

  miralib
}

fn detect_utf8() -> bool {
  if let Ok(locale) = std::env::var("LC_CTYPE") {
    locale.ends_with("UTF-8") || locale.ends_with("utf-8")
  } else if let Ok(locale) = std::env::var("LANG") {
    locale.ends_with("UTF-8") || locale.ends_with("utf-8")
  } else {
    false
  }
}

/// Converts an integer version of the form 1234 into a version string of the form "1.234". If the given integer
/// version is out of range, returns the string "???".
pub fn make_version_string(version_number: i32) -> String {
  // Sanity check.
  if version_number < 0 || version_number > 999999 {
    return "???".to_string();
  }
  format!("{:.3}", version_number as f32 / 1000.0)
}

fn log_stderr() -> Result<Redirect<File>, ()> {
  let log = OpenOptions::new()
      .truncate(true)
      .read(true)
      .create(true)
      .write(true)
      .open(LOG_FILE_PATH).map_err(|_| ())?;
  let stderr_redirect: Redirect<File> = Redirect::stderr(log).map_err(|_| ())?;

  Ok(stderr_redirect)
}

fn fixup_editor(editor: &mut String) {
  match editor.as_str() {

    | "vi"
    | "pico"
    | "nano"
    | "joe"
    | "jpico"
    | "vim" => { *editor = format!("{} +!", editor); }

    | "gvim"
    | "emacs" => { *editor = format!("{} +! % &", editor); }

    _ => {
      if let Some(idx) = editor.rfind('/') {
        // Take the part of the string after the last '/'
        *editor = editor.split_off(idx + 1);
      }

      // Todo: Why is "vi" distinguished here? Maybe `fix_editor(..)` should be called on `editor` at this point?
      if editor == "vi" {
        editor.push_str(" +!");
      }
    }

  } // end match

  // It is possible that a `!` was not originally or inserted in the editor name. Miranda checks to make sure the
  // editor string contains an unescaped `!` character. We don't.

  // Todo: Why should an `&` affect the rechecking setting?
  // if editor.contains('&') {
  //   rechecking = 2;
  // }

}

/**

Options are described in detail at https://www.cs.kent.ac.uk/people/staff/dat/miranda/manual/31/7.html


# MIRA

November 2019

# NAME

mira - the Miranda(tm) functional programming system
SYNOPSIS

mira [options] [file]

# DESCRIPTION

Miranda is a functional programming system with lazy evaluation, polymorphic strong typing and function definition by pattern matching.

The mira program takes a single argument which is the name of a file of definitions (called a "script"). If no argument is given a default name "script.m" is assumed. The names of files containing miranda scripts must end in ".m" and mira will add this if missing. The specified file need not yet exist - in this case you will be starting a Miranda session with an empty current script.

The basic action of the Miranda system is to evaluate expressions in the environment established by the script, so in its simplest mode of use it behaves like a desk calculator. Expressions are typed one per line, terminated by ENTER. The interpreter also accepts certain commands (mostly beginning with a `/' character) - these include /help or /h which prints a summary of the available commands, and /man or /m which gives access to the online manual of the Miranda system (menu driven and self explanatory). This documents all aspects of the Miranda language and system and should be consulted for further details. It is also possible to access the Miranda system manual directly from a UNIX shell by the command mira -man.

# OPTIONS

*/
pub fn setup_argument_parser() -> Options {
  let mut exec2              = false;         // Same as `exec` but redirects output to a log file.
  let mut return_to_menu     = String::new(); // A bool paramter set according to a string
  let mut print_version      = false;         // Show version info and exit with success.
  let mut print_full_version = false;         // Show full version info and exit with success.

  let mut ops = Options::default();

  // The scope of the argument parser (and its barrows).
  {
    let mut ap = ArgumentParser::new();

    // region Description
    ap.set_description(r#"SYNOPSIS

mira [options] [file]

DESCRIPTION

Miranda is a functional programming system with lazy evaluation, polymorphic strong typing and function definition by pattern matching.

The mira program takes a single argument which is the name of a file of definitions (called a "script"). If no argument is given a default name "script.m" is assumed. The names of files containing miranda scripts must end in ".m" and mira will add this if missing. The specified file need not yet exist - in this case you will be starting a Miranda session with an empty current script.

The basic action of the Miranda system is to evaluate expressions in the environment established by the script, so in its simplest mode of use it behaves like a desk calculator. Expressions are typed one per line, terminated by ENTER. The interpreter also accepts certain commands (mostly beginning with a `/' character) - these include /help or /h which prints a summary of the available commands, and /man or /m which gives access to the online manual of the Miranda system (menu driven and self explanatory). This documents all aspects of the Miranda language and system and should be consulted for further details. It is also possible to access the Miranda system manual directly from a UNIX shell by the command mira -man.
"#);
    // endregion
    // region Standard Options
    ap.refer(&mut ops.miralib)
        .add_option(&["--lib"], Store, "Specifies location of the miralib directory. For default see FILES. Can also \
        be done by setting environment variable MIRALIB. The location of the miralib directory can be interrogated (but not changed) from within the miranda session by the command `/miralib'.")
        .metavar("pathname");

    ap.refer(&mut ops.at_gc)
        .add_option(&["--gc"], StoreTrue, "Switches on a flag causing the garbage collector to print information each\
         time a garbage collection takes place. This flag can also be switched on and off from within the miranda session by the commands `/gc', `/nogc'.");
    ap.refer(&mut ops.at_count)
        .add_option(&["--count"], StoreTrue, "Switches on a flag causing statistics to be printed after each \
        expression evaluation. This flag can also be switched on and off from within the miranda session by the commands `/count', `/nocount'.");
    ap.refer(&mut ops.listing)
        .add_option(&["--list"], StoreTrue, "Switches on (off) a flag causing Miranda scripts to be listed to the \
        screen during compilation. This flag can also be switched on and off from within the miranda session by the commands `/list', `/nolist'.")
        .add_option(&["--nolist"], StoreFalse, "Switches on (off) a flag causing Miranda scripts to be listed to the \
        screen during compilation. This flag can also be switched on and off from within the miranda session by the commands `/list', `/nolist'.");
    ap.refer(&mut ops.strict_if)
        .add_option(&["--nostrictif"], StoreFalse, "Enables the compiler to accept old Miranda scripts with no `if' \
        after the guard comma.");
    ap.refer(&mut ops.verbose)
        .add_option(&["--hush"], StoreFalse, "The miranda system decides whether or not to give prompts and other \
        feedback by testing its standard input with `isatty'. If the standard input does not appear to be a terminal it assumes that prompts would be inappropriate, otherwise it gives them. In either case this behaviour can be overriden by an explicit flag (\"-hush\" for silence, \"-nohush\" for prompts etc). This switch is also available from within a miranda session by the commands `/hush', `/nohush'.")
        .add_option(&["--nohush"], StoreTrue, "The miranda system decides whether or not to give prompts and other \
        feedback by testing its standard input with `isatty'. If the standard input does not appear to be a terminal it assumes that prompts would be inappropriate, otherwise it gives them. In either case this behaviour can be overriden by an explicit flag (\"-hush\" for silence, \"-nohush\" for prompts etc). This switch is also available from within a miranda session by the commands `/hush', `/nohush'.");
    // endregion
    // region Memory Management and locale
    ap.refer(&mut ops.dict_space)
        .add_option(&["--dic"], Store, "Causes the dictionary, used by the compiler to store identifiers etc., to be \
        SIZE bytes (default 100k). This can be interrogated (but not changed) from within the miranda session by the command `/dic'.")
        .metavar("SIZE");
    ap.refer(&mut ops.space_limit)
        .add_option(&["--heap"], Store, "Causes the heap to be SIZE cells (default 2500k). This can changed within \
        the miranda session by the command `/heap SIZE'. A cell is 9 bytes (2 words of 32 bits, and a tag field).")
        .metavar("SIZE");
    ap.refer(&mut ops.editor)
        .add_option(&["--editor"], Store, "Causes the resident editor (usual default `vi') to be prog instead. This \
        can also be done from within the miranda session by the command /editor prog. Any occurrences of ! and % in prog will be replaced by the line number and the name of the file to be edited, respectively. For more detailed discussion see online manual subsection 31/5.")
        .metavar("program");
    ap.refer(&mut ops.use_utf8)
        .add_option(&["--UTF-8"], StoreTrue,
                    "Assume the current locale is (is not) UTF-8 overriding environment vars (version 2.044 and later).")
        .add_option(&["--noUTF-8"], StoreFalse,
                    "Assume the current locale is (is not) UTF-8 overriding environment vars (version 2.044 and later).");
    // endregion
    //region Standard Environment and compiler debuging.
    ap.refer(&mut ops.no_stdenv)
        .add_option(&["--stdenv"], StoreTrue, "Run mira without loading the standard environment. Any script needing \
        functions from <stdenv> will then have to explicitly %include <stdenv>, or define the required functions itself. Not recommended as normal practise and may have unexpected consequences.");
    ap.refer(&mut ops.at_object)
        .add_option(&["--object"], StoreTrue, "Used for debugging the compiler. Modifies the behaviour of ?identifier\
        (s) to show the associated combinator code, which may or may not be comprehensible as there is no documentation other than the source code.");
    // endregion

    // region Version and Manual
    // # SPECIAL CALLS
    // "The following special calls to mira do not start a Miranda session but accomplish another purpose."

    ap.refer(&mut ops.manual_only)
        .add_option(&["--man"], StoreTrue, "Enter Miranda online manual from the UNIX shell. From within a Miranda \
        session this is done by the command `/man' or `/m'.");
    ap.refer(&mut print_version)
      .add_option(
        &["--version"],
        StoreTrue,
        "Prints version information. This information can be obtained within a Miranda session by the command `/version' or `/v'."
    );
    ap.refer(&mut print_full_version)
      .add_option(
        &["-V"],
        StoreTrue,
        "More detailed version information. Can be obtained within a Miranda session by the command `/V'."
    );
    // Help is supplied automagically by the `argparse` library.
    // ap.refer(&mut print_help)
    //   .add_option(
    //   &["-h", "--help"],
    //   StoreTrue,
    //   "Print this help message."
    // );

    // endregion
    // region Script execution and source files
    // "The remaining special calls are discussed in more detail in the online manual - we list them here for completeness."

    ap.refer(&mut ops.exec)
        .add_option(&["--exec", "--exp"], StoreTrue, "Special call permitting the use of miranda script as a \
        stand-alone program. See online manual subsection 31/4 for details.");
    ap.refer(&mut exec2)
        .add_option(&["--exec2", "--log"], StoreTrue, "As `--exec` except that it redirects stderr to a file \
         log/mira.errors, if log directory exists in the current directory and mira has write permission to it.");

    // "These three relate to separate compilation and Miranda's built in `make' facility. See online manual section 27 (the library mechanism):-"

    ap.refer(&mut ops.make_file_list)
        .add_option(&["--make"], StoreList,
                    "Checks that all the miranda source files listed have up-to-date .x \
                          (intermediate code) files, triggering compilation processes if necessary.")
        .metavar("files");

    ap.refer(&mut ops.make_exports)
        .add_option(&["--exports"], StoreList, "Sends to stdout a list of the identifiers exported from the given \
        miranda source files, together with their types (may force compilation if needed).")
        .metavar("files");
    ap.refer(&mut ops.make_sources)
        .add_option(&["--sources"], StoreList, "Send to stdout a list of all the Miranda source files on which the \
        given source files directly or indirectly depend (via %include or %insert statements), excluding the standard environment <stdenv>.")
        .metavar("files");
    // endregion
    // region Recheck sourcefiles, shell, and prompt
    ap.refer(&mut ops.recheck_mira)
        // .add_option(&[], StoreTrue, "If this is set to any non-empty string the Miranda system checks to see if any relevant source file has been updated, and performs any necessary recompilation, before each interaction with the user. This is the appropriate behaviour if an editor window is being kept open during the Miranda session. By default the check is performed only after `/e' commands and `!' escapes. This can also be controlled from within a Miranda session by the commands `/recheck', `/norecheck'.")
        .envvar("RECHECKMIRA");

    ap.refer(&mut ops.shell)
        // .add_option(&[], Store,
        //             "Determines what shell is used in `!' escapes. This will normally contain the name of the user's \
        //             login shell. If no SHELL is present in the environment, /bin/sh is assumed.")
        .envvar("SHELL");

    ap.refer(&mut ops.mira_prompt)
        // .add_option(&[], Store,
        //             "Sets a string to be used as session prompt instead of the default prompt \"Miranda \" (version 2.044 and later).")
        .envvar("MIRAPROMPT");

    // ap.refer(&mut nostrictif)
    //   .add_option(&[], Store,
    //               "If this is set to any non-empty string Miranda accepts old scripts with no `if' after the guard comma. Equivalent to calling mira with option -nostrictif. Deprecated - you should put the `if's in.")
    //   ;
    // endregion

    // region Menu driver program
    // "The behaviour of the menudriver program that displays pages of the online manual can be modified using three environment variables:-"

    ap.refer(&mut ops.viewer)
        // .add_option(&[], Store,
        //             "The program used for displaying pages of the online manual. If this variable is not set the default is normally `more -d' or (roughly equivalent) `less -EX'. If you set VIEWER to something, you may also need to set an environment variable RETURNTOMENU."
        // )
        .envvar("VIEWER");
    ap.refer(&mut return_to_menu)
       // .add_option(&[], Store, "Prevents another prompt being given after displaying each section, causing \
       // instead an immediate return to contents page. Appropriate if VIEWER is a program that pauses for input at end of \
       // file (e.g. `less'). It should be `NO' if VIEWER is a program that quits silently at end of file (e.g. `more -d', `less -EX').")
       .envvar("RETURNTOMENU");
    ap.refer(&mut ops.menu_viewer)
        // .add_option(&[], Store,
        //             "Can be used to specify the program used to display manual contents pages (default is usually `cat' or `more')."
        // )
        .envvar("MENUVIEWER");

    // "To find the current settings of the online manual enter ??? to the "next selection" prompt of the manual system."
    // endregion

    // region Obsolete
    ap.add_option(&["--log", "--exp"],
                  Print(
                    "mira: obsolete flag \"-exp\"/\"-log\"\nuse \"-exec\" or \"-exec2\", see manual\n".to_string()
                  ),
                  "Obsolete");
    // endregion

    ap.refer(&mut ops.script)
        .add_argument("script", Store, "The mira program takes a single argument which is the name of a file of \
        definitions (called a \"script\"). If no argument is given a default name \"script.m\" is assumed. The names of files containing miranda scripts must end in \".m\" and mira will add this if missing. The specified file need not yet exist - in this case you will be starting a Miranda session with an empty current script.");

    ap.parse_args_or_exit();

    // Help is provided automagically by the `argparse` library.
    // if print_help {
    //   ap.print_help();
    //   exit_immediately = true;
    // }
  }


  // region Print and exit if a relevant argument was given.
  if print_version {
    println!("Version {}.", make_version_string(VERSION));
  } else if print_full_version {
    println!("{} last revised {}\n{}\n{}", make_version_string(VERSION), BUILD_DATE, COMPILER_HOST_TARGET, XVERSION);
  }

  if print_version || print_full_version {
    exit(0);
  }
  // endregion

  // Redirect stderr before an error is possible. The redirect is active during the life of the value in
  // `stderr_redirect`, so it is stored in `ops`, which lasts the lifetime of the program.
  // `exec2` iff `exec` and `log_errors`
  if exec2 {
    ops.exec = true;
    ops.stderr_redirect =
      match log_stderr() {
        Ok(stderr_redirect) => Some(stderr_redirect),
        Err(_) => {
          eprintln!("Could not redirect stderr to {}. Check that the directory exists with the correct \
        permissions.", LOG_FILE_PATH);
          None
        }
      };
  }

  // region Now do any validation, default values, or whatever else is needed to form the result.

  if ops.miralib.is_empty() {
    // Exits on failure.
    ops.miralib = find_miralib();
  }

  if return_to_menu.eq_ignore_ascii_case("no")
      || return_to_menu.eq_ignore_ascii_case("false")
      || return_to_menu.eq_ignore_ascii_case("0")
  {
    ops.return_to_menu = false;
  } else if return_to_menu.eq_ignore_ascii_case("yes")
      || return_to_menu.eq_ignore_ascii_case("true")
      || return_to_menu.eq_ignore_ascii_case("1")
      // || std::env::var(RETURNTOMENU).is_ok() // Defined but empty?
  {
    ops.return_to_menu = true;
  }

  if ops.exec {
    ops.magic   = true;
    ops.verbose = false;
  }

  // Not verbose if we are making.
  if !ops.make_sources.is_empty() || !ops.make_exports.is_empty() || !ops.make_file_list.is_empty() {
    ops.verbose = false;
  }

  fixup_editor(&mut ops.editor);

  // endregion

  // Some options are acted upon immediately.

  ops

}

