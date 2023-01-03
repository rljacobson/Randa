
/// The bytecode version. Incremented for every non-backwards compatible release. The last release of Miranda v.2.066
/// has `XVERSION` 83. Because Randa's bytecode is incompatible, we increment to 84. If Miranda has another
/// bytecode-backwards-incompatible release, we are in trouble.
pub const XVERSION: i32 = 84;
/// Corresponds to the latest Miranda version that this version of Randa is language-level compatible with.
pub const VERSION: i32 = 2066;

// Constants generated as compile time. They are used in `Options.rs` and reported via `/v`, `/V`, `-v`,
// and `-V`. They are:
//    * `COMPILER_HOST_TARGET`
//    * `BUILD_DATE`
include!(concat!(env!("OUT_DIR"), "/constants.rs"));


// region Constants for heap size and GC

/// Default size of heap.
pub(crate) static DEFAULT_SPACE: usize = 2500000;
/// SPACE_LIMIT controls the size of the heap (i.e. the number of heap cells available) -
/// the minimum survivable number given the need to compile the prelude, etc., is probably
/// about 6000.
///
/// This value can be set manually with the `--heap` command line argument.
pub(crate) static SPACE_LIMIT: usize = DEFAULT_SPACE;
/// False ceiling in heap to improve paging behaviour during compilation
pub(crate) static INIT_SPACE   : usize = 1250000;
pub(crate) static BIG_TOP      : usize = SPACE_LIMIT; //+ (ATOM_LIMIT as usize);   // 2500000 + 477 = 2500477

// Todo: Is this the right home for this?
/// Unused: Space allocated for the symbol table. Instead we grow dynamically as needed.
pub(crate) static DEFAULT_DICT_SPACE   : usize = 100000;
/// Note: This value is unused. Instead we grow dynamically as needed.
/// The size in bytes of the dictionary (symbol table), used by the compiler to store identifiers etc. (default 100k).
///
/// This value can be set manually with the `--dic` command line argument and can be
/// interrogated (but not changed) from within the miranda session by the command `/dic'.
pub(crate) static DICT_SPACE: usize = DEFAULT_DICT_SPACE;
// endregion


// region Paths
pub(crate) static LOG_FILE_PATH: &'static str = "log/mira.errors";
// endregion
