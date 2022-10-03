/*!



*/
#![allow(dead_code)]

use num_traits::{FromPrimitive, ToPrimitive};

use super::COMBINATOR_BASE;
use crate::data::{Value, ValueRepresentationType};

// region Combinator Names
static COMBINATOR_NAMES: [&str; 141] = [
  "S",            // S
  "K",            // K
  "Y",            // Y
  "C",            // C
  "B",            // B
  "CB",           // CB
  "I",            // I
  "HD",           // Hd
  "TL",           // Tl
  "BODY",         // Body
  "LAST",         // Last
  "S_p",          // S_p
  "U",            // U
  "Uf",           // Uf
  "U_",           // U_
  "Ug",           // Ug
  "COND",         // Cond
  "EQ",           // Eq
  "NEQ",          // NEq
  "NEG",          // Neg
  "AND",          // And
  "OR",           // Or
  "NOT",          // Not
  "APPEND",       // Append
  "STEP",         // Step
  "STEPUNTIL",    // StepUntil
  "GENSEQ",       // GenSeq
  "MAP",          // Map
  "ZIP",          // Zip
  "TAKE",         // Take
  "DROP",         // Drop
  "FLATMAP",      // FlatMap
  "FILTER",       // Filter
  "FOLDL",        // FoldL
  "MERGE",        // Merge
  "FOLDL1",       // FoldL1
  "ListLast",     // ListLast
  "FOLDR",        // FoldR
  "MATCH",        // Match
  "MATCHINT",     // MatchInt
  "TRY",          // Try
  "SUBSCRIPT",    // Subscript
  "ATLEAST",      // AtLeast
  "P",            // P
  "B_p",          // B_p
  "C_p",          // C_p
  "S1",           // S1
  "B1",           // B1
  "C1",           // C1
  "ITERATE",      // Iterate
  "ITERATE1",     // Iterate1
  "SEQ",          // Seq
  "FORCE",        // Force
  "MINUS",        // Minus
  "PLUS",         // Plus
  "TIMES",        // Times
  "INTDIV",       // IntDiv
  "FDIV",         // FDiv
  "MOD",          // Mod
  "GR",           // Gr
  "GRE",          // Gre
  "POWER",        // Power
  "CODE",         // Code
  "DECODE",       // Decode
  "LENGTH",       // Length
  "ARCTAN_FN",    // Arctan_Fn
  "EXP_FN",       // Exp_Fn
  "ENTIER_FN",    // Entier_Fn
  "LOG_FN",       // Log_Fn
  "LOG10_FN",     // Log10_Fn
  "SIN_FN",       // Sin_Fn
  "COS_FN",       // Cos_Fn
  "SQRT_FN",      // Sqrt_Fn
  "FILEMODE",     // FileMode
  "FILESTAT",     // FileStat
  "GETENV",       // GetEnv
  "EXEC",         // Exec
  "WAIT",         // Wait
  "INTEGER",      // Integer
  "SHOWNUM",      // ShowNum
  "SHOWHEX",      // ShowHex
  "SHOWOCT",      // ShowOct
  "SHOWSCALED",   // ShowScaled
  "SHOWFLOAT",    // ShowFloat
  "NUMVAL",       // NumVal
  "STARTREAD",    // StartRead
  "STARTREADBIN", // StartReadBin
  "NB_STARTREAD", // NB_StartRead
  "READVALS",     // ReadVals
  "NB_READ",      // NB_Read
  "READ",         // Read
  "READBIN",      // ReadBin
  "GETARGS",      // GetArgs
  "Ush",          // Ush
  "Ush1",         // Ush1
  "KI",           // KI
  "G_ERROR",      // G_Error
  "G_ALT",        // G_Alt
  "G_OPT",        // G_Opt
  "G_STAR",       // G_Star
  "G_FBSTAR",     // G_FbStar
  "G_SYMB",       // G_Symb
  "G_ANY",        // G_Any
  "G_SUCHTHAT",   // G_SuchThat
  "G_END",        // G_End
  "G_STATE",      // G_State
  "G_SEQ",        // G_Seq
  "G_RULE",       // G_Rule
  "G_UNIT",       // G_Unit
  "G_ZERO",       // G_Zero
  "G_CLOSE",      // G_Close
  "G_COUNT",      // G_Count
  "LEX_RPT",      // Lex_Rpt
  "LEX_RPT1",     // Lex_Rpt1
  "LEX_TRY",      // Lex_Try
  "LEX_TRY_",     // Lex_Try_
  "LEX_TRY1",     // Lex_Try1
  "LEX_TRY1_",    // Lex_Try1_
  "DESTREV",      // DestRev
  "LEX_COUNT",    // Lex_Count
  "LEX_COUNT0",   // Lex_Count0
  "LEX_FAIL",     // Lex_Fail
  "LEX_STRING",   // Lex_String
  "LEX_CLASS",    // Lex_Class
  "LEX_CHAR",     // Lex_Char
  "LEX_DOT",      // Lex_Dot
  "LEX_SEQ",      // Lex_Seq
  "LEX_OR",       // Lex_Or
  "LEX_RCONTEXT", // Lex_RContext
  "LEX_STAR",     // Lex_Star
  "LEX_OPT",      // Lex_Opt
  "MKSTRICT",     // MkStrict
  "BADCASE",      // BadCase
  "CONFERROR",    // ConfError
  "ERROR",        // Error_
  "FAIL",         // Fail
  "False",        // False
  "True",         // True
  "NIL",          // Nil
  "NILS",         // Nils
  "UNDEF",        // Undef
];
// endregion

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Primitive)]
#[repr(usize)]
#[allow(non_camel_case_types)]
pub enum Combinator{
  S            = 336, // = 0 + COMBINATOR_BASE   // "S"
  K            = 337, // = 1 + COMBINATOR_BASE   // "K"
  Y            = 338, // = 2 + COMBINATOR_BASE   // "Y"
  C            = 339, // = 3 + COMBINATOR_BASE   // "C"
  B            = 340, // = 4 + COMBINATOR_BASE   // "B"
  CB           = 341, // = 5 + COMBINATOR_BASE   // "CB"
  I            = 342, // = 6 + COMBINATOR_BASE   // "I"
  Hd           = 343, // = 7 + COMBINATOR_BASE   // "HD"
  Tl           = 344, // = 8 + COMBINATOR_BASE   // "TL"
  Body         = 345, // = 9 + COMBINATOR_BASE   // "BODY"
  Last         = 346, // = 10 + COMBINATOR_BASE  // "LAST"
  S_p          = 347, // = 11 + COMBINATOR_BASE  // "S_p"
  U            = 348, // = 12 + COMBINATOR_BASE  // "U"
  Uf           = 349, // = 13 + COMBINATOR_BASE  // "Uf"
  U_           = 350, // = 14 + COMBINATOR_BASE  // "U_"
  Ug           = 351, // = 15 + COMBINATOR_BASE  // "Ug"
  Cond         = 352, // = 16 + COMBINATOR_BASE  // "COND"
  Eq           = 353, // = 17 + COMBINATOR_BASE  // "EQ"
  NEq          = 354, // = 18 + COMBINATOR_BASE  // "NEQ"
  Neg          = 355, // = 19 + COMBINATOR_BASE  // "NEG"
  And          = 356, // = 20 + COMBINATOR_BASE  // "AND"
  Or           = 357, // = 21 + COMBINATOR_BASE  // "OR"
  Not          = 358, // = 22 + COMBINATOR_BASE  // "NOT"
  Append       = 359, // = 23 + COMBINATOR_BASE  // "APPEND"
  Step         = 360, // = 24 + COMBINATOR_BASE  // "STEP"
  StepUntil    = 361, // = 25 + COMBINATOR_BASE  // "STEPUNTIL"
  GenSeq       = 362, // = 26 + COMBINATOR_BASE  // "GENSEQ"
  Map          = 363, // = 27 + COMBINATOR_BASE  // "MAP"
  Zip          = 364, // = 28 + COMBINATOR_BASE  // "ZIP"
  Take         = 365, // = 29 + COMBINATOR_BASE  // "TAKE"
  Drop         = 366, // = 30 + COMBINATOR_BASE  // "DROP"
  FlatMap      = 367, // = 31 + COMBINATOR_BASE  // "FLATMAP"
  Filter       = 368, // = 32 + COMBINATOR_BASE  // "FILTER"
  FoldL        = 369, // = 33 + COMBINATOR_BASE  // "FOLDL"
  Merge        = 370, // = 34 + COMBINATOR_BASE  // "MERGE"
  FoldL1       = 371, // = 35 + COMBINATOR_BASE  // "FOLDL1"
  ListLast     = 372, // = 36 + COMBINATOR_BASE  // "ListLast"
  FoldR        = 373, // = 37 + COMBINATOR_BASE  // "FOLDR"
  Match        = 374, // = 38 + COMBINATOR_BASE  // "MATCH"
  MatchInt     = 375, // = 39 + COMBINATOR_BASE  // "MATCHINT"
  Try          = 376, // = 40 + COMBINATOR_BASE  // "TRY"
  Subscript    = 377, // = 41 + COMBINATOR_BASE  // "SUBSCRIPT"
  AtLeast      = 378, // = 42 + COMBINATOR_BASE  // "ATLEAST"
  P            = 379, // = 43 + COMBINATOR_BASE  // "P"
  B_p          = 380, // = 44 + COMBINATOR_BASE  // "B_p"
  C_p          = 381, // = 45 + COMBINATOR_BASE  // "C_p"
  S1           = 382, // = 46 + COMBINATOR_BASE  // "S1"
  B1           = 383, // = 47 + COMBINATOR_BASE  // "B1"
  C1           = 384, // = 48 + COMBINATOR_BASE  // "C1"
  Iterate      = 385, // = 49 + COMBINATOR_BASE  // "ITERATE"
  Iterate1     = 386, // = 50 + COMBINATOR_BASE  // "ITERATE1"
  Seq          = 387, // = 51 + COMBINATOR_BASE  // "SEQ"
  Force        = 388, // = 52 + COMBINATOR_BASE  // "FORCE"
  Minus        = 389, // = 53 + COMBINATOR_BASE  // "MINUS"
  Plus         = 390, // = 54 + COMBINATOR_BASE  // "PLUS"
  Times        = 391, // = 55 + COMBINATOR_BASE  // "TIMES"
  IntDiv       = 392, // = 56 + COMBINATOR_BASE  // "INTDIV"
  FDiv         = 393, // = 57 + COMBINATOR_BASE  // "FDIV"
  Mod          = 394, // = 58 + COMBINATOR_BASE  // "MOD"
  Gr           = 395, // = 59 + COMBINATOR_BASE  // "GR"
  Gre          = 396, // = 60 + COMBINATOR_BASE  // "GRE"
  Power        = 397, // = 61 + COMBINATOR_BASE  // "POWER"
  Code         = 398, // = 62 + COMBINATOR_BASE  // "CODE"
  Decode       = 399, // = 63 + COMBINATOR_BASE  // "DECODE"
  Length       = 400, // = 64 + COMBINATOR_BASE  // "LENGTH"
  Arctan_Fn    = 401, // = 65 + COMBINATOR_BASE  // "ARCTAN_FN"
  Exp_Fn       = 402, // = 66 + COMBINATOR_BASE  // "EXP_FN"
  Entier_Fn    = 403, // = 67 + COMBINATOR_BASE  // "ENTIER_FN"
  Log_Fn       = 404, // = 68 + COMBINATOR_BASE  // "LOG_FN"
  Log10_Fn     = 405, // = 69 + COMBINATOR_BASE  // "LOG10_FN"
  Sin_Fn       = 406, // = 70 + COMBINATOR_BASE  // "SIN_FN"
  Cos_Fn       = 407, // = 71 + COMBINATOR_BASE  // "COS_FN"
  Sqrt_Fn      = 408, // = 72 + COMBINATOR_BASE  // "SQRT_FN"
  FileMode     = 409, // = 73 + COMBINATOR_BASE  // "FILEMODE"
  FileStat     = 410, // = 74 + COMBINATOR_BASE  // "FILESTAT"
  GetEnv       = 411, // = 75 + COMBINATOR_BASE  // "GETENV"
  Exec         = 412, // = 76 + COMBINATOR_BASE  // "EXEC"
  Wait         = 413, // = 77 + COMBINATOR_BASE  // "WAIT"
  Integer      = 414, // = 78 + COMBINATOR_BASE  // "INTEGER"
  ShowNum      = 415, // = 79 + COMBINATOR_BASE  // "SHOWNUM"
  ShowHex      = 416, // = 80 + COMBINATOR_BASE  // "SHOWHEX"
  ShowOct      = 417, // = 81 + COMBINATOR_BASE  // "SHOWOCT"
  ShowScaled   = 418, // = 82 + COMBINATOR_BASE  // "SHOWSCALED"
  ShowFloat    = 419, // = 83 + COMBINATOR_BASE  // "SHOWFLOAT"
  NumVal       = 420, // = 84 + COMBINATOR_BASE  // "NUMVAL"
  StartRead    = 421, // = 85 + COMBINATOR_BASE  // "STARTREAD"
  StartReadBin = 422, // = 86 + COMBINATOR_BASE  // "STARTREADBIN"
  NB_StartRead = 423, // = 87 + COMBINATOR_BASE  // "NB_STARTREAD"
  ReadVals     = 424, // = 88 + COMBINATOR_BASE  // "READVALS"
  NB_Read      = 425, // = 89 + COMBINATOR_BASE  // "NB_READ"
  Read         = 426, // = 90 + COMBINATOR_BASE  // "READ"
  ReadBin      = 427, // = 91 + COMBINATOR_BASE  // "READBIN"
  GetArgs      = 428, // = 92 + COMBINATOR_BASE  // "GETARGS"
  Ush          = 429, // = 93 + COMBINATOR_BASE  // "Ush"
  Ush1         = 430, // = 94 + COMBINATOR_BASE  // "Ush1"
  KI           = 431, // = 95 + COMBINATOR_BASE  // "KI"
  G_Error      = 432, // = 96 + COMBINATOR_BASE  // "G_ERROR"
  G_Alt        = 433, // = 97 + COMBINATOR_BASE  // "G_ALT"
  G_Opt        = 434, // = 98 + COMBINATOR_BASE  // "G_OPT"
  G_Star       = 435, // = 99 + COMBINATOR_BASE  // "G_STAR"
  G_FbStar     = 436, // = 100 + COMBINATOR_BASE // "G_FBSTAR"
  G_Symb       = 437, // = 101 + COMBINATOR_BASE // "G_SYMB"
  G_Any        = 438, // = 102 + COMBINATOR_BASE // "G_ANY"
  G_SuchThat   = 439, // = 103 + COMBINATOR_BASE // "G_SUCHTHAT"
  G_End        = 440, // = 104 + COMBINATOR_BASE // "G_END"
  G_State      = 441, // = 105 + COMBINATOR_BASE // "G_STATE"
  G_Seq        = 442, // = 106 + COMBINATOR_BASE // "G_SEQ"
  G_Rule       = 443, // = 107 + COMBINATOR_BASE // "G_RULE"
  G_Unit       = 444, // = 108 + COMBINATOR_BASE // "G_UNIT"
  G_Zero       = 445, // = 109 + COMBINATOR_BASE // "G_ZERO"
  G_Close      = 446, // = 110 + COMBINATOR_BASE // "G_CLOSE"
  G_Count      = 447, // = 111 + COMBINATOR_BASE // "G_COUNT"
  Lex_Rpt      = 448, // = 112 + COMBINATOR_BASE // "LEX_RPT"
  Lex_Rpt1     = 449, // = 113 + COMBINATOR_BASE // "LEX_RPT1"
  Lex_Try      = 450, // = 114 + COMBINATOR_BASE // "LEX_TRY"
  Lex_Try_     = 451, // = 115 + COMBINATOR_BASE // "LEX_TRY_"
  Lex_Try1     = 452, // = 116 + COMBINATOR_BASE // "LEX_TRY1"
  Lex_Try1_    = 453, // = 117 + COMBINATOR_BASE // "LEX_TRY1_"
  DestRev      = 454, // = 118 + COMBINATOR_BASE // "DESTREV"
  Lex_Count    = 455, // = 119 + COMBINATOR_BASE // "LEX_COUNT"
  Lex_Count0   = 456, // = 120 + COMBINATOR_BASE // "LEX_COUNT0"
  Lex_Fail     = 457, // = 121 + COMBINATOR_BASE // "LEX_FAIL"
  Lex_String   = 458, // = 122 + COMBINATOR_BASE // "LEX_STRING"
  Lex_Class    = 459, // = 123 + COMBINATOR_BASE // "LEX_CLASS"
  Lex_Char     = 460, // = 124 + COMBINATOR_BASE // "LEX_CHAR"
  Lex_Dot      = 461, // = 125 + COMBINATOR_BASE // "LEX_DOT"
  Lex_Seq      = 462, // = 126 + COMBINATOR_BASE // "LEX_SEQ"
  Lex_Or       = 463, // = 127 + COMBINATOR_BASE // "LEX_OR"
  Lex_RContext = 464, // = 128 + COMBINATOR_BASE // "LEX_RCONTEXT"
  Lex_Star     = 465, // = 129 + COMBINATOR_BASE // "LEX_STAR"
  Lex_Opt      = 466, // = 130 + COMBINATOR_BASE // "LEX_OPT"
  MkStrict     = 467, // = 131 + COMBINATOR_BASE // "MKSTRICT"
  BadCase      = 468, // = 132 + COMBINATOR_BASE // "BADCASE"
  ConfError    = 469, // = 133 + COMBINATOR_BASE // "CONFERROR"
  Error_       = 470, // = 134 + COMBINATOR_BASE // "ERROR"
  Fail         = 471, // = 135 + COMBINATOR_BASE // "FAIL"
  False        = 472, // = 136 + COMBINATOR_BASE // "False"
  True         = 473, // = 137 + COMBINATOR_BASE // "True"
  Nil          = 474, // = 138 + COMBINATOR_BASE // "NIL"
  Nils         = 475, // = 139 + COMBINATOR_BASE // "NILS"
  Undef        = 476, // = 140 + COMBINATOR_BASE // "UNDEF"
  AtomLimit    = 477, // = 141 + COMBINATOR_BASE // AtomLimit, not a combinator.
}

impl Combinator {
  pub fn name(&self) -> &str{
    COMBINATOR_NAMES[(*self as ValueRepresentationType - COMBINATOR_BASE) as usize]
  }

  pub fn into_value(self) -> Value {
    Value::Combinator(self)
  }
}
