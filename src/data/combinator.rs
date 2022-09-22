/*!



*/
#![allow(dead_code)]

use num_traits::{FromPrimitive, ToPrimitive};

use super::COMBINATOR_BASE;
use crate::data::ValueRepresentationType;

// region Combinator Names
static COMBINATOR_NAMES: [&str; 141] = [
  "S",
  "K",
  "Y",
  "C",
  "B",
  "CB",
  "I",
  "HD",
  "TL",
  "BODY",
  "LAST",
  "S_p",
  "U",
  "Uf",
  "U_",
  "Ug",
  "COND",
  "EQ",
  "NEQ",
  "NEG",
  "AND",
  "OR",
  "NOT",
  "APPEND",
  "STEP",
  "STEPUNTIL",
  "GENSEQ",
  "MAP",
  "ZIP",
  "TAKE",
  "DROP",
  "FLATMAP",
  "FILTER",
  "FOLDL",
  "MERGE",
  "FOLDL1",
  "ListLast",
  "FOLDR",
  "MATCH",
  "MATCHINT",
  "TRY",
  "SUBSCRIPT",
  "ATLEAST",
  "P",
  "B_p",
  "C_p",
  "S1",
  "B1",
  "C1",
  "ITERATE",
  "ITERATE1",
  "SEQ",
  "FORCE",
  "MINUS",
  "PLUS",
  "TIMES",
  "INTDIV",
  "FDIV",
  "MOD",
  "GR",
  "GRE",
  "POWER",
  "CODE",
  "DECODE",
  "LENGTH",
  "ARCTAN_FN",
  "EXP_FN",
  "ENTIER_FN",
  "LOG_FN",
  "LOG10_FN",
  "SIN_FN",
  "COS_FN",
  "SQRT_FN",
  "FILEMODE",
  "FILESTAT",
  "GETENV",
  "EXEC",
  "WAIT",
  "INTEGER",
  "SHOWNUM",
  "SHOWHEX",
  "SHOWOCT",
  "SHOWSCALED",
  "SHOWFLOAT",
  "NUMVAL",
  "STARTREAD",
  "STARTREADBIN",
  "NB_STARTREAD",
  "READVALS",
  "NB_READ",
  "READ",
  "READBIN",
  "GETARGS",
  "Ush",
  "Ush1",
  "KI",
  "G_ERROR",
  "G_ALT",
  "G_OPT",
  "G_STAR",
  "G_FBSTAR",
  "G_SYMB",
  "G_ANY",
  "G_SUCHTHAT",
  "G_END",
  "G_STATE",
  "G_SEQ",
  "G_RULE",
  "G_UNIT",
  "G_ZERO",
  "G_CLOSE",
  "G_COUNT",
  "LEX_RPT",
  "LEX_RPT1",
  "LEX_TRY",
  "LEX_TRY_",
  "LEX_TRY1",
  "LEX_TRY1_",
  "DESTREV",
  "LEX_COUNT",
  "LEX_COUNT0",
  "LEX_FAIL",
  "LEX_STRING",
  "LEX_CLASS",
  "LEX_CHAR",
  "LEX_DOT",
  "LEX_SEQ",
  "LEX_OR",
  "LEX_RCONTEXT",
  "LEX_STAR",
  "LEX_OPT",
  "MKSTRICT",
  "BADCASE",
  "CONFERROR",
  "ERROR",
  "FAIL",
  "False",
  "True",
  "NIL",
  "NILS",
  "UNDEF",
];
// endregion

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Primitive)]
#[repr(usize)]
#[allow(non_camel_case_types)]
pub enum Combinator{
  S             = 306, //  "S"
  K             = 307, //  "K"
  Y             = 308, //  "Y"
  C             = 309, //  "C"
  B             = 310, //  "B"
  CB            = 311, //  "CB"
  I             = 312, //  "I"
  Hd            = 313, //  "HD"
  Tl            = 314, //  "TL"
  Body          = 315, //  "BODY"
  Last          = 316, //  "LAST"
  S_p           = 317, //  "S_p"
  U             = 318, //  "U"
  Uf            = 319, //  "Uf"
  U_            = 320, //  "U_"
  Ug            = 321, //  "Ug"
  Cond          = 322, //  "COND"
  Eq            = 323, //  "EQ"
  NEq           = 324, //  "NEQ"
  Neg           = 325, //  "NEG"
  And           = 326, //  "AND"
  Or            = 327, //  "OR"
  Not           = 328, //  "NOT"
  Append        = 329, //  "APPEND"
  Step          = 330, //  "STEP"
  StepUntil     = 331, //  "STEPUNTIL"
  GenSeq        = 332, //  "GENSEQ"
  Map           = 333, //  "MAP"
  Zip           = 334, //  "ZIP"
  Take          = 335, //  "TAKE"
  Drop          = 336, //  "DROP"
  FlatMap       = 337, //  "FLATMAP"
  Filter        = 338, //  "FILTER"
  FoldL         = 339, //  "FOLDL"
  Merge         = 340, //  "MERGE"
  FoldL1        = 341, //  "FOLDL1"
  ListLast      = 342, //  "ListLast"
  FoldR         = 343, //  "FOLDR"
  Match         = 344, //  "MATCH"
  MatchInt      = 345, //  "MATCHINT"
  Try           = 346, //  "TRY"
  Subscript     = 347, //  "SUBSCRIPT"
  AtLeast       = 348, //  "ATLEAST"
  P             = 349, //  "P"
  B_p           = 350, //  "B_p"
  C_p           = 351, //  "C_p"
  S1            = 352, //  "S1"
  B1            = 353, //  "B1"
  C1            = 354, //  "C1"
  Iterate       = 355, //  "ITERATE"
  Iterate1      = 356, //  "ITERATE1"
  Seq           = 357, //  "SEQ"
  Force         = 358, //  "FORCE"
  Minus         = 359, //  "MINUS"
  Plus          = 360, //  "PLUS"
  Times         = 361, //  "TIMES"
  IntDiv        = 362, //  "INTDIV"
  FDiv          = 363, //  "FDIV"
  Mod           = 364, //  "MOD"
  Gr            = 365, //  "GR"
  Gre           = 366, //  "GRE"
  Power         = 367, //  "POWER"
  Code          = 368, //  "CODE"
  Decode        = 369, //  "DECODE"
  Length        = 370, //  "LENGTH"
  Arctan_Fn     = 371, //  "ARCTAN_FN"
  Exp_Fn        = 372, //  "EXP_FN"
  Entier_Fn     = 373, //  "ENTIER_FN"
  Log_Fn        = 374, //  "LOG_FN"
  Log10_Fn      = 375, //  "LOG10_FN"
  Sin_Fn        = 376, //  "SIN_FN"
  Cos_Fn        = 377, //  "COS_FN"
  Sqrt_Fn       = 378, //  "SQRT_FN"
  FileMode      = 379, //  "FILEMODE"
  FileStat      = 380, //  "FILESTAT"
  GetEnv        = 381, //  "GETENV"
  Exec          = 382, //  "EXEC"
  Wait          = 383, //  "WAIT"
  Integer       = 384, //  "INTEGER"
  ShowNum       = 385, //  "SHOWNUM"
  ShowHex       = 386, //  "SHOWHEX"
  ShowOct       = 387, //  "SHOWOCT"
  ShowScaled    = 388, //  "SHOWSCALED"
  ShowFloat     = 389, //  "SHOWFLOAT"
  NumVal        = 390, //  "NUMVAL"
  StartRead     = 391, //  "STARTREAD"
  StartReadBin  = 392, //  "STARTREADBIN"
  NB_StartRead  = 393, //  "NB_STARTREAD"
  ReadVals      = 394, //  "READVALS"
  NB_Read       = 395, //  "NB_READ"
  Read          = 396, //  "READ"
  ReadBin       = 397, //  "READBIN"
  GetArgs       = 398, //  "GETARGS"
  Ush           = 399, //  "Ush"
  Ush1          = 400, //  "Ush1"
  KI            = 401, //  "KI"
  G_Error       = 402, //  "G_ERROR"
  G_Alt         = 403, //  "G_ALT"
  G_Opt         = 404, //  "G_OPT"
  G_Star        = 405, //  "G_STAR"
  G_FbStar      = 406, //  "G_FBSTAR"
  G_Symb        = 407, //  "G_SYMB"
  G_Any         = 408, //  "G_ANY"
  G_SuchThat    = 409, //  "G_SUCHTHAT"
  G_End         = 410, //  "G_END"
  G_State       = 411, //  "G_STATE"
  G_Seq         = 412, //  "G_SEQ"
  G_Rule        = 413, //  "G_RULE"
  G_Unit        = 414, //  "G_UNIT"
  G_Zero        = 415, //  "G_ZERO"
  G_Close       = 416, //  "G_CLOSE"
  G_Count       = 417, //  "G_COUNT"
  Lex_Rpt       = 418, //  "LEX_RPT"
  Lex_Rpt1      = 419, //  "LEX_RPT1"
  Lex_Try       = 420, //  "LEX_TRY"
  Lex_Try_      = 421, //  "LEX_TRY_"
  Lex_Try1      = 422, //  "LEX_TRY1"
  Lex_Try1_     = 423, //  "LEX_TRY1_"
  DestRev       = 424, //  "DESTREV"
  Lex_Count     = 425, //  "LEX_COUNT"
  Lex_Count0    = 426, //  "LEX_COUNT0"
  Lex_Fail      = 427, //  "LEX_FAIL"
  Lex_String    = 428, //  "LEX_STRING"
  Lex_Class     = 429, //  "LEX_CLASS"
  Lex_Char      = 430, //  "LEX_CHAR"
  Lex_Dot       = 431, //  "LEX_DOT"
  Lex_Seq       = 432, //  "LEX_SEQ"
  Lex_Or        = 433, //  "LEX_OR"
  Lex_RContext  = 434, //  "LEX_RCONTEXT"
  Lex_Star      = 435, //  "LEX_STAR"
  Lex_Opt       = 436, //  "LEX_OPT"
  MkStrict      = 437, //  "MKSTRICT"
  BadCase       = 438, //  "BADCASE"
  ConfError     = 439, //  "CONFERROR"
  Error_         = 440, //  "ERROR"
  Fail          = 441, //  "FAIL"
  False         = 442, //  "False"
  True          = 443, //  "True"
  Nil           = 444, //  "NIL"
  Nils          = 445, //  "NILS"
  Undef         = 446, //  "UNDEF"
  AtomLimit     = 447, //  AtomLimit    = 141 + COMBINATOR_BASE,
}

impl Combinator {
  pub fn name(&self) -> &str{
    COMBINATOR_NAMES[(*self as ValueRepresentationType - COMBINATOR_BASE) as usize]
  }
}


#[cfg(test)]
mod tests {

  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }

}
