/*
Combinator tags and name-table mapping.
*/
#![allow(dead_code)]

use super::{RawValue, COMBINATOR_BASE};
use crate::data::Value;
use enum_primitive_derive::Primitive;
use num_traits::ToPrimitive;

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
#[repr(isize)]
#[allow(non_camel_case_types)]
pub enum Combinator {
    S = 338,             // = 0 + COMBINATOR_BASE   // "S"
    K = 339,             // = 1 + COMBINATOR_BASE   // "K"
    Y = 340,             // = 2 + COMBINATOR_BASE   // "Y"
    C = 341,             // = 3 + COMBINATOR_BASE   // "C"
    B = 342,             // = 4 + COMBINATOR_BASE   // "B"
    CB = 343,            // = 5 + COMBINATOR_BASE   // "CB"
    I = 344,             // = 6 + COMBINATOR_BASE   // "I"
    Hd = 345,            // = 7 + COMBINATOR_BASE   // "HD"
    Tl = 346,            // = 8 + COMBINATOR_BASE   // "TL"
    Body = 347,          // = 9 + COMBINATOR_BASE   // "BODY"
    Last = 348,          // = 10 + COMBINATOR_BASE  // "LAST"
    S_p = 349,           // = 11 + COMBINATOR_BASE  // "S_p"
    U = 350,             // = 12 + COMBINATOR_BASE  // "U"
    Uf = 351,            // = 13 + COMBINATOR_BASE  // "Uf"
    U_ = 352,            // = 14 + COMBINATOR_BASE  // "U_"
    Ug = 353,            // = 15 + COMBINATOR_BASE  // "Ug"
    Cond = 354,          // = 16 + COMBINATOR_BASE  // "COND"
    Eq = 355,            // = 17 + COMBINATOR_BASE  // "EQ"
    NEq = 356,           // = 18 + COMBINATOR_BASE  // "NEQ"
    Neg = 357,           // = 19 + COMBINATOR_BASE  // "NEG"
    And = 358,           // = 20 + COMBINATOR_BASE  // "AND"
    Or = 359,            // = 21 + COMBINATOR_BASE  // "OR"
    Not = 360,           // = 22 + COMBINATOR_BASE  // "NOT"
    Append = 361,        // = 23 + COMBINATOR_BASE  // "APPEND"
    Step = 362,          // = 24 + COMBINATOR_BASE  // "STEP"
    StepUntil = 363,     // = 25 + COMBINATOR_BASE  // "STEPUNTIL"
    GenSeq = 364,        // = 26 + COMBINATOR_BASE  // "GENSEQ"
    Map = 365,           // = 27 + COMBINATOR_BASE  // "MAP"
    Zip = 366,           // = 28 + COMBINATOR_BASE  // "ZIP"
    Take = 367,          // = 29 + COMBINATOR_BASE  // "TAKE"
    Drop = 368,          // = 30 + COMBINATOR_BASE  // "DROP"
    FlatMap = 369,       // = 31 + COMBINATOR_BASE  // "FLATMAP"
    Filter = 370,        // = 32 + COMBINATOR_BASE  // "FILTER"
    FoldL = 371,         // = 33 + COMBINATOR_BASE  // "FOLDL"
    Merge = 372,         // = 34 + COMBINATOR_BASE  // "MERGE"
    FoldL1 = 373,        // = 35 + COMBINATOR_BASE  // "FOLDL1"
    ListLast = 374,      // = 36 + COMBINATOR_BASE  // "ListLast"
    FoldR = 375,         // = 37 + COMBINATOR_BASE  // "FOLDR"
    Match = 376,         // = 38 + COMBINATOR_BASE  // "MATCH"
    MatchInt = 377,      // = 39 + COMBINATOR_BASE  // "MATCHINT"
    Try = 378,           // = 40 + COMBINATOR_BASE  // "TRY"
    Subscript = 379,     // = 41 + COMBINATOR_BASE  // "SUBSCRIPT"
    AtLeast = 380,       // = 42 + COMBINATOR_BASE  // "ATLEAST"
    P = 381,             // = 43 + COMBINATOR_BASE  // "P"
    B_p = 382,           // = 44 + COMBINATOR_BASE  // "B_p"
    C_p = 383,           // = 45 + COMBINATOR_BASE  // "C_p"
    S1 = 384,            // = 46 + COMBINATOR_BASE  // "S1"
    B1 = 385,            // = 47 + COMBINATOR_BASE  // "B1"
    C1 = 386,            // = 48 + COMBINATOR_BASE  // "C1"
    Iterate = 387,       // = 49 + COMBINATOR_BASE  // "ITERATE"
    Iterate1 = 388,      // = 50 + COMBINATOR_BASE  // "ITERATE1"
    Seq = 389,           // = 51 + COMBINATOR_BASE  // "SEQ"
    Force = 390,         // = 52 + COMBINATOR_BASE  // "FORCE"
    Minus = 391,         // = 53 + COMBINATOR_BASE  // "MINUS"
    Plus = 392,          // = 54 + COMBINATOR_BASE  // "PLUS"
    Times = 393,         // = 55 + COMBINATOR_BASE  // "TIMES"
    DivideInteger = 394, // = 56 + COMBINATOR_BASE  // "INTDIV"
    DivideFloat = 395,   // = 57 + COMBINATOR_BASE  // "FDIV"
    Remainder = 396,     // = 58 + COMBINATOR_BASE  // "MOD"
    Gr = 397,            // = 59 + COMBINATOR_BASE  // "GR"
    Gre = 398,           // = 60 + COMBINATOR_BASE  // "GRE"
    Power = 399,         // = 61 + COMBINATOR_BASE  // "POWER"
    Code = 400,          // = 62 + COMBINATOR_BASE  // "CODE"
    Decode = 401,        // = 63 + COMBINATOR_BASE  // "DECODE"
    Length = 402,        // = 64 + COMBINATOR_BASE  // "LENGTH"
    Arctan_Fn = 403,     // = 65 + COMBINATOR_BASE  // "ARCTAN_FN"
    Exp_Fn = 404,        // = 66 + COMBINATOR_BASE  // "EXP_FN"
    Entier_Fn = 405,     // = 67 + COMBINATOR_BASE  // "ENTIER_FN"
    Log_Fn = 406,        // = 68 + COMBINATOR_BASE  // "LOG_FN"
    Log10_Fn = 407,      // = 69 + COMBINATOR_BASE  // "LOG10_FN"
    Sin_Fn = 408,        // = 70 + COMBINATOR_BASE  // "SIN_FN"
    Cos_Fn = 409,        // = 71 + COMBINATOR_BASE  // "COS_FN"
    Sqrt_Fn = 410,       // = 72 + COMBINATOR_BASE  // "SQRT_FN"
    FileMode = 411,      // = 73 + COMBINATOR_BASE  // "FILEMODE"
    FileStat = 412,      // = 74 + COMBINATOR_BASE  // "FILESTAT"
    GetEnv = 413,        // = 75 + COMBINATOR_BASE  // "GETENV"
    Exec = 414,          // = 76 + COMBINATOR_BASE  // "EXEC"
    Wait = 415,          // = 77 + COMBINATOR_BASE  // "WAIT"
    Integer = 416,       // = 78 + COMBINATOR_BASE  // "INTEGER"
    ShowNum = 417,       // = 79 + COMBINATOR_BASE  // "SHOWNUM"
    ShowHex = 418,       // = 80 + COMBINATOR_BASE  // "SHOWHEX"
    ShowOct = 419,       // = 81 + COMBINATOR_BASE  // "SHOWOCT"
    ShowScaled = 420,    // = 82 + COMBINATOR_BASE  // "SHOWSCALED"
    ShowFloat = 421,     // = 83 + COMBINATOR_BASE  // "SHOWFLOAT"
    NumVal = 422,        // = 84 + COMBINATOR_BASE  // "NUMVAL"
    StartRead = 423,     // = 85 + COMBINATOR_BASE  // "STARTREAD"
    StartReadBin = 424,  // = 86 + COMBINATOR_BASE  // "STARTREADBIN"
    NB_StartRead = 425,  // = 87 + COMBINATOR_BASE  // "NB_STARTREAD"
    ReadVals = 426,      // = 88 + COMBINATOR_BASE  // "READVALS"
    NB_Read = 427,       // = 89 + COMBINATOR_BASE  // "NB_READ"
    Read = 428,          // = 90 + COMBINATOR_BASE  // "READ"
    ReadBin = 429,       // = 91 + COMBINATOR_BASE  // "READBIN"
    GetArgs = 430,       // = 92 + COMBINATOR_BASE  // "GETARGS"
    Ush = 431,           // = 93 + COMBINATOR_BASE  // "Ush"
    Ush1 = 432,          // = 94 + COMBINATOR_BASE  // "Ush1"
    KI = 433,            // = 95 + COMBINATOR_BASE  // "KI"
    G_Error = 434,       // = 96 + COMBINATOR_BASE  // "G_ERROR"
    G_Alt = 435,         // = 97 + COMBINATOR_BASE  // "G_ALT"
    G_Opt = 436,         // = 98 + COMBINATOR_BASE  // "G_OPT"
    G_Star = 437,        // = 99 + COMBINATOR_BASE  // "G_STAR"
    G_FbStar = 438,      // = 100 + COMBINATOR_BASE // "G_FBSTAR"
    G_Symb = 439,        // = 101 + COMBINATOR_BASE // "G_SYMB"
    G_Any = 440,         // = 102 + COMBINATOR_BASE // "G_ANY"
    G_SuchThat = 441,    // = 103 + COMBINATOR_BASE // "G_SUCHTHAT"
    G_End = 442,         // = 104 + COMBINATOR_BASE // "G_END"
    G_State = 443,       // = 105 + COMBINATOR_BASE // "G_STATE"
    G_Seq = 444,         // = 106 + COMBINATOR_BASE // "G_SEQ"
    G_Rule = 445,        // = 107 + COMBINATOR_BASE // "G_RULE"
    G_Unit = 446,        // = 108 + COMBINATOR_BASE // "G_UNIT"
    G_Zero = 447,        // = 109 + COMBINATOR_BASE // "G_ZERO"
    G_Close = 448,       // = 110 + COMBINATOR_BASE // "G_CLOSE"
    G_Count = 449,       // = 111 + COMBINATOR_BASE // "G_COUNT"
    Lex_Rpt = 450,       // = 112 + COMBINATOR_BASE // "LEX_RPT"
    Lex_Rpt1 = 451,      // = 113 + COMBINATOR_BASE // "LEX_RPT1"
    Lex_Try = 452,       // = 114 + COMBINATOR_BASE // "LEX_TRY"
    Lex_Try_ = 453,      // = 115 + COMBINATOR_BASE // "LEX_TRY_"
    Lex_Try1 = 454,      // = 116 + COMBINATOR_BASE // "LEX_TRY1"
    Lex_Try1_ = 455,     // = 117 + COMBINATOR_BASE // "LEX_TRY1_"
    DestRev = 456,       // = 118 + COMBINATOR_BASE // "DESTREV"
    Lex_Count = 457,     // = 119 + COMBINATOR_BASE // "LEX_COUNT"
    Lex_Count0 = 458,    // = 120 + COMBINATOR_BASE // "LEX_COUNT0"
    Lex_Fail = 459,      // = 121 + COMBINATOR_BASE // "LEX_FAIL"
    Lex_String = 460,    // = 122 + COMBINATOR_BASE // "LEX_STRING"
    Lex_Class = 461,     // = 123 + COMBINATOR_BASE // "LEX_CLASS"
    Lex_Char = 462,      // = 124 + COMBINATOR_BASE // "LEX_CHAR"
    Lex_Dot = 463,       // = 125 + COMBINATOR_BASE // "LEX_DOT"
    Lex_Seq = 464,       // = 126 + COMBINATOR_BASE // "LEX_SEQ"
    Lex_Or = 465,        // = 127 + COMBINATOR_BASE // "LEX_OR"
    Lex_RContext = 466,  // = 128 + COMBINATOR_BASE // "LEX_RCONTEXT"
    Lex_Star = 467,      // = 129 + COMBINATOR_BASE // "LEX_STAR"
    Lex_Opt = 468,       // = 130 + COMBINATOR_BASE // "LEX_OPT"
    MkStrict = 469,      // = 131 + COMBINATOR_BASE // "MKSTRICT"
    BadCase = 470,       // = 132 + COMBINATOR_BASE // "BADCASE"
    ConfError = 471,     // = 133 + COMBINATOR_BASE // "CONFERROR"
    Error_ = 472,        // = 134 + COMBINATOR_BASE // "ERROR"
    Fail = 473,          // = 135 + COMBINATOR_BASE // "FAIL"
    False = 474,         // = 136 + COMBINATOR_BASE // "False"
    True = 475,          // = 137 + COMBINATOR_BASE // "True"
    Nil = 476,           // = 138 + COMBINATOR_BASE // "NIL"
    Nils = 477,          // = 139 + COMBINATOR_BASE // "NILS"
    Undef = 478,         // = 140 + COMBINATOR_BASE // "UNDEF"
}

impl Combinator {
    pub(crate) const NIL: Value = Value::Combinator(Combinator::Nil);

    pub fn name(&self) -> &str {
        COMBINATOR_NAMES[(*self as RawValue - COMBINATOR_BASE) as usize]
    }

    pub fn into_value(self) -> Value {
        Value::Combinator(self)
    }
}

impl From<Combinator> for RawValue {
    fn from(value: Combinator) -> Self {
        value.to_isize().unwrap()
    }
}
