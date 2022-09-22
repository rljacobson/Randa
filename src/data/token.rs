/*
  Miranda values of tokens.
*/
#![allow(dead_code)]

use std::fmt::{Display, Formatter};

use saucepan::Span;
// use num_traits::{FromPrimitive, ToPrimitive};

use super::{
  TOKEN_BASE,
  ValueRepresentationType
};

// region Token Strings
/// Equivalent to `char *yysterm[]` in y.tab.c. This list must be kept in sync with the `Token` enum.
static TOKEN_STRINGS: [&str; 50] = [
  "VALUE",
  "EVAL",
  "where",
  "if",
  "&>",
  "<-",
  "::",
  "::=",
  "TYPEVAR",
  "NAME",
  "CONSTRUCTOR-NAME",
  "CONST",
  "$$",
  "OFFSIDE",
  "OFFSIDE =",
  "abstype",
  "with",
  "//",
  "==",
  "%free",
  "%include",
  "%export",
  "type",
  "otherwise",
  "show",
  "PATHNAME",
  "%bnf",
  "%lex",
  "%%",
  "error",
  "end",
  "empty",
  "readvals",
  "NAME",
  "`char-class`",
  "`char-class`",
  "%%begin",
  "->",
  "++",
  "--",
  "..",
  "\\/",
  ">=",
  "~=",
  "<=",
  "mod",
  "div",
  "$NAME",
  "$CONSTRUCTOR",
  ";",
  // "EOF",
];
// endregion

// Equivalent to `#define`s in y.tab.h
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug, Primitive)]
#[repr(usize)]
pub enum Token{
  // Note: 256 = TOKEN_BASE
  Value           = 256, // 0 + TOKEN_BASE, //  "VALUE"
  Eval            = 257, // 1 + TOKEN_BASE, //  "EVAL"
  Where           = 258, // 2 + TOKEN_BASE, //  "where"
  If              = 259, // 3 + TOKEN_BASE, //  "if"
  To              = 260, // 4 + TOKEN_BASE, //  "&>"
  LeftArrow       = 261, // 5 + TOKEN_BASE, //  "<-"
  ColonColon      = 262, // 6 + TOKEN_BASE, //  "::"
  Colon2Equal     = 263, // 7 + TOKEN_BASE, //  "::="
  TypeVar         = 264, // 8 + TOKEN_BASE, //  "TYPEVAR"
  Name            = 265, // 9 + TOKEN_BASE, //  "NAME"
  ConstructorName = 266, // 10 + TOKEN_BASE, //  "CONSTRUCTOR-NAME"
  Constant        = 267, // 11 + TOKEN_BASE, //  "CONST"
  DollarDollar    = 268, // 12 + TOKEN_BASE, //  "$$"
  Offside         = 269, // 13 + TOKEN_BASE, //  "OFFSIDE"
  ElseEqual       = 270, // 14 + TOKEN_BASE, //  "OFFSIDE ="
  AbsoluteType    = 271, // 15 + TOKEN_BASE, //  "abstype"
  With            = 272, // 16 + TOKEN_BASE, //  "with"
  Diagonal        = 273, // 17 + TOKEN_BASE, //  "//"
  EqualEqual      = 274, // 18 + TOKEN_BASE, //  "=="
  Free            = 275, // 19 + TOKEN_BASE, //  "%free"
  Include         = 276, // 20 + TOKEN_BASE, //  "%include"
  Export          = 277, // 21 + TOKEN_BASE, //  "%export"
  Type            = 278, // 22 + TOKEN_BASE, //  "type"
  Otherwise       = 279, // 23 + TOKEN_BASE, //  "otherwise"
  ShowSymbol      = 280, // 24 + TOKEN_BASE, //  "show"
  PathName        = 281, // 25 + TOKEN_BASE, //  "PATHNAME"
  Bnf             = 282, // 26 + TOKEN_BASE, //  "%bnf"
  Lex             = 283, // 27 + TOKEN_BASE, //  "%lex"
  EndIR           = 284, // 28 + TOKEN_BASE, //  "%%"
  ErrorSymbol     = 285, // 29 + TOKEN_BASE, //  "error"
  EndSymbol       = 286, // 30 + TOKEN_BASE, //  "end"
  EmptySymbol     = 287, // 31 + TOKEN_BASE, //  "empty"
  ReadValsSymbol  = 288, // 32 + TOKEN_BASE, //  "readvals"
  LexDef          = 289, // 33 + TOKEN_BASE, //  "NAME"
  CharClass       = 290, // 34 + TOKEN_BASE, //  "`char-class`"
  AntiCharClass   = 291, // 35 + TOKEN_BASE, //  "`char-class`"
  Begin           = 292, // 36 + TOKEN_BASE, //  "%%begin"
  RightArrow      = 293, // 37 + TOKEN_BASE, //  "->"
  PlusPlus        = 294, // 38 + TOKEN_BASE, //  "++"
  MinusMinus      = 295, // 39 + TOKEN_BASE, //  "--"
  DotDot          = 296, // 40 + TOKEN_BASE, //  ".."
  Vel             = 297, // 41 + TOKEN_BASE, //  "\\/"
  GreaterEqual    = 298, // 42 + TOKEN_BASE, //  ">="
  NotEqual        = 299, // 43 + TOKEN_BASE, //  "~="
  LessEqual       = 300, // 44 + TOKEN_BASE, //  "<="
  Remainder       = 301, // 45 + TOKEN_BASE, //  "mod"
  Divide          = 302, // 46 + TOKEN_BASE, //  "div"
  InfixName       = 303, // 47 + TOKEN_BASE, //  "$NAME"
  InfixCName      = 304, // 48 + TOKEN_BASE, //  "$CONSTRUCTOR"
  Semicolon       = 305, // 49 + TOKEN_BASE,
  // EOF             = 50 + TOKEN_BASE,
}

// impl From<Token> for ValueRepresentationType {
//
// }


impl Display for Token {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "token<\"{}\">", self.string_representation())
  }
}

impl Token {
  pub fn string_representation(&self) -> &str {
    TOKEN_STRINGS[(*self as ValueRepresentationType - TOKEN_BASE) as usize]
  }
}
