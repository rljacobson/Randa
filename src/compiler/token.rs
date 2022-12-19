/*
  Miranda values of tokens.
*/
#![allow(dead_code)]

use std::fmt::{Display, Formatter};

use logos::Logos;
// use crate::logos::Logos;

use saucepan::Span;
// use num_traits::{FromPrimitive, ToPrimitive, Primitive};

use crate::{
  data::{
    TOKEN_BASE,
    Value,
    ValueRepresentationType
  }
};

// region Token Strings
/// Equivalent to `char *yysterm[]` in y.tab.c. This list must be kept in sync with the `Token` enum.
static TOKEN_STRINGS: [&str; 80] = [
  "VALUE",            // 256 = 0 + TOKEN_BASE
  "EVAL",             // 257 = 1 + TOKEN_BASE
  "\"where\"",        // 258 = 2 + TOKEN_BASE
  "\"if\"",           // 259 = 3 + TOKEN_BASE
  "\"&>\"",           // 260 = 4 + TOKEN_BASE
  "\"<-\"",           // 261 = 5 + TOKEN_BASE
  "\"::\"",           // 262 = 6 + TOKEN_BASE
  "\"::=\"",          // 263 = 7 + TOKEN_BASE
  "IDENTIFIER",       // 264 = 8 + TOKEN_BASE
  "TYPEVAR",          // 265 = 9 + TOKEN_BASE
  "NAME",             // 266 = 10 + TOKEN_BASE
  "CONST",            // 267 = 11 + TOKEN_BASE
  "CONSTRUCTOR-NAME", // 268 = 12 + TOKEN_BASE
  "\"$$\"",           // 269 = 13 + TOKEN_BASE
  "OFFSIDE",          // 270 = 14 + TOKEN_BASE
  "OFFSIDE =",        // 271 = 15 + TOKEN_BASE
  "\"abstype\"",      // 272 = 16 + TOKEN_BASE
  "\"with\"",         // 273 = 17 + TOKEN_BASE
  "\"//\"",           // 274 = 18 + TOKEN_BASE
  "\"==\"",           // 275 = 19 + TOKEN_BASE
  "\"%free\"",        // 276 = 20 + TOKEN_BASE
  "\"%include\"",     // 277 = 21 + TOKEN_BASE
  "\"%export\"",      // 278 = 22 + TOKEN_BASE
  "\"type\"",         // 279 = 23 + TOKEN_BASE
  "\"otherwise\"",    // 280 = 24 + TOKEN_BASE
  "\"show\"",         // 281 = 25 + TOKEN_BASE
  "PATHNAME",         // 282 = 26 + TOKEN_BASE
  "\"%bnf\"",         // 283 = 27 + TOKEN_BASE
  "\"%lex\"",         // 284 = 28 + TOKEN_BASE
  "\"%%\"",           // 285 = 29 + TOKEN_BASE
  "\"error\"",        // 286 = 30 + TOKEN_BASE
  "\"end\"",          // 287 = 31 + TOKEN_BASE
  "\"empty\"",        // 288 = 32 + TOKEN_BASE
  "\"readvals\"",     // 289 = 33 + TOKEN_BASE
  "LEX_DEF",          // 290 = 34 + TOKEN_BASE
  "\"`char-class`\"", // 291 = 35 + TOKEN_BASE
  "\"`anti-char-class\"", // 292 = 36 + TOKEN_BASE
  "\"%%begin\"",      // 293 = 37 + TOKEN_BASE
  "\"->\"",           // 294 = 38 + TOKEN_BASE
  "\"++\"",           // 295 = 39 + TOKEN_BASE
  "\"--\"",           // 296 = 40 + TOKEN_BASE
  "\"..\"",           // 297 = 41 + TOKEN_BASE
  "\"\\/\"",           // 298 = 42 + TOKEN_BASE
  "\">=\"",           // 299 = 43 + TOKEN_BASE
  "\"~=\"",           // 300 = 44 + TOKEN_BASE
  "\"<=\"",           // 301 = 45 + TOKEN_BASE
  "\"mod\"",          // 302 = 46 + TOKEN_BASE
  "\"div\"",          // 303 = 47 + TOKEN_BASE
  "$NAME",            // 304 = 48 + TOKEN_BASE
  "$CONSTRUCTOR",     // 305 = 49 + TOKEN_BASE
  "\":\"",            // 306 = 50 + TOKEN_BASE
  "\"&\"",            // 307 = 51 + TOKEN_BASE
  "\">\"",            // 308 = 52 + TOKEN_BASE
  "\"=\"",            // 309 = 53 + TOKEN_BASE
  "\"<\"",            // 310 = 54 + TOKEN_BASE
  "\"+\"",            // 311 = 55 + TOKEN_BASE
  "\"-\"",            // 312 = 56 + TOKEN_BASE
  "\"*\"",            // 313 = 57 + TOKEN_BASE
  "\"/\"",            // 314 = 58 + TOKEN_BASE
  "\"^\"",            // 315 = 59 + TOKEN_BASE
  "\".\"",            // 316 = 60 + TOKEN_BASE
  "\"!\"",            // 317 = 61 + TOKEN_BASE
  "\"~\"",            // 318 = 62 + TOKEN_BASE
  "\"#\"",            // 319 = 63 + TOKEN_BASE
  "\",\"",            // 320 = 64 + TOKEN_BASE
  "\"|\"",            // 321 = 65 + TOKEN_BASE
  "\"?\"",            // 322 = 66 + TOKEN_BASE
  "\";\"",            // 323 = 67 + TOKEN_BASE
  "Newline",      // 324 = 68 + TOKEN_BASE
  "\"{\"",            // 325 = 69 + TOKEN_BASE
  "\"}\"",            // 326 = 70 + TOKEN_BASE
  "\"(\"",            // 327 = 71 + TOKEN_BASE
  "\")\"",            // 328 = 72 + TOKEN_BASE
  "\"[\"",            // 329 = 73 + TOKEN_BASE
  "\"]\"",            // 330 = 74 + TOKEN_BASE
  "STRING",           // 331 = 75 + TOKEN_BASE
  "INTEGER",          // 332 = 76 + TOKEN_BASE
  "FLOAT",            // 333 = 77 + TOKEN_BASE
  "EOF",              // 334 = 78 + TOKEN_BASE
  "ERROR",            // 335 = 79 + TOKEN_BASE
];
// endregion

// Equivalent to `#define`s in y.tab.h
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug, Logos, Primitive)]
#[repr(usize)]
pub enum Token{
  Value                                 = 256, // = 0 + TOKEN_BASE,  // VALUE
  Eval                                  = 257, // = 1 + TOKEN_BASE,  // EVAL
  #[token("where")]
  Where                                 = 258, // = 2 + TOKEN_BASE,  // "where"
  #[token("if")]
  If                                    = 259, // = 3 + TOKEN_BASE,  // "if"
  #[token("&>")]
  To                                    = 260, // = 4 + TOKEN_BASE,  // "&>"
  #[token("<-")]
  LeftArrow                             = 261, // = 5 + TOKEN_BASE,  // "<-"
  #[token("::")]
  ColonColon                            = 262, // = 6 + TOKEN_BASE,  // "::"
  #[token("::=")]
  Colon2Equal                           = 263, // = 7 + TOKEN_BASE,  // "::="
  #[regex(r"[a-z][a-zA-Z0-9_']*")]
  Identifier                            = 264, // = 8 + TOKEN_BASE,  // IDENTIFIER
  TypeVar                               = 265, // = 9 + TOKEN_BASE,  // TYPEVAR
  Name                                  = 266, // = 10 + TOKEN_BASE, // NAME
  Constant                              = 267, // = 11 + TOKEN_BASE, // CONST
  #[regex(r"[A-Z][a-zA-Z0-9_']*")]
  ConstructorName                       = 268, // = 12 + TOKEN_BASE, // CONSTRUCTOR-NAME
  #[token("$$")]
  DollarDollar                          = 269, // = 13 + TOKEN_BASE, // "$$"
  Offside                               = 270, // = 14 + TOKEN_BASE, // OFFSIDE
  ElseEqual                             = 271, // = 15 + TOKEN_BASE, // OFFSIDE =
  #[token("abstype")]
  AbsoluteType                          = 272, // = 16 + TOKEN_BASE, // "abstype"
  #[token("with")]
  With                                  = 273, // = 17 + TOKEN_BASE, // "with"
  #[token("//")]
  Diagonal                              = 274, // = 18 + TOKEN_BASE, // "//"
  #[token("==")]
  EqualEqual                            = 275, // = 19 + TOKEN_BASE, // "=="
  #[token("%free")]
  Free                                  = 276, // = 20 + TOKEN_BASE, // "%free"
  #[token("%include")]
  Include                               = 277, // = 21 + TOKEN_BASE, // "%include"
  #[token("%export")]
  Export                                = 278, // = 22 + TOKEN_BASE, // "%export"
  #[token("type")]
  Type                                  = 279, // = 23 + TOKEN_BASE, // "type"
  #[token("otherwise")]
  Otherwise                             = 280, // = 24 + TOKEN_BASE, // "otherwise"
  #[token("show")]
  Show                                  = 281, // = 25 + TOKEN_BASE, // "show"
  PathName                              = 282, // = 26 + TOKEN_BASE, // PATHNAME
  #[token("%bnf")]
  BNF                                   = 283, // = 27 + TOKEN_BASE, // "%bnf"
  #[token("%lex")]
  Lex                                   = 284, // = 28 + TOKEN_BASE, // "%lex"
  #[token("%%")]
  EndIR                                 = 285, // = 29 + TOKEN_BASE, // "%%"
  #[token("error")]
  ErrorSymbol                           = 286, // = 30 + TOKEN_BASE, // "error"
  #[token("end")]
  EndSymbol                             = 287, // = 31 + TOKEN_BASE, // "end"
  #[token("empty")]
  EmptySymbol                           = 288, // = 32 + TOKEN_BASE, // "empty"
  #[token("readvals")]
  ReadVals                              = 289, // = 33 + TOKEN_BASE, // "readvals"
  LexDef                                = 290, // = 34 + TOKEN_BASE, // LEX_DEF
  #[token("`char-class`")]
  CharClass                             = 291, // = 35 + TOKEN_BASE, // "`char-class`"
  AntiCharClass                         = 292, // = 36 + TOKEN_BASE, // `anti-char-class
  #[token("%%begin")]
  Begin                                 = 293, // = 37 + TOKEN_BASE, // "%%begin"
  #[token("->")]
  RightArrow                            = 294, // = 38 + TOKEN_BASE, // "->"
  #[token("++")]
  PlusPlus                              = 295, // = 39 + TOKEN_BASE, // "++"
  #[token("--")]
  MinusMinus                            = 296, // = 40 + TOKEN_BASE, // "--"
  #[token("..")]
  DotDot                                = 297, // = 41 + TOKEN_BASE, // ".."
  #[token("\\/")]
  Vel                                   = 298, // = 42 + TOKEN_BASE, // "\/"
  #[token(">=")]
  GreaterEqual                          = 299, // = 43 + TOKEN_BASE, // ">="
  #[token("~=")]
  NotEqual                              = 300, // = 44 + TOKEN_BASE, // "~="
  #[token("<=")]
  LessEqual                             = 301, // = 45 + TOKEN_BASE, // "<="
  #[token("mod")]
  Remainder                             = 302, // = 46 + TOKEN_BASE, // "mod"
  #[token("div")]
  IntegerDivide                         = 303, // = 47 + TOKEN_BASE, // "div"
  #[regex(r"\$[a-z][a-zA-Z0-9_']*")]
  InfixName                             = 304, // = 48 + TOKEN_BASE, // $NAME
  #[regex(r"\$[A-Z][a-zA-Z0-9_']*")]
  InfixCName                            = 305, // = 49 + TOKEN_BASE, // $CONSTRUCTOR
  #[token(":")]
  Colon                                 = 306, // = 50 + TOKEN_BASE, // ":"
  #[token("&")]
  Ampersand                             = 307, // = 51 + TOKEN_BASE, // "&"
  #[token(">")]
  Greater                               = 308, // = 52 + TOKEN_BASE, // ">"
  #[token("=")]
  Equal                                 = 309, // = 53 + TOKEN_BASE, // "="
  #[token("<")]
  Less                                  = 310, // = 54 + TOKEN_BASE, // "<"
  #[token("+")]
  Plus                                  = 311, // = 55 + TOKEN_BASE, // "+"
  #[token("-")]
  Minus                                 = 312, // = 56 + TOKEN_BASE, // "-"
  #[token("*")]
  Times                                 = 313, // = 57 + TOKEN_BASE, // "*"
  #[token("/")]
  Divide                                = 314, // = 58 + TOKEN_BASE, // "/"
  #[token("^")]
  Caret                                 = 315, // = 59 + TOKEN_BASE, // "^"
  #[token(".")]
  Dot                                   = 316, // = 60 + TOKEN_BASE, // "."
  #[token("!")]
  Bang                                  = 317, // = 61 + TOKEN_BASE, // "!"
  #[token("~")]
  Tilde                                 = 318, // = 62 + TOKEN_BASE, // "~"
  #[token("#")]
  Hash                                  = 319, // = 63 + TOKEN_BASE, // "#"
  #[token(",")]
  Comma                                 = 320, // = 64 + TOKEN_BASE, // ","
  #[token("|")]
  Pipe                                  = 321, // = 65 + TOKEN_BASE, // "|"
  #[token("?")]
  QuestionMark                          = 322, // = 66 + TOKEN_BASE, // "?"
  #[token(";")]
  Semicolon                             = 323, // = 67 + TOKEN_BASE, // ";"
  #[token("\n")]
  Newline                               = 324, // = 68 + TOKEN_BASE, // "Newline"
  #[token("{")]
  OpenBrace                             = 325, // = 69 + TOKEN_BASE, // "{"
  #[token("}")]
  CloseBrace                            = 326, // = 70 + TOKEN_BASE, // "}"
  #[token("(")]
  OpenParenthesis                       = 327, // = 71 + TOKEN_BASE, // "("
  #[token(")")]
  CloseParenthesis                      = 328, // = 72 + TOKEN_BASE, // ")"
  #[token("[")]
  OpenBracket                           = 329, // = 73 + TOKEN_BASE, // "["
  #[token("]")]
  CloseBracket                          = 330, // = 74 + TOKEN_BASE, // "]"
  #[regex(r#""[^"\\]*(\\.[^"\\]*)*""#)]
  StringLiteral                         = 331, // = 75 + TOKEN_BASE, // STRING
  #[regex(r"[0-9]+")]
  Integer                               = 332, // = 76 + TOKEN_BASE, // INTEGER
  #[regex(r"[0-9]+\.[0-9]*")]
  Float                                 = 333, // = 77 + TOKEN_BASE, // FLOAT
  EOF                                   = 334, // = 78 + TOKEN_BASE, // EOF


  #[error]
  #[regex(r"[ \t]*\|\|[^\n]*\n", logos::skip)]
  #[regex(r"[ \t]*", logos::skip)]
  Error = 335, // = 79 + TOKEN_BASE,  // ERROR
}




impl Display for Token {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "token<{}>", self.string_representation())
  }
}

impl Token {
  pub fn string_representation(&self) -> &str {
    // println!("self: {}\tTOKEN_BASE: {}", (*self as ValueRepresentationType), TOKEN_BASE);
    TOKEN_STRINGS[*self as usize - TOKEN_BASE]
  }

  pub fn into_value(self) -> Value {
    Value::Token(self)
  }
}

