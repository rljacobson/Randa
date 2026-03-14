/*!
There are mild impedance mismatches between libraries. Logos returns ranges as "spans". Saucepan can make a
`saucepan::Span` from a range for a `Source` using slice notation: `let span: Span = source[range];`. OTOH, so can
Logos: `text[range]` gives the token as a slice. The advantage of using Saucepan is that Saucepan keeps track of
multiple files.

Bison has distinct notions of token and value. A value wraps a token. Rules produce values. For us, values will be
AST nodes.

*/

pub(crate) mod bytecode;
pub mod errors;
mod lexer;
// mod parser;
mod parser_boundary;
mod parser_session;
pub mod token;

use crate::data::api::HeapString;
pub use parser_boundary::{ParserBoundary, ParserSupportError};
#[allow(unused_imports)]
pub use parser_session::{ParserDeferredState, ParserSessionState};
pub use token::Token;

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct Loc {
    pub begin: u32,
    pub end: u32,
}

impl Loc {
    pub const fn new(begin: u32, end: u32) -> Self {
        Self { begin, end }
    }

    pub fn start_offset(self) -> usize {
        self.begin as usize
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HereInfo {
    pub(crate) script_file: HeapString,
    pub(crate) line_number: isize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserDiagnostic {
    pub message: String,
    pub location: Option<Loc>,
    pub here_info: Option<HereInfo>,
}

impl ParserDiagnostic {
    pub fn syntax(
        message: impl Into<String>,
        location: Option<Loc>,
        here_info: Option<HereInfo>,
    ) -> Self {
        Self {
            message: message.into(),
            location,
            here_info,
        }
    }
}

pub fn line_number_for_location(source_text: &str, location: Option<Loc>) -> isize {
    let Some(location) = location else {
        return 0;
    };

    let line_count = source_text
        .bytes()
        .take(location.start_offset().min(source_text.len()))
        .filter(|byte| *byte == b'\n')
        .count();

    (line_count + 1) as isize
}

/*
// Todo: Why only these keywords? This list is from steer.c, line ~1582.
pub static KEYWORDS: HashMap<&str, u8> = HashMap::from([
  ("abstype", 21),
  ("div", 8),
  ("if", 15),
  ("mod", 8),
  ("otherwise", 15),
  ("readvals", 31),
  ("show", 23),
  ("type", 22),
  ("where", 15),
  ("with", 21)
]);
*/
