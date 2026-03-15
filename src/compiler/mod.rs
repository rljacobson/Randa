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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HereInfo {
    pub(crate) script_file: HeapString,
    pub(crate) line_number: isize,
}

impl HereInfo {
    /// Constructs `HereInfo` from a source path, source text, and optional parser location.
    /// This exists so parser diagnostic anchoring owns location-to-line conversion on the `HereInfo` type instead of on unrelated boundary implementors.
    /// The invariant is that the line number is derived by counting preceding `\n` bytes before `location.begin`, or `0` when no location is present.
    pub fn from_source_location(
        source_path: &str,
        source_text: &str,
        location: Option<Loc>,
    ) -> Self {
        let line_number = location.map_or(0, |location| {
            let line_count = source_text
                .bytes()
                .take((location.begin as usize).min(source_text.len()))
                .filter(|byte| *byte == b'\n')
                .count();
            (line_count + 1) as isize
        });

        Self {
            script_file: source_path.to_string(),
            line_number,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserDiagnostic {
    pub message: String,
    pub location: Option<Loc>,
    pub here_info: Option<HereInfo>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn here_info_from_source_location_counts_lines_and_handles_missing_location() {
        let source_text = "alpha\nbeta\ngamma\n";

        let here_info =
            HereInfo::from_source_location("demo.m", source_text, Some(Loc::new(6, 10)));
        assert_eq!(here_info.script_file, "demo.m");
        assert_eq!(here_info.line_number, 2);

        let missing = HereInfo::from_source_location("demo.m", source_text, None);
        assert_eq!(missing.line_number, 0);
    }
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
