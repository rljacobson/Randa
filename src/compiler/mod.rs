/*!
There are mild impedance mismatches between libraries. Logos returns ranges as byte spans over the source text, and the
current lexer keeps the source name and source text directly so it can recover token slices and locations without an
extra source-wrapper dependency.

Bison has distinct notions of token and value. A value wraps a token. Rules produce values. For us, values will be
AST nodes.

*/

pub(crate) mod bytecode;
pub mod errors;
mod lexer;
// This clippy lint allow is needed because it triggers on generated skeleton code and thus
// can't be addressed by fixes in `parser.y`. Since this can hide genuine issues in future edits to
// `parser.y`, we should periodically disable this allow to check that warnings are only baseline,
// not real problems in new code.
#[allow(clippy::clone_on_copy)]
pub(crate) mod parser;
mod parser_activation;
mod parser_session;
mod parser_support;
pub mod token;

use crate::data::api::HeapString;
pub(crate) use lexer::Lexer;
pub use parser_activation::{ParserActivation, ParserVmContext};
pub use parser_session::{ParserDeferredState, ParserSessionState};
pub use parser_support::{
    ParserConstructorPayload, ParserDefinitionPayload, ParserExportDirectivePayload,
    ParserFreeBindingPayload, ParserIncludeDirectivePayload, ParserRunDiagnostics, ParserRunResult,
    ParserSpecificationPayload, ParserSupportError, ParserTopLevelDirectivePayload,
    ParserTopLevelScriptPayload, ParserTypeDeclarationPayload,
};
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
