use thiserror::Error;

use crate::data::Value;

use super::ParserDiagnostic;

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum ParserSupportError {
    #[error("deferred parser mutation is not implemented: {operation}")]
    DeferredMutation { operation: &'static str },
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ParserRunDiagnostics {
    pub diagnostics: Vec<ParserDiagnostic>,
}

impl ParserRunDiagnostics {
    pub fn push(&mut self, diagnostic: ParserDiagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserRunResult {
    ParsedExpression(Value),
    SyntaxError(ParserRunDiagnostics),
}
