use thiserror::Error;

use crate::data::{RawValue, Value};

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
pub struct ParserIncludeDirectivePayload {
    pub anchor: RawValue,
    pub target_path: RawValue,
    pub modifiers: RawValue,
    pub bindings: RawValue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserExportDirectivePayload {
    pub anchor: RawValue,
    pub exported_ids: RawValue,
    pub pathname_requests: RawValue,
    pub embargoes: RawValue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserDefinitionPayload {
    pub identifier: RawValue,
    pub body: RawValue,
    pub anchor: RawValue,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ParserTopLevelDirectivePayload {
    pub include_requests: Vec<ParserIncludeDirectivePayload>,
    pub export: Option<ParserExportDirectivePayload>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ParserTopLevelScriptPayload {
    pub directives: ParserTopLevelDirectivePayload,
    pub definitions: Vec<ParserDefinitionPayload>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserRunResult {
    ParsedExpression(Value),
    ParsedTopLevelScript(ParserTopLevelScriptPayload),
    SyntaxError(ParserRunDiagnostics),
}
