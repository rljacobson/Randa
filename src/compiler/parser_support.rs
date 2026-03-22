use thiserror::Error;

use crate::data::api::{IdentifierValueRef, IdentifierValueTypeKind};
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
pub enum ParserIncludeBindingPayload {
    Value {
        identifier: RawValue,
        body: Value,
    },
    Type {
        identifier: RawValue,
        type_value: IdentifierValueRef,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserIncludeModifierPayload {
    Rename {
        source: RawValue,
        destination: RawValue,
    },
    Suppress {
        identifier: RawValue,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserIncludeDirectivePayload {
    pub anchor: RawValue,
    pub target_path: RawValue,
    pub modifiers: Vec<ParserIncludeModifierPayload>,
    pub bindings: Vec<ParserIncludeBindingPayload>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserSpecificationPayload {
    pub identifier: RawValue,
    pub type_expr: Value,
    pub anchor: RawValue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserTypeDeclarationPayload {
    pub type_identifier: RawValue,
    pub arity: isize,
    pub kind: IdentifierValueTypeKind,
    pub info: Value,
    pub anchor: RawValue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserConstructorFieldPayload {
    pub type_expr: Value,
    pub is_strict: bool,
}

impl ParserConstructorFieldPayload {
    /// Constructs constructor-field payload metadata from the parsed field value and any decoded strict inner type.
    /// This exists so constructor-field payload ownership keeps strictness normalization on the payload type instead of in parser-local assembly code.
    /// The invariant is that strict fields store the decoded inner type with `is_strict = true`, while non-strict fields store the original field value with `is_strict = false`.
    pub fn from_field_type(field: Value, strict_field_type: Option<Value>) -> Self {
        match strict_field_type {
            Some(type_expr) => Self {
                type_expr,
                is_strict: true,
            },
            None => Self {
                type_expr: field,
                is_strict: false,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserConstructorPayload {
    pub constructor: RawValue,
    pub parent_type: RawValue,
    pub parent_type_arity: isize,
    pub arity: isize,
    pub fields: Vec<ParserConstructorFieldPayload>,
    pub anchor: RawValue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserFreeBindingPayload {
    pub identifier: RawValue,
    pub type_expr: Value,
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
    pub specifications: Vec<ParserSpecificationPayload>,
    pub type_declarations: Vec<ParserTypeDeclarationPayload>,
    pub constructor_declarations: Vec<ParserConstructorPayload>,
    pub free_bindings: Vec<ParserFreeBindingPayload>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserRunResult {
    ParsedExpression(Value),
    ParsedTopLevelScript(ParserTopLevelScriptPayload),
    SyntaxError(ParserRunDiagnostics),
}
