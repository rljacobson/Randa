use thiserror::Error;

use crate::data::{
    api::IdentifierValueTypeKind,
    api::{DataPair, HeapString, IdentifierRecordRef},
    Value,
};

use super::ParserDiagnostic;

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum ParserSupportError {
    #[error("deferred parser mutation is not implemented: {operation}")]
    DeferredMutation { operation: &'static str },
}

pub trait ParserBoundary {
    fn record_syntax_diagnostic(&mut self, diagnostic: ParserDiagnostic);

    fn intern_identifier(&mut self, name: &str) -> IdentifierRecordRef;

    fn identifier_name(&self, identifier: IdentifierRecordRef) -> HeapString;

    fn source_name_metadata(&mut self, source_name: &str) -> DataPair;

    fn private_name(&mut self, value: Value) -> Value;

    fn translate_type_identifier(&self, identifier: IdentifierRecordRef) -> Value;

    fn listdiff_function(&self) -> Value;

    fn numeric_one(&self) -> Value;

    fn void_tuple(&self) -> Value;

    fn attach_type_show_function(
        &mut self,
        _type_identifier: IdentifierRecordRef,
        _show_function: IdentifierRecordRef,
    ) -> Result<(), ParserSupportError> {
        Err(ParserSupportError::DeferredMutation {
            operation: "attach_type_show_function",
        })
    }

    fn indent_function(&self) -> Result<Value, ParserSupportError> {
        Err(ParserSupportError::DeferredMutation {
            operation: "indent_function",
        })
    }

    fn outdent_function(&self) -> Result<Value, ParserSupportError> {
        Err(ParserSupportError::DeferredMutation {
            operation: "outdent_function",
        })
    }

    fn record_deferred_exports(
        &mut self,
        _exports: Value,
        _exportfiles: Value,
        _embargoes: Value,
    ) -> Result<(), ParserSupportError> {
        Err(ParserSupportError::DeferredMutation {
            operation: "record_deferred_exports",
        })
    }

    fn record_deferred_includees(&mut self, _includees: Value) -> Result<(), ParserSupportError> {
        Err(ParserSupportError::DeferredMutation {
            operation: "record_deferred_includees",
        })
    }

    fn record_deferred_freeids(&mut self, _freeids: Value) -> Result<(), ParserSupportError> {
        Err(ParserSupportError::DeferredMutation {
            operation: "record_deferred_freeids",
        })
    }

    fn declare_definition(&mut self, _definition: Value) -> Result<(), ParserSupportError> {
        Err(ParserSupportError::DeferredMutation {
            operation: "declare_definition",
        })
    }

    fn apply_specification(
        &mut self,
        _identifier: Value,
        _specification: Value,
        _here: Value,
    ) -> Result<(), ParserSupportError> {
        Err(ParserSupportError::DeferredMutation {
            operation: "apply_specification",
        })
    }

    fn declare_type(
        &mut self,
        _identifier: Value,
        _kind: IdentifierValueTypeKind,
        _info: Value,
        _here: Value,
    ) -> Result<(), ParserSupportError> {
        Err(ParserSupportError::DeferredMutation {
            operation: "declare_type",
        })
    }

    fn declare_constructor(
        &mut self,
        _identifier: Value,
        _type_value: Value,
        _rhs_ids: Value,
        _here: Value,
    ) -> Result<(), ParserSupportError> {
        Err(ParserSupportError::DeferredMutation {
            operation: "declare_constructor",
        })
    }

    fn free_value(&self) -> Result<Value, ParserSupportError> {
        Err(ParserSupportError::DeferredMutation {
            operation: "free_value",
        })
    }
}
