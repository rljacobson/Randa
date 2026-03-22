use crate::data::{
    api::{ConsList, FileInfoRef, IdentifierRecordRef},
    Combinator, RawValue, Value,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserSessionState {
    pub lex_states: ConsList<Value>,
    pub used_identifiers: ConsList<Value>,
    pub lex_rule_definitions: ConsList<Value>,
    pub last_identifier: RawValue,
    pub inherited_attributes: RawValue,
    pub nonterminals: ConsList<IdentifierRecordRef>,
    pub empty_production_nonterminals: ConsList<IdentifierRecordRef>,
    pub nonterminal_specification_map: ConsList<Value>,
    pub nonterminal_map: RawValue,
    pub last_diagnostic_location: Option<FileInfoRef>,
    pub bnf_mode: u8,
    pub export_list_mode: bool,
    pub lex_mode: u8,
    pub type_variable_scope: bool,
    pub semantic_reduction_count: i16,
    pub open_bracket_count: i16,
}

impl Default for ParserSessionState {
    fn default() -> Self {
        Self {
            lex_states: ConsList::EMPTY,
            used_identifiers: ConsList::EMPTY,
            lex_rule_definitions: ConsList::EMPTY,
            last_identifier: 0,
            inherited_attributes: 0,
            nonterminals: ConsList::EMPTY,
            empty_production_nonterminals: ConsList::EMPTY,
            nonterminal_specification_map: ConsList::EMPTY,
            nonterminal_map: Combinator::Nil.into(),
            last_diagnostic_location: None,
            bnf_mode: 0,
            export_list_mode: false,
            lex_mode: 0,
            type_variable_scope: false,
            semantic_reduction_count: 0,
            open_bracket_count: 0,
        }
    }
}

impl ParserSessionState {
    /// Resets every parser-session field back to the canonical NIL/zero defaults.
    /// This exists so future parser entry/reset sites use one owned state reset path instead of duplicating field-by-field initialization.
    /// The invariant is that a fresh parse starts with the same parser-local flags and accumulators as `Default::default()`.
    pub fn reset_for_new_parse(&mut self) {
        *self = Self::default();
    }

    /// Resets the expression-path tracking fields without disturbing unrelated parser-mode state.
    /// The invariant is that `used_identifiers`, `last_identifier`, `last_diagnostic_location`, `type_variable_scope`, `semantic_reduction_count`, and `open_bracket_count` return to their canonical empty-state values together.
    pub fn reset_expression_path_state(&mut self) {
        self.used_identifiers = ConsList::EMPTY;
        self.last_identifier = 0;
        self.last_diagnostic_location = None;
        self.type_variable_scope = false;
        self.semantic_reduction_count = 0;
        self.open_bracket_count = 0;
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserDeferredState {
    pub exported_identifiers: RawValue,
    pub export_path_requests: ConsList<Value>,
    pub export_embargoes: ConsList<IdentifierRecordRef>,
    pub include_requests: RawValue,
    pub free_identifiers: RawValue,
}

impl Default for ParserDeferredState {
    fn default() -> Self {
        Self {
            exported_identifiers: Combinator::Nil.into(),
            export_path_requests: ConsList::EMPTY,
            export_embargoes: ConsList::EMPTY,
            include_requests: Combinator::Nil.into(),
            free_identifiers: Combinator::Nil.into(),
        }
    }
}

impl ParserDeferredState {
    /// Clears every deferred parser-output accumulator back to the NIL-backed empty state.
    /// This exists so deferred directive state has one owner for reset semantics instead of duplicating NIL restoration across future parser handoff sites.
    /// The invariant is that `exported_identifiers`, `export_path_requests`, `export_embargoes`, `include_requests`, and `free_identifiers` always reset together to the same defaults as `Default::default()`.
    pub fn clear(&mut self) {
        *self = Self::default();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data::api::HeapObjectProxy;

    #[test]
    fn parser_session_state_reset_methods_restore_expected_defaults() {
        let mut state = ParserSessionState {
            used_identifiers: ConsList::from_ref(17),
            last_identifier: 9,
            last_diagnostic_location: Some(FileInfoRef::from_ref(13)),
            type_variable_scope: true,
            semantic_reduction_count: 4,
            open_bracket_count: -2,
            ..Default::default()
        };

        state.reset_expression_path_state();

        assert_eq!(state.used_identifiers, ConsList::EMPTY);
        assert_eq!(state.last_identifier, 0);
        assert_eq!(state.last_diagnostic_location, None);
        assert!(!state.type_variable_scope);
        assert_eq!(state.semantic_reduction_count, 0);
        assert_eq!(state.open_bracket_count, 0);

        state.lex_rule_definitions = ConsList::from_ref(88);
        state.lex_mode = 2;
        state.reset_for_new_parse();

        assert_eq!(state, ParserSessionState::default());
    }

    #[test]
    fn parser_deferred_state_clear_restores_nil_backing() {
        let mut state = ParserDeferredState {
            exported_identifiers: 10,
            export_path_requests: ConsList::from_ref(11),
            export_embargoes: ConsList::from_ref(12),
            include_requests: 13,
            free_identifiers: 14,
        };

        state.clear();

        assert_eq!(state, ParserDeferredState::default());
    }
}
