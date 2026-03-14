use crate::data::{Combinator, RawValue};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserSessionState {
    pub lexstates: RawValue,
    pub idsused: RawValue,
    pub lexdefs: RawValue,
    pub last_name: RawValue,
    pub ihlist: RawValue,
    pub nonterminals: RawValue,
    pub eprodnts: RawValue,
    pub ntspecmap: RawValue,
    pub ntmap: RawValue,
    pub last_here: RawValue,
    pub inbnf: u8,
    pub inexplist: bool,
    pub inlex: u8,
    pub tvarscope: bool,
    pub sreds: i16,
    pub open_bracket_count: i16,
}

impl Default for ParserSessionState {
    fn default() -> Self {
        Self {
            lexstates: Combinator::Nil.into(),
            idsused: Combinator::Nil.into(),
            lexdefs: Combinator::Nil.into(),
            last_name: 0,
            ihlist: 0,
            nonterminals: Combinator::Nil.into(),
            eprodnts: Combinator::Nil.into(),
            ntspecmap: Combinator::Nil.into(),
            ntmap: Combinator::Nil.into(),
            last_here: 0,
            inbnf: 0,
            inexplist: false,
            inlex: 0,
            tvarscope: false,
            sreds: 0,
            open_bracket_count: 0,
        }
    }
}

impl ParserSessionState {
    pub fn reset_for_new_parse(&mut self) {
        *self = Self::default();
    }

    pub fn reset_expression_path_state(&mut self) {
        self.idsused = Combinator::Nil.into();
        self.last_name = 0;
        self.last_here = 0;
        self.tvarscope = false;
        self.sreds = 0;
        self.open_bracket_count = 0;
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserDeferredState {
    pub exports: RawValue,
    pub exportfiles: RawValue,
    pub embargoes: RawValue,
    pub includees: RawValue,
    pub freeids: RawValue,
}

impl Default for ParserDeferredState {
    fn default() -> Self {
        Self {
            exports: Combinator::Nil.into(),
            exportfiles: Combinator::Nil.into(),
            embargoes: Combinator::Nil.into(),
            includees: Combinator::Nil.into(),
            freeids: Combinator::Nil.into(),
        }
    }
}

impl ParserDeferredState {
    pub fn clear(&mut self) {
        *self = Self::default();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parser_session_state_reset_methods_restore_expected_defaults() {
        let mut state = ParserSessionState {
            idsused: 17,
            last_name: 9,
            last_here: 13,
            tvarscope: true,
            sreds: 4,
            open_bracket_count: -2,
            ..Default::default()
        };

        state.reset_expression_path_state();

        assert_eq!(state.idsused, Combinator::Nil.into());
        assert_eq!(state.last_name, 0);
        assert_eq!(state.last_here, 0);
        assert!(!state.tvarscope);
        assert_eq!(state.sreds, 0);
        assert_eq!(state.open_bracket_count, 0);

        state.lexdefs = 88;
        state.inlex = 2;
        state.reset_for_new_parse();

        assert_eq!(state, ParserSessionState::default());
    }

    #[test]
    fn parser_deferred_state_clear_restores_nil_backing() {
        let mut state = ParserDeferredState {
            exports: 10,
            exportfiles: 11,
            embargoes: 12,
            includees: 13,
            freeids: 14,
        };

        state.clear();

        assert_eq!(state, ParserDeferredState::default());
    }
}
