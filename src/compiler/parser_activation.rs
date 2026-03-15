use crate::data::Heap;

use crate::data::Value;

use super::{ParserDeferredState, ParserSessionState};

#[derive(Debug, Copy, Clone)]
pub struct ParserVmContext {
    listdiff_function: Value,
    void_tuple: Value,
}

impl ParserVmContext {
    pub fn new(listdiff_function: Value, void_tuple: Value) -> Self {
        Self {
            listdiff_function,
            void_tuple,
        }
    }

    pub fn listdiff_function(&self) -> Value {
        self.listdiff_function
    }

    pub fn void_tuple(&self) -> Value {
        self.void_tuple
    }
}

pub struct ParserActivation<'ctx> {
    pub heap: &'ctx mut Heap,
    pub vm: ParserVmContext,
    pub session: ParserSessionState,
    pub deferred: ParserDeferredState,
}
