/*
Lexical error types.
*/

use std::error::Error as StdError;
use thiserror::Error;

#[derive(Debug, Error)]
#[allow(clippy::upper_case_acronyms)]
pub enum LexError {
    #[error("encountered EOF")]
    EOF,
    #[error("formal text not delimited by blank line")]
    BlankErr,
    #[error("unknown error: {source}")]
    UnknownError {
        #[source]
        source: Box<dyn StdError + 'static>,
    },
}
