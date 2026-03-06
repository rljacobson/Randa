/*!

This module needs some work.

*/

use std::error::Error as StdError;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum LexerError {
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

pub fn emit_error(e: LexerError) {
    println!("Error: {}", e);
}
