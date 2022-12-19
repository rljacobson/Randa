/*!



 */


// mod parser;
pub mod token;
pub mod errors;
mod lexer;
mod lexical_value;


pub use saucepan::Span;
pub use token::Token;
pub use lexer::Lexer;

pub type Loc = std::ops::Range<u32>;

