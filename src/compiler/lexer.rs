/*!

A minimal wrapper around a Logos lexer to satisfy the Bison parser interface.

 */

use logos::Logos;
use std::fmt::{Debug, Formatter};
use std::ops::Range;

use super::{errors::LexerError, token::ParserLookahead, Loc, Token};
use crate::data::Value;

const YYEOF_TOKEN_TYPE: i32 = 0;
const YYUNDEF_TOKEN_TYPE: i32 = 257;

pub struct Lexer {
    tokens: Vec<(Token, Range<usize>)>,
    token_index: usize,
    source_name: String,
    source_text: String,
    span: Range<usize>,
    token: Token,
}

impl Debug for Lexer {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Lexer{{source={}}}", self.source_name)
    }
}

impl Lexer {
    pub fn new(source_name: &str, source_text: &str) -> Lexer {
        let source_name = source_name.to_string();
        let source_text = source_text.to_string();
        let tokens = Token::lexer(&source_text)
            .spanned()
            .map(|(token, span)| (token.unwrap_or(Token::Error), span))
            .collect();
        Lexer {
            tokens,
            token_index: 0,
            source_name,
            source_text,
            span: 0..0,
            token: Token::EmptySymbol,
        }
    }

    pub fn yylex(&mut self) -> Result<ParserLookahead, LexerError> {
        let (token, span) = match self.tokens.get(self.token_index) {
            Some((token, span)) => {
                self.token_index += 1;
                (*token, span.clone())
            }

            None => {
                let n = self.source_text.len();
                (Token::EOF, (n..n))
            }
        };

        self.span = span;
        self.token = token;

        let token_type = match token {
            Token::EOF => YYEOF_TOKEN_TYPE,
            Token::Error => YYUNDEF_TOKEN_TYPE,
            _ => token as i32 + 2,
        };

        Ok(ParserLookahead {
            token_type,
            token,
            value: Value::Token(token),
            loc: Loc::new(self.span.start as u32, self.span.end as u32),
        })
    }

    /// Returns the last token.
    pub fn token(&self) -> Token {
        self.token
    }

    pub fn current_loc(&self) -> Loc {
        Loc::new(self.span.start as u32, self.span.end as u32)
    }

    pub fn source_name(&self) -> &str {
        &self.source_name
    }

    pub fn source_text(&self) -> &str {
        &self.source_text
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn skips_indented_line_comments_and_returns_next_token() {
        let mut lexer = Lexer::new("commented.m", "   || ignored comment\nwhere");

        let lookahead = lexer.yylex().expect("lexer should produce a token");

        assert_eq!(lookahead.token, Token::Where);
        assert_eq!(lookahead.loc, Loc::new(22, 27));
    }

    #[test]
    fn skips_horizontal_whitespace_without_emitting_error_tokens() {
        let mut lexer = Lexer::new("whitespace.m", " \t\tif");

        let lookahead = lexer.yylex().expect("lexer should produce a token");

        assert_eq!(lookahead.token, Token::If);
        assert_eq!(lookahead.loc, Loc::new(3, 5));
    }

    #[test]
    fn plain_pipe_is_not_treated_as_comment_start() {
        let mut lexer = Lexer::new("pipe.m", "| |");

        let first = lexer.yylex().expect("lexer should produce first token");
        let second = lexer.yylex().expect("lexer should produce second token");

        assert_eq!(first.token, Token::Pipe);
        assert_eq!(first.loc, Loc::new(0, 1));
        assert_eq!(second.token, Token::Pipe);
        assert_eq!(second.loc, Loc::new(2, 3));
    }

    #[test]
    fn consecutive_stars_tokenize_as_typevar() {
        let mut lexer = Lexer::new("typevar.m", "**");

        let lookahead = lexer.yylex().expect("lexer should produce a token");

        assert_eq!(lookahead.token, Token::TypeVar);
        assert_eq!(lookahead.loc, Loc::new(0, 2));
    }
}
