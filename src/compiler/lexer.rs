/*!

A minimal wrapper around a Logos lexer to satisfy the Bison parser interface.

 */

use logos::Logos;
use std::collections::VecDeque;
use std::fmt::{Debug, Formatter};
use std::ops::Range;

use super::{errors::LexerError, token::ParserLookahead, Loc, Token};
use crate::data::Value;

const YYEOF_TOKEN_TYPE: i32 = 0;
const YYUNDEF_TOKEN_TYPE: i32 = 257;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct LayoutContext {
    margin: usize,
}

pub struct Lexer {
    tokens: Vec<(Token, Range<usize>)>,
    token_index: usize,
    source_name: String,
    source_text: String,
    span: Range<usize>,
    token: Token,
    pending_tokens: VecDeque<(Token, Range<usize>)>,
    layout_stack: Vec<LayoutContext>,
    bracket_depth: usize,
    where_layout_pending: bool,
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
            pending_tokens: VecDeque::new(),
            layout_stack: Vec::new(),
            bracket_depth: 0,
            where_layout_pending: false,
        }
    }

    /// Pushes one active layout margin for the current parser-entered region.
    /// This exists so the lexer can synthesize `Offside` / `ElseEqual` tokens for the active subset instead of relying on VM no-op hooks.
    /// The invariant is that the pushed margin is the indentation of the current token's line.
    pub fn push_layout_margin(&mut self) {
        let margin = self.line_indent_at(self.span.start);
        self.layout_stack.push(LayoutContext { margin });
    }

    pub fn yylex(&mut self) -> Result<ParserLookahead, LexerError> {
        loop {
            let (token, span) = if let Some((token, span)) = self.pending_tokens.pop_front() {
                (token, span)
            } else {
                match self.tokens.get(self.token_index) {
                    Some((token, span)) => {
                        self.token_index += 1;
                        (*token, span.clone())
                    }
                    None => {
                        let n = self.source_text.len();
                        if !self.layout_stack.is_empty() {
                            self.pop_layout_margin();
                            (Token::Offside, n..n)
                        } else {
                            (Token::EOF, n..n)
                        }
                    }
                }
            };

            if token == Token::Newline {
                if self.bracket_depth > 0 {
                    continue;
                }

                if self.where_layout_pending {
                    let Some((next_token, next_span)) = self.tokens[self.token_index..]
                        .iter()
                        .find(|(token, _)| *token != Token::Newline)
                        .map(|(token, span)| (*token, span.clone()))
                    else {
                        self.where_layout_pending = false;
                        continue;
                    };
                    let margin = self.line_indent_at(next_span.start);
                    self.layout_stack.push(LayoutContext { margin });

                    if next_token == Token::Equal {
                        self.pending_tokens
                            .push_back((Token::ElseEqual, next_span.clone()));
                        self.token_index += 1;
                    } else if margin <= self.line_indent_at(self.span.start) {
                        self.pending_tokens.push_back((Token::Offside, span.clone()));
                    }

                    self.where_layout_pending = false;
                    continue;
                }

                let Some(active_layout) = self.layout_stack.last().copied() else {
                    continue;
                };
                let Some((next_token, next_span)) = self.tokens[self.token_index..]
                    .iter()
                    .find(|(token, _)| *token != Token::Newline)
                    .map(|(token, span)| (*token, span.clone()))
                else {
                    self.pending_tokens.push_back((Token::Offside, span.clone()));
                    continue;
                };
                let next_margin = self.line_indent_at(next_span.start);
                if next_margin > active_layout.margin {
                    continue;
                }

                if next_token == Token::Equal && next_margin == active_layout.margin {
                    self.pending_tokens
                        .push_back((Token::ElseEqual, next_span.clone()));
                    self.token_index += 1;
                } else {
                    self.pending_tokens.push_back((Token::Offside, span.clone()));
                }
                continue;
            }

            self.span = span;
            self.token = token;

            match token {
                Token::OpenParenthesis | Token::OpenBracket | Token::OpenBrace => {
                    self.bracket_depth += 1;
                }
                Token::CloseParenthesis | Token::CloseBracket | Token::CloseBrace => {
                    self.bracket_depth = self.bracket_depth.saturating_sub(1);
                }
                Token::Where => {
                    self.where_layout_pending = true;
                }
                _ => {}
            }

            let token_type = match token {
                Token::EOF => YYEOF_TOKEN_TYPE,
                Token::Error => YYUNDEF_TOKEN_TYPE,
                _ => token as i32 + 2,
            };

            return Ok(ParserLookahead {
                token_type,
                token,
                value: Value::Token(token),
                loc: Loc::new(self.span.start as u32, self.span.end as u32),
            });
        }
    }

    /// Pops one active layout margin as the current parser-entered region exits.
    /// This exists so parser-driven layout exits and EOF-driven offside draining use the same stack transition instead of mutating the layout stack ad hoc.
    /// The invariant is that exactly one active layout context is removed when present.
    pub fn pop_layout_margin(&mut self) {
        let _ = self.layout_stack.pop();
    }

    /// Returns the indentation width of the line containing `offset`.
    /// This exists so all active layout entry and separator paths count indentation with one rule instead of reopening line-start scans inline.
    /// The invariant is that indentation is counted from the containing line start to `offset` using only leading spaces and tabs.
    fn line_indent_at(&self, offset: usize) -> usize {
        let safe_offset = offset.min(self.source_text.len());
        let line_start = self.source_text[..safe_offset]
            .rfind('\n')
            .map_or(0, |index| index + 1);
        self.source_text[line_start..safe_offset]
            .chars()
            .take_while(|character| matches!(character, ' ' | '\t'))
            .count()
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

    #[test]
    fn where_layout_emits_offside_for_peer_local_definition_lines() {
        let mut lexer = Lexer::new("where_layout.m", "where\n  x = 1\n  y = 2\n");

        assert_eq!(lexer.yylex().expect("where token").token, Token::Where);
        assert_eq!(lexer.yylex().expect("first local name").token, Token::Identifier);
        assert_eq!(lexer.yylex().expect("equal token").token, Token::Equal);
        assert_eq!(lexer.yylex().expect("first rhs integer").token, Token::Integer);
        assert_eq!(lexer.yylex().expect("peer separator").token, Token::Offside);
        assert_eq!(lexer.yylex().expect("second local name").token, Token::Identifier);
    }

    #[test]
    fn layout_context_turns_equal_at_margin_into_else_equal() {
        let mut lexer = Lexer::new("guards.m", "value\n= otherwise\n");
        lexer.push_layout_margin();

        assert_eq!(lexer.yylex().expect("value token").token, Token::Identifier);
        assert_eq!(lexer.yylex().expect("guard separator").token, Token::ElseEqual);
    }
}
