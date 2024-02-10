use rowan::{TextRange, TextSize};
use unscanny::Scanner;

use crate::syntax_error::SyntaxError;
use crate::syntax_kind::SyntaxKind;
use crate::token::Token;

pub struct Lexer<'a> {
    s: Scanner<'a>,
    tokens: Vec<Token<'a>>,
    errors: Vec<SyntaxError>,
    error: Option<String>,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            s: Scanner::new(text),
            tokens: Vec::new(),
            errors: Vec::new(),
            error: None,
        }
    }

    pub fn tokenize(mut self) -> (Vec<Token<'a>>, Vec<SyntaxError>) {
        loop {
            let start = self.s.cursor();
            let kind = self.identify_token();
            let end = self.s.cursor();
            let range = TextRange::new(TextSize::new(start as u32), TextSize::new(end as u32));
            let text = self.s.get(range.into());
            self.tokens.push(Token {
                kind,
                text,
                range,
            });
            if let Some(message) = self.error.take() {
                self.errors.push(SyntaxError {
                    message,
                    range,
                });
            }
            if kind.is_eof() {
                return (self.tokens, self.errors)
            }
        }
    }

    fn error(&mut self, message: impl Into<String>) -> SyntaxKind {
        self.error = Some(message.into());
        SyntaxKind::Error
    }

    fn identify_token(&mut self) -> SyntaxKind {
        match self.s.eat() {
            Some(c) if c.is_ascii_whitespace() => self.whitespace(),
            Some(c) if c.is_ascii_digit() => self.number(),
            Some('+') => SyntaxKind::Plus,
            Some('*') => SyntaxKind::Star,
            None => SyntaxKind::Eof,
            _ => self.error("Unexpected character"),
        }
    }

    fn whitespace(&mut self) -> SyntaxKind {
        self.s.eat_while(char::is_ascii_whitespace);
        SyntaxKind::Whitespace
    }

    fn number(&mut self) -> SyntaxKind {
        self.s.eat_while(char::is_ascii_digit);
        SyntaxKind::Number
    }
}
