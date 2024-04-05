use rowan::{TextRange, TextSize};
use unscanny::Scanner;

use crate::diagnostic::{Diagnostic, DiagnosticKind};
use crate::syntax_kind::SyntaxKind;
use crate::token::Token;

pub struct Lexer<'a> {
    s: Scanner<'a>,
    tokens: Vec<Token<'a>>,
    current_diagnostic_kind: Option<DiagnosticKind>,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            s: Scanner::new(text),
            tokens: Vec::new(),
            current_diagnostic_kind: None,
        }
    }

    pub fn tokenize(mut self) -> (Vec<Token<'a>>, Vec<Diagnostic>) {
        let mut diagnostics = Vec::new();
        loop {
            let start = self.s.cursor();
            let kind = self.identify_token(start);
            let end = self.s.cursor();
            let range = TextRange::new(TextSize::new(start as u32), TextSize::new(end as u32));
            let text = self.s.get(range.into());
            self.tokens.push(Token {
                kind,
                text,
                range,
            });
            if let Some(diagnostic_kind) = self.current_diagnostic_kind.take() {
                diagnostics.push(Diagnostic::new(diagnostic_kind, range));
            }
            if kind.is_eof() {
                return (self.tokens, diagnostics);
            }
        }
    }

    fn error(&mut self, diagnostic_kind: DiagnosticKind) -> SyntaxKind {
        self.current_diagnostic_kind = Some(diagnostic_kind);
        SyntaxKind::Error
    }

    fn identify_token(&mut self, start: usize) -> SyntaxKind {
        match self.s.eat() {
            Some(c) if c.is_ascii_whitespace() => self.whitespace(),
            Some(c) if c.is_ascii_digit() => self.number(),
            Some(c) if Self::is_ident_start(c) => self.ident(start),
            Some(';') => SyntaxKind::Semicolon,
            Some('=') => SyntaxKind::Equals,
            Some('+') => SyntaxKind::Plus,
            Some('-') => SyntaxKind::Minus,
            Some('*') => SyntaxKind::Star,
            Some('/') => SyntaxKind::Slash,
            Some('%') => SyntaxKind::Percent,
            Some('(') => SyntaxKind::OpenParen,
            Some(')') => SyntaxKind::CloseParen,
            Some('{') => SyntaxKind::OpenBrace,
            Some('}') => SyntaxKind::CloseBrace,
            None => SyntaxKind::Eof,
            _ => self.error(DiagnosticKind::UnexpectedCharacter),
        }
    }

    fn is_ident_start(c: char) -> bool {
        c.is_ascii_alphabetic() || c == '_'
    }

    fn is_ident_continue(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }

    fn whitespace(&mut self) -> SyntaxKind {
        self.s.eat_while(char::is_ascii_whitespace);
        SyntaxKind::Whitespace
    }

    fn ident(&mut self, start: usize) -> SyntaxKind {
        self.s.eat_while(Self::is_ident_continue);
        let ident = self.s.from(start);
        match ident {
            "let" => SyntaxKind::LetKw,
            _ => SyntaxKind::Ident,
        }
    }

    fn number(&mut self) -> SyntaxKind {
        self.s.eat_while(char::is_ascii_digit);
        SyntaxKind::Number
    }
}

#[cfg(test)]
mod tests {
    use crate::diagnostic::Diagnostic;
    use crate::lexer::Lexer;
    use crate::token::Token;

    fn tokenize<'a>(text: &'a str) -> (Vec<Token<'a>>, Vec<Diagnostic>) {
        let lexer = Lexer::new(text);
        lexer.tokenize()
    }

    #[test]
    fn number() {
        insta::assert_debug_snapshot!(tokenize("1 23"));
    }

    #[test]
    fn operator() {
        insta::assert_debug_snapshot!(tokenize("+ - * / %"));
    }

    #[test]
    fn paren() {
        insta::assert_debug_snapshot!(tokenize("( ) { }"));
    }

    #[test]
    fn ketword() {
        insta::assert_debug_snapshot!(tokenize("let"));
    }

    #[test]
    fn ident() {
        insta::assert_debug_snapshot!(tokenize("a bc"));
    }
}
