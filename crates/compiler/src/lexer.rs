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
            Some('=') if self.s.peek() == Some('=') => { self.s.eat(); SyntaxKind::EqEq },
            Some('!') if self.s.peek() == Some('=') => { self.s.eat(); SyntaxKind::Neq },
            Some('>') if self.s.peek() == Some('=') => { self.s.eat(); SyntaxKind::Ge },
            Some('<') if self.s.peek() == Some('=') => { self.s.eat(); SyntaxKind::Le },
            Some('&') if self.s.peek() == Some('&') => { self.s.eat(); SyntaxKind::And },
            Some('|') if self.s.peek() == Some('|') => { self.s.eat(); SyntaxKind::Or },
            Some('>') => SyntaxKind::Gt,
            Some('<') => SyntaxKind::Lt,
            Some('/') if self.s.peek() == Some('/') => self.line_comment(),
            Some('"') => self.string_literal(),
            Some('\'') => self.char_literal(),
            Some(',') => SyntaxKind::Comma,
            Some('.') => SyntaxKind::Dot,
            Some(':') => SyntaxKind::Colon,
            Some(';') => SyntaxKind::Semicolon,
            Some('?') => SyntaxKind::Question,
            Some('=') => SyntaxKind::Equals,
            Some('+') => SyntaxKind::Plus,
            Some('-') => SyntaxKind::Minus,
            Some('*') => SyntaxKind::Star,
            Some('/') => SyntaxKind::Slash,
            Some('%') => SyntaxKind::Percent,
            Some('(') => SyntaxKind::OpenParen,
            Some(')') => SyntaxKind::CloseParen,
            Some('[') => SyntaxKind::OpenBracket,
            Some(']') => SyntaxKind::CloseBracket,
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

    fn line_comment(&mut self) -> SyntaxKind {
        self.s.eat_while(|c| c != '\n');
        SyntaxKind::LineComment
    }

    fn string_literal(&mut self) -> SyntaxKind {
        self.s.eat_while(|c| c != '"' && c != '\n');
        if self.s.eat_if('"') {
            SyntaxKind::String
        } else {
            self.error(DiagnosticKind::UnterminatedStringLiteral)
        }
    }

    fn char_literal(&mut self) -> SyntaxKind {
        let inner_literal = self.s.eat();
        if let Some('\'') = inner_literal {
            self.error(DiagnosticKind::EmptyCharLiteral)
        } else if inner_literal.is_none() || !self.s.eat_if('\'') {
            self.error(DiagnosticKind::UnterminatedCharLiteral)
        } else {
            SyntaxKind::Char
        }
    }

    fn ident(&mut self, start: usize) -> SyntaxKind {
        self.s.eat_while(Self::is_ident_continue);
        let ident = self.s.from(start);
        match ident {
            "let" => SyntaxKind::LetKw,
            "if" => SyntaxKind::IfKw,
            "else" => SyntaxKind::ElseKw,
            "while" => SyntaxKind::WhileKw,
            "break" => SyntaxKind::BreakKw,
            "continue" => SyntaxKind::ContinueKw,
            "fun" => SyntaxKind::FunKw,
            "none" => SyntaxKind::NoneKw,
            "true" => SyntaxKind::TrueKw,
            "false" => SyntaxKind::FalseKw,
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

    fn tokenize(text: &str) -> (Vec<Token>, Vec<Diagnostic>) {
        let lexer = Lexer::new(text);
        lexer.tokenize()
    }

    #[test]
    fn number() {
        insta::assert_debug_snapshot!(tokenize("1 23"));
    }

    #[test]
    fn operator() {
        insta::assert_debug_snapshot!(tokenize("+ - * / % == != >= <= > < && ||"));
    }

    #[test]
    fn paren() {
        insta::assert_debug_snapshot!(tokenize("( ) { } [ ]"));
    }

    #[test]
    fn ketword() {
        insta::assert_debug_snapshot!(tokenize("let if else while break continue fun true false"));
    }

    #[test]
    fn ident() {
        insta::assert_debug_snapshot!(tokenize("a bc"));
    }
}
