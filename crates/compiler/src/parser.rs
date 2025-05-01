use drop_bomb::DropBomb;
use rowan::TextRange;

use crate::diagnostic::{Diagnostic, DiagnosticKind};
use crate::event;
use crate::event::Event;
use crate::grammar::root;
use crate::language::SyntaxNode;
use crate::syntax_kind::SyntaxKind;
use crate::token::Token;
use crate::token_set::TokenSet;

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    cursor: usize,
    events: Vec<Event>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Self {
            tokens,
            cursor: 0,
            events: Vec::new(),
        }
    }
    pub fn parse(mut self) -> (SyntaxNode, Vec<Diagnostic>) {
        root(&mut self);
        event::process(&mut self.events, self.tokens)
    }
    pub fn current(&self) -> SyntaxKind {
        self.nth(0)
    }
    pub fn nth(&self, n: usize) -> SyntaxKind {
        let mut temp_cursor = self.cursor;
        for _ in 0..n {
            if self.tokens[temp_cursor].kind.is_eof() {
                break;
            }
            temp_cursor += 1;
            while self.tokens[temp_cursor].kind.is_trivia() {
                temp_cursor += 1;
            }
        }
        self.tokens[temp_cursor].kind
    }
    fn current_range(&self) -> TextRange {
        self.tokens[self.cursor].range
    }
    pub fn at(&self, kind: SyntaxKind) -> bool {
        self.nth_at(0, kind)
    }
    pub fn at_set(&self, set: &TokenSet) -> bool {
        set.contains(self.current())
    }
    pub fn nth_at(&self, n: usize, kind: SyntaxKind) -> bool {
        self.nth(n) == kind
    }
    pub fn eat(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            false
        }
    }
    pub fn assert(&mut self, kind: SyntaxKind) {
        assert!(self.at(kind));
        self.bump();
    }
    pub fn bump(&mut self) {
        if self.current() != SyntaxKind::Eof {
            self.consume_token();
            self.eat_trivia();
        }
    }
    pub fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.eat(kind) {
            true
        } else {
            self.error(&[kind]);
            false
        }
    }
    pub fn error(&mut self, expected: &[SyntaxKind]) {
        self.events.push(Event::Error(Diagnostic::new(DiagnosticKind::UnexpectedToken {
            expected: expected.to_vec(),
            actual: self.current(),
        }, self.current_range())));
    }
    pub fn error_and_recover(&mut self, expected: &[SyntaxKind], recovery: &TokenSet) {
        if self.at_set(recovery) {
            self.error(expected);
            return;
        }
        let m = self.start();
        self.error(expected);
        self.bump();
        m.complete(self, SyntaxKind::Error);
    }
    pub fn eat_trivia(&mut self) {
        while self.current().is_trivia() {
            self.consume_token();
        }
    }
    pub fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);
        Marker {
            pos,
            bomb: DropBomb::new("Marker must be either completed or abandoned")
        }
    }
    fn consume_token(&mut self) {
        self.events.push(Event::Token(self.current()));
        self.cursor += 1;
    }
}

pub struct Marker {
    pos: usize,
    bomb: DropBomb,
}

impl Marker {
    pub fn complete(mut self, p: &mut Parser<'_>, kind: SyntaxKind) -> CompletedMarker {
        self.bomb.defuse();
        let event = &mut p.events[self.pos];
        *event = Event::StartNode(kind, None);
        p.events.push(Event::FinishNode);
        CompletedMarker {
            pos: self.pos
        }
    }
    pub fn abandon(mut self) {
        self.bomb.defuse();
    }
}

pub struct CompletedMarker {
    pos: usize,
}

impl CompletedMarker {
    pub fn precede(self, p: &mut Parser<'_>) -> Marker {
        let m = p.start();
        match p.events[self.pos] {
            Event::StartNode(_, ref mut forward_parent) => {
                *forward_parent = Some(m.pos);
            },
            _ => unreachable!(),
        }
        m
    }
}
