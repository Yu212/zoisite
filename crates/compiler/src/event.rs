use std::mem;

use rowan::GreenNodeBuilder;

use crate::diagnostic::Diagnostic;
use crate::language::SyntaxNode;
use crate::syntax_kind::SyntaxKind;
use crate::token::Token;

pub enum Event {
    Placeholder,
    StartNode(SyntaxKind, Option<usize>),
    FinishNode,
    Token(SyntaxKind),
    Error(Diagnostic),
}

pub fn process(events: &mut Vec<Event>, tokens: Vec<Token>) -> (SyntaxNode, Vec<Diagnostic>) {
    let mut diagnostics = Vec::new();
    let mut builder = GreenNodeBuilder::new();
    let mut forward_parents = Vec::new();
    let mut idx = 0;
    for i in 0..events.len() {
        match mem::replace(&mut events[i], Event::Placeholder) {
            Event::StartNode(kind, forward_parent) => {
                forward_parents.push(kind);
                let mut fp = forward_parent;
                while let Some(next) = fp {
                    fp = match mem::replace(&mut events[next], Event::Placeholder) {
                        Event::StartNode(kind, forward_parent) => {
                            forward_parents.push(kind);
                            forward_parent
                        },
                        _ => unreachable!()
                    }
                }
                for kind in forward_parents.drain(..).rev() {
                    builder.start_node(kind.into());
                }
            },
            Event::FinishNode => builder.finish_node(),
            Event::Token(kind) => {
                builder.token(kind.into(), tokens[idx].text);
                idx += 1;
            },
            Event::Error(diagnostic) => diagnostics.push(diagnostic),
            Event::Placeholder => {},
        }
    }
    (SyntaxNode::new_root(builder.finish()), diagnostics)
}
