use rowan::ast::AstNode;

use crate::ast::Literal;
use crate::ast::Root;
use crate::diagnostic::{Diagnostic, DiagnosticKind};

pub fn validate(root: &Root) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    for node in root.syntax().descendants() {
        if let Some(literal) = Literal::cast(node) {
            validate_literal(literal, &mut diagnostics);
        }
    }
    diagnostics
}

fn validate_literal(literal: Literal, diagnostics: &mut Vec<Diagnostic>) {
    if literal.parse().is_none() {
        let range = literal.syntax().first_token().unwrap().text_range();
        diagnostics.push(Diagnostic::new(DiagnosticKind::NumberTooLarge, range));
    }
}
