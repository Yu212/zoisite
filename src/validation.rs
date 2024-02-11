use rowan::ast::AstNode;

use crate::ast::Literal;
use crate::ast::Root;
use crate::syntax_error::SyntaxError;

pub fn validate(root: &Root) -> Vec<SyntaxError> {
    let mut errors = Vec::new();
    for node in root.syntax().descendants() {
        if let Some(literal) = Literal::cast(node) {
            validate_literal(literal, &mut errors);
        }
    }
    errors
}

fn validate_literal(literal: Literal, errors: &mut Vec<SyntaxError>) {
    if literal.parse().is_none() {
        errors.push(SyntaxError {
            message: format!("number too large"),
            range: literal.syntax().first_token().unwrap().text_range(),
        });
    }
}
