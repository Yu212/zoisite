use rowan::TextRange;

use crate::syntax_kind::SyntaxKind;

#[derive(Debug)]
pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub range: TextRange,
}

impl Diagnostic {
    pub fn new(kind: DiagnosticKind, range: TextRange) -> Self {
        Self {
            kind, range
        }
    }
}

#[derive(Debug)]
pub enum DiagnosticKind {
    NumberTooLarge,
    UnexpectedCharacter,
    UnexpectedToken {
        expected: Vec<SyntaxKind>,
        actual: SyntaxKind,
    },
    UndeclaredVariable,
    UndeclaredFunction,
    InvalidLhs,
    BreakOutsideLoop,
}
