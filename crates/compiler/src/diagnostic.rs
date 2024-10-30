use rowan::TextRange;
use crate::hir::BinaryOp;
use crate::r#type::Type;
use crate::syntax_kind::SyntaxKind;

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum DiagnosticKind {
    NumberTooLarge,
    UnterminatedStringLiteral,
    UnexpectedCharacter,
    UnexpectedToken {
        expected: Vec<SyntaxKind>,
        actual: SyntaxKind,
    },
    UndeclaredVariable,
    UndeclaredFunction,
    InvalidType,
    InvalidLhs,
    BreakOutsideLoop,
    ContinueOutsideLoop,
    TypeMismatched {
        ty1: Type,
        ty2: Type,
    },
    InvalidOperation {
        op: BinaryOp,
        ty1: Type,
        ty2: Type,
    },
}
