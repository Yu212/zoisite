use rowan::TextRange;

use crate::syntax_kind::SyntaxKind;

pub struct Token<'a> {
    pub kind: SyntaxKind,
    pub text: &'a str,
    pub range: TextRange,
}
