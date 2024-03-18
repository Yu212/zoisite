#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    Error,
    Eof,
    Root,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    BinaryExpr,
    Number,
    Literal,
    PrefixExpr,
    Whitespace,
}

impl SyntaxKind {
    pub fn is_eof(self) -> bool {
        self == Self::Eof
    }

    pub fn is_error(self) -> bool {
        self == Self::Error
    }

    pub fn is_trivia(self) -> bool {
        matches!(self, SyntaxKind::Whitespace)
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}
