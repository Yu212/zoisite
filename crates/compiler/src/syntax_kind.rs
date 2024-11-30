#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    Error,
    Whitespace,
    Eof,
    LineComment,
    String,
    Char,
    EqEq,
    Neq,
    Ge,
    Le,
    Gt,
    Lt,
    And,
    Or,
    Comma,
    Dot,
    Semicolon,
    Question,
    Colon,
    Ident,
    TypedIdent,
    TypeSpec,
    IdentTypeSpec,
    ArrayTypeSpec,
    OptionTypeSpec,
    TupleTypeSpec,
    LetKw,
    IfKw,
    ElseKw,
    WhileKw,
    BreakKw,
    ContinueKw,
    FunKw,
    TrueKw,
    FalseKw,
    NoneKw,
    Equals,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Number,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    Root,
    FuncDef,
    ParamList,
    NoneLiteral,
    IntLiteral,
    FloatLiteral,
    BoolLiteral,
    StringLiteral,
    CharLiteral,
    ArrayLiteral,
    BinaryExpr,
    PrefixExpr,
    ParenExpr,
    TupleExpr,
    RefExpr,
    IfExpr,
    FnCallExpr,
    IndexExpr,
    BlockExpr,
    Stmt,
    EmptyStmt,
    LetStmt,
    WhileStmt,
    BreakStmt,
    ContinueStmt,
    ExprStmt,
}

impl SyntaxKind {
    pub fn is_eof(self) -> bool {
        self == Self::Eof
    }

    pub fn is_error(self) -> bool {
        self == Self::Error
    }

    pub fn is_trivia(self) -> bool {
        matches!(self, SyntaxKind::Whitespace | SyntaxKind::LineComment)
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}
