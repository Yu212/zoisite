use rowan::ast::AstNode;
use rowan::SyntaxElement;

use crate::language::{MyLanguage, SyntaxNode, SyntaxToken};
use crate::syntax_kind::SyntaxKind;

macro_rules! ast {
    ($name:ident) => {
        #[derive(Debug)]
        pub struct $name(SyntaxNode);

        impl AstNode for $name {
            type Language = MyLanguage;

            fn can_cast(kind: SyntaxKind) -> bool {
                kind == SyntaxKind::$name
            }

            fn cast(node: SyntaxNode) -> Option<Self> {
                if node.kind() == SyntaxKind::$name {
                    Some(Self(node))
                } else {
                    None
                }
            }

            fn syntax(&self) -> &SyntaxNode {
                &self.0
            }
        }
    };
}

ast!(Root);
ast!(BinaryExpr);
ast!(Literal);

impl Root {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

#[derive(Debug)]
pub enum Expr {
    BinaryExpr(BinaryExpr),
    Literal(Literal),
}

impl Expr {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::BinaryExpr => BinaryExpr::cast(node).map(Self::BinaryExpr),
            SyntaxKind::Literal => Literal::cast(node).map(Self::Literal),
            _ => None
        }
    }
}

impl BinaryExpr {
    pub fn lhs(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub fn rhs(&self) -> Option<Expr> {
        self.0.children().filter_map(Expr::cast).nth(1)
    }

    pub fn op(&self) -> Option<SyntaxToken> {
        self.0.children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| matches!(token.kind(), SyntaxKind::Plus | SyntaxKind::Star))
    }
}

impl Literal {
    pub fn parse(&self) -> Option<u64> {
        self.0.first_token()?.text().parse().ok()
    }
}
