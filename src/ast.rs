use rowan::SyntaxElement;

use crate::language::{SyntaxNode, SyntaxToken};
use crate::syntax_kind::SyntaxKind;

#[derive(Debug)]
pub struct Root(SyntaxNode);

impl Root {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::Root {
            Some(Self(node))
        } else {
            None
        }
    }

    pub fn syntax(&self) -> &SyntaxNode {
        &self.0
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
            SyntaxKind::InfixExpr => BinaryExpr::cast(node).map(Self::BinaryExpr),
            SyntaxKind::Literal => Literal::cast(node).map(Self::Literal),
            _ => None
        }
    }
}

#[derive(Debug)]
pub struct BinaryExpr(SyntaxNode);

impl BinaryExpr {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::InfixExpr {
            Some(Self(node))
        } else {
            None
        }
    }

    pub fn syntax(&self) -> &SyntaxNode {
        &self.0
    }

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

#[derive(Debug)]
pub struct Literal(SyntaxNode);

impl Literal {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::Literal {
            Some(Self(node))
        } else {
            None
        }
    }

    pub fn parse(&self) -> Option<u64> {
        self.0.first_token()?.text().parse().ok()
    }

    pub fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}
