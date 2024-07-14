use rowan::ast::AstNode;
use rowan::SyntaxElement;

use crate::language::{MyLanguage, SyntaxNode, SyntaxToken};
use crate::syntax_kind::SyntaxKind;

macro_rules! asts {
    () => {};
    ($name:ident; $($rest:tt)*) => {
        ast!($name);
        asts!($($rest)*);
    };
    ($name:ident $body:tt; $($rest:tt)*) => {
        ast!($name $body);
        asts!($($rest)*);
    };
}

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
    ($name:ident [$($item:ident,)*]) => {
        #[derive(Debug)]
        pub enum $name {
            $($item($item),)*
        }

        impl AstNode for $name {
            type Language = MyLanguage;

            fn can_cast(kind: SyntaxKind) -> bool {
                matches!(kind, $(SyntaxKind::$item)|*)
            }

            fn cast(node: SyntaxNode) -> Option<Self> {
                match node.kind() {
                    $(SyntaxKind::$item => $item::cast(node).map(Self::$item),)*
                    _ => None,
                }
            }

            fn syntax(&self) -> &SyntaxNode {
                match self {
                    $(Self::$item(v) => v.syntax(),)*
                }
            }
        }
    };
}

asts! {
    Root;
    Stmt [
        LetStmt,
        WhileStmt,
        BreakStmt,
        ExprStmt,
        FuncDef,
    ];
    FuncDef;
    LetStmt;
    WhileStmt;
    BreakStmt;
    ExprStmt;
    TypeSpec [
        IdentTypeSpec,
        ArrayTypeSpec,
    ];
    IdentTypeSpec;
    ArrayTypeSpec;
    Expr [
        BinaryExpr,
        PrefixExpr,
        ParenExpr,
        RefExpr,
        IfExpr,
        FnCallExpr,
        BlockExpr,
        NumberLiteral,
        BoolLiteral,
    ];
    BinaryExpr;
    PrefixExpr;
    ParenExpr;
    RefExpr;
    IfExpr;
    FnCallExpr;
    BlockExpr;
    NumberLiteral;
    BoolLiteral;
    TypedIdent;
}

impl Root {
    pub fn stmts(&self) -> impl Iterator<Item = Stmt> {
        self.0.children().filter_map(Stmt::cast)
    }
}

impl FuncDef {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.0.children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn param_list(&self) -> impl Iterator<Item = TypedIdent> {
        self.0.children()
            .filter(|node| node.kind() == SyntaxKind::ParamList)
            .flat_map(|node| node.children())
            .filter_map(TypedIdent::cast)
    }

    pub fn return_ty(&self) -> Option<TypeSpec> {
        self.0.children().find_map(TypeSpec::cast)
    }

    pub fn block(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl LetStmt {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.0.children().find_map(TypedIdent::cast).and_then(|typed_ident| typed_ident.ident())
    }

    pub fn type_spec(&self) -> Option<TypeSpec> {
        self.0.children().find_map(TypedIdent::cast)
            .and_then(|typed_ident| typed_ident.type_spec())
    }

    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl WhileStmt {
    pub fn cond(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub fn block(&self) -> Option<Expr> {
        self.0.children().filter_map(Expr::cast).nth(1)
    }
}

impl ExprStmt {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl IdentTypeSpec {
    pub fn ident(&self) -> Option<SyntaxToken> {
        self.0.children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }
}

impl ArrayTypeSpec {
    pub fn inner_ty(&self) -> Option<TypeSpec> {
        self.0.children().find_map(TypeSpec::cast)
    }

    pub fn len(&self) -> Option<u64> {
        self.0.children().find_map(NumberLiteral::cast).and_then(|literal| literal.parse())
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
            .find(|token| matches!(token.kind(), SyntaxKind::Plus | SyntaxKind::Minus | SyntaxKind::Star | SyntaxKind::Slash | SyntaxKind::Percent | SyntaxKind::Equals | SyntaxKind::EqEq | SyntaxKind::Neq))
    }
}

impl PrefixExpr {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub fn op(&self) -> Option<SyntaxToken> {
        self.0.children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Minus)
    }
}

impl ParenExpr {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl RefExpr {
    pub fn ident(&self) -> Option<SyntaxToken> {
        self.0.children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }
}

impl IfExpr {
    pub fn cond(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub fn then_expr(&self) -> Option<Expr> {
        self.0.children().filter_map(Expr::cast).nth(1)
    }

    pub fn else_expr(&self) -> Option<Expr> {
        self.0.children().filter_map(Expr::cast).nth(2)
    }
}

impl FnCallExpr {
    pub fn ident(&self) -> Option<SyntaxToken> {
        self.0.children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn args(&self) -> impl Iterator<Item = Expr> {
        self.0.children().filter_map(Expr::cast)
    }
}

impl BlockExpr {
    pub fn stmts(&self) -> impl Iterator<Item = Stmt> {
        self.0.children().filter_map(Stmt::cast)
    }
}

impl NumberLiteral {
    pub fn parse(&self) -> Option<u64> {
        self.0.first_token()?.text().parse().ok()
    }
}

impl BoolLiteral {
    pub fn parse(&self) -> bool {
        match self.0.first_token().unwrap().text() {
            "true" => true,
            "false" => false,
            _ => unreachable!(),
        }
    }
}

impl TypedIdent {
    pub fn ident(&self) -> Option<SyntaxToken> {
        self.0.children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SyntaxKind::Ident)
    }

    pub fn type_spec(&self) -> Option<TypeSpec> {
        self.0.children().find_map(TypeSpec::cast)
    }
}
