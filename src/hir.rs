use crate::ast;
use crate::syntax_kind::SyntaxKind;

#[derive(Debug)]
pub struct Root {
    pub expr: Expr,
}

#[derive(Debug)]
pub enum Expr {
    Missing,
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Literal {
        n: Option<u64>,
    }
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Mul,
}

impl BinaryOp {
    pub fn binding_power(&self) -> (u8, u8) {
        match self {
            BinaryOp::Add => (1, 2),
            BinaryOp::Mul => (3, 4),
        }
    }
}

pub fn lower_root(ast: ast::Root) -> Root {
    Root {
        expr: lower_expr(ast.expr()),
    }
}

pub fn lower_expr(ast: Option<ast::Expr>) -> Expr {
    match ast {
        Some(ast::Expr::BinaryExpr(ast)) => lower_binary_expr(ast),
        Some(ast::Expr::Literal(ast)) => lower_literal(ast),
        None => Expr::Missing,
    }
}

pub fn lower_binary_expr(ast: ast::BinaryExpr) -> Expr {
    let op = match ast.op().unwrap().kind() {
        SyntaxKind::Plus => BinaryOp::Add,
        SyntaxKind::Star => BinaryOp::Mul,
        _ => unreachable!(),
    };
    let lhs = lower_expr(ast.lhs());
    let rhs = lower_expr(ast.rhs());
    Expr::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn lower_literal(ast: ast::Literal) -> Expr {
    Expr::Literal {
        n: ast.parse(),
    }
}
