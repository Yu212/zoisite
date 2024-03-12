use la_arena::Arena;

use crate::ast;
use crate::hir::{BinaryOp, Expr, Root};
use crate::syntax_kind::SyntaxKind;

#[derive(Default)]
pub struct Database {
    pub exprs: Arena<Expr>,
}

impl Database {
    pub fn lower_root(&mut self, ast: ast::Root) -> Root {
        let expr = self.lower_expr(ast.expr());
        Root {
            expr: self.exprs.alloc(expr),
        }
    }
    pub fn lower_expr(&mut self, ast: Option<ast::Expr>) -> Expr {
        match ast {
            Some(ast::Expr::BinaryExpr(ast)) => self.lower_binary_expr(ast),
            Some(ast::Expr::Literal(ast)) => self.lower_literal(ast),
            None => Expr::Missing,
        }
    }
    pub fn lower_binary_expr(&mut self, ast: ast::BinaryExpr) -> Expr {
        let op = match ast.op().unwrap().kind() {
            SyntaxKind::Plus => BinaryOp::Add,
            SyntaxKind::Star => BinaryOp::Mul,
            _ => unreachable!(),
        };
        let lhs = self.lower_expr(ast.lhs());
        let rhs = self.lower_expr(ast.rhs());
        Expr::Binary {
            op,
            lhs: self.exprs.alloc(lhs),
            rhs: self.exprs.alloc(rhs),
        }
    }
    pub fn lower_literal(&mut self, ast: ast::Literal) -> Expr {
        Expr::Literal {
            n: ast.parse(),
        }
    }
}
