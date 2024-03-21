use la_arena::Arena;

use crate::ast;
use crate::hir::{BinaryOp, Expr, Root, Stmt, UnaryOp};
use crate::syntax_kind::SyntaxKind;

#[derive(Default)]
pub struct Database {
    pub exprs: Arena<Expr>,
    pub stmts: Arena<Stmt>,
}

impl Database {
    pub fn lower_root(&mut self, ast: ast::Root) -> Root {
        Root {
            stmts: ast.stmts().map(|stmt| {
                let temp = self.lower_stmt(stmt);
                self.stmts.alloc(temp)
            }).collect(),
        }
    }
    pub fn lower_stmt(&mut self, ast: ast::Stmt) -> Stmt {
        match ast {
            ast::Stmt::LetStmt(ast) => self.lower_let_stmt(ast),
            ast::Stmt::ExprStmt(ast) => self.lower_expr_stmt(ast),
        }
    }
    pub fn lower_let_stmt(&mut self, ast: ast::LetStmt) -> Stmt {
        let expr = self.lower_expr(ast.expr());
        Stmt::LetStmt {
            name: ast.name().and_then(|ident| ident.value()),
            expr: self.exprs.alloc(expr),
        }
    }
    pub fn lower_expr_stmt(&mut self, ast: ast::ExprStmt) -> Stmt {
        let expr = self.lower_expr(ast.expr());
        Stmt::ExprStmt {
            expr: self.exprs.alloc(expr),
        }
    }
    pub fn lower_expr(&mut self, ast: Option<ast::Expr>) -> Expr {
        match ast {
            Some(ast::Expr::BinaryExpr(ast)) => self.lower_binary_expr(ast),
            Some(ast::Expr::PrefixExpr(ast)) => self.lower_prefix_expr(ast),
            Some(ast::Expr::ParenExpr(ast)) => self.lower_expr(ast.expr()),
            Some(ast::Expr::Literal(ast)) => self.lower_literal(ast),
            None => Expr::Missing,
        }
    }
    pub fn lower_binary_expr(&mut self, ast: ast::BinaryExpr) -> Expr {
        let op = match ast.op().unwrap().kind() {
            SyntaxKind::Plus => BinaryOp::Add,
            SyntaxKind::Minus => BinaryOp::Sub,
            SyntaxKind::Star => BinaryOp::Mul,
            SyntaxKind::Slash => BinaryOp::Div,
            SyntaxKind::Percent => BinaryOp::Rem,
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
    pub fn lower_prefix_expr(&mut self, ast: ast::PrefixExpr) -> Expr {
        let expr = self.lower_expr(ast.expr());
        Expr::Unary {
            op: UnaryOp::Neg,
            expr: self.exprs.alloc(expr),
        }
    }
    pub fn lower_literal(&mut self, ast: ast::Literal) -> Expr {
        Expr::Literal {
            n: ast.parse(),
        }
    }
}
