use ecow::EcoString;
use la_arena::Arena;
use rowan::ast::AstNode;

use crate::ast;
use crate::diagnostic::{Diagnostic, DiagnosticKind};
use crate::hir::{BinaryOp, Expr, Identifier, Root, Stmt, UnaryOp};
use crate::language::SyntaxToken;
use crate::resolve_context::ResolveContext;
use crate::syntax_kind::SyntaxKind;

pub struct Database {
    pub exprs: Arena<Expr>,
    pub stmts: Arena<Stmt>,
    pub resolve_ctx: ResolveContext,
    pub diagnostics: Vec<Diagnostic>,
}

impl Database {
    pub fn new() -> Self {
        Database {
            exprs: Arena::default(),
            stmts: Arena::default(),
            resolve_ctx: ResolveContext::new(),
            diagnostics: Vec::new(),
        }
    }
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
        let var_id = self.lower_ident(ast.name()).map(|ident| self.resolve_ctx.define_var(ident.name.clone()));
        Stmt::LetStmt {
            var_id,
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
            Some(ast::Expr::RefExpr(ast)) => self.lower_ref_expr(ast),
            Some(ast::Expr::FnCallExpr(ast)) => self.lower_fn_call_expr(ast),
            Some(ast::Expr::BlockExpr(ast)) => self.lower_block_expr(ast),
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
    pub fn lower_ref_expr(&mut self, ast: ast::RefExpr) -> Expr {
        let var_info = self.lower_ident(ast.ident()).and_then(|ident| self.resolve_ctx.resolve_var(&ident.name));
        if var_info.is_none() {
            let range = ast.syntax().text_range();
            self.diagnostics.push(Diagnostic::new(DiagnosticKind::UndeclaredVariable, range));
        }
        let var_id = var_info.map(|info| info.id);
        Expr::Ref {
            var_id,
        }
    }
    pub fn lower_fn_call_expr(&mut self, ast: ast::FnCallExpr) -> Expr {
        let args: Vec<_> = ast.args().map(|expr| {
            let temp = self.lower_expr(Some(expr));
            self.exprs.alloc(temp)
        }).collect();
        let fn_info = self.lower_ident(ast.ident()).and_then(|ident| self.resolve_ctx.resolve_fn(&ident.name, args.len()));
        if fn_info.is_none() {
            let range = ast.syntax().text_range();
            self.diagnostics.push(Diagnostic::new(DiagnosticKind::UndeclaredFunction, range));
        }
        let fn_id = fn_info.map(|info| info.id);
        Expr::FnCall {
            fn_id,
            args,
        }
    }
    pub fn lower_block_expr(&mut self, ast: ast::BlockExpr) -> Expr {
        self.resolve_ctx.push_scope();
        let stmts = ast.stmts().map(|stmt| {
            let temp = self.lower_stmt(stmt);
            self.stmts.alloc(temp)
        }).collect();
        self.resolve_ctx.pop_scope();
        Expr::Block {
            stmts,
        }
    }
    pub fn lower_literal(&mut self, ast: ast::Literal) -> Expr {
        let parsed = ast.parse();
        if parsed.is_none() {
            let range = ast.syntax().first_token().unwrap().text_range();
            self.diagnostics.push(Diagnostic::new(DiagnosticKind::NumberTooLarge, range));
        }
        Expr::Literal {
            n: parsed,
        }
    }
    pub fn lower_ident(&mut self, ast: Option<SyntaxToken>) -> Option<Identifier> {
        Some(Identifier {
            name: EcoString::from(ast?.text()),
        })
    }
}
