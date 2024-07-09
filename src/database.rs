use ecow::EcoString;
use la_arena::Arena;
use rowan::ast::AstNode;

use crate::ast;
use crate::diagnostic::{Diagnostic, DiagnosticKind};
use crate::hir::{BinaryOp, Expr, Func, Identifier, Root, Stmt, UnaryOp};
use crate::language::SyntaxToken;
use crate::resolve_context::ResolveContext;
use crate::syntax_kind::SyntaxKind;
use crate::type_checker::Type;

pub struct Database {
    pub exprs: Arena<Expr>,
    pub stmts: Arena<Stmt>,
    pub funcs: Arena<Func>,
    pub resolve_ctx: ResolveContext,
    pub diagnostics: Vec<Diagnostic>,
    pub loop_nest: usize,
}

impl Database {
    pub fn new() -> Self {
        Database {
            exprs: Arena::default(),
            stmts: Arena::default(),
            funcs: Arena::default(),
            resolve_ctx: ResolveContext::new(),
            diagnostics: Vec::new(),
            loop_nest: 0,
        }
    }
    pub fn lower_root(&mut self, ast: ast::Root) -> Root {
        self.resolve_ctx.define_builtins();
        Root {
            stmts: ast.stmts().map(|stmt| {
                let temp = self.lower_stmt(stmt);
                self.stmts.alloc(temp)
            }).collect(),
        }
    }
    pub fn lower_func(&mut self, ast: ast::FuncDef) -> Stmt {
        self.resolve_ctx.push_scope(true);
        let params: Vec<_> = ast.param_list()
            .flat_map(|param| self.lower_ident(param.ident()))
            .collect();
        let params_ty_ident: Vec<_> = ast.param_list()
            .map(|param| self.lower_ident(param.ty()))
            .collect();
        let params_ty: Vec<_> = params_ty_ident.iter()
            .map(|ident| ident.as_ref().map_or(Type::Invalid, |ident| self.resolve_ctx.resolve_ty(&ident.name)))
            .collect();
        let return_ty = self.lower_ident(ast.return_ty())
            .map_or(Type::Invalid, |ident| self.resolve_ctx.resolve_ty(&ident.name));
        let params: Vec<_> = params.iter().zip(params_ty)
            .map(|(ident, ty)| self.resolve_ctx.define_var(ident.name.clone(), ty))
            .collect();
        let name = self.lower_ident(ast.name()).map(|ident| ident.name);
        let fn_info = name.map(|name| self.resolve_ctx.define_fn(name.clone(), params, return_ty));
        let block = self.lower_expr(ast.block());
        let func = Func {
            fn_info,
            block: self.exprs.alloc(block)
        };
        self.resolve_ctx.pop_scope();
        Stmt::FuncDef {
            func: self.funcs.alloc(func),
        }
    }
    pub fn lower_stmt(&mut self, ast: ast::Stmt) -> Stmt {
        match ast {
            ast::Stmt::LetStmt(ast) => self.lower_let_stmt(ast),
            ast::Stmt::WhileStmt(ast) => self.lower_while_stmt(ast),
            ast::Stmt::BreakStmt(ast) => self.lower_break_stmt(ast),
            ast::Stmt::ExprStmt(ast) => self.lower_expr_stmt(ast),
            ast::Stmt::FuncDef(ast) => self.lower_func(ast),
        }
    }
    pub fn lower_let_stmt(&mut self, ast: ast::LetStmt) -> Stmt {
        let expr = self.lower_expr(ast.expr());
        let name = self.lower_ident(ast.name());
        let ty = self.lower_ident(ast.ty());
        let var_id = if let (Some(name), Some(ty)) = (name, ty) {
            let ty_id = self.resolve_ctx.resolve_ty(&ty.name);
            Some(self.resolve_ctx.define_var(name.name.clone(), ty_id))
        } else {
            None
        };
        Stmt::LetStmt {
            var_id,
            expr: self.exprs.alloc(expr),
        }
    }
    pub fn lower_while_stmt(&mut self, ast: ast::WhileStmt) -> Stmt {
        self.loop_nest += 1;
        let cond = self.lower_expr(ast.cond());
        let block = self.lower_expr(ast.block());
        self.loop_nest -= 1;
        Stmt::WhileStmt {
            cond: self.exprs.alloc(cond),
            block: self.exprs.alloc(block),
        }
    }
    pub fn lower_break_stmt(&mut self, ast: ast::BreakStmt) -> Stmt {
        if self.loop_nest == 0 {
            let range = ast.syntax().text_range();
            self.diagnostics.push(Diagnostic::new(DiagnosticKind::BreakOutsideLoop, Some(range)));
        }
        Stmt::BreakStmt {
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
            Some(ast::Expr::IfExpr(ast)) => self.lower_if_expr(ast),
            Some(ast::Expr::FnCallExpr(ast)) => self.lower_fn_call_expr(ast),
            Some(ast::Expr::BlockExpr(ast)) => self.lower_block_expr(ast),
            Some(ast::Expr::Literal(ast)) => self.lower_literal(ast),
            Some(ast::Expr::BoolLiteral(ast)) => self.lower_bool_literal(ast),
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
            SyntaxKind::Equals => BinaryOp::Assign,
            SyntaxKind::EqEq => BinaryOp::EqEq,
            SyntaxKind::Neq => BinaryOp::Neq,
            _ => unreachable!(),
        };
        let lhs = self.lower_expr(ast.lhs());
        let rhs = self.lower_expr(ast.rhs());
        if let Expr::Ref { var_id: _ } = lhs {
        } else if op == BinaryOp::Assign {
            let range = ast.syntax().text_range();
            self.diagnostics.push(Diagnostic::new(DiagnosticKind::InvalidLhs, Some(range)));
        }
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
        let var_id = self.lower_ident(ast.ident()).and_then(|ident| self.resolve_ctx.resolve_var(&ident.name));
        if var_id.is_none() {
            let range = ast.syntax().text_range();
            self.diagnostics.push(Diagnostic::new(DiagnosticKind::UndeclaredVariable, Some(range)));
        }
        Expr::Ref {
            var_id,
        }
    }
    pub fn lower_if_expr(&mut self, ast: ast::IfExpr) -> Expr {
        let cond = self.lower_expr(ast.cond());
        let then_expr = self.lower_expr(ast.then_expr());
        let else_expr = ast.else_expr().map(|expr| self.lower_expr(Some(expr)));
        Expr::If {
            cond: self.exprs.alloc(cond),
            then_expr: self.exprs.alloc(then_expr),
            else_expr: else_expr.map(|expr| self.exprs.alloc(expr)),
        }
    }
    pub fn lower_fn_call_expr(&mut self, ast: ast::FnCallExpr) -> Expr {
        let args: Vec<_> = ast.args().map(|expr| {
            let temp = self.lower_expr(Some(expr));
            self.exprs.alloc(temp)
        }).collect();
        let fn_id = self.lower_ident(ast.ident()).and_then(|ident| self.resolve_ctx.resolve_fn(&ident.name, args.len()));
        if fn_id.is_none() {
            let range = ast.syntax().text_range();
            self.diagnostics.push(Diagnostic::new(DiagnosticKind::UndeclaredFunction, Some(range)));
        }
        Expr::FnCall {
            fn_id,
            args,
        }
    }
    pub fn lower_block_expr(&mut self, ast: ast::BlockExpr) -> Expr {
        self.resolve_ctx.push_scope(false);
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
            self.diagnostics.push(Diagnostic::new(DiagnosticKind::NumberTooLarge, Some(range)));
        }
        Expr::Literal {
            n: parsed,
        }
    }
    pub fn lower_bool_literal(&mut self, ast: ast::BoolLiteral) -> Expr {
        let parsed = ast.parse();
        Expr::BoolLiteral {
            val: parsed,
        }
    }
    pub fn lower_ident(&mut self, ast: Option<SyntaxToken>) -> Option<Identifier> {
        Some(Identifier {
            name: EcoString::from(ast?.text()),
        })
    }
}
