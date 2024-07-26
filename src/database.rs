use std::mem;

use ecow::EcoString;
use la_arena::Arena;
use rowan::ast::AstNode;

use crate::ast;
use crate::ast::TypeSpec;
use crate::diagnostic::{Diagnostic, DiagnosticKind};
use crate::hir::{BinaryOp, Expr, Func, Identifier, Root, Stmt, UnaryOp};
use crate::language::SyntaxToken;
use crate::r#type::Type;
use crate::resolve_context::ResolveContext;
use crate::syntax_kind::SyntaxKind;

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
    pub fn lower_root(&mut self, ast: ast::Root) -> (Root, Vec<Diagnostic>) {
        self.resolve_ctx.define_builtins();
        let root = Root {
            stmts: ast.stmts().map(|stmt| {
                let temp = self.lower_stmt(stmt);
                self.stmts.alloc(temp)
            }).collect(),
        };
        (root, mem::take(&mut self.diagnostics))
    }
    pub fn lower_func(&mut self, ast: ast::FuncDef) -> Stmt {
        self.resolve_ctx.push_scope(true);
        let params: Vec<_> = ast.param_list()
            .flat_map(|param| self.lower_ident(param.ident()))
            .collect();
        let params_ty: Vec<_> = ast.param_list()
            .map(|param| self.lower_type(param.type_spec()))
            .collect();
        let return_ty = self.lower_type(ast.return_ty());
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
        let ty = self.lower_type(ast.type_spec());
        let var_id = if let Some(name) = name {
            Some(self.resolve_ctx.define_var(name.name.clone(), ty))
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
    pub fn lower_type(&mut self, ast: Option<TypeSpec>) -> Type {
        match ast {
            Some(ast::TypeSpec::IdentTypeSpec(ast)) => self.lower_ident_type(ast),
            Some(ast::TypeSpec::ArrayTypeSpec(ast)) => self.lower_array_type(ast),
            None => Type::Invalid,
        }
    }
    pub fn lower_ident_type(&mut self, ast: ast::IdentTypeSpec) -> Type {
        let name = self.lower_ident(ast.ident()).map(|ident| ident.name);
        if let Some(name) = name {
            self.resolve_ctx.resolve_ty(&name)
        } else {
            let range = ast.syntax().text_range();
            self.diagnostics.push(Diagnostic::new(DiagnosticKind::InvalidType, Some(range)));
            Type::Invalid
        }
    }
    pub fn lower_array_type(&mut self, ast: ast::ArrayTypeSpec) -> Type {
        let inner_ty = self.lower_type(ast.inner_ty());
        Type::Array(Box::new(inner_ty))
    }
    pub fn lower_expr(&mut self, ast: Option<ast::Expr>) -> Expr {
        match ast {
            Some(ast::Expr::BinaryExpr(ast)) => self.lower_binary_expr(ast),
            Some(ast::Expr::PrefixExpr(ast)) => self.lower_prefix_expr(ast),
            Some(ast::Expr::ParenExpr(ast)) => self.lower_expr(ast.expr()),
            Some(ast::Expr::RefExpr(ast)) => self.lower_ref_expr(ast),
            Some(ast::Expr::IfExpr(ast)) => self.lower_if_expr(ast),
            Some(ast::Expr::FnCallExpr(ast)) => self.lower_fn_call_expr(ast),
            Some(ast::Expr::IndexExpr(ast)) => self.lower_index_expr(ast),
            Some(ast::Expr::BlockExpr(ast)) => self.lower_block_expr(ast),
            Some(ast::Expr::NumberLiteral(ast)) => self.lower_number_literal(ast),
            Some(ast::Expr::BoolLiteral(ast)) => self.lower_bool_literal(ast),
            Some(ast::Expr::StringLiteral(ast)) => self.lower_string_literal(ast),
            Some(ast::Expr::ArrayLiteral(ast)) => self.lower_array_literal(ast),
            None => Expr::Missing,
        }
    }
    pub fn is_lvalue(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Ref { var_id: _ } => true,
            Expr::Index { main_expr, index_expr: _ } => self.is_lvalue(&self.exprs[*main_expr]),
            _ => false,
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
            SyntaxKind::Ge => BinaryOp::Ge,
            SyntaxKind::Le => BinaryOp::Le,
            SyntaxKind::Gt => BinaryOp::Gt,
            SyntaxKind::Lt => BinaryOp::Lt,
            _ => unreachable!(),
        };
        let lhs = self.lower_expr(ast.lhs());
        let rhs = self.lower_expr(ast.rhs());
        if op == BinaryOp::Assign && !self.is_lvalue(&lhs) {
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
    pub fn lower_index_expr(&mut self, ast: ast::IndexExpr) -> Expr {
        let main_expr = self.lower_expr(ast.main());
        let index_expr = self.lower_expr(ast.index());
        Expr::Index {
            main_expr: self.exprs.alloc(main_expr),
            index_expr: self.exprs.alloc(index_expr),
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
    pub fn lower_number_literal(&mut self, ast: ast::NumberLiteral) -> Expr {
        let parsed = ast.parse();
        if parsed.is_none() {
            let range = ast.syntax().first_token().unwrap().text_range();
            self.diagnostics.push(Diagnostic::new(DiagnosticKind::NumberTooLarge, Some(range)));
        }
        Expr::NumberLiteral {
            n: parsed,
        }
    }
    pub fn lower_bool_literal(&mut self, ast: ast::BoolLiteral) -> Expr {
        let parsed = ast.parse();
        Expr::BoolLiteral {
            val: parsed,
        }
    }
    pub fn lower_string_literal(&mut self, ast: ast::StringLiteral) -> Expr {
        Expr::StringLiteral {
            val: ast.parse(),
        }
    }
    pub fn lower_array_literal(&mut self, ast: ast::ArrayLiteral) -> Expr {
        let len = self.lower_expr(ast.len());
        let initial = self.lower_expr(ast.initial());
        Expr::ArrayLiteral {
            len: self.exprs.alloc(len),
            initial: self.exprs.alloc(initial),
        }
    }
    pub fn lower_ident(&mut self, ast: Option<SyntaxToken>) -> Option<Identifier> {
        Some(Identifier {
            name: EcoString::from(ast?.text()),
        })
    }
}
