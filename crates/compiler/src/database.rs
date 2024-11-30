use std::mem;

use ecow::EcoString;
use la_arena::Arena;
use rowan::ast::AstNode;

use crate::ast;
use crate::ast::TypeSpec;
use crate::diagnostic::{Diagnostic, DiagnosticKind};
use crate::hir::{BinaryOp, Expr, ExprIdx, Func, Identifier, Root, Stmt, UnaryOp};
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
            range: ast.syntax().text_range(),
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
            .map(|(ident, ty)| self.resolve_ctx.define_var(ident.name.clone(), Some(ident.clone()), Some(ty)))
            .collect();
        let name = self.lower_ident(ast.name()).map(|ident| ident.name);
        let fn_info = name.map(|name| self.resolve_ctx.define_fn(name.clone(), params, return_ty));
        let block = self.lower_expr(ast.block());
        let func = Func {
            fn_info,
            block,
            range: ast.syntax().text_range(),
        };
        self.resolve_ctx.pop_scope();
        Stmt::FuncDef {
            func: self.funcs.alloc(func),
            range: ast.syntax().text_range(),
        }
    }
    pub fn lower_stmt(&mut self, ast: ast::Stmt) -> Stmt {
        match ast {
            ast::Stmt::EmptyStmt(ast) => self.lower_empty_stmt(ast),
            ast::Stmt::LetStmt(ast) => self.lower_let_stmt(ast),
            ast::Stmt::WhileStmt(ast) => self.lower_while_stmt(ast),
            ast::Stmt::BreakStmt(ast) => self.lower_break_stmt(ast),
            ast::Stmt::ContinueStmt(ast) => self.lower_continue_stmt(ast),
            ast::Stmt::ExprStmt(ast) => self.lower_expr_stmt(ast),
            ast::Stmt::FuncDef(ast) => self.lower_func(ast),
        }
    }
    pub fn lower_empty_stmt(&mut self, ast: ast::EmptyStmt) -> Stmt {
        Stmt::EmptyStmt {
            range: ast.syntax().text_range(),
        }
    }
    pub fn lower_let_stmt(&mut self, ast: ast::LetStmt) -> Stmt {
        let expr = self.lower_expr(ast.expr());
        let name = self.lower_ident(ast.name());
        let ty = ast.type_spec().map(|type_spec| self.lower_type(Some(type_spec)));
        let var_id = if let Some(name) = name {
            Some(self.resolve_ctx.define_var(name.clone().name, Some(name), ty))
        } else {
            None
        };
        Stmt::LetStmt {
            var_id,
            expr,
            range: ast.syntax().text_range(),
        }
    }
    pub fn lower_while_stmt(&mut self, ast: ast::WhileStmt) -> Stmt {
        self.loop_nest += 1;
        let cond = self.lower_expr(ast.cond());
        let block = self.lower_expr(ast.block());
        self.loop_nest -= 1;
        Stmt::WhileStmt {
            cond,
            block,
            range: ast.syntax().text_range(),
        }
    }
    pub fn lower_break_stmt(&mut self, ast: ast::BreakStmt) -> Stmt {
        if self.loop_nest == 0 {
            let range = ast.syntax().text_range();
            self.diagnostics.push(Diagnostic::new(DiagnosticKind::BreakOutsideLoop, range));
        }
        Stmt::BreakStmt {
            range: ast.syntax().text_range(),
        }
    }
    pub fn lower_continue_stmt(&mut self, ast: ast::ContinueStmt) -> Stmt {
        if self.loop_nest == 0 {
            let range = ast.syntax().text_range();
            self.diagnostics.push(Diagnostic::new(DiagnosticKind::ContinueOutsideLoop, range));
        }
        Stmt::ContinueStmt {
            range: ast.syntax().text_range(),
        }
    }
    pub fn lower_expr_stmt(&mut self, ast: ast::ExprStmt) -> Stmt {
        let expr = self.lower_expr(ast.expr());
        Stmt::ExprStmt {
            expr,
            range: ast.syntax().text_range(),
        }
    }
    pub fn lower_type(&mut self, ast: Option<TypeSpec>) -> Type {
        match ast {
            Some(TypeSpec::IdentTypeSpec(ast)) => self.lower_ident_type(ast),
            Some(TypeSpec::ArrayTypeSpec(ast)) => self.lower_array_type(ast),
            Some(TypeSpec::OptionTypeSpec(ast)) => self.lower_option_type(ast),
            Some(TypeSpec::TupleTypeSpec(ast)) => self.lower_tuple_type(ast),
            None => Type::Invalid,
        }
    }
    pub fn lower_ident_type(&mut self, ast: ast::IdentTypeSpec) -> Type {
        let name = self.lower_ident(ast.ident()).map(|ident| ident.name);
        if let Some(name) = name {
            self.resolve_ctx.resolve_ty(&name)
        } else {
            let range = ast.syntax().text_range();
            self.diagnostics.push(Diagnostic::new(DiagnosticKind::InvalidType, range));
            Type::Invalid
        }
    }
    pub fn lower_array_type(&mut self, ast: ast::ArrayTypeSpec) -> Type {
        let inner_ty = self.lower_type(ast.inner_ty());
        inner_ty.wrap_in_array()
    }
    pub fn lower_option_type(&mut self, ast: ast::OptionTypeSpec) -> Type {
        let inner_ty = self.lower_type(ast.inner_ty());
        inner_ty.wrap_in_option()
    }
    pub fn lower_tuple_type(&mut self, ast: ast::TupleTypeSpec) -> Type {
        Type::Tuple(ast.inner_tys().map(|inner_ty| self.lower_type(Some(inner_ty))).collect::<Vec<_>>())
    }
    pub fn lower_expr(&mut self, ast: Option<ast::Expr>) -> ExprIdx {
        let expr = match ast {
            Some(ast::Expr::BinaryExpr(ast)) => self.lower_binary_expr(ast),
            Some(ast::Expr::PrefixExpr(ast)) => self.lower_prefix_expr(ast),
            Some(ast::Expr::ParenExpr(ast)) => return self.lower_expr(ast.expr()),
            Some(ast::Expr::TupleExpr(ast)) => self.lower_tuple_expr(ast),
            Some(ast::Expr::RefExpr(ast)) => self.lower_ref_expr(ast),
            Some(ast::Expr::IfExpr(ast)) => self.lower_if_expr(ast),
            Some(ast::Expr::FnCallExpr(ast)) => self.lower_fn_call_expr(ast),
            Some(ast::Expr::IndexExpr(ast)) => self.lower_index_expr(ast),
            Some(ast::Expr::BlockExpr(ast)) => self.lower_block_expr(ast),
            Some(ast::Expr::NoneLiteral(ast)) => self.lower_none_literal(ast),
            Some(ast::Expr::IntLiteral(ast)) => self.lower_int_literal(ast),
            Some(ast::Expr::FloatLiteral(ast)) => self.lower_float_literal(ast),
            Some(ast::Expr::BoolLiteral(ast)) => self.lower_bool_literal(ast),
            Some(ast::Expr::StringLiteral(ast)) => self.lower_string_literal(ast),
            Some(ast::Expr::CharLiteral(ast)) => self.lower_char_literal(ast),
            Some(ast::Expr::ArrayLiteral(ast)) => self.lower_array_literal(ast),
            None => Expr::Missing,
        };
        self.exprs.alloc(expr)
    }
    pub fn is_lvalue(&self, expr: &ExprIdx) -> bool {
        match self.exprs[*expr] {
            Expr::Ref { var_id: _, range: _ } => true,
            Expr::Index { main_expr, index_expr: _, range: _ } => self.is_lvalue(&main_expr),
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
            SyntaxKind::And => BinaryOp::And,
            SyntaxKind::Or => BinaryOp::Or,
            _ => unreachable!(),
        };
        let lhs = self.lower_expr(ast.lhs());
        let rhs = self.lower_expr(ast.rhs());
        if op == BinaryOp::Assign && !self.is_lvalue(&lhs) {
            let range = ast.syntax().text_range();
            self.diagnostics.push(Diagnostic::new(DiagnosticKind::InvalidLhs, range));
        }
        Expr::Binary {
            op,
            lhs,
            rhs,
            range: ast.syntax().text_range(),
        }
    }
    pub fn lower_prefix_expr(&mut self, ast: ast::PrefixExpr) -> Expr {
        let expr = self.lower_expr(ast.expr());
        Expr::Unary {
            op: UnaryOp::Neg,
            expr,
            range: ast.syntax().text_range(),
        }
    }
    pub fn lower_tuple_expr(&mut self, ast: ast::TupleExpr) -> Expr {
        let elements: Vec<_> = ast.elements().map(|expr| self.lower_expr(Some(expr))).collect();
        Expr::Tuple {
            elements,
            range: ast.syntax().text_range(),
        }
    }
    pub fn lower_ref_expr(&mut self, ast: ast::RefExpr) -> Expr {
        let var_id = self.lower_ident(ast.ident()).and_then(|ident| self.resolve_ctx.resolve_var(&ident.name));
        if var_id.is_none() {
            let range = ast.ident().map_or(ast.syntax().text_range(), |ident| ident.text_range());
            self.diagnostics.push(Diagnostic::new(DiagnosticKind::UndeclaredVariable, range));
        }
        Expr::Ref {
            var_id,
            range: ast.syntax().text_range(),
        }
    }
    pub fn lower_if_expr(&mut self, ast: ast::IfExpr) -> Expr {
        let cond = self.lower_expr(ast.cond());
        let then_expr = self.lower_expr(ast.then_expr());
        let else_expr = ast.else_expr().map(|expr| self.lower_expr(Some(expr)));
        Expr::If {
            cond,
            then_expr,
            else_expr,
            range: ast.syntax().text_range(),
        }
    }
    pub fn lower_fn_call_expr(&mut self, ast: ast::FnCallExpr) -> Expr {
        let args: Vec<_> = ast.args().map(|expr| self.lower_expr(Some(expr))).collect();
        let fn_id = self.lower_ident(ast.ident()).and_then(|ident| self.resolve_ctx.resolve_fn(&ident.name, args.len()));
        let range = ast.syntax().text_range();
        if fn_id.is_none() {
            self.diagnostics.push(Diagnostic::new(DiagnosticKind::UndeclaredFunction, range));
        }
        Expr::FnCall {
            fn_id,
            args,
            range,
        }
    }
    pub fn lower_index_expr(&mut self, ast: ast::IndexExpr) -> Expr {
        let main_expr = self.lower_expr(ast.main());
        let index_expr = self.lower_expr(ast.index());
        Expr::Index {
            main_expr,
            index_expr,
            range: ast.syntax().text_range(),
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
            range: ast.syntax().text_range(),
        }
    }
    pub fn lower_none_literal(&mut self, ast: ast::NoneLiteral) -> Expr {
        Expr::NoneLiteral {
            range: ast.syntax().text_range(),
        }
    }
    pub fn lower_int_literal(&mut self, ast: ast::IntLiteral) -> Expr {
        let parsed = ast.parse();
        if parsed.is_none() {
            let range = ast.syntax().first_token().unwrap().text_range();
            self.diagnostics.push(Diagnostic::new(DiagnosticKind::NumberTooLarge, range));
        }
        Expr::IntLiteral {
            n: parsed,
            range: ast.syntax().text_range(),
        }
    }
    pub fn lower_float_literal(&mut self, ast: ast::FloatLiteral) -> Expr {
        let parsed = ast.parse();
        if parsed.is_none() {
            let range = ast.syntax().first_token().unwrap().text_range();
            self.diagnostics.push(Diagnostic::new(DiagnosticKind::NumberTooLarge, range));
        }
        Expr::FloatLiteral {
            n: parsed,
            range: ast.syntax().text_range(),
        }
    }
    pub fn lower_bool_literal(&mut self, ast: ast::BoolLiteral) -> Expr {
        let parsed = ast.parse();
        Expr::BoolLiteral {
            val: parsed,
            range: ast.syntax().text_range(),
        }
    }
    pub fn lower_string_literal(&mut self, ast: ast::StringLiteral) -> Expr {
        Expr::StringLiteral {
            val: ast.parse(),
            range: ast.syntax().text_range(),
        }
    }
    pub fn lower_char_literal(&mut self, ast: ast::CharLiteral) -> Expr {
        Expr::CharLiteral {
            val: ast.parse(),
            range: ast.syntax().text_range(),
        }
    }
    pub fn lower_array_literal(&mut self, ast: ast::ArrayLiteral) -> Expr {
        let len: Vec<_> = ast.len().map(|len_expr| self.lower_expr(Some(len_expr))).collect();
        let initial = self.lower_expr(ast.initial());
        Expr::ArrayLiteral {
            len,
            initial,
            range: ast.syntax().text_range(),
        }
    }
    pub fn lower_ident(&mut self, ast: Option<SyntaxToken>) -> Option<Identifier> {
        ast.map(|ast| Identifier {
            name: EcoString::from(ast.text()),
            range: ast.text_range(),
        })
    }
}
