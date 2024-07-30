use crate::database::Database;
use crate::hir::{Expr, ExprIdx, Func, FuncIdx, Root, Stmt, StmtIdx};

pub trait Visitor: Sized {
    fn visit_root(&mut self, root: Root) {
        walk_root(self, root);
    }
    fn visit_func(&mut self, func: Func) {
        walk_func(self, func);
    }
    fn visit_func_idx(&mut self, idx: FuncIdx) {
        walk_func_idx(self, idx);
    }
    fn visit_stmt(&mut self, stmt: Stmt) {
        walk_stmt(self, stmt);
    }
    fn visit_stmt_idx(&mut self, idx: StmtIdx) {
        walk_stmt_idx(self, idx);
    }
    fn visit_expr(&mut self, expr: Expr) {
        walk_expr(self, expr);
    }
    fn visit_expr_idx(&mut self, idx: ExprIdx) {
        walk_expr_idx(self, idx);
    }
    fn db(&self) -> &Database;
}

pub fn walk_root<V: Visitor>(visitor: &mut V, node: Root) {
    for stmt in node.stmts {
        visitor.visit_stmt_idx(stmt);
    }
}
pub fn walk_func<V: Visitor>(visitor: &mut V, node: Func) {
    visitor.visit_expr_idx(node.block);
}
pub fn walk_stmt<V: Visitor>(visitor: &mut V, node: Stmt) {
    match node {
        Stmt::LetStmt { var_id: _, expr, range: _ } => {
            visitor.visit_expr_idx(expr);
        }
        Stmt::WhileStmt { cond, block, range: _ } => {
            visitor.visit_expr_idx(cond);
            visitor.visit_expr_idx(block);
        }
        Stmt::BreakStmt { range: _ } => {}
        Stmt::ExprStmt { expr, range: _ } => {
            visitor.visit_expr_idx(expr);
        }
        Stmt::FuncDef { func, range: _ } => {
            visitor.visit_func_idx(func);
        }
    }
}
pub fn walk_expr<V: Visitor>(visitor: &mut V, node: Expr) {
    match node {
        Expr::Missing => {}
        Expr::Binary { op: _, lhs, rhs, range: _ } => {
            visitor.visit_expr_idx(lhs);
            visitor.visit_expr_idx(rhs);
        }
        Expr::Unary { op: _, expr, range: _ } => {
            visitor.visit_expr_idx(expr);
        }
        Expr::Ref { var_id: _, range: _ } => {}
        Expr::If { cond, then_expr, else_expr, range: _ } => {
            visitor.visit_expr_idx(cond);
            visitor.visit_expr_idx(then_expr);
            if let Some(else_expr) = else_expr {
                visitor.visit_expr_idx(else_expr);
            }
        }
        Expr::FnCall { fn_id: _, args, range: _ } => {
            for arg in args {
                visitor.visit_expr_idx(arg);
            }
        }
        Expr::Index { main_expr, index_expr, range: _ } => {
            visitor.visit_expr_idx(main_expr);
            visitor.visit_expr_idx(index_expr);
        }
        Expr::Block { stmts, range: _ } => {
            for stmt in stmts {
                visitor.visit_stmt_idx(stmt);
            }
        }
        Expr::NumberLiteral { n: _, range: _ } => {}
        Expr::BoolLiteral { val: _, range: _ } => {}
        Expr::StringLiteral { val: _, range: _ } => {}
        Expr::ArrayLiteral { len, initial, range: _ } => {
            visitor.visit_expr_idx(len);
            visitor.visit_expr_idx(initial);
        }
    }
}
pub fn walk_func_idx<V: Visitor>(visitor: &mut V, node: FuncIdx) {
    visitor.visit_func(visitor.db().funcs[node].clone());
}
pub fn walk_stmt_idx<V: Visitor>(visitor: &mut V, node: StmtIdx) {
    visitor.visit_stmt(visitor.db().stmts[node].clone());
}
pub fn walk_expr_idx<V: Visitor>(visitor: &mut V, node: ExprIdx) {
    visitor.visit_expr(visitor.db().exprs[node].clone());
}
