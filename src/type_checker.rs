use la_arena::{ArenaMap, Idx};

use crate::database::Database;
use crate::diagnostic::{Diagnostic, DiagnosticKind};
use crate::hir::{Expr, Stmt};

type ExprIdx = Idx<Expr>;
type StmtIdx = Idx<Stmt>;

pub struct TypeChecker {
    pub ty_map: ArenaMap<ExprIdx, Type>,
    pub diagnostics: Vec<Diagnostic>,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            ty_map: ArenaMap::default(),
            diagnostics: Vec::new(),
        }
    }

    pub fn check(mut self, db: &Database) -> Vec<Diagnostic> {
        for (idx, _) in db.exprs.iter() {
            self.expr_ty(db, idx.clone());
        }
        for (idx, _) in db.stmts.iter() {
            self.check_stmt(db, idx.clone());
        }
        self.diagnostics
    }

    fn mismatched(&mut self) -> Type {
        self.diagnostics.push(Diagnostic::new(DiagnosticKind::TypeUnmatched, None));
        Type::Invalid
    }

    pub fn check_stmt(&mut self, db: &Database, idx: StmtIdx) {
        let stmt = db.stmts[idx].clone();
        match stmt {
            Stmt::LetStmt { var_id, expr } => {
                if let Some(var_id) = var_id {
                    let expr_ty = self.expr_ty(db, expr);
                    let var = db.resolve_ctx.get_var(var_id);
                    if var.ty != expr_ty {
                        self.mismatched();
                    }
                }
            }
            Stmt::WhileStmt { .. } => {}
            Stmt::BreakStmt { .. } => {}
            Stmt::ExprStmt { .. } => {}
            Stmt::FuncDef { .. } => {}
        }
    }

    pub fn expr_ty(&mut self, db: &Database, idx: ExprIdx) -> Type {
        if let Some(&ty) = self.ty_map.get(idx) {
            return ty;
        }
        let expr = db.exprs[idx].clone();
        let ty = match expr {
            Expr::Missing => Type::Unit,
            Expr::Binary { op: _, lhs, rhs } => {
                let lhs_ty = self.expr_ty(db, lhs);
                let rhs_ty = self.expr_ty(db, rhs);
                if lhs_ty != rhs_ty {
                    self.mismatched()
                } else {
                    lhs_ty
                }
            }
            Expr::Unary { op: _, expr } => self.expr_ty(db, expr),
            Expr::Ref { var_id } => {
                if let Some(var_id) = var_id {
                    let var = db.resolve_ctx.get_var(var_id);
                    var.ty.clone()
                } else {
                    Type::Unit
                }
            }
            Expr::If { cond: _, then_expr, else_expr } => {
                let then_ty = self.expr_ty(db, then_expr);
                let else_ty = else_expr.map_or(Type::Unit, |expr| self.expr_ty(db, expr));
                if then_ty != else_ty {
                    self.mismatched()
                } else {
                    then_ty
                }
            }
            Expr::FnCall { fn_id, args } => {
                if let Some(fn_id) = fn_id {
                    let func = db.resolve_ctx.get_fn(fn_id);
                    for (&arg, &param_ty) in args.iter().zip(&func.param_ty) {
                        let arg_ty = self.expr_ty(db, arg);
                        if arg_ty != param_ty {
                            self.mismatched();
                        }
                    }
                    func.return_ty
                } else {
                    Type::Unit
                }
            },
            Expr::Block { stmts } => {
                if let Some(&stmt) = stmts.last() {
                    if let Stmt::ExprStmt { expr } = db.stmts[stmt] {
                        self.expr_ty(db, expr)
                    } else {
                        Type::Unit
                    }
                } else {
                    Type::Unit
                }
            },
            Expr::Literal { n: _ } => Type::Int,
        };
        self.ty_map.insert(idx, ty);
        ty
    }
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Type {
    Unit,
    Int,
    Invalid,
}
