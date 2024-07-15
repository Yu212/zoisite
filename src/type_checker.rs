use la_arena::{ArenaMap, Idx};

use crate::database::Database;
use crate::diagnostic::{Diagnostic, DiagnosticKind};
use crate::hir::{BinaryOp, Expr, Stmt};
use crate::r#type::Type;

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

    pub fn check(mut self, db: &Database) -> (ArenaMap<ExprIdx, Type>, Vec<Diagnostic>) {
        for (idx, _) in db.exprs.iter() {
            self.expr_ty(db, idx.clone());
        }
        for (idx, _) in db.stmts.iter() {
            self.check_stmt(db, idx.clone());
        }
        (self.ty_map, self.diagnostics)
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
            Stmt::FuncDef { func } => {
                let func = db.funcs[func].clone();
                if let Some(func_info) = func.fn_info {
                    let block_ty = self.expr_ty(db, func.block);
                    if block_ty != func_info.return_ty {
                        self.mismatched();
                    }
                }
            }
        }
    }

    pub fn expr_ty(&mut self, db: &Database, idx: ExprIdx) -> Type {
        if let Some(ty) = self.ty_map.get(idx) {
            return ty.clone();
        }
        let expr = db.exprs[idx].clone();
        let ty = match expr {
            Expr::Missing => Type::Unit,
            Expr::Binary { op, lhs, rhs } => {
                let lhs_ty = self.expr_ty(db, lhs);
                let rhs_ty = self.expr_ty(db, rhs);
                if lhs_ty != rhs_ty {
                    self.mismatched()
                } else {
                    match op {
                        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => Type::Int,
                        BinaryOp::EqEq | BinaryOp::Neq => Type::Bool,
                        BinaryOp::Assign => rhs_ty,
                    }
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
            Expr::If { cond, then_expr, else_expr } => {
                let cond_ty = self.expr_ty(db, cond);
                if cond_ty != Type::Bool {
                    self.mismatched();
                }
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
                    for (&arg, params_ty) in args.iter().zip(&func.params_ty) {
                        let args_ty = self.expr_ty(db, arg);
                        if args_ty != *params_ty {
                            self.mismatched();
                        }
                    }
                    func.return_ty.clone()
                } else {
                    Type::Unit
                }
            },
            Expr::Index { main_expr, index_expr } => {
                let main_ty = self.expr_ty(db, main_expr);
                let index_ty = self.expr_ty(db, index_expr);
                if index_ty != Type::Int {
                    self.mismatched();
                }
                if let Type::Array(inner_ty) = main_ty {
                    *inner_ty
                } else {
                    self.mismatched()
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
            Expr::NumberLiteral { n: _ } => Type::Int,
            Expr::BoolLiteral { val: _ } => Type::Bool,
            Expr::ArrayLiteral { len, initial } => {
                let len_ty = self.expr_ty(db, len);
                let initial_ty = self.expr_ty(db, initial);
                if len_ty != Type::Int {
                    self.mismatched();
                }
                Type::Array(Box::new(initial_ty))
            },
        };
        self.ty_map.insert(idx, ty.clone());
        ty
    }
}
