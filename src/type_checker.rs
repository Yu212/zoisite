use la_arena::ArenaMap;

use crate::database::Database;
use crate::diagnostic::{Diagnostic, DiagnosticKind};
use crate::hir::{BinaryOp, Expr, ExprIdx, Root, Stmt, StmtIdx};
use crate::r#type::Type;
use crate::visitor::{Visitor, walk_expr_idx, walk_stmt_idx};

pub struct TypeChecker<'db> {
    pub db: &'db Database,
    pub ty_map: ArenaMap<ExprIdx, Type>,
    pub diagnostics: Vec<Diagnostic>,
}

impl Visitor for TypeChecker<'_> {
    fn visit_stmt_idx(&mut self, idx: StmtIdx) {
        walk_stmt_idx(self, idx);
        let stmt = self.db.stmts[idx].clone();
        match stmt {
            Stmt::LetStmt { var_id, expr } => {
                if let Some(var_id) = var_id {
                    let expr_ty = self.expr_ty(expr);
                    let var = self.db.resolve_ctx.get_var(var_id);
                    if var.ty != expr_ty {
                        self.mismatched();
                    }
                }
            }
            Stmt::WhileStmt { .. } => {}
            Stmt::BreakStmt { .. } => {}
            Stmt::ExprStmt { .. } => {}
            Stmt::FuncDef { func } => {
                let func = self.db.funcs[func].clone();
                if let Some(func_info) = func.fn_info {
                    let block_ty = self.expr_ty(func.block);
                    if block_ty != func_info.return_ty {
                        self.mismatched();
                    }
                }
            }
        }
    }

    fn visit_expr_idx(&mut self, idx: ExprIdx) {
        walk_expr_idx(self, idx);
        let expr = self.db.exprs[idx].clone();
        let ty = match expr {
            Expr::Missing => Type::Unit,
            Expr::Binary { op, lhs, rhs } => {
                let lhs_ty = self.expr_ty(lhs);
                let rhs_ty = self.expr_ty(rhs);
                if lhs_ty != rhs_ty {
                    self.mismatched()
                } else {
                    match op {
                        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => Type::Int,
                        BinaryOp::EqEq | BinaryOp::Neq | BinaryOp::Ge | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Lt => Type::Bool,
                        BinaryOp::Assign => rhs_ty,
                    }
                }
            }
            Expr::Unary { op: _, expr } => self.expr_ty(expr),
            Expr::Ref { var_id } => {
                if let Some(var_id) = var_id {
                    let var = self.db.resolve_ctx.get_var(var_id);
                    var.ty.clone()
                } else {
                    Type::Unit
                }
            }
            Expr::If { cond, then_expr, else_expr } => {
                let cond_ty = self.expr_ty(cond);
                if cond_ty != Type::Bool {
                    self.mismatched();
                }
                let then_ty = self.expr_ty(then_expr);
                let else_ty = else_expr.map_or(Type::Unit, |expr| self.expr_ty(expr));
                if then_ty != else_ty {
                    self.mismatched()
                } else {
                    then_ty
                }
            }
            Expr::FnCall { fn_id, args } => {
                if let Some(fn_id) = fn_id {
                    let func = self.db.resolve_ctx.get_fn(fn_id);
                    for (&arg, params_ty) in args.iter().zip(&func.params_ty) {
                        let args_ty = self.expr_ty(arg);
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
                let main_ty = self.expr_ty(main_expr);
                let index_ty = self.expr_ty(index_expr);
                if index_ty != Type::Int {
                    self.mismatched();
                }
                main_ty.inner_ty().unwrap_or_else(|| self.mismatched())
            },
            Expr::Block { stmts } => {
                if let Some(&stmt) = stmts.last() {
                    if let Stmt::ExprStmt { expr } = self.db.stmts[stmt] {
                        self.expr_ty(expr)
                    } else {
                        Type::Unit
                    }
                } else {
                    Type::Unit
                }
            },
            Expr::NumberLiteral { n: _ } => Type::Int,
            Expr::BoolLiteral { val: _ } => Type::Bool,
            Expr::StringLiteral { val: _ } => Type::Str,
            Expr::ArrayLiteral { len, initial } => {
                let len_ty = self.expr_ty(len);
                let initial_ty = self.expr_ty(initial);
                if len_ty != Type::Int {
                    self.mismatched();
                }
                Type::Array(Box::new(initial_ty))
            },
        };
        self.ty_map.insert(idx, ty.clone());
    }
    
    fn db(&self) -> &Database {
        self.db
    }
}

impl TypeChecker<'_> {
    pub fn new(db: &Database) -> TypeChecker {
        TypeChecker {
            db,
            ty_map: ArenaMap::default(),
            diagnostics: Vec::new(),
        }
    }

    pub fn check(mut self, root: Root) -> (ArenaMap<ExprIdx, Type>, Vec<Diagnostic>) {
        self.visit_root(root);
        (self.ty_map, self.diagnostics)
    }

    fn mismatched(&mut self) -> Type {
        self.diagnostics.push(Diagnostic::new(DiagnosticKind::TypeUnmatched, None));
        Type::Invalid
    }

    pub fn expr_ty(&mut self, idx: ExprIdx) -> Type {
        self.ty_map.get(idx).unwrap().clone()
    }
}
