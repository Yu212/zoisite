use itertools::Itertools;
use la_arena::ArenaMap;
use rowan::TextRange;
use std::collections::HashMap;

use crate::database::Database;
use crate::diagnostic::{Diagnostic, DiagnosticKind};
use crate::hir::{BinaryOp, Expr, ExprIdx, Root, Stmt, StmtIdx, UnaryOp};
use crate::r#type::{FuncType, Type};
use crate::scope::VarId;
use crate::visitor::{walk_expr_idx, walk_stmt_idx, Visitor};

pub struct TypeInfer<'db> {
    db: &'db mut Database,
    diagnostics: Vec<Diagnostic>,
    inferred: TypeInferResult,
    ty_env: HashMap<VarId, Type>,
}

pub struct TypeInferResult {
    expr_tys: ArenaMap<ExprIdx, Type>,
    pub fn_calls: ArenaMap<ExprIdx, FuncType>,
    pub subst: HashMap<usize, Type>,
}

impl TypeInferResult {
    pub fn expr_ty(&self, idx: ExprIdx) -> Type {
        let ty = self.expr_tys.get(idx).unwrap().clone();
        self.substitute(&ty).into()
    }

    pub fn substitute(&self, ty: &Type) -> Type {
        ty.substitute_with(&self.subst)
    }

    pub fn substitute_func(&self, ty: &FuncType) -> FuncType {
        ty.substitute_with(&self.subst)
    }

    fn add_subst(&mut self, id: usize, ty: Type) {
        self.subst.insert(id, ty);
    }
}

impl TypeInfer<'_> {
    pub fn new(db: &mut Database) -> TypeInfer {
        TypeInfer {
            db,
            diagnostics: Vec::new(),
            inferred: TypeInferResult {
                expr_tys: ArenaMap::default(),
                fn_calls: ArenaMap::default(),
                subst: Default::default(),
            },
            ty_env: Default::default(),
        }
    }

    pub fn infer(mut self, root: Root) -> (TypeInferResult, Vec<Diagnostic>) {
        self.visit_root(root);
        for (&var_id, ty) in self.ty_env.iter() {
            let var_info = self.db.resolve_ctx.get_var(var_id);
            let inferred = self.inferred.substitute(&ty);
            if var_info.ty_hint.is_none() {
                // eprintln!("{:?} {:?}", var_info.name, inferred);
            }
            if inferred.contains_ty_var() {
                let range = var_info.ident.clone().unwrap().range;
                self.diagnostics.push(Diagnostic::new(DiagnosticKind::TypeInferenceFailure, range));
            } else {
                var_info.ty.replace(inferred.into());
            }
        }
        for fn_info in &mut self.db.resolve_ctx.functions {
            fn_info.instances = fn_info.instances.iter().map(|instance| self.inferred.substitute_func(instance)).unique().collect();
        }
        self.inferred.fn_calls = self.inferred.fn_calls.iter().map(|(key, fn_ty)| (key, fn_ty.substitute_with(&self.inferred.subst))).collect();
        (self.inferred, self.diagnostics.clone())
    }

    fn mismatched(&mut self, ty1: &Type, ty2: &Type, range: TextRange) {
        self.diagnostics.push(Diagnostic::new(DiagnosticKind::TypeMismatched {
            ty1: self.inferred.substitute(ty1).into(),
            ty2: self.inferred.substitute(ty2).into(),
        }, range));
    }

    fn invalid_operation(&mut self, op: BinaryOp, ty1: &Type, ty2: &Type, range: TextRange) {
        self.diagnostics.push(Diagnostic::new(DiagnosticKind::InvalidOperation {
            op,
            ty1: self.inferred.substitute(ty1).into(),
            ty2: self.inferred.substitute(ty2).into(),
        }, range));
    }

    fn unify(&mut self, ty1: &Type, ty2: &Type, range: TextRange) -> Option<Type> {
        let ty1 = self.inferred.substitute(ty1);
        let ty2 = self.inferred.substitute(ty2);
        // eprintln!("{:?} {:?} {:?}", ty1, ty2, range);
        match (&ty1, &ty2) {
            (_, &Type::Invalid) => Some(ty1),
            (&Type::Invalid, _) => Some(ty2),
            (&Type::TyVar(id1), &Type::TyVar(id2)) => {
                if id1 != id2 {
                    self.inferred.add_subst(id1, ty2.clone());
                }
                Some(ty2)
            },
            (&Type::TyVar(id), _) => {
                self.inferred.add_subst(id, ty2.clone());
                Some(ty2)
            },
            (_, &Type::TyVar(id)) => {
                self.inferred.add_subst(id, ty1.clone());
                Some(ty1)
            },
            (&Type::Unit, &Type::Unit) => Some(Type::Unit),
            (&Type::Int, &Type::Int) => Some(Type::Int),
            (&Type::Bool, &Type::Bool) => Some(Type::Bool),
            (&Type::Str, &Type::Str) => Some(Type::Str),
            (&Type::Char, &Type::Char) => Some(Type::Char),
            (&Type::Array(ref inner_ty1), &Type::Array(ref inner_ty2)) => {
                self.unify(&inner_ty1, &inner_ty2, range)?;
                Some(ty1)
            },
            (&Type::Tuple(ref inner_ty1), &Type::Tuple(ref inner_ty2)) => {
                if inner_ty1.len() != inner_ty2.len() {
                    self.mismatched(&ty1, &ty2, range.clone());
                    None
                } else if inner_ty1.iter().zip(inner_ty2.iter()).all(|(ty1, ty2)| self.unify(ty1, ty2, range).is_some()) {
                    Some(ty1)
                } else {
                    None
                }
            },
            (&Type::Option(ref inner_ty1), &Type::Option(ref inner_ty2)) => {
                self.unify(&inner_ty1, &inner_ty2, range)?;
                Some(ty1)
            },
            _ => {
                self.mismatched(&ty1, &ty2, range.clone());
                None
            }
        }
    }

    fn var_ty(&mut self, var: VarId) -> Type {
        self.ty_env.get(&var).unwrap().clone()
    }

    fn define_var(&mut self, var_id: VarId, range: TextRange) -> Type {
        let ty_var = self.db.resolve_ctx.new_ty_var();
        let var = self.db.resolve_ctx.get_var(var_id);
        self.ty_env.insert(var.id, ty_var.clone());
        if let Some(ref ty_hint) = var.ty_hint {
            self.unify(&ty_var, &ty_hint.clone(), range);
        }
        ty_var
    }
}

impl Visitor for TypeInfer<'_> {
    fn visit_stmt_idx(&mut self, idx: StmtIdx) {
        let stmt = self.db.stmts[idx].clone();
        match stmt {
            Stmt::EmptyStmt { range: _ } => walk_stmt_idx(self, idx),
            Stmt::LetStmt { var_id, expr, range } => {
                if let Some(var_id) = var_id {
                    let ty_var = self.define_var(var_id, range);
                    walk_stmt_idx(self, idx);
                    let expr_ty = self.inferred.expr_ty(expr);
                    self.unify(&ty_var, &expr_ty, range);
                } else {
                    walk_stmt_idx(self, idx);
                }
            }
            Stmt::WhileStmt { .. } => walk_stmt_idx(self, idx),
            Stmt::BreakStmt { .. } => walk_stmt_idx(self, idx),
            Stmt::ContinueStmt { .. } => walk_stmt_idx(self, idx),
            Stmt::ExprStmt { .. } => walk_stmt_idx(self, idx),
            Stmt::FuncDef { func, range } => {
                let func = self.db.funcs[func].clone();
                if let Some(func_info) = func.fn_info {
                    for &param in &func_info.params {
                        self.define_var(param, range);
                    }
                    walk_stmt_idx(self, idx);
                    let block_ty = self.inferred.expr_ty(func.block);
                    self.unify(&block_ty, &func_info.ty.return_ty, range);
                } else {
                    walk_stmt_idx(self, idx);
                }
            }
        }
    }

    fn visit_expr_idx(&mut self, idx: ExprIdx) {
        walk_expr_idx(self, idx);
        let expr = self.db.exprs[idx].clone();
        let ty = match expr {
            Expr::Missing => Type::Unit,
            Expr::Binary { op, lhs, rhs, range } => {
                let lhs_ty = self.inferred.expr_ty(lhs);
                let rhs_ty = self.inferred.expr_ty(rhs);
                match op {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem if lhs_ty == Type::Int && rhs_ty == Type::Int => {
                        Type::Int
                    },
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem if lhs_ty == Type::Str && rhs_ty == Type::Str => {
                        Type::Str
                    },
                    BinaryOp::EqEq | BinaryOp::Neq | BinaryOp::Ge | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Lt if lhs_ty == Type::Int && rhs_ty == Type::Int => {
                        Type::Bool
                    },
                    BinaryOp::And | BinaryOp::Or if lhs_ty == Type::Bool && rhs_ty == Type::Bool => {
                        Type::Bool
                    },
                    BinaryOp::Assign => {
                        self.unify(&lhs_ty, &rhs_ty, range);
                        rhs_ty
                    },
                    _ => {
                        self.invalid_operation(op, &lhs_ty, &rhs_ty, range);
                        Type::Invalid
                    },
                }
            }
            Expr::Unary { op, expr, range } => {
                let expr_ty = self.inferred.expr_ty(expr);
                match op {
                    UnaryOp::Neg => {
                        self.unify(&expr_ty, &Type::Int, range);
                        Type::Int
                    },
                }
            },
            Expr::Ref { var_id, range: _ } => {
                if let Some(var_id) = var_id {
                    let var = self.db.resolve_ctx.get_var(var_id);
                    self.var_ty(var.id)
                } else {
                    unreachable!()
                }
            }
            Expr::Tuple { elements, range: _ } => {
                let elements_ty = elements.iter().map(|&expr| self.inferred.expr_ty(expr)).collect();
                Type::Tuple(elements_ty)
            }
            Expr::If { cond, then_expr, else_expr, range } => {
                let cond_ty = self.inferred.expr_ty(cond);
                self.unify(&cond_ty, &Type::Bool, range);
                let then_ty = self.inferred.expr_ty(then_expr);
                let else_ty = else_expr.map_or(Type::Unit, |expr| self.inferred.expr_ty(expr));
                self.unify(&then_ty, &else_ty, range);
                then_ty
            }
            Expr::FnCall { fn_id, args, range } => {
                if let Some(fn_id) = fn_id {
                    let func = self.db.resolve_ctx.get_fn(fn_id);
                    let fn_ty = func.ty.clone();
                    let fn_ty = fn_ty.instantiate(&mut self.db.resolve_ctx);
                    self.db.resolve_ctx.add_instance(fn_id, &fn_ty);
                    let return_ty = fn_ty.return_ty.clone();
                    let args_with_ty = args.iter().zip(fn_ty.params_ty.clone());
                    for (&arg, ty) in args_with_ty {
                        let args_ty = self.inferred.expr_ty(arg);
                        self.unify(&args_ty, &ty, range);
                    }
                    self.inferred.fn_calls.insert(idx, fn_ty);
                    return_ty
                } else {
                    unreachable!()
                }
            },
            Expr::Index { main_expr, index_expr, range } => {
                let main_ty = self.inferred.expr_ty(main_expr);
                let index_ty = self.inferred.expr_ty(index_expr);
                self.unify(&index_ty, &Type::Int, range);
                let ret_ty = self.db.resolve_ctx.new_ty_var();
                match main_ty {
                    Type::Str => self.unify(&ret_ty, &Type::Char, range),
                    _ => self.unify(&main_ty, &ret_ty.clone().wrap_in_array(), range),
                };
                ret_ty
            },
            Expr::Block { stmts, range: _ } => {
                if let Some(&stmt) = stmts.last() {
                    if let Stmt::ExprStmt { expr, range: _ } = self.db.stmts[stmt] {
                        self.inferred.expr_ty(expr)
                    } else {
                        Type::Unit
                    }
                } else {
                    unreachable!()
                }
            },
            Expr::NoneLiteral { range: _ } => self.db.resolve_ctx.new_ty_var().wrap_in_option(),
            Expr::NumberLiteral { n: _, range: _ } => Type::Int,
            Expr::BoolLiteral { val: _, range: _ } => Type::Bool,
            Expr::StringLiteral { val: _, range: _ } => Type::Str,
            Expr::ArrayLiteral { len, initial, range } => {
                let mut ty = self.inferred.expr_ty(initial);
                for len_expr in len {
                    let len_ty = self.inferred.expr_ty(len_expr);
                    self.unify(&len_ty, &Type::Int, range);
                    ty = ty.wrap_in_array();
                }
                ty
            },
        };
        self.inferred.expr_tys.insert(idx, ty.clone());
    }
    
    fn db(&self) -> &Database {
        self.db
    }
}
