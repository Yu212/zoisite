use std::collections::HashMap;

use la_arena::ArenaMap;
use rowan::TextRange;

use crate::database::Database;
use crate::diagnostic::{Diagnostic, DiagnosticKind};
use crate::hir::{BinaryOp, Expr, ExprIdx, Root, Stmt, StmtIdx, UnaryOp};
use crate::r#type::Type;
use crate::resolve_context::VariableInfo;
use crate::scope::VarId;
use crate::visitor::{walk_expr_idx, walk_stmt_idx, Visitor};

pub struct TypeInfer<'db> {
    pub db: &'db Database,
    diagnostics: Vec<Diagnostic>,
    expr_tys: ArenaMap<ExprIdx, Typing>,
    subst: HashMap<usize, Typing>,
    ty_env: HashMap<VarId, Typing>,
    next_ty_var_id: usize,
}

impl TypeInfer<'_> {
    pub fn new(db: &Database) -> TypeInfer {
        TypeInfer {
            db,
            diagnostics: Vec::new(),
            expr_tys: ArenaMap::default(),
            subst: Default::default(),
            ty_env: Default::default(),
            next_ty_var_id: 0,
        }
    }

    pub fn check(&mut self, root: Root) -> Vec<Diagnostic> {
        self.visit_root(root);
        for (&var_id, ty) in self.ty_env.iter() {
            let var_info = self.db.resolve_ctx.get_var(var_id);
            let inferred = self.substitute(&ty);
            if var_info.ty_hint.is_none() {
                // eprintln!("{:?} {:?}", var_info.name, inferred);
            }
            var_info.ty.replace(inferred.into());
        }
        self.diagnostics.clone()
    }

    fn mismatched(&mut self, ty1: &Typing, ty2: &Typing, range: TextRange) {
        self.diagnostics.push(Diagnostic::new(DiagnosticKind::TypeMismatched {
            ty1: self.substitute(ty1).into(),
            ty2: self.substitute(ty2).into(),
        }, range));
    }

    fn invalid_operation(&mut self, op: BinaryOp, ty1: &Typing, ty2: &Typing, range: TextRange) {
        self.diagnostics.push(Diagnostic::new(DiagnosticKind::InvalidOperation {
            op,
            ty1: self.substitute(ty1).into(),
            ty2: self.substitute(ty2).into(),
        }, range));
    }

    fn substitute(&self, ty: &Typing) -> Typing {
        match ty {
            Typing::TyVar(id) => self.subst.get(id).map_or(ty.clone(), |ty| self.substitute(ty)),
            Typing::Array(inner_ty) => Typing::Array(Box::new(self.substitute(inner_ty))),
            Typing::Tuple(inner_ty) => Typing::Tuple(inner_ty.iter().map(|ty| self.substitute(ty)).collect()),
            Typing::Option(inner_ty) => Typing::Option(Box::new(self.substitute(inner_ty))),
            _ => ty.clone(),
        }
    }

    fn add_subst(&mut self, id: usize, ty: Typing) {
        self.subst.insert(id, ty);
    }

    fn unify(&mut self, ty1: &Typing, ty2: &Typing, range: TextRange) -> Option<Typing> {
        let ty1 = self.substitute(ty1);
        let ty2 = self.substitute(ty2);
        // eprintln!("{:?} {:?} {:?}", ty1, ty2, range);
        match (&ty1, &ty2) {
            (_, &Typing::Unknown) => Some(ty1),
            (&Typing::Unknown, _) => Some(ty2),
            (&Typing::TyVar(id1), &Typing::TyVar(id2)) => {
                if id1 != id2 {
                    self.add_subst(id1, ty2.clone());
                }
                Some(ty2)
            },
            (&Typing::TyVar(id), _) => {
                self.add_subst(id, ty2.clone());
                Some(ty2)
            },
            (_, &Typing::TyVar(id)) => {
                self.add_subst(id, ty1.clone());
                Some(ty1)
            },
            (&Typing::Unit, &Typing::Unit) => Some(Typing::Unit),
            (&Typing::Int, &Typing::Int) => Some(Typing::Int),
            (&Typing::Bool, &Typing::Bool) => Some(Typing::Bool),
            (&Typing::Str, &Typing::Str) => Some(Typing::Str),
            (&Typing::Char, &Typing::Char) => Some(Typing::Char),
            (&Typing::Array(ref inner_ty1), &Typing::Array(ref inner_ty2)) => {
                self.unify(&inner_ty1, &inner_ty2, range)?;
                Some(ty1)
            },
            (&Typing::Tuple(ref inner_ty1), &Typing::Tuple(ref inner_ty2)) => {
                if inner_ty1.len() != inner_ty2.len() {
                    self.mismatched(&ty1, &ty2, range.clone());
                    None
                } else if inner_ty1.iter().zip(inner_ty2.iter()).all(|(ty1, ty2)| self.unify(ty1, ty2, range).is_some()) {
                    Some(ty1)
                } else {
                    None
                }
            },
            (&Typing::Option(ref inner_ty1), &Typing::Option(ref inner_ty2)) => {
                self.unify(&inner_ty1, &inner_ty2, range)?;
                Some(ty1)
            },
            _ => {
                self.mismatched(&ty1, &ty2, range.clone());
                None
            }
        }
    }

    fn var_ty(&mut self, var: VarId) -> Typing {
        self.ty_env.get(&var).unwrap().clone()
    }

    fn expr_typing(&mut self, idx: ExprIdx) -> Typing {
        let ty = self.expr_tys.get(idx).unwrap().clone();
        self.substitute(&ty).into()
    }

    pub fn expr_ty(&mut self, idx: ExprIdx) -> Type {
        self.expr_typing(idx).into()
    }

    fn new_ty_var(&mut self) -> Typing {
        let id = self.next_ty_var_id;
        self.next_ty_var_id += 1;
        Typing::TyVar(id)
    }

    fn define_var(&mut self, var: &VariableInfo, range: TextRange) -> Typing {
        let ty_var = self.new_ty_var();
        self.ty_env.insert(var.id, ty_var.clone());
        if let Some(ref ty_hint) = var.ty_hint {
            self.unify(&ty_var, &Typing::from(ty_hint.clone()), range);
        }
        ty_var
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum Typing {
    TyVar(usize),
    Unit,
    Int,
    Bool,
    Str,
    Char,
    Array(Box<Typing>),
    Tuple(Vec<Typing>),
    Option(Box<Typing>),
    Unknown,
}

impl From<Type> for Typing {
    fn from(ty: Type) -> Self {
        match ty {
            Type::Unit => Typing::Unit,
            Type::Int => Typing::Int,
            Type::Bool => Typing::Bool,
            Type::Str => Typing::Str,
            Type::Char => Typing::Char,
            Type::Array(inner_ty) => Typing::Array(Box::new((*inner_ty).into())),
            Type::Tuple(inner_ty) => Typing::Tuple(inner_ty.into_iter().map(|ty| ty.into()).collect()),
            Type::Option(inner_ty) => Typing::Option(Box::new((*inner_ty).into())),
            Type::Invalid => Typing::Unknown,
        }
    }
}

impl From<Typing> for Type {
    fn from(ty: Typing) -> Self {
        match ty {
            Typing::Unit => Type::Unit,
            Typing::Int => Type::Int,
            Typing::Bool => Type::Bool,
            Typing::Str => Type::Str,
            Typing::Char => Type::Char,
            Typing::Array(inner_ty) => Type::Array(Box::new((*inner_ty).into())),
            Typing::Tuple(inner_ty) => Type::Tuple(inner_ty.into_iter().map(|ty| ty.into()).collect()),
            Typing::Option(inner_ty) => Type::Option(Box::new((*inner_ty).into())),
            Typing::Unknown => Type::Invalid,
            Typing::TyVar(_) => Type::Invalid,
        }
    }
}

impl Visitor for TypeInfer<'_> {
    fn visit_stmt_idx(&mut self, idx: StmtIdx) {
        let stmt = self.db.stmts[idx].clone();
        match stmt {
            Stmt::EmptyStmt { range: _ } => walk_stmt_idx(self, idx),
            Stmt::LetStmt { var_id, expr, range } => {
                if let Some(var_id) = var_id {
                    let var = self.db.resolve_ctx.get_var(var_id);
                    let ty_var = self.define_var(var, range);
                    walk_stmt_idx(self, idx);
                    let expr_ty = self.expr_typing(expr);
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
                        let var = self.db.resolve_ctx.get_var(param);
                        self.define_var(var, range);
                    }
                    walk_stmt_idx(self, idx);
                    let block_ty = self.expr_typing(func.block);
                    self.unify(&block_ty, &Typing::from(func_info.return_ty), range);
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
            Expr::Missing => Typing::Unit,
            Expr::Binary { op, lhs, rhs, range } => {
                let lhs_ty = self.expr_typing(lhs);
                let rhs_ty = self.expr_typing(rhs);
                match op {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem if lhs_ty == Typing::Int && rhs_ty == Typing::Int => {
                        Typing::Int
                    },
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem if lhs_ty == Typing::Str && rhs_ty == Typing::Str => {
                        Typing::Str
                    },
                    BinaryOp::EqEq | BinaryOp::Neq | BinaryOp::Ge | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Lt if lhs_ty == Typing::Int && rhs_ty == Typing::Int => {
                        Typing::Bool
                    },
                    BinaryOp::And | BinaryOp::Or if lhs_ty == Typing::Bool && rhs_ty == Typing::Bool => {
                        Typing::Bool
                    },
                    BinaryOp::Assign => {
                        self.unify(&lhs_ty, &rhs_ty, range);
                        rhs_ty
                    },
                    _ => {
                        self.invalid_operation(op, &lhs_ty, &rhs_ty, range);
                        Typing::Unknown
                    },
                }
            }
            Expr::Unary { op, expr, range } => {
                let expr_ty = self.expr_typing(expr);
                match op {
                    UnaryOp::Neg => {
                        self.unify(&expr_ty, &Typing::Int, range);
                        Typing::Int
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
                let elements_ty = elements.iter().map(|&expr| self.expr_typing(expr)).collect();
                Typing::Tuple(elements_ty)
            }
            Expr::If { cond, then_expr, else_expr, range } => {
                let cond_ty = self.expr_typing(cond);
                self.unify(&cond_ty, &Typing::Bool, range);
                let then_ty = self.expr_typing(then_expr);
                let else_ty = else_expr.map_or(Typing::Unit, |expr| self.expr_typing(expr));
                self.unify(&then_ty, &else_ty, range);
                then_ty
            }
            Expr::FnCall { fn_id, args, range } => {
                if let Some(fn_id) = fn_id {
                    let func = self.db.resolve_ctx.get_fn(fn_id);
                    for (&arg, params_ty) in args.iter().zip(&func.params_ty) {
                        let args_ty = self.expr_typing(arg);
                        self.unify(&args_ty, &Typing::from(params_ty.clone()), range);
                    }
                    Typing::from(func.return_ty.clone())
                } else {
                    unreachable!()
                }
            },
            Expr::Index { main_expr, index_expr, range } => {
                let main_ty = self.expr_typing(main_expr);
                let index_ty = self.expr_typing(index_expr);
                self.unify(&index_ty, &Typing::Int, range);
                let ret_ty = self.new_ty_var();
                match main_ty {
                    Typing::Str => self.unify(&ret_ty, &Typing::Char, range),
                    _ => self.unify(&main_ty, &Typing::Array(Box::new(ret_ty.clone())), range),
                };
                ret_ty
            },
            Expr::Block { stmts, range: _ } => {
                if let Some(&stmt) = stmts.last() {
                    if let Stmt::ExprStmt { expr, range: _ } = self.db.stmts[stmt] {
                        self.expr_typing(expr)
                    } else {
                        Typing::Unit
                    }
                } else {
                    unreachable!()
                }
            },
            Expr::NumberLiteral { n: _, range: _ } => Typing::Int,
            Expr::BoolLiteral { val: _, range: _ } => Typing::Bool,
            Expr::StringLiteral { val: _, range: _ } => Typing::Str,
            Expr::ArrayLiteral { len, initial, range } => {
                let mut ty = self.expr_typing(initial);
                for len_expr in len {
                    let len_ty = self.expr_typing(len_expr);
                    self.unify(&len_ty, &Typing::Int, range);
                    ty = Typing::Array(Box::new(ty))
                }
                ty
            },
        };
        self.expr_tys.insert(idx, ty.clone());
    }
    
    fn db(&self) -> &Database {
        self.db
    }
}
