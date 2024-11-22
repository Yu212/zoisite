use crate::resolve_context::ResolveContext;
use itertools::Itertools;
use std::collections::{BTreeSet, HashMap};
use std::iter::once;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum Type {
    TyVar(usize),
    Unit,
    Int,
    Float,
    Bool,
    Str,
    Char,
    Array(Box<Type>),
    Tuple(Vec<Type>),
    Option(Box<Type>),
    Invalid,
}

impl Type {
    pub fn inner_ty(&self) -> Option<Type> {
        match self {
            Type::Str => Some(Type::Char),
            Type::Array(inner_ty) => Some(*inner_ty.clone()),
            Type::Option(inner_ty) => Some(*inner_ty.clone()),
            _ => None,
        }
    }
    pub fn contains_ty_var(&self) -> bool {
        match self {
            Type::TyVar(_) => true,
            Type::Unit => false,
            Type::Int => false,
            Type::Float => false,
            Type::Bool => false,
            Type::Str => false,
            Type::Char => false,
            Type::Array(inner_ty) => inner_ty.contains_ty_var(),
            Type::Tuple(inner_tys) => inner_tys.into_iter().any(Self::contains_ty_var),
            Type::Option(inner_ty) => inner_ty.contains_ty_var(),
            Type::Invalid => false,
        }
    }
    pub fn wrap_in_array(self) -> Type {
        Type::Array(Box::new(self))
    }
    pub fn wrap_in_option(self) -> Type {
        Type::Option(Box::new(self))
    }
    pub fn substitute_with(&self, subst: &HashMap<usize, Type>) -> Type {
        match self {
            Type::TyVar(id) => subst.get(id).map_or(self.clone(), |ty| ty.substitute_with(subst)),
            Type::Array(inner_ty) => inner_ty.substitute_with(subst).wrap_in_array(),
            Type::Tuple(inner_ty) => Type::Tuple(inner_ty.iter().map(|ty| ty.substitute_with(subst)).collect()),
            Type::Option(inner_ty) => inner_ty.substitute_with(subst).wrap_in_option(),
            _ => self.clone(),
        }
    }
    pub fn list_ty_var(&self) -> BTreeSet<usize> {
        match self {
            Type::TyVar(ty_var_id) => BTreeSet::from([*ty_var_id]),
            Type::Array(inner_ty) => inner_ty.list_ty_var(),
            Type::Tuple(inner_tys) => inner_tys.into_iter().flat_map(|inner_ty| inner_ty.list_ty_var()).collect(),
            Type::Option(inner_ty) => inner_ty.list_ty_var(),
            _ => BTreeSet::new(),
        }
    }
    pub fn mangle(&self) -> String {
        match self {
            Type::TyVar(_) => "a".to_owned(),
            Type::Unit => "u".to_owned(),
            Type::Int => "i".to_owned(),
            Type::Float => "f".to_owned(),
            Type::Bool => "b".to_owned(),
            Type::Str => "s".to_owned(),
            Type::Char => "c".to_owned(),
            Type::Array(inner_ty) => inner_ty.mangle() + "[]",
            Type::Tuple(inner_tys) => inner_tys.into_iter().map(Self::mangle).join(""),
            Type::Option(inner_ty) => inner_ty.mangle() + "?",
            Type::Invalid => "x".to_owned(),
        }
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct FuncType {
    pub params_ty: Vec<Type>,
    pub return_ty: Type,
}

impl FuncType {
    pub fn new(params_ty: Vec<Type>, return_ty: Type) -> Self {
        Self {
            params_ty,
            return_ty,
        }
    }
    pub fn ty_vars(&self) -> BTreeSet<usize> {
        self.params_ty.iter().chain(once(&self.return_ty)).flat_map(|ty| ty.list_ty_var()).collect()
    }
    pub fn instantiate(&self, resolve_ctx: &mut ResolveContext) -> Self {
        let ty_vars_subst = self.ty_vars().into_iter().map(|ty_var_id| (ty_var_id, resolve_ctx.new_ty_var())).collect();
        let params_ty = self.params_ty.iter().map(|ty| ty.substitute_with(&ty_vars_subst)).collect();
        let return_ty = self.return_ty.substitute_with(&ty_vars_subst);
        Self::new(params_ty, return_ty)
    }
    pub fn substitute_with(&self, subst: &HashMap<usize, Type>) -> Self {
        Self {
            params_ty: self.params_ty.iter().map(|ty| ty.substitute_with(subst)).collect(),
            return_ty: self.return_ty.substitute_with(subst),
        }
    }
    pub fn mangle(&self) -> String {
        format!("{}:{}", self.params_ty.iter().map(Type::mangle).join(""), self.return_ty.mangle())
    }
}
