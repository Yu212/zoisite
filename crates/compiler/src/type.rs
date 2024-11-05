use crate::resolve_context::ResolveContext;
use inkwell::context::Context;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::AddressSpace;
use std::collections::{HashMap, HashSet};
use std::iter::once;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Type {
    TyVar(usize),
    Unit,
    Int,
    Bool,
    Str,
    Char,
    Array(Box<Type>),
    Tuple(Vec<Type>),
    Option(Box<Type>),
    Invalid,
}

impl Type {
    pub fn llvm_ty<'ctx>(&self, ctx: &'ctx Context) -> Option<BasicTypeEnum<'ctx>> {
        match self {
            Type::TyVar(_) => None,
            Type::Unit => Some(ctx.i8_type().into()),
            Type::Int => Some(ctx.i64_type().into()),
            Type::Bool => Some(ctx.bool_type().into()),
            Type::Str => Some(ctx.struct_type(&[ctx.i64_type().into(), ctx.i8_type().ptr_type(AddressSpace::default()).into()], false).into()),
            Type::Char => Some(ctx.i8_type().into()),
            Type::Array(inner_ty) => Some(inner_ty.llvm_ty(ctx)?.ptr_type(AddressSpace::default()).into()),
            Type::Tuple(inner_ty) => {
                let tys = inner_ty.iter().map(|ty| ty.llvm_ty(ctx)).collect::<Option<Vec<_>>>();
                Some(ctx.struct_type(tys?.as_slice(), false).into())
            },
            Type::Option(inner_ty) => Some(inner_ty.llvm_ty(ctx)?.ptr_type(AddressSpace::default()).into()),
            Type::Invalid => None,
        }
    }
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
    pub fn list_ty_var(&self) -> HashSet<usize> {
        match self {
            Type::TyVar(ty_var_id) => HashSet::from([*ty_var_id]),
            Type::Array(inner_ty) => inner_ty.list_ty_var(),
            Type::Tuple(inner_tys) => inner_tys.into_iter().flat_map(|inner_ty| inner_ty.list_ty_var()).collect(),
            Type::Option(inner_ty) => inner_ty.list_ty_var(),
            _ => HashSet::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncType {
    pub params_ty: Vec<Type>,
    pub return_ty: Type,
}

impl FuncType {
    pub fn new(params_ty: Vec<Type>, return_ty: Type) -> Self {
        FuncType {
            params_ty,
            return_ty,
        }
    }
    pub fn instantiate(&self, resolve_ctx: &mut ResolveContext) -> Self {
        let ty_vars = self.params_ty.iter().chain(once(&self.return_ty)).flat_map(|ty| ty.list_ty_var());
        let ty_vars_subst = ty_vars.into_iter().map(|ty_var_id| (ty_var_id, resolve_ctx.new_ty_var())).collect();
        let params_ty = self.params_ty.iter().map(|ty| ty.substitute_with(&ty_vars_subst)).collect();
        let return_ty = self.return_ty.substitute_with(&ty_vars_subst);
        Self::new(params_ty, return_ty)
    }
}
