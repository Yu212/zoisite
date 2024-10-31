use inkwell::context::Context;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::AddressSpace;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Type {
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
}
