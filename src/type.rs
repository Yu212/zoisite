use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::types::{BasicType, BasicTypeEnum};

#[derive(PartialEq, Clone, Debug)]
pub enum Type {
    Unit,
    Int,
    Bool,
    Str,
    Array(Box<Type>),
    Invalid,
}

impl Type {
    pub fn llvm_ty<'ctx>(&self, ctx: &'ctx Context) -> Option<BasicTypeEnum<'ctx>> {
        match self {
            Type::Unit => Some(ctx.i8_type().into()),
            Type::Int => Some(ctx.i64_type().into()),
            Type::Bool => Some(ctx.bool_type().into()),
            Type::Str => Some(ctx.i8_type().ptr_type(AddressSpace::default()).into()),
            Type::Array(inner_ty) => Some(inner_ty.llvm_ty(ctx)?.ptr_type(AddressSpace::default()).into()),
            Type::Invalid => None,
        }
    }
}
