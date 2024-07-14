use inkwell::context::Context;
use inkwell::types::BasicTypeEnum;

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Type {
    Unit,
    Int,
    Bool,
    Invalid,
}

impl Type {
    pub fn llvm_ty<'ctx>(&self, ctx: &'ctx Context) -> Option<BasicTypeEnum<'ctx>> {
        match self {
            Type::Unit => Some(ctx.i8_type().into()),
            Type::Int => Some(ctx.i64_type().into()),
            Type::Bool => Some(ctx.bool_type().into()),
            Type::Invalid => None,
        }
    }
}
