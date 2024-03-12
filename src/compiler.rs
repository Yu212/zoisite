use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::IntValue;

use crate::database::Database;
use crate::hir::{BinaryOp, Expr, Root};

pub struct Compiler<'ctx> {
    pub db: Database,
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context, db: Database, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        Self {
            db,
            context,
            module,
            builder,
        }
    }
    pub fn compile(self, root: &Root) -> Module<'ctx> {
        let i32_type = self.context.i32_type();
        let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let void_type = self.context.void_type();
        let printf_type = void_type.fn_type(&[i8_ptr_type.into()], true);
        let printf_function = self.module.add_function("printf", printf_type, None);
        let fn_type = i32_type.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);
        let format_str = self.builder.build_global_string_ptr("%lld\n", "printf_str").unwrap();
        let result = self.compile_root(&root).unwrap();
        let _ = self.builder.build_call(printf_function, &[format_str.as_pointer_value().into(), result.into()], "printf_ret");
        self.builder.build_return(Some(&i32_type.const_int(0, false))).unwrap();
        self.module
    }
    fn compile_root(&self, root: &Root) -> Option<IntValue<'ctx>> {
        self.compile_expr(&self.db.exprs[root.expr])
    }
    fn compile_expr(&self, expr: &Expr) -> Option<IntValue<'ctx>> {
        match expr {
            Expr::Missing => None,
            Expr::Binary { op, lhs, rhs } => {
                let lhs_value = self.compile_expr(&self.db.exprs[*lhs])?;
                let rhs_value = self.compile_expr(&self.db.exprs[*rhs])?;
                match op {
                    BinaryOp::Add => self.builder.build_int_add(lhs_value, rhs_value, "add").ok(),
                    BinaryOp::Mul => self.builder.build_int_mul(lhs_value, rhs_value, "add").ok(),
                }
            },
            Expr::Literal { n } => {
                let i64_type = self.context.i64_type();
                Some(i64_type.const_int((*n)?, false))
            },
        }
    }
}
