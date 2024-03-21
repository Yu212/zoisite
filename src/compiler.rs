use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::IntValue;

use crate::database::Database;
use crate::hir::{BinaryOp, Expr, Root, Stmt, UnaryOp};

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
        let fn_type = i32_type.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);
        self.compile_root(&root);
        self.builder.build_return(Some(&i32_type.const_int(0, false))).unwrap();
        self.module
    }
    fn compile_root(&self, root: &Root) {
        let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let void_type = self.context.void_type();
        let printf_type = void_type.fn_type(&[i8_ptr_type.into()], true);
        let printf_function = self.module.add_function("printf", printf_type, None);
        let format_str = self.builder.build_global_string_ptr("%lld\n", "printf_str").unwrap();
        for &stmt in &root.stmts {
            let result = self.compile_stmt(&self.db.stmts[stmt]).unwrap();
            let _ = self.builder.build_call(printf_function, &[format_str.as_pointer_value().into(), result.into()], "printf_ret");
        }
    }
    fn compile_stmt(&self, stmt: &Stmt) -> Option<IntValue<'ctx>> {
        match stmt {
            Stmt::LetStmt { name: _, expr } => {
                self.compile_expr(&self.db.exprs[*expr])
            },
            Stmt::ExprStmt { expr } => {
                self.compile_expr(&self.db.exprs[*expr])
            },
        }
    }
    fn compile_expr(&self, expr: &Expr) -> Option<IntValue<'ctx>> {
        match expr {
            Expr::Missing => None,
            Expr::Binary { op, lhs, rhs } => {
                let lhs_value = self.compile_expr(&self.db.exprs[*lhs])?;
                let rhs_value = self.compile_expr(&self.db.exprs[*rhs])?;
                match op {
                    BinaryOp::Add => self.builder.build_int_add(lhs_value, rhs_value, "add").ok(),
                    BinaryOp::Sub => self.builder.build_int_sub(lhs_value, rhs_value, "sub").ok(),
                    BinaryOp::Mul => self.builder.build_int_mul(lhs_value, rhs_value, "mul").ok(),
                    BinaryOp::Div => self.builder.build_int_signed_div(lhs_value, rhs_value, "div").ok(),
                    BinaryOp::Rem => self.builder.build_int_signed_rem(lhs_value, rhs_value, "rem").ok(),
                }
            },
            Expr::Unary { op, expr } => {
                let expr_value = self.compile_expr(&self.db.exprs[*expr])?;
                match op {
                    UnaryOp::Neg => self.builder.build_int_neg(expr_value, "neg").ok(),
                }
            },
            Expr::Literal { n } => {
                let i64_type = self.context.i64_type();
                Some(i64_type.const_int((*n)?, false))
            },
        }
    }
}
