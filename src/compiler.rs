use std::collections::HashMap;

use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{IntValue, PointerValue};

use crate::database::Database;
use crate::hir::{BinaryOp, Expr, Root, Stmt, UnaryOp};
use crate::scope::VarId;

pub struct Compiler<'ctx> {
    pub db: Database,
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub addresses: HashMap<VarId, PointerValue<'ctx>>,
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
            addresses: HashMap::new(),
        }
    }
    pub fn compile(mut self, root: &Root) -> Module<'ctx> {
        let i32_type = self.context.i32_type();
        let fn_type = i32_type.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);
        self.compile_root(&root);
        self.builder.build_return(Some(&i32_type.const_int(0, false))).unwrap();
        self.module
    }
    fn compile_root(&mut self, root: &Root) {
        let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let void_type = self.context.void_type();
        let printf_type = void_type.fn_type(&[i8_ptr_type.into()], true);
        let printf_function = self.module.add_function("printf", printf_type, None);
        let format_str = self.builder.build_global_string_ptr("%lld\n", "printf_str").unwrap();
        for &stmt in &root.stmts {
            if let Some(result) = self.compile_stmt(self.db.stmts[stmt].clone()) {
                let _ = self.builder.build_call(printf_function, &[format_str.as_pointer_value().into(), result.into()], "printf_ret");
            }
        }
    }
    fn compile_stmt(&mut self, stmt: Stmt) -> Option<IntValue<'ctx>> {
        match stmt {
            Stmt::LetStmt { var_id, expr } => {
                let var_id = var_id?;
                let i64_type = self.context.i64_type();
                let var = self.db.resolve_ctx.get_var(var_id);
                let addr = self.builder.build_alloca(i64_type, var.name.as_str()).ok()?;
                let val = self.compile_expr(self.db.exprs[expr].clone())?;
                self.builder.build_store(addr, val).ok()?;
                self.addresses.insert(var_id, addr);
                Some(i64_type.const_int(0, false))
            },
            Stmt::ExprStmt { expr } => {
                self.compile_expr(self.db.exprs[expr].clone())
            },
        }
    }
    fn compile_expr(&mut self, expr: Expr) -> Option<IntValue<'ctx>> {
        match expr {
            Expr::Missing => None,
            Expr::Binary { op, lhs, rhs } => {
                let lhs_value = self.compile_expr(self.db.exprs[lhs].clone())?;
                let rhs_value = self.compile_expr(self.db.exprs[rhs].clone())?;
                match op {
                    BinaryOp::Add => self.builder.build_int_add(lhs_value, rhs_value, "add").ok(),
                    BinaryOp::Sub => self.builder.build_int_sub(lhs_value, rhs_value, "sub").ok(),
                    BinaryOp::Mul => self.builder.build_int_mul(lhs_value, rhs_value, "mul").ok(),
                    BinaryOp::Div => self.builder.build_int_signed_div(lhs_value, rhs_value, "div").ok(),
                    BinaryOp::Rem => self.builder.build_int_signed_rem(lhs_value, rhs_value, "rem").ok(),
                }
            },
            Expr::Unary { op, expr } => {
                let expr_value = self.compile_expr(self.db.exprs[expr].clone())?;
                match op {
                    UnaryOp::Neg => self.builder.build_int_neg(expr_value, "neg").ok(),
                }
            },
            Expr::Ref { var_id } => {
                let var_id = var_id?;
                let i64_type = self.context.i64_type();
                let ptr = self.addresses[&var_id];
                let val = self.builder.build_load(i64_type, ptr, "tmp").ok()?;
                Some(val.into_int_value())
            },
            Expr::Block { stmts } => {
                stmts.iter().map(|&stmt| self.compile_stmt(self.db.stmts[stmt].clone())).last().flatten()
            },
            Expr::Literal { n } => {
                let i64_type = self.context.i64_type();
                Some(i64_type.const_int(n?, false))
            },
        }
    }
}
