use std::collections::HashMap;
use std::iter;

use inkwell::{AddressSpace, IntPredicate};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{BasicMetadataValueEnum, FunctionValue, IntValue, PointerValue};

use crate::database::Database;
use crate::hir::{BinaryOp, Expr, Func, Root, Stmt, UnaryOp};
use crate::scope::{FnId, VarId};

pub struct Compiler<'ctx> {
    pub db: Database,
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub addresses: HashMap<VarId, PointerValue<'ctx>>,
    pub functions: HashMap<FnId, FunctionValue<'ctx>>,
    pub cur_function: Option<FunctionValue<'ctx>>,
    pub loop_stack: Vec<(BasicBlock<'ctx>, BasicBlock<'ctx>)>,
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
            functions: HashMap::new(),
            cur_function: None,
            loop_stack: Vec::new(),
        }
    }
    pub fn compile(mut self, root: &Root) -> Module<'ctx> {
        self.add_builtins();
        let funcs: Vec<_> = self.db.funcs.values().cloned().collect();
        for func in funcs {
            self.compile_func(func.clone());
        }
        let i32_type = self.context.i32_type();
        let fn_type = i32_type.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);
        self.cur_function = Some(function);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);
        self.compile_root(&root);
        self.builder.build_return(Some(&i32_type.const_int(0, false))).unwrap();
        self.module
    }
    fn compile_root(&mut self, root: &Root) {
        for &stmt in &root.stmts {
            self.compile_stmt(self.db.stmts[stmt].clone()).unwrap();
        }
    }
    pub fn add_builtins(&mut self) {
        let i64_type = self.context.i64_type();
        let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let void_type = self.context.void_type();
        {
            let printf_type = void_type.fn_type(&[i8_ptr_type.into()], true);
            let printf_function = self.module.add_function("printf", printf_type, None);
            let print_type = i64_type.fn_type(&[i64_type.into()], false);
            let print_fn = self.module.add_function("print", print_type, None);
            let basic_block = self.context.append_basic_block(print_fn, "entry");
            self.builder.position_at_end(basic_block);
            let format_str = self.builder.build_global_string_ptr("%lld\n", "printf_str").unwrap();
            let param = print_fn.get_first_param().unwrap();
            self.builder.build_call(printf_function, &[format_str.as_pointer_value().into(), param.into()], "").expect("build_call failed");
            self.builder.build_return(Some(&i64_type.const_int(0, false))).unwrap();
            self.functions.insert(FnId(0), print_fn);
        }
        {
            let scanf_type = void_type.fn_type(&[i8_ptr_type.into()], true);
            let scanf_function = self.module.add_function("scanf", scanf_type, None);
            let input_type = i64_type.fn_type(&[], false);
            let input_fn = self.module.add_function("input", input_type, None);
            let basic_block = self.context.append_basic_block(input_fn, "entry");
            self.builder.position_at_end(basic_block);
            let format_str = self.builder.build_global_string_ptr("%lld\n", "scanf_str").unwrap();
            let scanf_ptr = self.builder.build_alloca(i64_type, "scanf_ptr").unwrap();
            self.builder.build_call(scanf_function, &[format_str.as_pointer_value().into(), scanf_ptr.into()], "").expect("build_call failed");
            let val = self.builder.build_load(i64_type, scanf_ptr, "tmp").unwrap();
            self.builder.build_return(Some(&val)).unwrap();
            self.functions.insert(FnId(1), input_fn);
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
            Stmt::WhileStmt { cond, block } => {
                let i64_type = self.context.i64_type();
                let cur_func = self.cur_function.unwrap();
                let cond_block = self.context.append_basic_block(cur_func, "loopcond");
                let loop_block = self.context.append_basic_block(cur_func, "loop");
                let after_block = self.context.append_basic_block(cur_func, "after");

                self.builder.build_unconditional_branch(cond_block).ok()?;
                self.builder.position_at_end(cond_block);
                let cond_val = self.compile_expr(self.db.exprs[cond].clone())?;
                let bool_cond = self.builder.build_int_compare(IntPredicate::NE, cond_val, i64_type.const_int(0, false), "cmp").ok()?;
                self.builder.build_conditional_branch(bool_cond, loop_block, after_block).ok()?;

                self.builder.position_at_end(loop_block);
                self.loop_stack.push((cond_block, after_block));
                self.compile_expr(self.db.exprs[block].clone())?;
                self.loop_stack.pop();
                self.builder.build_unconditional_branch(cond_block).ok()?;

                self.builder.position_at_end(after_block);
                Some(i64_type.const_int(0, false))
            },
            Stmt::BreakStmt {} => {
                let i64_type = self.context.i64_type();
                let cur_func = self.cur_function.unwrap();
                let unreachable_block = self.context.append_basic_block(cur_func, "unreachable");
                let &(_, after_block) = self.loop_stack.last().unwrap();
                self.builder.build_unconditional_branch(after_block).ok()?;
                self.builder.position_at_end(unreachable_block);
                Some(i64_type.const_int(0, false))
            },
            Stmt::ExprStmt { expr } => {
                self.compile_expr(self.db.exprs[expr].clone())
            },
            Stmt::FuncDef { func: _ } => {
                let i64_type = self.context.i64_type();
                Some(i64_type.const_int(0, false))
            },
        }
    }
    fn compile_func(&mut self, func: Func) {
        let i64_type = self.context.i64_type();
        if let Some(fn_info) = func.fn_info {
            let params_type: Vec<_> = iter::repeat(i64_type.into()).take(fn_info.params.len()).collect();
            let func_type = i64_type.fn_type(params_type.as_slice(), false);
            let func_value = self.module.add_function(fn_info.name.as_str(), func_type, None);
            self.functions.insert(fn_info.id, func_value);
            self.cur_function = Some(func_value);
            let basic_block = self.context.append_basic_block(func_value, "entry");
            self.builder.position_at_end(basic_block);
            for (param, var_id) in func_value.get_param_iter().zip(fn_info.params) {
                let var_name = &self.db.resolve_ctx.get_var(var_id).name;
                let addr = self.builder.build_alloca(i64_type, var_name.as_str()).ok().unwrap();
                self.builder.build_store(addr, param.into_int_value()).ok().unwrap();
                self.addresses.insert(var_id, addr);
            }
            let ret = self.compile_expr(self.db.exprs[func.block].clone()).unwrap();
            self.builder.build_return(Some(&ret)).unwrap();
        }
    }
    fn compile_expr(&mut self, expr: Expr) -> Option<IntValue<'ctx>> {
        match expr {
            Expr::Missing => None,
            Expr::Binary { op: BinaryOp::Assign, lhs, rhs } => {
                let Expr::Ref { var_id } = self.db.exprs[lhs].clone() else { unreachable!() };
                let rhs_value = self.compile_expr(self.db.exprs[rhs].clone())?;
                let ptr = self.addresses[&var_id?];
                self.builder.build_store(ptr, rhs_value).ok()?;
                return Some(rhs_value);
            }
            Expr::Binary { op, lhs, rhs } => {
                let i64_type = self.context.i64_type();
                let lhs_value = self.compile_expr(self.db.exprs[lhs].clone())?;
                let rhs_value = self.compile_expr(self.db.exprs[rhs].clone())?;
                match op {
                    BinaryOp::Add => self.builder.build_int_add(lhs_value, rhs_value, "add").ok(),
                    BinaryOp::Sub => self.builder.build_int_sub(lhs_value, rhs_value, "sub").ok(),
                    BinaryOp::Mul => self.builder.build_int_mul(lhs_value, rhs_value, "mul").ok(),
                    BinaryOp::Div => self.builder.build_int_signed_div(lhs_value, rhs_value, "div").ok(),
                    BinaryOp::Rem => self.builder.build_int_signed_rem(lhs_value, rhs_value, "rem").ok(),
                    BinaryOp::EqEq => self.builder.build_int_z_extend(self.builder.build_int_compare(IntPredicate::EQ, lhs_value, rhs_value, "eq").ok()?, i64_type, "ext").ok(),
                    BinaryOp::Neq => self.builder.build_int_z_extend(self.builder.build_int_compare(IntPredicate::NE, lhs_value, rhs_value, "ne").ok()?, i64_type, "ext").ok(),
                    BinaryOp::Assign => unreachable!(),
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
            Expr::If { cond, then_expr, else_expr } => {
                let i64_type = self.context.i64_type();
                let cond_val = self.compile_expr(self.db.exprs[cond].clone())?;
                let cur_func = self.cur_function.unwrap();
                let then_block = self.context.append_basic_block(cur_func, "then");
                let else_block = self.context.append_basic_block(cur_func, "else");
                let merge_block = self.context.append_basic_block(cur_func, "merge");
                let bool_cond = self.builder.build_int_compare(IntPredicate::NE, cond_val, i64_type.const_int(0, false), "cmp").ok()?;
                self.builder.build_conditional_branch(bool_cond, then_block, else_block).ok()?;

                self.builder.position_at_end(then_block);
                let then_val = self.compile_expr(self.db.exprs[then_expr].clone())?;
                self.builder.build_unconditional_branch(merge_block).ok()?;
                let then_block = self.builder.get_insert_block()?;

                self.builder.position_at_end(else_block);
                let else_val = if let Some(else_expr) = else_expr {
                    self.compile_expr(self.db.exprs[else_expr].clone())?
                } else {
                    i64_type.const_int(0, false)
                };
                self.builder.build_unconditional_branch(merge_block).ok()?;
                let else_block = self.builder.get_insert_block()?;

                self.builder.position_at_end(merge_block);
                let phi_node = self.builder.build_phi(i64_type, "merge").ok()?;
                phi_node.add_incoming(&[(&then_val, then_block), (&else_val, else_block)]);
                Some(phi_node.as_basic_value().into_int_value())
            },
            Expr::FnCall { fn_id, args } => {
                let fn_id = fn_id?;
                let function = self.functions[&fn_id];
                let args: Vec<_> = args.iter()
                    .filter_map(|&expr| self.compile_expr(self.db.exprs[expr].clone()))
                    .map(|expr| BasicMetadataValueEnum::from(expr))
                    .collect();
                let call_site = self.builder.build_call(function, &*args, "tmp").ok()?;
                let ret_val = call_site.try_as_basic_value().left()?;
                Some(ret_val.into_int_value())
            },
            Expr::Block { stmts } => {
                let i64_type = self.context.i64_type();
                stmts.iter().map(|&stmt| self.compile_stmt(self.db.stmts[stmt].clone())).last().unwrap_or(Some(i64_type.const_int(0, false)))
            },
            Expr::Literal { n } => {
                let i64_type = self.context.i64_type();
                Some(i64_type.const_int(n?, false))
            },
            Expr::BoolLiteral { val } => {
                let i64_type = self.context.i64_type();
                Some(i64_type.const_int(val as u64, false))
            },
        }
    }
}
