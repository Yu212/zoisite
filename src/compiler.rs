use std::collections::HashMap;

use inkwell::{AddressSpace, IntPredicate};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, IntValue, PointerValue};

use crate::database::Database;
use crate::hir::{BinaryOp, Expr, ExprIdx, Func, Root, Stmt, UnaryOp};
use crate::r#type::Type;
use crate::scope::{FnId, VarId};
use crate::type_infer::TypeInfer;

pub struct Compiler<'ctx> {
    pub db: &'ctx Database,
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub addresses: HashMap<VarId, PointerValue<'ctx>>,
    pub functions: HashMap<FnId, FunctionValue<'ctx>>,
    pub cur_function: Option<FunctionValue<'ctx>>,
    pub loop_stack: Vec<(BasicBlock<'ctx>, BasicBlock<'ctx>)>,
    pub type_infer: TypeInfer<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context, db: &'ctx Database, type_infer: TypeInfer<'ctx>, module_name: &str) -> Self {
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
            type_infer,
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
        let i8_type = self.context.i8_type();
        let i8_ptr_type = i8_type.ptr_type(AddressSpace::default());
        let void_type = self.context.void_type();
        let printf_type = void_type.fn_type(&[i8_ptr_type.into()], true);
        let printf_function = self.module.add_function("printf", printf_type, None);
        let scanf_type = void_type.fn_type(&[i8_ptr_type.into()], true);
        let scanf_function = self.module.add_function("scanf", scanf_type, None);
        {
            let print_type = i8_type.fn_type(&[i64_type.into()], false);
            let print_fn = self.module.add_function("printInt", print_type, None);
            let basic_block = self.context.append_basic_block(print_fn, "entry");
            self.builder.position_at_end(basic_block);
            let format_str = self.builder.build_global_string_ptr("%lld\n", "printf_int_format").unwrap();
            let param = print_fn.get_first_param().unwrap();
            self.builder.build_call(printf_function, &[format_str.as_pointer_value().into(), param.into()], "").unwrap();
            self.builder.build_return(Some(&i8_type.const_int(0, false))).unwrap();
            self.functions.insert(FnId(0), print_fn);
        }
        {
            let str_type = Type::Str.llvm_ty(self.context).unwrap();
            let print_type = i8_type.fn_type(&[str_type.into()], false);
            let print_fn = self.module.add_function("printStr", print_type, None);
            let basic_block = self.context.append_basic_block(print_fn, "entry");
            self.builder.position_at_end(basic_block);
            let format_str = self.builder.build_global_string_ptr("%s\n", "printf_str_format").unwrap();
            let param = print_fn.get_first_param().unwrap();
            self.builder.build_call(printf_function, &[format_str.as_pointer_value().into(), param.into()], "").unwrap();
            self.builder.build_return(Some(&i8_type.const_int(0, false))).unwrap();
            self.functions.insert(FnId(1), print_fn);
        }
        {
            let input_type = i64_type.fn_type(&[], false);
            let input_fn = self.module.add_function("inputInt", input_type, None);
            let basic_block = self.context.append_basic_block(input_fn, "entry");
            self.builder.position_at_end(basic_block);
            let format_str = self.builder.build_global_string_ptr("%lld", "scanf_int_format").unwrap();
            let scanf_ptr = self.builder.build_alloca(i64_type, "scanf_ptr").unwrap();
            self.builder.build_call(scanf_function, &[format_str.as_pointer_value().into(), scanf_ptr.into()], "").unwrap();
            let val = self.builder.build_load(i64_type, scanf_ptr, "tmp").unwrap();
            self.builder.build_return(Some(&val)).unwrap();
            self.functions.insert(FnId(2), input_fn);
        }
        {
            let str_type = Type::Str.llvm_ty(self.context).unwrap();
            let input_type = str_type.fn_type(&[i64_type.into()], false);
            let input_fn = self.module.add_function("inputStr", input_type, None);
            let basic_block = self.context.append_basic_block(input_fn, "entry");
            self.builder.position_at_end(basic_block);
            let format_str = self.builder.build_global_string_ptr("%s", "scanf_str_format").unwrap();
            let param = input_fn.get_first_param().unwrap();
            let scanf_ptr = self.builder.build_array_malloc(i8_type, param.into_int_value(), "str").unwrap();
            self.builder.build_call(scanf_function, &[format_str.as_pointer_value().into(), scanf_ptr.into()], "").unwrap();
            self.builder.build_return(Some(&scanf_ptr)).unwrap();
            self.functions.insert(FnId(3), input_fn);
        }
        {
            let chr_type = i8_type.fn_type(&[i64_type.into()], false);
            let chr_fn = self.module.add_function("chr", chr_type, None);
            let basic_block = self.context.append_basic_block(chr_fn, "entry");
            self.builder.position_at_end(basic_block);
            let param = chr_fn.get_first_param().unwrap().into_int_value();
            let ret_val = self.builder.build_int_truncate(param, i8_type, "tmp").unwrap();
            self.builder.build_return(Some(&ret_val)).unwrap();
            self.functions.insert(FnId(4), chr_fn);
        }
        {
            let ord_type = i64_type.fn_type(&[i8_type.into()], false);
            let ord_fn = self.module.add_function("ord", ord_type, None);
            let basic_block = self.context.append_basic_block(ord_fn, "entry");
            self.builder.position_at_end(basic_block);
            let param = ord_fn.get_first_param().unwrap().into_int_value();
            let ret_val = self.builder.build_int_z_extend(param, i64_type, "tmp").unwrap();
            self.builder.build_return(Some(&ret_val)).unwrap();
            self.functions.insert(FnId(5), ord_fn);
        }
    }
    fn compile_stmt(&mut self, stmt: Stmt) -> Option<BasicValueEnum<'ctx>> {
        match stmt {
            Stmt::EmptyStmt { range: _ } => {
                let i8_type = self.context.i8_type();
                Some(i8_type.const_int(0, false).into())
            },
            Stmt::LetStmt { var_id, expr, range: _ } => {
                let var_id = var_id?;
                let var_info = self.db.resolve_ctx.get_var(var_id);
                let ty = var_info.ty.borrow().llvm_ty(self.context).unwrap();
                let i8_type = self.context.i8_type();
                let var = self.db.resolve_ctx.get_var(var_id);
                let addr = self.builder.build_alloca(ty, var.name.as_str()).ok()?;
                let val = self.compile_expr_idx(expr)?;
                self.builder.build_store(addr, val).ok()?;
                self.addresses.insert(var_id, addr);
                Some(i8_type.const_int(0, false).into())
            },
            Stmt::WhileStmt { cond, block, range: _ } => {
                let i8_type = self.context.i8_type();
                let cur_func = self.cur_function.unwrap();
                let cond_block = self.context.append_basic_block(cur_func, "loopcond");
                let loop_block = self.context.append_basic_block(cur_func, "loop");
                let after_block = self.context.append_basic_block(cur_func, "after");

                self.builder.build_unconditional_branch(cond_block).ok()?;
                self.builder.position_at_end(cond_block);
                let cond_val = self.compile_expr_idx(cond)?.into_int_value();
                self.builder.build_conditional_branch(cond_val, loop_block, after_block).ok()?;

                self.builder.position_at_end(loop_block);
                self.loop_stack.push((cond_block, after_block));
                self.compile_expr_idx(block)?;
                self.loop_stack.pop();
                self.builder.build_unconditional_branch(cond_block).ok()?;

                self.builder.position_at_end(after_block);
                Some(i8_type.const_int(0, false).into())
            },
            Stmt::BreakStmt { range: _ } => {
                let i8_type = self.context.i8_type();
                let cur_func = self.cur_function.unwrap();
                let unreachable_block = self.context.append_basic_block(cur_func, "unreachable");
                let &(_, after_block) = self.loop_stack.last().unwrap();
                self.builder.build_unconditional_branch(after_block).ok()?;
                self.builder.position_at_end(unreachable_block);
                Some(i8_type.const_int(0, false).into())
            },
            Stmt::ContinueStmt { range: _ } => {
                let i8_type = self.context.i8_type();
                let cur_func = self.cur_function.unwrap();
                let unreachable_block = self.context.append_basic_block(cur_func, "unreachable");
                let &(cond_block, _) = self.loop_stack.last().unwrap();
                self.builder.build_unconditional_branch(cond_block).ok()?;
                self.builder.position_at_end(unreachable_block);
                Some(i8_type.const_int(0, false).into())
            },
            Stmt::ExprStmt { expr, range: _ } => {
                self.compile_expr_idx(expr)
            },
            Stmt::FuncDef { func: _, range: _ } => {
                let i8_type = self.context.i8_type();
                Some(i8_type.const_int(0, false).into())
            },
        }
    }
    fn compile_func(&mut self, func: Func) {
        if let Some(fn_info) = func.fn_info {
            let params_type: Vec<_> = fn_info.params_ty.iter().map(|ty| ty.llvm_ty(self.context).unwrap().into()).collect();
            let return_ty = fn_info.return_ty.llvm_ty(self.context).unwrap();
            let func_type = return_ty.fn_type(params_type.as_slice(), false);
            let func_value = self.module.add_function(fn_info.name.as_str(), func_type, None);
            self.functions.insert(fn_info.id, func_value);
            self.cur_function = Some(func_value);
            let basic_block = self.context.append_basic_block(func_value, "entry");
            self.builder.position_at_end(basic_block);
            for (param, var_id) in func_value.get_param_iter().zip(fn_info.params) {
                let var_name = &self.db.resolve_ctx.get_var(var_id).name;
                let addr = self.builder.build_alloca(param.get_type(), var_name.as_str()).ok().unwrap();
                self.builder.build_store(addr, param).ok().unwrap();
                self.addresses.insert(var_id, addr);
            }
            let ret = self.compile_expr_idx(func.block).unwrap();
            self.builder.build_return(Some(&ret)).unwrap();
        }
    }
    fn compile_lvalue(&mut self, expr: Expr) -> Option<PointerValue<'ctx>> {
        match expr {
            Expr::Ref { var_id, range: _ } => {
                Some(self.addresses[&var_id?])
            },
            Expr::Index { main_expr, index_expr, range: _ } => {
                let main_ty = &self.type_infer.expr_ty(main_expr);
                let inner_ty = main_ty.inner_ty().unwrap().llvm_ty(self.context).unwrap();
                let main_ty = main_ty.llvm_ty(self.context).unwrap();
                let lvalue = self.compile_lvalue(self.db.exprs[main_expr].clone())?;
                let main_val = self.builder.build_load(main_ty, lvalue, "tmp").ok()?.into_pointer_value();
                let index_val = self.compile_expr_idx(index_expr)?.into_int_value();
                let ptr = unsafe { self.builder.build_gep(inner_ty, main_val, &[index_val], "ptr").ok()? };
                Some(ptr)
            },
            _ => unreachable!()
        }
    }
    fn compile_expr_idx(&mut self, idx: ExprIdx) -> Option<BasicValueEnum<'ctx>> {
        self.compile_expr(idx, self.db.exprs[idx].clone())
    }
    fn compile_expr(&mut self, idx: ExprIdx, expr: Expr) -> Option<BasicValueEnum<'ctx>> {
        match expr {
            Expr::Missing => None,
            Expr::Binary { op, lhs, rhs, range: _ } => {
                if let BinaryOp::Assign = op {
                    let ptr = self.compile_lvalue(self.db.exprs[lhs].clone())?;
                    let rhs_value = self.compile_expr_idx(rhs)?;
                    self.builder.build_store(ptr, rhs_value).ok()?;
                    return Some(rhs_value);
                }
                let lhs_value = self.compile_expr_idx(lhs)?.into_int_value();
                let rhs_value = self.compile_expr_idx(rhs)?.into_int_value();
                let int_ret = match op {
                    BinaryOp::Add => self.builder.build_int_add(lhs_value, rhs_value, "add").ok(),
                    BinaryOp::Sub => self.builder.build_int_sub(lhs_value, rhs_value, "sub").ok(),
                    BinaryOp::Mul => self.builder.build_int_mul(lhs_value, rhs_value, "mul").ok(),
                    BinaryOp::Div => self.builder.build_int_signed_div(lhs_value, rhs_value, "div").ok(),
                    BinaryOp::Rem => self.builder.build_int_signed_rem(lhs_value, rhs_value, "rem").ok(),
                    BinaryOp::EqEq => self.builder.build_int_compare(IntPredicate::EQ, lhs_value, rhs_value, "eq").ok(),
                    BinaryOp::Neq => self.builder.build_int_compare(IntPredicate::NE, lhs_value, rhs_value, "ne").ok(),
                    BinaryOp::Ge => self.builder.build_int_compare(IntPredicate::SGE, lhs_value, rhs_value, "ge").ok(),
                    BinaryOp::Le => self.builder.build_int_compare(IntPredicate::SLE, lhs_value, rhs_value, "le").ok(),
                    BinaryOp::Gt => self.builder.build_int_compare(IntPredicate::SGT, lhs_value, rhs_value, "gt").ok(),
                    BinaryOp::Lt => self.builder.build_int_compare(IntPredicate::SLT, lhs_value, rhs_value, "lt").ok(),
                    BinaryOp::Assign => unreachable!(),
                };
                Some(int_ret?.into())
            },
            Expr::Unary { op, expr, range: _ } => {
                let expr_value = self.compile_expr_idx(expr)?.into_int_value();
                let int_ret = match op {
                    UnaryOp::Neg => self.builder.build_int_neg(expr_value, "neg").ok(),
                };
                Some(int_ret?.into())
            },
            Expr::Ref { var_id, range: _ } => {
                let var_id = var_id?;
                let var_info = self.db.resolve_ctx.get_var(var_id);
                let ty = var_info.ty.borrow().llvm_ty(self.context).unwrap();
                let ptr = self.addresses[&var_id];
                self.builder.build_load(ty, ptr, "tmp").ok()
            },
            Expr::Tuple { elements, range: _ } => {
                let struct_ty = self.type_infer.expr_ty(idx).llvm_ty(self.context)?;
                let val = self.builder.build_malloc(struct_ty, "val").ok()?;
                for (i, &expr) in elements.iter().enumerate() {
                    let ptr = self.builder.build_struct_gep(struct_ty, val, i as u32, "tmp").ok()?;
                    let element = self.compile_expr_idx(expr)?;
                    self.builder.build_store(ptr, element).ok()?;
                }
                Some(val.into())
            },
            Expr::If { cond, then_expr, else_expr, range: _ } => {
                let i8_type = self.context.i8_type();
                let cond_val = self.compile_expr_idx(cond)?.into_int_value();
                let cur_func = self.cur_function.unwrap();
                let then_block = self.context.append_basic_block(cur_func, "then");
                let else_block = self.context.append_basic_block(cur_func, "else");
                let merge_block = self.context.append_basic_block(cur_func, "merge");
                self.builder.build_conditional_branch(cond_val, then_block, else_block).ok()?;

                self.builder.position_at_end(then_block);
                let then_val = self.compile_expr_idx(then_expr)?;
                self.builder.build_unconditional_branch(merge_block).ok()?;
                let then_block = self.builder.get_insert_block()?;

                self.builder.position_at_end(else_block);
                let else_val = if let Some(else_expr) = else_expr {
                    self.compile_expr_idx(else_expr)?
                } else {
                    i8_type.const_int(0, false).into()
                };
                self.builder.build_unconditional_branch(merge_block).ok()?;
                let else_block = self.builder.get_insert_block()?;

                self.builder.position_at_end(merge_block);
                let phi_node = self.builder.build_phi(then_val.get_type(), "merge").ok()?;
                phi_node.add_incoming(&[(&then_val, then_block), (&else_val, else_block)]);
                Some(phi_node.as_basic_value())
            },
            Expr::FnCall { fn_id, args, range: _ } => {
                let fn_id = fn_id?;
                let function = self.functions[&fn_id];
                let args: Vec<_> = args.iter()
                    .filter_map(|&expr| self.compile_expr_idx(expr))
                    .map(|expr| BasicMetadataValueEnum::from(expr))
                    .collect();
                let call_site = self.builder.build_call(function, &*args, "tmp").ok()?;
                let ret_val = call_site.try_as_basic_value().left()?;
                Some(ret_val)
            },
            Expr::Index { main_expr, index_expr, range: _ } => {
                let main_ty = &self.type_infer.expr_ty(main_expr);
                let inner_ty = main_ty.inner_ty().unwrap().llvm_ty(self.context).unwrap();
                let main_val = self.compile_expr_idx(main_expr)?.into_pointer_value();
                let index_val = self.compile_expr_idx(index_expr)?.into_int_value();
                let val_ptr = unsafe { self.builder.build_gep(inner_ty, main_val, &[index_val], "ptr").ok()? };
                Some(self.builder.build_load(inner_ty, val_ptr, "tmp").ok()?.into())
            },
            Expr::Block { stmts, range: _ } => {
                let i8_type = self.context.i8_type();
                stmts.iter().map(|&stmt| self.compile_stmt(self.db.stmts[stmt].clone())).last().unwrap_or(Some(i8_type.const_int(0, false).into()))
            },
            Expr::NumberLiteral { n, range: _ } => {
                let i64_type = self.context.i64_type();
                Some(i64_type.const_int(n?, false).into())
            },
            Expr::BoolLiteral { val, range: _ } => {
                let bool_type = self.context.bool_type();
                Some(bool_type.const_int(val as u64, false).into())
            },
            Expr::StringLiteral { val, range: _ } => {
                let str = self.context.const_string(val?.as_bytes(), true);
                let str_ptr = self.builder.build_malloc(str.get_type(), "str").ok()?;
                self.builder.build_store(str_ptr, str).ok()?;
                Some(str_ptr.into())
            },
            Expr::ArrayLiteral { len, initial, range: _ } => {
                let i64_type = self.context.i64_type();

                let initial_val = self.compile_expr_idx(initial)?;
                let mut len_mul = i64_type.const_int(1, false);
                let mut arr_types = vec![initial_val.get_type()];
                let mut len_vals = vec![];
                for x in len {
                    let x_val = self.compile_expr_idx(x)?.into_int_value();
                    len_vals.push(x_val);
                    len_mul = self.builder.build_int_mul(len_mul, x_val, "tmp").ok()?;
                    arr_types.push(arr_types.last().unwrap().ptr_type(AddressSpace::default()).into());
                }
                arr_types.reverse();
                let all_array = self.builder.build_array_malloc(initial_val.get_type(), len_mul, "array").ok()?;

                let cur_func = self.cur_function.unwrap();
                let cond_block = self.context.append_basic_block(cur_func, "loopcond");
                let loop_block = self.context.append_basic_block(cur_func, "loop");
                let after_block = self.context.append_basic_block(cur_func, "after");

                let idx_addr = self.builder.build_alloca(i64_type, "idx").ok()?;
                self.builder.build_store(idx_addr, i64_type.const_int(0, false)).ok()?;
                self.builder.build_unconditional_branch(cond_block).ok()?;

                self.builder.position_at_end(cond_block);
                let idx_val = self.builder.build_load(i64_type, idx_addr, "idx").ok()?.into_int_value();
                let cond_val = self.builder.build_int_compare(IntPredicate::SLT, idx_val, len_mul, "cmp").ok()?;
                self.builder.build_conditional_branch(cond_val, loop_block, after_block).ok()?;

                self.builder.position_at_end(loop_block);
                unsafe {
                    let val = self.builder.build_in_bounds_gep(initial_val.get_type(), all_array, &[idx_val], "tmp").ok()?;
                    self.builder.build_store(val, initial_val).ok()?;
                };
                let new_idx_val = self.builder.build_int_add(idx_val, i64_type.const_int(1, false), "add").ok()?;
                self.builder.build_store(idx_addr, new_idx_val).ok()?;
                self.builder.build_unconditional_branch(cond_block).ok()?;

                self.builder.position_at_end(after_block);

                let array = self.array_initializer(0, i64_type.const_int(0, false), len_vals, arr_types, all_array)?;
                Some(array.into())
            },
        }
    }

    fn array_initializer(&mut self, i: usize, all_idx_val: IntValue<'ctx>, len_vals: Vec<IntValue<'ctx>>, arr_types: Vec<BasicTypeEnum<'ctx>>, all_array: PointerValue<'ctx>) -> Option<PointerValue<'ctx>> {
        if i + 1 == len_vals.len() {
            unsafe {
                let ptr = self.builder.build_in_bounds_gep(arr_types[0], all_array, &[all_idx_val], "tmp").ok()?;
                return Some(ptr);
            }
        }
        let i64_type = self.context.i64_type();
        let array = self.builder.build_array_malloc(arr_types[i], len_vals[i], "array").ok()?;

        let cur_func = self.cur_function.unwrap();
        let cond_block = self.context.append_basic_block(cur_func, "loopcond");
        let loop_block = self.context.append_basic_block(cur_func, "loop");
        let after_block = self.context.append_basic_block(cur_func, "after");

        let idx_addr = self.builder.build_alloca(i64_type, "idx").ok()?;
        self.builder.build_store(idx_addr, i64_type.const_int(0, false)).ok()?;
        self.builder.build_unconditional_branch(cond_block).ok()?;

        self.builder.position_at_end(cond_block);
        let idx_val = self.builder.build_load(i64_type, idx_addr, "idx").ok()?.into_int_value();
        let cond_val = self.builder.build_int_compare(IntPredicate::SLT, idx_val, len_vals[i], "cmp").ok()?;
        self.builder.build_conditional_branch(cond_val, loop_block, after_block).ok()?;

        self.builder.position_at_end(loop_block);
        unsafe {
            let val = self.array_initializer(i + 1, self.builder.build_int_mul(idx_val, len_vals[i + 1], "tmp").ok()?, len_vals, arr_types, all_array)?;
            let ptr = self.builder.build_in_bounds_gep(val.get_type(), array, &[idx_val], "tmp").ok()?;
            self.builder.build_store(ptr, val).ok()?;
        };
        let new_idx_val = self.builder.build_int_add(idx_val, i64_type.const_int(1, false), "add").ok()?;
        self.builder.build_store(idx_addr, new_idx_val).ok()?;
        self.builder.build_unconditional_branch(cond_block).ok()?;

        self.builder.position_at_end(after_block);

        Some(array)
    }
}
