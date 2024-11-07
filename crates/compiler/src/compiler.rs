use crate::database::Database;
use crate::hir::{BinaryOp, Expr, ExprIdx, Func, Root, Stmt, UnaryOp};
use crate::r#type::{FuncType, Type};
use crate::scope::{FnId, VarId};
use crate::type_infer::TypeInferResult;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::{Builder, BuilderError};
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue};
use inkwell::{AddressSpace, IntPredicate};
use std::collections::HashMap;

pub struct Compiler<'ctx> {
    pub db: &'ctx Database,
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub addresses: HashMap<VarId, PointerValue<'ctx>>,
    pub functions: HashMap<(FnId, FuncType), FunctionValue<'ctx>>,
    pub cur_function: Option<FunctionValue<'ctx>>,
    pub loop_stack: Vec<(BasicBlock<'ctx>, BasicBlock<'ctx>)>,
    pub type_inferred: TypeInferResult,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context, db: &'ctx Database, type_inferred: TypeInferResult, module_name: &str) -> Self {
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
            type_inferred,
        }
    }

    fn compiler_error(&self, msg: &str) -> CompileError {
        CompileError::CompilerError(msg.to_owned())
    }

    pub fn compile(mut self, root: &Root) -> Result<Module<'ctx>, CompileError> {
        self.add_builtins()?;
        let funcs: Vec<_> = self.db.funcs.values().cloned().collect();
        for func in funcs {
            self.compile_func(func.clone())?;
        }
        let i32_type = self.context.i32_type();
        let fn_type = i32_type.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);
        self.cur_function = Some(function);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);
        self.compile_root(&root)?;
        self.builder.build_return(Some(&i32_type.const_int(0, false)))?;
        Ok(self.module)
    }

    fn compile_root(&mut self, root: &Root) -> Result<(), CompileError> {
        for &stmt in &root.stmts {
            self.compile_stmt(self.db.stmts[stmt].clone())?;
        }
        Ok(())
    }

    pub fn add_builtins(&mut self) -> Result<(), CompileError> {
        let i64_type = self.context.i64_type();
        let i8_type = self.context.i8_type();
        let i8_ptr_type = i8_type.ptr_type(AddressSpace::default());
        let void_type = self.context.void_type();
        let str_type = self.context.struct_type(&[i64_type.into(), i8_type.ptr_type(AddressSpace::default()).into()], false);
        let printf_type = void_type.fn_type(&[i8_ptr_type.into()], true);
        let printf_function = self.module.add_function("printf", printf_type, None);
        let scanf_type = void_type.fn_type(&[i8_ptr_type.into()], true);
        let scanf_function = self.module.add_function("scanf", scanf_type, None);
        let sprintf_type = void_type.fn_type(&[i8_ptr_type.into(), i8_ptr_type.into()], true);
        let sprintf_function = self.module.add_function("sprintf", sprintf_type, None);
        let strlen_type = i64_type.fn_type(&[i8_ptr_type.into()], true);
        let strlen_function = self.module.add_function("strlen", strlen_type, None);
        {
            let print_type = i8_type.fn_type(&[i64_type.into()], false);
            let print_fn = self.module.add_function("printInt", print_type, None);
            let basic_block = self.context.append_basic_block(print_fn, "entry");
            self.builder.position_at_end(basic_block);
            let format_str = self.builder.build_global_string_ptr("%lld\n", "printf_int_format")?;
            let param = print_fn.get_first_param().unwrap();
            self.builder.build_call(printf_function, &[format_str.as_pointer_value().into(), param.into()], "")?;
            self.builder.build_return(Some(&i8_type.const_int(0, false)))?;
            let print_fn_info = self.db.resolve_ctx.get_fn(FnId(0));
            self.functions.insert((FnId(0), print_fn_info.ty.clone()), print_fn);
        }
        {
            let print_type = i8_type.fn_type(&[str_type.into()], false);
            let print_fn = self.module.add_function("printStr", print_type, None);
            let basic_block = self.context.append_basic_block(print_fn, "entry");
            self.builder.position_at_end(basic_block);
            let format_str = self.builder.build_global_string_ptr("%s\n", "printf_str_format")?;
            let param = print_fn.get_first_param().unwrap();
            let str_ptr = self.builder.build_alloca(str_type, "str")?;
            self.builder.build_store(str_ptr, param.into_struct_value())?;

            let ptr_ptr = self.builder.build_struct_gep(str_type, str_ptr, 1, "ptr")?;
            let ptr = self.builder.build_load(i8_ptr_type, ptr_ptr, "str")?;

            self.builder.build_call(printf_function, &[format_str.as_pointer_value().into(), ptr.into()], "")?;
            self.builder.build_return(Some(&i8_type.const_int(0, false)))?;
            let print_fn_info = self.db.resolve_ctx.get_fn(FnId(1));
            self.functions.insert((FnId(1), print_fn_info.ty.clone()), print_fn);
        }
        {
            let input_type = i64_type.fn_type(&[], false);
            let input_fn = self.module.add_function("inputInt", input_type, None);
            let basic_block = self.context.append_basic_block(input_fn, "entry");
            self.builder.position_at_end(basic_block);
            let format_str = self.builder.build_global_string_ptr("%lld", "scanf_int_format")?;
            let scanf_ptr = self.builder.build_alloca(i64_type, "scanf_ptr")?;
            self.builder.build_call(scanf_function, &[format_str.as_pointer_value().into(), scanf_ptr.into()], "")?;
            let val = self.builder.build_load(i64_type, scanf_ptr, "tmp")?;
            self.builder.build_return(Some(&val))?;

            let input_fn_info = self.db.resolve_ctx.get_fn(FnId(2));
            self.functions.insert((FnId(2), input_fn_info.ty.clone()), input_fn);
        }
        {
            let input_type = str_type.fn_type(&[i64_type.into()], false);
            let input_fn = self.module.add_function("inputStr", input_type, None);
            let basic_block = self.context.append_basic_block(input_fn, "entry");
            self.builder.position_at_end(basic_block);
            let format_str = self.builder.build_global_string_ptr("%s", "scanf_str_format")?;
            let param = input_fn.get_first_param().unwrap();
            let scanf_ptr = self.builder.build_array_malloc(i8_type, param.into_int_value(), "str")?;
            self.builder.build_call(scanf_function, &[format_str.as_pointer_value().into(), scanf_ptr.into()], "")?;

            let call_site = self.builder.build_call(strlen_function, &[scanf_ptr.into()], "")?;
            let len = call_site.try_as_basic_value().unwrap_left();
            let result = self.build_str_struct(len.into_int_value(), scanf_ptr)?;

            self.builder.build_return(Some(&result))?;
            let input_fn_info = self.db.resolve_ctx.get_fn(FnId(3));
            self.functions.insert((FnId(3), input_fn_info.ty.clone()), input_fn);
        }
        {
            let chr_type = i8_type.fn_type(&[i64_type.into()], false);
            let chr_fn = self.module.add_function("chr", chr_type, None);
            let basic_block = self.context.append_basic_block(chr_fn, "entry");
            self.builder.position_at_end(basic_block);
            let param = chr_fn.get_first_param().unwrap().into_int_value();
            let ret_val = self.builder.build_int_truncate(param, i8_type, "tmp")?;
            self.builder.build_return(Some(&ret_val))?;
            let chr_fn_info = self.db.resolve_ctx.get_fn(FnId(4));
            self.functions.insert((FnId(4), chr_fn_info.ty.clone()), chr_fn);
        }
        {
            let ord_type = i64_type.fn_type(&[i8_type.into()], false);
            let ord_fn = self.module.add_function("ord", ord_type, None);
            let basic_block = self.context.append_basic_block(ord_fn, "entry");
            self.builder.position_at_end(basic_block);
            let param = ord_fn.get_first_param().unwrap().into_int_value();
            let ret_val = self.builder.build_int_z_extend(param, i64_type, "tmp")?;
            self.builder.build_return(Some(&ret_val))?;
            let ord_fn_info = self.db.resolve_ctx.get_fn(FnId(5));
            self.functions.insert((FnId(5), ord_fn_info.ty.clone()), ord_fn);
        }
        {
            let str_type = str_type.fn_type(&[i64_type.into()], false);
            let str_fn = self.module.add_function("str", str_type, None);
            let basic_block = self.context.append_basic_block(str_fn, "entry");
            self.builder.position_at_end(basic_block);
            let format_str = self.builder.build_global_string_ptr("%d", "sprintf_str_format")?;
            let param = str_fn.get_first_param().unwrap();
            let sprintf_ptr = self.builder.build_array_malloc(i8_type, i64_type.const_int(20, false), "str")?;
            self.builder.build_call(sprintf_function, &[sprintf_ptr.into(), format_str.as_pointer_value().into(), param.into()], "")?;

            let call_site = self.builder.build_call(strlen_function, &[sprintf_ptr.into()], "")?;
            let len = call_site.try_as_basic_value().unwrap_left();
            let result = self.build_str_struct(len.into_int_value(), sprintf_ptr)?;

            self.builder.build_return(Some(&result))?;
            let str_fn_info = self.db.resolve_ctx.get_fn(FnId(6));
            self.functions.insert((FnId(6), str_fn_info.ty.clone()), str_fn);
        }
        {
            let some_fn_info = self.db.resolve_ctx.get_fn(FnId(7));
            for instance in &some_fn_info.instances {
                let some_fn = self.build_some_fn(&instance)?;
                self.functions.insert((FnId(7), instance.clone()), some_fn);
            }
        }
        Ok(())
    }

    pub fn build_some_fn(&mut self, func_ty: &FuncType) -> Result<FunctionValue<'ctx>, CompileError> {
        let ty = func_ty.params_ty.first().unwrap();
        let llvm_ty = self.compile_ty(ty)?;
        let some_type = llvm_ty.ptr_type(AddressSpace::default()).fn_type(&[llvm_ty.clone().into()], false);
        let mangled_name = format!("some#{}", func_ty.mangle());
        let some_fn = self.module.add_function(&mangled_name, some_type, None);
        let basic_block = self.context.append_basic_block(some_fn, "entry");
        self.builder.position_at_end(basic_block);
        let param = some_fn.get_first_param().unwrap();
        let ptr = self.builder.build_malloc(llvm_ty.clone(), "ptr")?;
        self.builder.build_store(ptr, param)?;
        self.builder.build_return(Some(&ptr))?;
        Ok(some_fn)
    }

    fn compile_ty(&self, ty: &Type) -> Result<BasicTypeEnum<'ctx>, CompileError> {
        match ty {
            Type::TyVar(_) => Err(self.compiler_error("Type variable cannot convert to LLVM type")),
            Type::Unit => Ok(self.context.i8_type().into()),
            Type::Int => Ok(self.context.i64_type().into()),
            Type::Bool => Ok(self.context.bool_type().into()),
            Type::Str => Ok(self.context.struct_type(&[self.context.i64_type().into(), self.context.i8_type().ptr_type(AddressSpace::default()).into()], false).into()),
            Type::Char => Ok(self.context.i8_type().into()),
            Type::Array(inner_ty) => Ok(self.compile_ty(inner_ty)?.ptr_type(AddressSpace::default()).into()),
            Type::Tuple(inner_ty) => {
                let tys = inner_ty.iter().map(|ty| self.compile_ty(ty)).collect::<Result<Vec<_>, _>>()?;
                Ok(self.context.struct_type(tys.as_slice(), false).into())
            },
            Type::Option(inner_ty) => Ok(self.compile_ty(inner_ty)?.ptr_type(AddressSpace::default()).into()),
            Type::Invalid => Err(self.compiler_error("Cannot convert to LLVM type")),
        }
    }

    fn compile_stmt(&mut self, stmt: Stmt) -> Result<BasicValueEnum<'ctx>, CompileError> {
        match stmt {
            Stmt::EmptyStmt { range: _ } => {
                let i8_type = self.context.i8_type();
                Ok(i8_type.const_int(0, false).into())
            },
            Stmt::LetStmt { var_id, expr, range: _ } => {
                let var_id = var_id.ok_or(self.compiler_error("Variable id is None"))?;
                let var_info = self.db.resolve_ctx.get_var(var_id);
                let ty = self.compile_ty(&*var_info.ty.borrow())?;
                let i8_type = self.context.i8_type();
                let var = self.db.resolve_ctx.get_var(var_id);
                let addr = self.builder.build_alloca(ty, var.name.as_str())?;
                let val = self.compile_expr_idx(expr)?;
                self.builder.build_store(addr, val)?;
                self.addresses.insert(var_id, addr);
                Ok(i8_type.const_int(0, false).into())
            },
            Stmt::WhileStmt { cond, block, range: _ } => {
                let i8_type = self.context.i8_type();
                let cur_func = self.cur_function.unwrap();
                let cond_block = self.context.append_basic_block(cur_func, "loopcond");
                let loop_block = self.context.append_basic_block(cur_func, "loop");
                let after_block = self.context.append_basic_block(cur_func, "after");

                self.builder.build_unconditional_branch(cond_block)?;
                self.builder.position_at_end(cond_block);
                let cond_val = self.compile_expr_idx(cond)?.into_int_value();
                self.builder.build_conditional_branch(cond_val, loop_block, after_block)?;

                self.builder.position_at_end(loop_block);
                self.loop_stack.push((cond_block, after_block));
                self.compile_expr_idx(block)?;
                self.loop_stack.pop();
                self.builder.build_unconditional_branch(cond_block)?;

                self.builder.position_at_end(after_block);
                Ok(i8_type.const_int(0, false).into())
            },
            Stmt::BreakStmt { range: _ } => {
                let i8_type = self.context.i8_type();
                let cur_func = self.cur_function.unwrap();
                let unreachable_block = self.context.append_basic_block(cur_func, "unreachable");
                let &(_, after_block) = self.loop_stack.last().unwrap();
                self.builder.build_unconditional_branch(after_block)?;
                self.builder.position_at_end(unreachable_block);
                Ok(i8_type.const_int(0, false).into())
            },
            Stmt::ContinueStmt { range: _ } => {
                let i8_type = self.context.i8_type();
                let cur_func = self.cur_function.unwrap();
                let unreachable_block = self.context.append_basic_block(cur_func, "unreachable");
                let &(cond_block, _) = self.loop_stack.last().unwrap();
                self.builder.build_unconditional_branch(cond_block)?;
                self.builder.position_at_end(unreachable_block);
                Ok(i8_type.const_int(0, false).into())
            },
            Stmt::ExprStmt { expr, range: _ } => {
                self.compile_expr_idx(expr)
            },
            Stmt::FuncDef { func: _, range: _ } => {
                let i8_type = self.context.i8_type();
                Ok(i8_type.const_int(0, false).into())
            },
        }
    }

    fn compile_func(&mut self, func: Func) -> Result<(), CompileError> {
        if let Some(fn_info) = func.fn_info {
            let params_type = fn_info.ty.params_ty.iter().map(|ty| self.compile_ty(ty).map(|ty| ty.into())).collect::<Result<Vec<_>, _>>()?;
            let return_ty = self.compile_ty(&fn_info.ty.return_ty)?;
            let func_type = return_ty.fn_type(params_type.as_slice(), false);
            let func_value = self.module.add_function(fn_info.name.as_str(), func_type, None);
            self.functions.insert((fn_info.id, fn_info.ty), func_value);
            self.cur_function = Some(func_value);
            let basic_block = self.context.append_basic_block(func_value, "entry");
            self.builder.position_at_end(basic_block);
            for (param, var_id) in func_value.get_param_iter().zip(fn_info.params) {
                let var_name = &self.db.resolve_ctx.get_var(var_id).name;
                let addr = self.builder.build_alloca(param.get_type(), var_name.as_str())?;
                self.builder.build_store(addr, param)?;
                self.addresses.insert(var_id, addr);
            }
            let ret = self.compile_expr_idx(func.block)?;
            self.builder.build_return(Some(&ret))?;
        }
        Ok(())
    }

    fn compile_lvalue(&mut self, expr: Expr) -> Result<PointerValue<'ctx>, CompileError> {
        match expr {
            Expr::Ref { var_id, range: _ } => {
                let var_id = var_id.ok_or(self.compiler_error("var"))?;
                Ok(self.addresses[&var_id])
            },
            Expr::Index { main_expr, index_expr, range: _ } => {
                let main_ty = &self.type_inferred.expr_ty(main_expr);
                let inner_ty = main_ty.inner_ty().ok_or(self.compiler_error("Indexed values do not have an inner type"))?;
                let inner_ty = self.compile_ty(&inner_ty)?;
                let main_ty = self.compile_ty(main_ty)?;
                let lvalue = self.compile_lvalue(self.db.exprs[main_expr].clone())?;
                let main_val = self.builder.build_load(main_ty, lvalue, "tmp")?.into_pointer_value();
                let index_val = self.compile_expr_idx(index_expr)?.into_int_value();
                let ptr = unsafe { self.builder.build_gep(inner_ty, main_val, &[index_val], "ptr")? };
                Ok(ptr)
            },
            _ => unreachable!()
        }
    }

    fn compile_expr_idx(&mut self, idx: ExprIdx) -> Result<BasicValueEnum<'ctx>, CompileError> {
        self.compile_expr(idx, self.db.exprs[idx].clone())
    }

    fn compile_expr(&mut self, idx: ExprIdx, expr: Expr) -> Result<BasicValueEnum<'ctx>, CompileError> {
        match expr {
            Expr::Missing => Err(self.compiler_error("Expr is missing")),
            Expr::Binary { op, lhs, rhs, range: _ } => {
                let lhs_ty = &self.type_inferred.expr_ty(lhs);
                let rhs_ty = &self.type_inferred.expr_ty(rhs);
                match (&op, lhs_ty, rhs_ty) {
                    (BinaryOp::Assign, _, _) => {
                        let ptr = self.compile_lvalue(self.db.exprs[lhs].clone())?;
                        let rhs_value = self.compile_expr_idx(rhs)?;
                        self.builder.build_store(ptr, rhs_value)?;
                        Ok(rhs_value)
                    },
                    (BinaryOp::Add, Type::Str, Type::Str) => {
                        let lhs_value = self.compile_expr_idx(lhs)?.into_struct_value();
                        let rhs_value = self.compile_expr_idx(rhs)?.into_struct_value();
                        let result = self.build_str_concat(lhs_value, rhs_value)?;
                        Ok(result.into())
                    },
                    (_, Type::Int, Type::Int) => {
                        let lhs_value = self.compile_expr_idx(lhs)?.into_int_value();
                        let rhs_value = self.compile_expr_idx(rhs)?.into_int_value();
                        let int_ret = match op {
                            BinaryOp::Add => self.builder.build_int_add(lhs_value, rhs_value, "add"),
                            BinaryOp::Sub => self.builder.build_int_sub(lhs_value, rhs_value, "sub"),
                            BinaryOp::Mul => self.builder.build_int_mul(lhs_value, rhs_value, "mul"),
                            BinaryOp::Div => self.builder.build_int_signed_div(lhs_value, rhs_value, "div"),
                            BinaryOp::Rem => self.builder.build_int_signed_rem(lhs_value, rhs_value, "rem"),
                            BinaryOp::EqEq => self.builder.build_int_compare(IntPredicate::EQ, lhs_value, rhs_value, "eq"),
                            BinaryOp::Neq => self.builder.build_int_compare(IntPredicate::NE, lhs_value, rhs_value, "ne"),
                            BinaryOp::Ge => self.builder.build_int_compare(IntPredicate::SGE, lhs_value, rhs_value, "ge"),
                            BinaryOp::Le => self.builder.build_int_compare(IntPredicate::SLE, lhs_value, rhs_value, "le"),
                            BinaryOp::Gt => self.builder.build_int_compare(IntPredicate::SGT, lhs_value, rhs_value, "gt"),
                            BinaryOp::Lt => self.builder.build_int_compare(IntPredicate::SLT, lhs_value, rhs_value, "lt"),
                            _ => unreachable!(),
                        };
                        Ok(int_ret?.into())
                    },
                    (_, Type::Bool, Type::Bool) => {
                        let lhs_value = self.compile_expr_idx(lhs)?.into_int_value();
                        let rhs_value = self.compile_expr_idx(rhs)?.into_int_value();
                        let int_ret = match op {
                            BinaryOp::And => self.builder.build_and(lhs_value, rhs_value, "and"),
                            BinaryOp::Or => self.builder.build_or(lhs_value, rhs_value, "or"),
                            _ => unreachable!(),
                        };
                        Ok(int_ret?.into())
                    },
                    _ => unreachable!("{:?}, {:?}, {:?}", op, lhs_ty, rhs_ty),
                }
            },
            Expr::Unary { op, expr, range: _ } => {
                let expr_value = self.compile_expr_idx(expr)?.into_int_value();
                let int_ret = match op {
                    UnaryOp::Neg => self.builder.build_int_neg(expr_value, "neg"),
                };
                Ok(int_ret?.into())
            },
            Expr::Ref { var_id, range: _ } => {
                let var_id = var_id.ok_or(self.compiler_error("Variable id is None"))?;
                let var_info = self.db.resolve_ctx.get_var(var_id);
                let ty = self.compile_ty(&*var_info.ty.borrow())?;
                let ptr = self.addresses[&var_id];
                Ok(self.builder.build_load(ty, ptr, "tmp")?)
            },
            Expr::Tuple { elements, range: _ } => {
                let struct_ty = self.compile_ty(&self.type_inferred.expr_ty(idx))?;
                let val = self.builder.build_malloc(struct_ty, "val")?;
                for (i, &expr) in elements.iter().enumerate() {
                    let ptr = self.builder.build_struct_gep(struct_ty, val, i as u32, "tmp")?;
                    let element = self.compile_expr_idx(expr)?;
                    self.builder.build_store(ptr, element)?;
                }
                Ok(val.into())
            },
            Expr::If { cond, then_expr, else_expr, range: _ } => {
                let i8_type = self.context.i8_type();
                let cond_val = self.compile_expr_idx(cond)?.into_int_value();
                let cur_func = self.cur_function.unwrap();
                let then_block = self.context.append_basic_block(cur_func, "then");
                let else_block = self.context.append_basic_block(cur_func, "else");
                let merge_block = self.context.append_basic_block(cur_func, "merge");
                self.builder.build_conditional_branch(cond_val, then_block, else_block)?;

                self.builder.position_at_end(then_block);
                let then_val = self.compile_expr_idx(then_expr)?;
                self.builder.build_unconditional_branch(merge_block)?;
                let then_block = self.builder.get_insert_block().ok_or(self.compiler_error("Insert block is missing"))?;

                self.builder.position_at_end(else_block);
                let else_val = if let Some(else_expr) = else_expr {
                    self.compile_expr_idx(else_expr)?
                } else {
                    i8_type.const_int(0, false).into()
                };
                self.builder.build_unconditional_branch(merge_block)?;
                let else_block = self.builder.get_insert_block().ok_or(self.compiler_error("Insert block is missing"))?;

                self.builder.position_at_end(merge_block);
                let phi_node = self.builder.build_phi(then_val.get_type(), "merge")?;
                phi_node.add_incoming(&[(&then_val, then_block), (&else_val, else_block)]);
                Ok(phi_node.as_basic_value())
            },
            Expr::FnCall { fn_id, args, range: _ } => {
                let fn_id = fn_id.ok_or(self.compiler_error("Function id is None"))?;
                let fn_ty = self.type_inferred.fn_calls.get(idx).ok_or(self.compiler_error("Function type is missing"))?.clone();
                let function = self.functions[&(fn_id, fn_ty)];
                let args: Vec<_> = args.iter()
                    .map(|&expr| self.compile_expr_idx(expr).map(|expr| BasicMetadataValueEnum::from(expr)))
                    .collect::<Result<_, _>>()?;
                let call_site = self.builder.build_call(function, &*args, "tmp")?;
                let ret_val = call_site.try_as_basic_value().left().ok_or(self.compiler_error("Return value is not a basic value"))?;
                Ok(ret_val)
            },
            Expr::Index { main_expr, index_expr, range: _ } => {
                let main_ty = &self.type_inferred.expr_ty(main_expr);
                if main_ty == &Type::Str {
                    let i64_type = self.context.i64_type();
                    let i8_type = self.context.i8_type();
                    let str_type = self.context.struct_type(&[i64_type.into(), i8_type.ptr_type(AddressSpace::default()).into()], false);
                    let i8_ptr_type = i8_type.ptr_type(AddressSpace::default());

                    let main_val = self.compile_expr_idx(main_expr)?;
                    let index_val = self.compile_expr_idx(index_expr)?.into_int_value();
                    let str_ptr = self.builder.build_alloca(str_type, "str")?;
                    self.builder.build_store(str_ptr, main_val)?;

                    let ptr_ptr = self.builder.build_struct_gep(str_type, str_ptr, 1, "ptr")?;
                    let ptr = self.builder.build_load(i8_ptr_type, ptr_ptr, "str")?;
                    let val_ptr = unsafe { self.builder.build_gep(i8_type, ptr.into_pointer_value(), &[index_val], "ptr")? };

                    Ok(self.builder.build_load(i8_type, val_ptr, "tmp")?.into())
                } else {
                    let inner_ty = main_ty.inner_ty().ok_or(self.compiler_error("Indexed values do not have an inner type"))?;
                    let inner_ty = self.compile_ty(&inner_ty)?;
                    let main_val = self.compile_expr_idx(main_expr)?.into_pointer_value();
                    let index_val = self.compile_expr_idx(index_expr)?.into_int_value();
                    let val_ptr = unsafe { self.builder.build_gep(inner_ty, main_val, &[index_val], "ptr")? };
                    Ok(self.builder.build_load(inner_ty, val_ptr, "tmp")?.into())
                }
            },
            Expr::Block { stmts, range: _ } => {
                let i8_type = self.context.i8_type();
                stmts.iter().map(|&stmt| self.compile_stmt(self.db.stmts[stmt].clone())).last().unwrap_or(Ok(i8_type.const_int(0, false).into()))
            },
            Expr::NoneLiteral { range: _ } => {
                let ty = self.compile_ty(&self.type_inferred.expr_ty(idx))?.into_pointer_type();
                Ok(ty.const_null().into())
            },
            Expr::NumberLiteral { n, range: _ } => {
                let i64_type = self.context.i64_type();
                let n = n.ok_or(self.compiler_error("Number literal is missing"))?;
                Ok(i64_type.const_int(n, false).into())
            },
            Expr::BoolLiteral { val, range: _ } => {
                let bool_type = self.context.bool_type();
                Ok(bool_type.const_int(val as u64, false).into())
            },
            Expr::StringLiteral { val, range: _ } => {
                let i64_type = self.context.i64_type();
                let val = val.ok_or(self.compiler_error("String literal is missing"))?;
                let len = val.len() as u64;
                let str = self.context.const_string(val.as_bytes(), true);
                let str_ptr = self.builder.build_malloc(str.get_type(), "str")?;
                self.builder.build_store(str_ptr, str)?;
                let result = self.build_str_struct(i64_type.const_int(len, false), str_ptr)?;
                Ok(result.into())
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
                    len_mul = self.builder.build_int_mul(len_mul, x_val, "tmp")?;
                    arr_types.push(arr_types.last().unwrap().ptr_type(AddressSpace::default()).into());
                }
                arr_types.reverse();
                let all_array = self.builder.build_array_malloc(initial_val.get_type(), len_mul, "array")?;

                let cur_func = self.cur_function.unwrap();
                let cond_block = self.context.append_basic_block(cur_func, "loopcond");
                let loop_block = self.context.append_basic_block(cur_func, "loop");
                let after_block = self.context.append_basic_block(cur_func, "after");

                let idx_addr = self.builder.build_alloca(i64_type, "idx")?;
                self.builder.build_store(idx_addr, i64_type.const_int(0, false))?;
                self.builder.build_unconditional_branch(cond_block)?;

                self.builder.position_at_end(cond_block);
                let idx_val = self.builder.build_load(i64_type, idx_addr, "idx")?.into_int_value();
                let cond_val = self.builder.build_int_compare(IntPredicate::SLT, idx_val, len_mul, "cmp")?;
                self.builder.build_conditional_branch(cond_val, loop_block, after_block)?;

                self.builder.position_at_end(loop_block);
                unsafe {
                    let val = self.builder.build_in_bounds_gep(initial_val.get_type(), all_array, &[idx_val], "tmp")?;
                    self.builder.build_store(val, initial_val)?;
                };
                let new_idx_val = self.builder.build_int_add(idx_val, i64_type.const_int(1, false), "add")?;
                self.builder.build_store(idx_addr, new_idx_val)?;
                self.builder.build_unconditional_branch(cond_block)?;

                self.builder.position_at_end(after_block);

                let array = self.build_array_initializer(0, i64_type.const_int(0, false), len_vals, arr_types, all_array)?;
                Ok(array.into())
            },
        }
    }

    fn build_array_initializer(&mut self, i: usize, all_idx_val: IntValue<'ctx>, len_vals: Vec<IntValue<'ctx>>, arr_types: Vec<BasicTypeEnum<'ctx>>, all_array: PointerValue<'ctx>) -> Result<PointerValue<'ctx>, CompileError> {
        if i + 1 == len_vals.len() {
            unsafe {
                let ptr = self.builder.build_in_bounds_gep(arr_types[i + 1], all_array, &[all_idx_val], "tmp")?;
                return Ok(ptr);
            }
        }
        let i64_type = self.context.i64_type();
        let array = self.builder.build_array_malloc(arr_types[i + 1], len_vals[i], "array")?;

        let cur_func = self.cur_function.unwrap();
        let cond_block = self.context.append_basic_block(cur_func, "loopcond");
        let loop_block = self.context.append_basic_block(cur_func, "loop");
        let after_block = self.context.append_basic_block(cur_func, "after");

        let idx_addr = self.builder.build_alloca(i64_type, "idx")?;
        self.builder.build_store(idx_addr, i64_type.const_int(0, false))?;
        self.builder.build_unconditional_branch(cond_block)?;

        self.builder.position_at_end(cond_block);
        let idx_val = self.builder.build_load(i64_type, idx_addr, "idx")?.into_int_value();
        let cond_val = self.builder.build_int_compare(IntPredicate::SLT, idx_val, len_vals[i], "cmp")?;
        self.builder.build_conditional_branch(cond_val, loop_block, after_block)?;

        self.builder.position_at_end(loop_block);
        unsafe {
            let val = self.build_array_initializer(i + 1, self.builder.build_int_mul(idx_val, len_vals[i + 1], "tmp")?, len_vals, arr_types, all_array)?;
            let ptr = self.builder.build_in_bounds_gep(val.get_type(), array, &[idx_val], "tmp")?;
            self.builder.build_store(ptr, val)?;
        };
        let new_idx_val = self.builder.build_int_add(idx_val, i64_type.const_int(1, false), "add")?;
        self.builder.build_store(idx_addr, new_idx_val)?;
        self.builder.build_unconditional_branch(cond_block)?;

        self.builder.position_at_end(after_block);

        Ok(array)
    }

    fn build_str_struct(&mut self, len: IntValue<'ctx>, ptr: PointerValue<'ctx>) -> Result<StructValue<'ctx>, CompileError> {
        let i64_type = self.context.i64_type();
        let i8_type = self.context.i8_type();
        let str_type = self.context.struct_type(&[i64_type.into(), i8_type.ptr_type(AddressSpace::default()).into()], false);
        let result = str_type.get_undef();
        let result = self.builder.build_insert_value(result, len, 0, "res")?;
        Ok(self.builder.build_insert_value(result, ptr, 1, "res")?.into_struct_value())
    }

    fn build_str_concat(&mut self, lhs: StructValue<'ctx>, rhs: StructValue<'ctx>) -> Result<StructValue<'ctx>, CompileError> {
        let i64_type = self.context.i64_type();
        let i8_type = self.context.i8_type();
        let i8_ptr_type = i8_type.ptr_type(AddressSpace::default());
        let str_type = self.context.struct_type(&[i64_type.into(), i8_type.ptr_type(AddressSpace::default()).into()], false);
        let lhs_str_ptr = self.builder.build_alloca(str_type, "str")?;
        let rhs_str_ptr = self.builder.build_alloca(str_type, "str")?;
        self.builder.build_store(lhs_str_ptr, lhs)?;
        self.builder.build_store(rhs_str_ptr, rhs)?;

        let lhs_len_ptr = self.builder.build_struct_gep(str_type, lhs_str_ptr, 0, "len")?;
        let lhs_ptr_ptr = self.builder.build_struct_gep(str_type, lhs_str_ptr, 1, "ptr")?;
        let lhs_len = self.builder.build_load(i64_type, lhs_len_ptr, "str")?.into_int_value();
        let lhs_ptr = self.builder.build_load(i8_ptr_type, lhs_ptr_ptr, "str")?;

        let rhs_len_ptr = self.builder.build_struct_gep(str_type, rhs_str_ptr, 0, "len")?;
        let rhs_ptr_ptr = self.builder.build_struct_gep(str_type, rhs_str_ptr, 1, "ptr")?;
        let rhs_len = self.builder.build_load(i64_type, rhs_len_ptr, "str")?.into_int_value();
        let rhs_ptr = self.builder.build_load(i8_ptr_type, rhs_ptr_ptr, "str")?;

        let new_len = self.builder.build_int_add(lhs_len, rhs_len, "add")?;
        let malloc_size = self.builder.build_int_add(new_len, i64_type.const_int(1, false), "size")?;
        let rhs_cpy_size = self.builder.build_int_add(rhs_len, i64_type.const_int(1, false), "tmp")?;
        let new_str_ptr = self.builder.build_array_malloc(i8_type, malloc_size, "str")?;

        self.builder.build_memcpy(new_str_ptr, 8, lhs_ptr.into_pointer_value(), 8, lhs_len)?;
        let rhs_dest = unsafe { self.builder.build_gep(i8_type, new_str_ptr, &[lhs_len], "tmp")? };
        self.builder.build_memcpy(rhs_dest, 1, rhs_ptr.into_pointer_value(), 8, rhs_cpy_size)?;

        self.build_str_struct(new_len, new_str_ptr)
    }
}

#[derive(Debug)]
pub enum CompileError {
    CompilerError(String),
    BuilderError(BuilderError),
}

impl From<BuilderError> for CompileError {
    fn from(err: BuilderError) -> CompileError {
        CompileError::BuilderError(err)
    }
}
