use crate::compiler::{CompileResult, Compiler};
use crate::scope::FnId;
use inkwell::types::{BasicTypeEnum, FunctionType};
use inkwell::values::FunctionValue;
use inkwell::AddressSpace;

pub fn build_function<'ctx, F>(cmp: &mut Compiler<'ctx>, fn_id: FnId, func_name: &str, body: F) -> CompileResult<()> where F: Fn(&mut Compiler<'ctx>, FunctionType<'ctx>, FunctionValue<'ctx>) -> CompileResult<()> {
    let fn_info = cmp.db.resolve_ctx.get_fn(fn_id);
    for instance in &fn_info.instances {
        let mangled_name = format!("{func_name}#{}", instance.mangle());
        let func_ty = cmp.compile_func_ty(instance)?;
        let function = cmp.module.add_function(&mangled_name, func_ty, None);
        body(cmp, func_ty, function)?;
        cmp.functions.insert((fn_info.id, instance.clone()), function);
    }
    Ok(())
}

pub fn add_builtins(cmp: &mut Compiler) -> CompileResult<()> {
    let i64_type = cmp.context.i64_type();
    let f64_type = cmp.context.f64_type();
    let i8_type = cmp.context.i8_type();
    let ptr_type = cmp.context.ptr_type(AddressSpace::default());
    let void_type = cmp.context.void_type();
    let str_type = cmp.context.struct_type(&[i64_type.into(), ptr_type.into()], false);
    let printf_type = void_type.fn_type(&[ptr_type.into()], true);
    let printf_function = cmp.module.add_function("printf", printf_type, None);
    let scanf_type = void_type.fn_type(&[ptr_type.into()], true);
    let scanf_function = cmp.module.add_function("scanf", scanf_type, None);
    let sprintf_type = void_type.fn_type(&[ptr_type.into(), ptr_type.into()], true);
    let sprintf_function = cmp.module.add_function("sprintf", sprintf_type, None);
    let strlen_type = i64_type.fn_type(&[ptr_type.into()], false);
    let strlen_function = cmp.module.add_function("strlen", strlen_type, None);

    build_function(cmp, FnId(0), "printInt", |cmp, _, function| {
        let basic_block = cmp.context.append_basic_block(function, "entry");
        cmp.builder.position_at_end(basic_block);
        let format_str = cmp.builder.build_global_string_ptr("%lld\n", "printf_int_format")?;
        let param = function.get_first_param().unwrap();
        cmp.builder.build_call(printf_function, &[format_str.as_pointer_value().into(), param.into()], "")?;
        cmp.builder.build_return(Some(&i8_type.const_int(0, false)))?;
        Ok(())
    })?;
    build_function(cmp, FnId(1), "printFloat", |cmp, _, function| {
        let basic_block = cmp.context.append_basic_block(function, "entry");
        cmp.builder.position_at_end(basic_block);
        let format_str = cmp.builder.build_global_string_ptr("%.10lf\n", "printf_float_format")?;
        let param = function.get_first_param().unwrap();
        cmp.builder.build_call(printf_function, &[format_str.as_pointer_value().into(), param.into()], "")?;
        cmp.builder.build_return(Some(&i8_type.const_int(0, false)))?;
        Ok(())
    })?;
    build_function(cmp, FnId(2), "printStr", |cmp, _, function| {
        let basic_block = cmp.context.append_basic_block(function, "entry");
        cmp.builder.position_at_end(basic_block);
        let format_str = cmp.builder.build_global_string_ptr("%s\n", "printf_str_format")?;
        let param = function.get_first_param().unwrap();
        let str_ptr = cmp.builder.build_alloca(str_type, "str")?;
        cmp.builder.build_store(str_ptr, param.into_struct_value())?;

        let ptr_ptr = cmp.builder.build_struct_gep(str_type, str_ptr, 1, "ptr")?;
        let ptr = cmp.builder.build_load(ptr_type, ptr_ptr, "str")?;

        cmp.builder.build_call(printf_function, &[format_str.as_pointer_value().into(), ptr.into()], "")?;
        cmp.builder.build_return(Some(&i8_type.const_int(0, false)))?;
        Ok(())
    })?;
    build_function(cmp, FnId(3), "inputInt", |cmp, _, function| {
        let basic_block = cmp.context.append_basic_block(function, "entry");
        cmp.builder.position_at_end(basic_block);
        let format_str = cmp.builder.build_global_string_ptr("%lld", "scanf_int_format")?;
        let scanf_ptr = cmp.builder.build_alloca(i64_type, "scanf_ptr")?;
        cmp.builder.build_call(scanf_function, &[format_str.as_pointer_value().into(), scanf_ptr.into()], "")?;
        let val = cmp.builder.build_load(i64_type, scanf_ptr, "tmp")?;
        cmp.builder.build_return(Some(&val))?;
        Ok(())
    })?;
    build_function(cmp, FnId(4), "inputStr", |cmp, _, function| {
        let basic_block = cmp.context.append_basic_block(function, "entry");
        cmp.builder.position_at_end(basic_block);
        let format_str = cmp.builder.build_global_string_ptr("%s", "scanf_str_format")?;
        let param = function.get_first_param().unwrap();
        let scanf_ptr = cmp.builder.build_array_malloc(i8_type, param.into_int_value(), "str")?;
        cmp.builder.build_call(scanf_function, &[format_str.as_pointer_value().into(), scanf_ptr.into()], "")?;
        let call_site = cmp.builder.build_call(strlen_function, &[scanf_ptr.into()], "")?;
        let len = call_site.try_as_basic_value().unwrap_left();
        let result = cmp.build_str_struct(len.into_int_value(), scanf_ptr)?;
        cmp.builder.build_return(Some(&result))?;
        Ok(())
    })?;
    build_function(cmp, FnId(5), "chr", |cmp, _, function| {
        let basic_block = cmp.context.append_basic_block(function, "entry");
        cmp.builder.position_at_end(basic_block);
        let param = function.get_first_param().unwrap().into_int_value();
        let ret_val = cmp.builder.build_int_truncate(param, i8_type, "tmp")?;
        cmp.builder.build_return(Some(&ret_val))?;
        Ok(())
    })?;
    build_function(cmp, FnId(6), "ord", |cmp, _, function| {
        let basic_block = cmp.context.append_basic_block(function, "entry");
        cmp.builder.position_at_end(basic_block);
        let param = function.get_first_param().unwrap().into_int_value();
        let ret_val = cmp.builder.build_int_z_extend(param, i64_type, "tmp")?;
        cmp.builder.build_return(Some(&ret_val))?;
        Ok(())
    })?;
    build_function(cmp, FnId(7), "str", |cmp, _, function| {
        let basic_block = cmp.context.append_basic_block(function, "entry");
        cmp.builder.position_at_end(basic_block);
        let format_str = cmp.builder.build_global_string_ptr("%lld", "sprintf_str_format")?;
        let param = function.get_first_param().unwrap();
        let sprintf_ptr = cmp.builder.build_array_malloc(i8_type, i64_type.const_int(21, false), "str")?;
        cmp.builder.build_call(sprintf_function, &[sprintf_ptr.into(), format_str.as_pointer_value().into(), param.into()], "")?;
        let call_site = cmp.builder.build_call(strlen_function, &[sprintf_ptr.into()], "")?;
        let len = call_site.try_as_basic_value().unwrap_left();
        let result = cmp.build_str_struct(len.into_int_value(), sprintf_ptr)?;
        cmp.builder.build_return(Some(&result))?;
        Ok(())
    })?;
    build_function(cmp, FnId(8), "float", |cmp, _, function| {
        let basic_block = cmp.context.append_basic_block(function, "entry");
        cmp.builder.position_at_end(basic_block);
        let param = function.get_first_param().unwrap();
        let result = cmp.builder.build_signed_int_to_float(param.into_int_value(), f64_type, "float")?;
        cmp.builder.build_return(Some(&result))?;
        Ok(())
    })?;
    build_function(cmp, FnId(9), "some", |cmp, llvm_ty, function| {
        let basic_block = cmp.context.append_basic_block(function, "entry");
        cmp.builder.position_at_end(basic_block);
        let param = function.get_first_param().unwrap();
        let ty: BasicTypeEnum = llvm_ty.get_param_types()[0].try_into().map_err(|_| cmp.compiler_error("Failed to convert value into BasicTypeEnum"))?;
        let ptr = cmp.builder.build_malloc(ty, "ptr")?;
        cmp.builder.build_store(ptr, param)?;
        cmp.builder.build_return(Some(&ptr))?;
        Ok(())
    })?;
    Ok(())
}