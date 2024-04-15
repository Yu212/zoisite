use std::mem;
use std::path::PathBuf;

use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::OptimizationLevel;
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{InitializationConfig, Target};
use rowan::ast::AstNode;

use crate::ast::Root;
use crate::compiler::Compiler;
use crate::database::Database;
use crate::lexer::Lexer;
use crate::parser::Parser;

pub mod parser;
pub mod grammar;
pub mod language;
pub mod lexer;
pub mod event;
pub mod diagnostic;
pub mod ast;
pub mod token;
pub mod syntax_kind;
pub mod hir;
pub mod compiler;
pub mod database;
pub mod token_set;
pub mod resolve_context;
pub mod scope;

pub fn compile_no_output(text: &str) {
    let lexer = Lexer::new(text);
    let (tokens, lexer_errors) = lexer.tokenize();
    let parser = Parser::new(tokens);
    let (syntax, parser_errors) = parser.parse();
    let root = Root::cast(syntax).unwrap();
    let mut db = Database::new();
    let hir = db.lower_root(root);
    let lower_errors = mem::take(&mut db.diagnostics);
    if !lexer_errors.is_empty() || !parser_errors.is_empty() || !lower_errors.is_empty() {
        return;
    }
    let context = Context::create();
    let compiler = Compiler::new(&context, db, "main");
    let module = compiler.compile(&hir);
    let engine = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();
    unsafe {
        engine.get_function::<unsafe extern "C" fn() -> i32>("main").unwrap().call();
    }
}

pub fn compile(text: &str) {
    let lexer = Lexer::new(text);
    let (tokens, lexer_errors) = lexer.tokenize();
    let parser = Parser::new(tokens);
    let (syntax, parser_errors) = parser.parse();
    println!("input: ");
    println!("{:?}", text);
    println!();
    println!("lexer errors: ");
    for err in &lexer_errors {
        println!("{:?}", err);
    }
    println!();
    println!("parser errors: ");
    for err in &parser_errors {
        println!("{:?}", err);
    }
    println!();
    println!("tree: ");
    println!("{:#?}", syntax);
    let root = Root::cast(syntax).unwrap();
    let mut db = Database::new();
    let hir = db.lower_root(root);
    let lower_errors = mem::take(&mut db.diagnostics);
    println!();
    println!("lower errors: ");
    for err in &lower_errors {
        println!("{:?}", err);
    }
    println!();
    println!("hir: ");
    println!("{:?}", hir);
    if !lexer_errors.is_empty() || !parser_errors.is_empty() || !lower_errors.is_empty() {
        return;
    }
    let context = Context::create();
    let compiler = Compiler::new(&context, db, "main");
    let module = compiler.compile(&hir);
    println!("llvm ir:");
    module.print_to_stderr();
    let engine = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();
    module.print_to_file(PathBuf::from("./files/output.ll")).expect("print_to_file failed");
    println!("output:");
    unsafe {
        engine.get_function::<unsafe extern "C" fn() -> i32>("main").unwrap().call();
    }
    optimize(&module);
    module.print_to_file(PathBuf::from("./files/output_optimized.ll")).expect("print_to_file failed");
}

pub fn optimize(module: &Module) {
    let config = InitializationConfig::default();
    Target::initialize_native(&config).unwrap();
    let pass_manager_builder = PassManagerBuilder::create();
    pass_manager_builder.set_optimization_level(OptimizationLevel::Aggressive);
    let pass_manager = PassManager::create(());
    pass_manager.add_promote_memory_to_register_pass();
    pass_manager.add_always_inliner_pass();
    pass_manager.add_gvn_pass();
    pass_manager.add_new_gvn_pass();
    pass_manager.add_function_attrs_pass();
    pass_manager.add_constant_merge_pass();
    pass_manager.add_scalarizer_pass();
    pass_manager.add_merged_load_store_motion_pass();
    pass_manager.add_instruction_combining_pass();
    pass_manager.add_memcpy_optimize_pass();
    pass_manager.add_partially_inline_lib_calls_pass();
    pass_manager.add_lower_switch_pass();
    pass_manager.add_reassociate_pass();
    pass_manager.add_simplify_lib_calls_pass();
    pass_manager.add_instruction_simplify_pass();
    pass_manager.add_function_inlining_pass();
    pass_manager.add_global_optimizer_pass();
    pass_manager.add_dead_arg_elimination_pass();
    pass_manager.add_strip_symbol_pass();
    pass_manager.add_strip_dead_prototypes_pass();
    pass_manager.add_internalize_pass(true);
    pass_manager.add_sccp_pass();
    pass_manager.add_aggressive_dce_pass();
    pass_manager.add_global_dce_pass();
    pass_manager.add_tail_call_elimination_pass();
    pass_manager.add_basic_alias_analysis_pass();
    pass_manager.add_licm_pass();
    pass_manager.add_ind_var_simplify_pass();
    pass_manager.add_loop_vectorize_pass();
    pass_manager.add_loop_idiom_pass();
    pass_manager.add_loop_rotate_pass();
    pass_manager.add_loop_unroll_pass();
    pass_manager.add_loop_deletion_pass();
    pass_manager.add_cfg_simplification_pass();
    pass_manager.add_verifier_pass();
    pass_manager.run_on(module);
    pass_manager_builder.populate_module_pass_manager(&pass_manager);
}
