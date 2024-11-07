use std::fs;
use std::fs::File;
use std::io::Write;
use std::ops::Index;
use std::path::PathBuf;
use std::process::Command;
use std::time::Instant;

use crate::ast::Root;
use crate::compiler::Compiler;
use crate::database::Database;
use crate::diagnostic::Diagnostic;
use crate::language::SyntaxToken;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::type_infer::TypeInfer;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{InitializationConfig, Target};
use inkwell::OptimizationLevel;
use rowan::ast::AstNode;
use rowan::NodeOrToken;

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
pub mod type_infer;
pub mod r#type;
pub mod visitor;

pub fn parse(text: &str) -> (Vec<SyntaxToken>, hir::Root, Vec<Diagnostic>) {
    let lexer = Lexer::new(text);
    let (tokens, lexer_errors) = lexer.tokenize();
    let parser = Parser::new(tokens);
    let (syntax, parser_errors) = parser.parse();
    let tokens: Vec<_> = syntax.descendants_with_tokens().filter_map(|descendant| match descendant {
        NodeOrToken::Token(token) => Some(token),
        _ => None
    }).collect();
    let root = Root::cast(syntax).unwrap();
    let mut db = Database::new();
    let (hir, lower_errors) = db.lower_root(root);
    if !lexer_errors.is_empty() || !parser_errors.is_empty() || !lower_errors.is_empty() {
        return (tokens, hir, [lexer_errors, parser_errors, lower_errors].concat());
    }
    let type_infer = TypeInfer::new(&mut db);
    let (_, type_check_errors) = type_infer.infer(hir.clone());
    if !type_check_errors.is_empty() {
        return (tokens, hir, type_check_errors);
    }
    (tokens, hir, Vec::new())
}

pub fn compile_no_output(text: &str) {
    let lexer = Lexer::new(text);
    let (tokens, lexer_errors) = lexer.tokenize();
    let parser = Parser::new(tokens);
    let (syntax, parser_errors) = parser.parse();
    let root = Root::cast(syntax).unwrap();
    let mut db = Database::new();
    let (hir, lower_errors) = db.lower_root(root);
    if !lexer_errors.is_empty() || !parser_errors.is_empty() || !lower_errors.is_empty() {
        return;
    }
    let type_infer = TypeInfer::new(&mut db);
    let (type_inferred, type_check_errors) = type_infer.infer(hir.clone());
    if !type_check_errors.is_empty() {
        return;
    }
    let context = Context::create();
    let compiler = Compiler::new(&context, &db, type_inferred, "main");
    let module = compiler.compile(&hir);
    let engine = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();
    unsafe {
        engine.get_function::<unsafe extern "C" fn() -> i32>("main").unwrap().call();
    }
}

pub fn compile(text: &str) {
    let start = Instant::now();
    let lexer = Lexer::new(text);
    let (tokens, lexer_errors) = lexer.tokenize();
    let parser = Parser::new(tokens);
    let (syntax, parser_errors) = parser.parse();
    eprintln!("lexer errors: ");
    for err in &lexer_errors {
        eprintln!("{:?} {:?}", err, text.index(err.range));
    }
    eprintln!("parser errors: ");
    for err in &parser_errors {
        eprintln!("{:?} {:?}", err, text.index(err.range));
    }
    let mut syntax_file = File::create("output.syntax").unwrap();
    syntax_file.write_all(format!("{:#?}", syntax).as_bytes()).unwrap();
    let root = Root::cast(syntax).unwrap();
    let mut db = Database::new();
    let (hir, lower_errors) = db.lower_root(root);
    eprintln!("lower errors: ");
    for err in &lower_errors {
        eprintln!("{:?} {:?}", err, text.index(err.range));
    }
    if !lexer_errors.is_empty() || !parser_errors.is_empty() || !lower_errors.is_empty() {
        return;
    }
    let type_infer = TypeInfer::new(&mut db);
    let (type_inferred, type_check_errors) = type_infer.infer(hir.clone());
    eprintln!("type check errors: ");
    for err in &type_check_errors {
        eprintln!("{:?} {:?}", err, text.index(err.range));
    }
    if !type_check_errors.is_empty() {
        return;
    }
    let context = Context::create();
    let compiler = Compiler::new(&context, &db, type_inferred, "main");
    let module = compiler.compile(&hir);
    println!("compile: {} ms", start.elapsed().as_millis());
    module.print_to_file(PathBuf::from("output.ll")).expect("print_to_file failed");
    optimize(&module);
    println!("optimize: {} ms", start.elapsed().as_millis());
    module.print_to_file(PathBuf::from("output_optimized.ll")).expect("print_to_file failed");
    generate_submission_file(text, &module);
    run_llvm_ir();
}

pub fn generate_submission_file(text: &str, module: &Module) {
    let template = fs::read_to_string("submission_template.ll").unwrap();
    let mut submission_file = File::create("submission.ll").unwrap();
    let commented_out = text.lines().map(|line| format!("; {}", line)).collect::<Vec<_>>().join("\n");
    submission_file.write_all(template.replace("{code}", &commented_out).replace("{ir}", &module.to_string()).as_bytes()).unwrap();
}

pub fn run_llvm_ir() {
    Command::new("/usr/lib/llvm-16/bin/clang")
        .current_dir(PathBuf::from("."))
        .args(vec!["-O2", "-o", "a.out", "output_optimized.ll"])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    Command::new("./a.out")
        .stdin(File::open("input.txt").unwrap())
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
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
