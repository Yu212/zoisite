use inkwell::context::Context;
use inkwell::OptimizationLevel;
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
pub mod validation;
pub mod database;
pub mod token_set;
pub mod resolve_context;
pub mod scope;

pub fn parse_no_output(text: &str) {
    let lexer = Lexer::new(text);
    let (tokens, lexer_errors) = lexer.tokenize();
    let parser = Parser::new(tokens);
    let (syntax, parser_errors) = parser.parse();
    let root = Root::cast(syntax).unwrap();
    let validation_errors = validation::validate(&root);
    if !lexer_errors.is_empty() || !parser_errors.is_empty() || !validation_errors.is_empty() {
        return;
    }
    let mut db = Database::new();
    let hir = db.lower_root(root);
    let context = Context::create();
    let compiler = Compiler::new(&context, db, "main");
    let module = compiler.compile(&hir);
    let engine = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();
    unsafe {
        engine.get_function::<unsafe extern "C" fn() -> i32>("main").unwrap().call();
    }
}

pub fn parse(text: &str) {
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
    let validation_errors = validation::validate(&root);
    if !lexer_errors.is_empty() || !parser_errors.is_empty() || !validation_errors.is_empty() {
        return;
    }
    let mut db = Database::new();
    let hir = db.lower_root(root);
    println!("hir: ");
    println!("{:?}", hir);
    let context = Context::create();
    let compiler = Compiler::new(&context, db, "main");
    let module = compiler.compile(&hir);
    println!("llvm ir:");
    module.print_to_stderr();
    let engine = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();
    println!("output:");
    unsafe {
        engine.get_function::<unsafe extern "C" fn() -> i32>("main").unwrap().call();
    }
}
