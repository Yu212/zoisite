use inkwell::context::Context;
use inkwell::OptimizationLevel;
use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::parser::Parser;

pub mod parser;
pub mod grammar;
pub mod language;
pub mod lexer;
pub mod event;
pub mod syntax_error;
pub mod ast;
pub mod token;
pub mod syntax_kind;
pub mod hir;
pub mod compiler;
pub mod validation;

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
        println!("{:?}@{:?}", err.message, err.range);
    }
    println!();
    println!("parser errors: ");
    for err in &parser_errors {
        println!("{:?}@{:?}", err.message, err.range);
    }
    println!();
    println!("tree: ");
    println!("{:#?}", syntax);
    let root = ast::Root::cast(syntax).unwrap();
    let validation_errors = validation::validate(&root);
    if !lexer_errors.is_empty() || !parser_errors.is_empty() || !validation_errors.is_empty() {
        return;
    }

    let hir = hir::lower_root(root);
    println!("hir: ");
    println!("{:?}", hir);
    let context = Context::create();
    let compiler = Compiler::new(&context, "main");
    let module = compiler.compile(hir);
    println!("llvm ir:");
    module.print_to_stderr();
    let engine = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();
    println!("output:");
    unsafe {
        engine.get_function::<unsafe extern "C" fn() -> i32>("main").unwrap().call();
    }
}
