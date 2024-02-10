use inkwell::context::Context;
use inkwell::OptimizationLevel;

use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::parser::Parser;

mod parser;
mod grammar;
mod language;
mod lexer;
mod event;
mod syntax_error;
mod ast;
mod token;
mod syntax_kind;
mod hir;
mod compiler;

fn main() {
    let text = " 123 + 6 * 4";
    let lexer = Lexer::new(text);
    let (tokens, lexer_errors) = lexer.tokenize();
    let parser = Parser::new(tokens);
    let (syntax, parser_errors) = parser.parse();
    println!("input: ");
    println!("{:?}", text);
    println!();
    println!("lexer errors: ");
    for err in lexer_errors {
        println!("{:?}@{:?}", err.message, err.range);
    }
    println!();
    println!("parser errors: ");
    for err in parser_errors {
        println!("{:?}@{:?}", err.message, err.range);
    }
    println!();
    println!("tree: ");
    println!("{:#?}", syntax);
    let root = ast::Root::cast(syntax).unwrap();
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
