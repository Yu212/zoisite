use crate::ast::Root;
use crate::compiler::Compiler;
use crate::database::Database;
use crate::diagnostic::Diagnostic;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::type_infer::TypeInfer;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::OptimizationLevel;
use language::SyntaxToken;
use rowan::ast::AstNode;
use rowan::NodeOrToken;
use std::fs::File;
use std::io::Write;
use std::ops::Index;
use std::process::{Command, Stdio};
use std::time::Instant;

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
pub mod builtins;

fn print_errors(label: &str, errors: &[Diagnostic], text: &str) {
    if !errors.is_empty() {
        eprintln!("{label}:");
        for err in errors {
            eprintln!("{:?} {:?}", err, text.index(err.range));
        }
    }
}

pub fn generate_submission_file(base_name: &str, code: &str, ir_code: &str) {
    let filename = format!("{base_name}.submission.ll");
    let mut submission_file = File::create(&filename).unwrap();
    let commented_code = code.lines().map(|line| format!("; {line}")).collect::<Vec<_>>().join("\n");
    let version = env!("CARGO_PKG_VERSION");
    let submission_str = format!("\
; compiled from Zoisite v{version}
; https://github.com/Yu212/zoisite

; original code:

{commented_code}



{ir_code}");
    submission_file.write_all(submission_str.as_bytes()).unwrap();
}

pub fn compile_to_executable(base_name: &str, ir_code: &str, clang_args: &[String]) {
    let mut clang = Command::new("clang")
        .arg("-x")
        .arg("ir")
        .args(clang_args)
        .arg("-o")
        .arg(format!("{base_name}.out"))
        .arg("-")
        .stdin(Stdio::piped())
        .spawn()
        .unwrap();
    clang.stdin.as_mut().unwrap().write_all(ir_code.as_bytes()).unwrap();
    clang.wait().unwrap();
}

pub fn optimize(module: &Module) {
    Target::initialize_all(&InitializationConfig::default());
    let target_triple = TargetMachine::get_default_triple();
    let cpu = TargetMachine::get_host_cpu_name().to_string();
    let features = TargetMachine::get_host_cpu_features().to_string();
    let target = Target::from_triple(&target_triple).unwrap();
    let target_machine = target.create_target_machine(&target_triple, &cpu, &features, OptimizationLevel::Aggressive, RelocMode::PIC, CodeModel::Default).unwrap();
    module.run_passes("default<O3>", &target_machine, PassBuilderOptions::create()).unwrap();
}

#[derive(Debug)]
pub struct LifecycleOptions {
    pub base_name: String,
    pub debug: bool,
    pub run_jit: bool,
    pub optimize: bool,
    pub output_syntax: bool,
    pub output_ir: bool,
    pub output_submission: bool,
    pub output_executable: bool,
    pub clang_args: Vec<String>,
}

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
    let mut db = Database::default();
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

pub fn run_lifecycle(text: &str, opts: LifecycleOptions) -> bool {
    let start = Instant::now();

    let lexer = Lexer::new(text);
    let (tokens, lex_errors) = lexer.tokenize();
    let parser = Parser::new(tokens);
    let (syntax, parse_errors) = parser.parse();

    if opts.output_syntax {
        let path = format!("{}.syntax", opts.base_name);
        let mut file = File::create(&path).unwrap();
        writeln!(file, "{syntax:#?}").unwrap();
        if opts.debug {
            println!("Syntax tree written to {path}");
        }
    }

    if opts.debug {
        print_errors("lexer errors", &lex_errors, text);
        print_errors("parser errors", &parse_errors, text);
    }
    if !lex_errors.is_empty() || !parse_errors.is_empty() {
        return false;
    }

    let root = Root::cast(syntax).unwrap();
    let mut db = Database::default();
    let (hir, lower_errs) = db.lower_root(root);
    if opts.debug {
        print_errors("lower errors", &lower_errs, text);
    }
    if !lower_errs.is_empty() {
        return false;
    }

    let ti = TypeInfer::new(&mut db);
    let (typed_hir, type_errs) = ti.infer(hir.clone());
    if opts.debug {
        print_errors("type errors", &type_errs, text);
    }
    if !type_errs.is_empty() {
        return false;
    }

    let context = Context::create();
    let comp = Compiler::new(&context, &db, typed_hir, "main");
    let module = comp.compile(&hir).expect("Codegen error");

    if opts.optimize {
        optimize(&module);
    }

    let ir = module.to_string();

    if opts.output_ir {
        let path = format!("{}.ll", opts.base_name);
        let mut file = File::create(&path).unwrap();
        file.write_all(ir.as_bytes()).unwrap();
        if opts.debug {
            println!("IR written to {path}");
        }
    }

    if opts.output_submission {
        generate_submission_file(&opts.base_name, text, &ir);
        if opts.debug {
            println!("Submission written");
        }
    }

    if opts.output_executable {
        compile_to_executable(&opts.base_name, &ir, &opts.clang_args);
        if opts.debug {
            println!("Executable compiled");
        }
    }

    if opts.run_jit {
        let engine = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();
        unsafe {
            let ret = engine.get_function::<unsafe extern "C" fn() -> i32>("main").unwrap().call();
            return ret == 0;
        }
    }

    if opts.debug {
        println!("Total time: {} ms", start.elapsed().as_millis());
    }

    true
}
