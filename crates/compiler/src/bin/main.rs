use clap::{Parser, ValueEnum};
use std::fs;
use zoisite::*;

#[derive(Debug, Clone, ValueEnum, PartialEq)]
enum EmitFormat {
    Ir,
    Submission,
    Executable,
    Syntax,
}

#[derive(Parser, Debug)]
#[command(name = "Zoisite")]
#[clap(author, version, about)]
struct Args {
    input: String,

    #[clap(
        long,
        value_enum,
        value_delimiter = ',',
        default_values_t = [EmitFormat::Ir, EmitFormat::Submission, EmitFormat::Executable],
    )]
    emit: Vec<EmitFormat>,

    #[clap(short = 'O', long = "optimize")]
    optimize: bool,
}

fn main() {
    let args = Args::parse();

    let source = fs::read_to_string(&args.input).expect("Failed to read source file");
    
    if args.emit.contains(&EmitFormat::Syntax) {
        let syntax = parse_syntax(&source);
        let output_syntax = format!("{}.ll", args.input);
        fs::write(&output_syntax, format!("{:#?}", syntax).as_bytes()).expect("Failed to write syntax file");
        println!("Syntax tree written to {}", output_syntax);
    }

    let ir_code = compile(&source, args.optimize).expect("Compilation failed");

    if args.emit.contains(&EmitFormat::Ir) {
        let output_ll = format!("{}.ll", args.input);
        fs::write(&output_ll, &ir_code).expect("Failed to write ll file");
        println!("LLVM IR written to {}", output_ll);
    }

    if args.emit.contains(&EmitFormat::Submission) {
        generate_submission_file(&source, &ir_code);
        println!("Submission file written");
    }

    if args.emit.contains(&EmitFormat::Executable) {
        compile_to_executable(&ir_code);
        println!("Executable compiled");
    }
}
