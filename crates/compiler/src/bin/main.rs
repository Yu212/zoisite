use clap::Parser;
use std::fs;
use zoisite::*;

#[derive(Parser, Debug)]
#[command(name = "Zoisite")]
#[clap(author, version, about)]
struct Args {
    input: String,

    #[clap(long, default_value = "ll,submission,executable")]
    emit: String,

    #[clap(short = 'O', long = "optimize")]
    optimize: bool,
}

fn main() {
    let args = Args::parse();
    let options: Vec<&str> = args.emit.split(',')
        .map(|s| s.trim())
        .collect();

    let source = fs::read_to_string(&args.input).expect("Failed to read source file");
    
    if options.contains(&"syntax") {
        let syntax = parse_syntax(&source);
        let output_syntax = format!("{}.ll", args.input);
        fs::write(&output_syntax, format!("{:#?}", syntax).as_bytes()).expect("Failed to write syntax file");
        println!("Syntax tree written to {}", output_syntax);
    }

    let ir_code = compile(&source, args.optimize).expect("Compilation failed");

    if options.contains(&"ll") {
        let output_ll = format!("{}.ll", args.input);
        fs::write(&output_ll, &ir_code).expect("Failed to write ll file");
        println!("LLVM IR written to {}", output_ll);
    }

    if options.contains(&"submission") {
        generate_submission_file(&source, &ir_code);
        println!("Submission file written");
    }

    if options.contains(&"executable") {
        compile_to_executable(&ir_code);
        println!("Executable compiled");
    }
}
