use clap::{Parser, ValueEnum};
use std::process::exit;
use std::{fs, path::Path};
use zoisite::{run_lifecycle, LifecycleOptions};

#[derive(Debug, Clone, ValueEnum, PartialEq)]
enum EmitFormat {
    Syntax,
    Ir,
    Submission,
    Executable,
}

#[derive(Parser, Debug)]
#[command(name = "Zoisite")]
#[clap(author, version, about)]
struct Args {
    #[clap(help = "Path to the Zoisite source file")]
    input: String,

    #[clap(
        long,
        value_enum,
        value_delimiter = ',',
        default_values_t = [EmitFormat::Ir, EmitFormat::Submission, EmitFormat::Executable],
        help = "Comma-separated list of output formats: syntaxx, ir, submission, executable",
        hide_default_value = true,
        hide_possible_values = true,
    )]
    emit: Vec<EmitFormat>,

    #[clap(short = 'O', long = "optimize", help = "Enable optimizations")]
    optimize: bool,

    #[clap(short = 'd', long = "debug", help = "Enable debug information")]
    debug: bool,
}

fn main() {
    let args = Args::parse();

    let base_name = Path::new(&args.input)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("output")
        .to_string();

    let source = fs::read_to_string(&args.input)
        .expect("Failed to read source file");

    let opts = LifecycleOptions {
        base_name,
        debug: args.debug,
        optimize: args.optimize,
        run_jit: false,
        output_syntax: args.emit.contains(&EmitFormat::Syntax),
        output_ir: args.emit.contains(&EmitFormat::Ir),
        output_submission: args.emit.contains(&EmitFormat::Submission),
        output_executable: args.emit.contains(&EmitFormat::Executable),
    };

    if !run_lifecycle(&source, opts) {
        eprintln!("Compilation failed");
        exit(1);
    }
}
