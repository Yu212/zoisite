use clap::Parser;
use std::fs::File;
use std::io::Read;
use zoisite::compile;

#[derive(Parser)]
struct Args {
    #[arg()]
    file: String,
}

fn main() {
    let args = Args::parse();
    let mut file = File::open(args.file).expect("file not found");
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).expect("failed to read file");
    compile(buffer.as_str());
}
