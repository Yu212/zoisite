use std::fs::File;
use std::io::Read;

use untitled_lang::compile;

fn main() {
    let mut file = File::open("./files/main").expect("file not found");
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).expect("failed to read file");
    compile(buffer.as_str());
}
