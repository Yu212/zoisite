use std::fs::File;
use std::io::Read;

use untitled_lang::parse;

fn main() {
    let mut file = File::open("./files/main").expect("file not found");
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).expect("failed to read file");
    parse(buffer.as_str());
}
