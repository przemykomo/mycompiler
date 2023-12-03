use std::env;
use std::fs;
use std::io::Write;

pub mod tokenizer;
use tokenizer::*;

pub mod parser;
use parser::*;

pub mod compiler;
use compiler::*;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        panic!("Not enough arguments.");
    }

    let read_file_path = &args[1];
    let write_file_path = &args[2];

    let contents = fs::read_to_string(read_file_path).expect("Should have been able to read the file.");
    println!("{}", contents);

    let tokens = tokenize(&contents);
    dbg!(&tokens);

    let parsed_unit = parse(&tokens);
    dbg!(&parsed_unit);

    let assembly = compile_to_assembly(&parsed_unit);
    dbg!(&assembly);

    let mut file = fs::File::create(write_file_path).expect("Should have been able to open this file for writing");
    file.write_all(assembly.as_bytes()).expect("Should have been able to write to this file.");
}
