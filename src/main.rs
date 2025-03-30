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

    let tokens = tokenize(&contents);
    if !tokens.errors.is_empty() {
        let mut line = 0;
        let mut lines = contents.lines();
        for error in tokens.errors {
            println!("error: {}", error.msg);
            println!("--> {}:{}:{}", read_file_path, error.pos.line, error.pos.column);
            println!("| {}", lines.nth(error.pos.line - line).unwrap());
            println!("{:>width$}", "^", width = error.pos.column + 2);
            line = error.pos.line + 1;
        }
        return;
    }

    let parsed_unit = parse(&tokens.tokens);

    let assembly = compile_to_assembly(&parsed_unit);

    let mut file = fs::File::create(write_file_path).expect("Should have been able to open this file for writing");
    file.write_all(assembly.as_bytes()).expect("Should have been able to write to this file.");
}
