use std::env;
use std::fs;
use std::io::Write;

mod tokenizer;
use itertools::Itertools;
use tokenizer::*;

mod parser;
use parser::*;

mod compiler;
use compiler::*;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        panic!("Not enough arguments.");
    }

    let read_file_path = &args[1];
    let write_file_path = &args[2];

    let contents = fs::read_to_string(read_file_path).expect("Should have been able to read the file.");
    let lines = contents.lines().collect_vec();

    let tokens = tokenize(&contents);
    if !tokens.errors.is_empty() {
        for error in tokens.errors {
            println!("Tokenizing error: {}", error.msg);
            println!("--> {}:{}:{}", read_file_path, error.pos.line + 1, error.pos.column);
            println!("| {}", lines[error.pos.line]);
            println!("{:>width$}", "^", width = error.pos.column + 2);
        }
        return;
    }

    let parsed_unit = parse(&tokens);
    if !parsed_unit.errors.is_empty() {
        for error in parsed_unit.errors {
            println!("Parsing error: {}", error.msg);
            println!("--> {}:{}:{}", read_file_path, error.pos.line + 1, error.pos.column);
            println!("| {}", lines[error.pos.line]);
            println!("{:>width$}", "^", width = error.pos.column + 2);
        }
        return;
    }

    let compiled_unit = compile_to_assembly(&parsed_unit);
    if !compiled_unit.errors.is_empty() {
        for error in compiled_unit.errors {
            println!("Compiling error: {}", error.msg);
            println!("--> {}:{}:{}", read_file_path, error.pos.line, error.pos.column);
            println!("| {}", lines[error.pos.line]);
            println!("{:>width$}", "^", width = error.pos.column + 2);
        }
        return;
    }

    let mut file = fs::File::create(write_file_path).expect("Should have been able to open this file for writing");
    file.write_all(compiled_unit.asm.as_bytes()).expect("Should have been able to write to this file.");
}
