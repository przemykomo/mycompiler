// #![allow(warnings)]
use std::env;
use std::fs;

mod tokenizer;
use ir::IRGen;
use itertools::Itertools;
use tokenizer::*;

mod parser;
use parser::*;

pub mod ast;
pub mod compile;
pub mod ir;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        panic!("Not enough arguments.");
    }

    let read_file_path = &args[1];
    let write_file_path = &args[2];

    let contents =
        fs::read_to_string(read_file_path).expect("Should have been able to read the file.");
    let lines = contents.lines().collect_vec();

    let tokens = tokenize(&contents);
    if print_errors(&tokens.errors, &lines, &read_file_path) {
        return;
    }

    let mut parser = Parser::new(&tokens);
    parser.parse();

    print_errors(&parser.errors, &lines, read_file_path);

    let mut irgen = IRGen::new(&parser);
    irgen.generate_ir();

    if print_errors(&irgen.errors, &lines, read_file_path) {
        return;
    }

    compile::compile_elf_object(&irgen, write_file_path);
}

fn print_errors(errors: &[Error], lines: &[&str], read_file_path: &str) -> bool {
    if !errors.is_empty() {
        for error in errors {
            println!("Error: {}", error.msg);
            let line = format!("| {}", lines[error.span.line]);
            println!(
                "--> {}:{}:{}",
                read_file_path,
                error.span.line + 1,
                error.span.column
            );
            println!("{}", line);
            print!("{: >width$}", "", width = error.span.column + 2);
            let end = if error.span.line == error.span.endline {
                error.span.endcolumn
            } else {
                lines[error.span.line].len()
            };
            println!("{:^>width$}", "", width = end - error.span.column + 1);
        }
        true
    } else {
        false
    }
}
