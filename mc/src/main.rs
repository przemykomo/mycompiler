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

    let contents =
        fs::read_to_string(read_file_path).expect("Should have been able to read the file.");
    let lines = contents.lines().collect_vec();

    let tokens = tokenize(&contents);
    if print_errors(&tokens.errors, &lines, &read_file_path) {
        return;
    }

    let parsed_unit = parse(&tokens);
    print_errors(&parsed_unit.errors, &lines, read_file_path);

    let compiled_unit = compile_to_assembly(&parsed_unit);
    if print_errors(&compiled_unit.errors, &lines, &read_file_path) {
        return;
    }

    let mut file = fs::File::create(write_file_path)
        .expect("Should have been able to open this file for writing");
    file.write_all(compiled_unit.asm.as_bytes())
        .expect("Should have been able to write to this file.");
}

fn print_errors(errors: &[Error], lines: &Vec<&str>, read_file_path: &str) -> bool {
    if !errors.is_empty() {
        for error in errors {
            println!("Error: {}", error.msg);
            let line = format!(
                "--> {}:{}:{}",
                read_file_path,
                error.span.line + 1,
                error.span.column
            );
            println!("{}", line);
            println!("| {}", lines[error.span.line]);
            print!("{: >width$}", "", width = error.span.column + 2);
            let end = if error.span.line == error.span.endline {
                error.span.endcolumn
            } else {
                line.len()
            };
            println!("{:^>width$}", "", width = end - error.span.column + 1);
        }
        true
    } else {
        false
    }
}
