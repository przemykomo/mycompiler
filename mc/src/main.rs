// #![allow(warnings)]
use std::env;
use std::fs;

mod tokenizer;
mod typecheck;
use inkwell::llvm_sys::core::LLVMContextCreate;
use inkwell::llvm_sys::core::LLVMDumpModule;
use tokenizer::*;

pub mod parser;
use parser::*;

pub mod ast;
// pub mod compile;
pub mod ir;
// pub mod lower;

use crate::typecheck::TypeChecker;

use crate::llvm::LLVMGen;
pub mod llvm;

fn main() {
    let mut args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        panic!("Not enough arguments.");
    }

    let write_file_path = args.remove(2);
    let read_file_path = args.remove(1);

    let contents =
        fs::read_to_string(&read_file_path).expect("Should have been able to read the file.");
    let lines: Vec<&str> = contents.lines().collect();

    let tokens = tokenize(&contents);
    if print_errors(&tokens.errors, &lines, &read_file_path) {
        return;
    }

    let mut parser = Parser::new(&tokens);
    parser.parse();

    if print_errors(&parser.errors, &lines, &read_file_path) {
        return;
    }

    let mut typechecker = TypeChecker {
        parser: &parser,
        typed_functions: Vec::new(),
        errors: Vec::new(),
    };

    typechecker.typecheck();

    if print_errors(&typechecker.errors, &lines, &read_file_path) {
        return;
    }

    let context = unsafe { LLVMContextCreate() };
    // let context = Context::create();
    let mut llvmgen = LLVMGen::new(&typechecker, context);
    llvmgen.generate_ir();

    if print_errors(&llvmgen.errors, &lines, &read_file_path) {
        return;
    }
    unsafe {
        LLVMDumpModule(llvmgen.module);
    }

    llvmgen.build(write_file_path);
    // llvmgen.module.print_to_stderr();

    // let mut irgen = IRGen::new(&parser);
    // irgen.generate_ir();
    //
    // if print_errors(&irgen.errors, &lines, read_file_path) {
    //     return;
    // }
    //
    // for function in &irgen.functions {
    //     print!("{}", function);
    // }

    // llvm::run(&irgen, write_file_path);

    // let mut lower = Lower::new(&irgen);
    // lower.lower();
    // compile::compile_elf_object(&irgen, write_file_path);
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
