use crate::parser::*;
use std::collections::HashMap;
use std::iter::Peekable;
use std::slice::Iter;
use indoc::formatdoc;
use indoc::indoc;

struct CompilationState<> {
    assembly : String,
}

struct ScopeState<'a> {
    iter: Peekable<Iter<'a, Statement>>,
    stack_size: i32,
    variables: HashMap<String, i32>
}

pub fn compile_to_assembly(functions: &Vec<Function>) -> String {
    let mut compilation_state = CompilationState {
        assembly: String::new()
    };

    let mut function_iter = functions.iter();

    while let Some(function) = function_iter.next() {
        let mut to_append : String;
        if function.public {
            to_append = format!("global {}\n", function.name);
            compilation_state.assembly.push_str(&to_append);
        }
        //TODO: make compile_scope return information about used registries and only push those.
        //Use scratch registries only if possible.
        to_append = formatdoc!("{}:
                                push rbx
                                push r12
                                push r13
                                push r14
                                push r15
                                push rbp
                                mov rbp, rsp\n", function.name);
        compilation_state.assembly.push_str(&to_append);
        let mut scope_state = ScopeState {
            iter: function.body.iter().peekable(),
            stack_size: 0,
            variables : HashMap::<String, i32>::new() // Identifier -> stack location
        };
        compile_scope(&mut compilation_state, &mut scope_state);
        to_append = indoc!("mov rsp, rbp
                            pop rbp
                            pop r15
                            pop r14
                            pop r13
                            pop r12
                            pop rbx
                            ret\n").to_string();
        compilation_state.assembly.push_str(&to_append);
    }

    return compilation_state.assembly;
}

fn compile_scope(compilation_state: &mut CompilationState, state: &mut ScopeState) {
    while let Some(statement) = state.iter.next() {
        match statement {
            Statement::Exit(expression) => {
                compile_expression(compilation_state, state, &expression);
                compilation_state.assembly.push_str("mov rdi, rax\n mov rax, 60\n syscall\n");
            },
            Statement::VariableDefinition { identifier, expression } => {
                if state.variables.contains_key(identifier) {
                    panic!("Variable \"{}\" already exists!", identifier);
                }

                compile_expression(compilation_state, state, &expression);
                state.variables.insert(identifier.clone(), state.stack_size);
                compilation_state.assembly.push_str("push rax\n");
                state.stack_size += 1;
            },
            Statement::VariableAssigment { identifier, expression } => {
                if !state.variables.contains_key(identifier) {
                    panic!("Undeclared identifier: \"{}\"", identifier);
                }

                compile_expression(compilation_state, state, &expression);
                let stack_location = state.variables.get(identifier).unwrap();
                let to_append = format!("mov QWORD [rsp + {}], rax\n", (state.stack_size - stack_location - 1) * 8);
                compilation_state.assembly.push_str(&to_append);
            },
            Statement::Increment(identifier) => {
                if !state.variables.contains_key(identifier) {
                    panic!("Undeclared identifier: \"{}\"", identifier);
                }

                let stack_location = state.variables.get(identifier).unwrap();
                let offset = (state.stack_size - stack_location - 1) * 8;
                let to_append = format!("mov rax, QWORD [rsp + {0}]\n inc rax\n mov QWORD [rsp + {0}], rax\n", offset);
                compilation_state.assembly.push_str(&to_append);
            },
            Statement::Decrement(identifier) => {
                if !state.variables.contains_key(identifier) {
                    panic!("Undeclared identifier: \"{}\"", identifier);
                }

                let stack_location = state.variables.get(identifier).unwrap();
                let offset = (state.stack_size - stack_location - 1) * 8;
                let to_append = format!("mov rax, QWORD [rsp + {0}]\n dec rax\n mov QWORD [rsp + {0}], rax\n", offset);
                compilation_state.assembly.push_str(&to_append);
            },
            Statement::FunctionCall { identifier } => {
                let to_append = formatdoc!("push rax,
                                            push rdi
                                            push rsi
                                            push rdx
                                            push rcx
                                            push r8
                                            push r9
                                            push r10
                                            push r11
                                            call {}
                                            pop r11
                                            pop r10
                                            pop r9
                                            pop r8
                                            pop rcx
                                            pop rdx
                                            pop rsi
                                            pop rdi
                                            pop rax\n", identifier);
                compilation_state.assembly.push_str(&to_append);
            }
        }
    }
}

fn compile_expression(compilation_state: &mut CompilationState, state: &mut ScopeState, expression: &Expression) {
    match expression {
        Expression::IntLiteral(value) => {
            let to_append = format!("mov rax, {}\n", value);
            compilation_state.assembly.push_str(&to_append);
        },
        Expression::Identifier(other) => {
            if !state.variables.contains_key(other) {
                panic!("Undeclared identifier: \"{}\"", other);
            }
            let stack_location = state.variables.get(other).unwrap();
            let to_append = format!("mov rax, QWORD [rsp + {}]\n", (state.stack_size - stack_location - 1) * 8);
            compilation_state.assembly.push_str(&to_append);
        },
        Expression::Add { left, right } => {
            compile_expression(compilation_state, state, right);
            compilation_state.assembly.push_str("push rax\n");
            state.stack_size += 1;
            compile_expression(compilation_state, state, left);
            compilation_state.assembly.push_str("pop rbx\n add rax, rbx\n");
            state.stack_size -= 1;
        },
        Expression::Multiply { left, right } => {
            compile_expression(compilation_state, state, right);
            compilation_state.assembly.push_str("push rax\n");
            state.stack_size += 1;
            compile_expression(compilation_state, state, left);
            compilation_state.assembly.push_str("pop rbx\n imul rbx\n");
            state.stack_size -= 1;
        },
        Expression::Subtract{ left, right } => {
            compile_expression(compilation_state, state, right);
            compilation_state.assembly.push_str("push rax\n");
            state.stack_size += 1;
            compile_expression(compilation_state, state, left);
            compilation_state.assembly.push_str("pop rbx\n sub rax, rbx\n");
            state.stack_size -= 1;
        },
        Expression::Divide{ left, right } => {
            compile_expression(compilation_state, state, right);
            compilation_state.assembly.push_str("push rax\n");
            state.stack_size += 1;
            compile_expression(compilation_state, state, left);
            compilation_state.assembly.push_str("pop rbx\n idiv rbx\n");
            state.stack_size -= 1;
        },
    }
}
