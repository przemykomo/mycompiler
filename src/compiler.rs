use crate::parser::*;
use std::collections::HashMap;
use std::iter::Peekable;
use std::slice::Iter;

struct CompilationState<'a> {
    assembly : String,
    iter : Peekable<Iter<'a, Statement>>,
    stack_size : i32,
    variables : HashMap<String, i32>
}

pub fn compile_to_assembly(abstract_syntax_tree: &Vec<Statement>) -> String {
    let mut state = CompilationState {
        assembly : "global _start\n_start:\n".to_string(),
        iter : abstract_syntax_tree.iter().peekable(),
        stack_size : 0,
        variables : HashMap::<String, i32>::new() // Identifier -> stack location
    };

    while let Some(statement) = state.iter.next() {
        match statement {
            Statement::Exit(expression) => {
                compile_expression(&mut state, expression);
                state.assembly.push_str("mov rdi, rax\n mov rax, 60\n syscall");
            },
            Statement::VariableDefinition { identifier, expression } => {
                if state.variables.contains_key(identifier) {
                    panic!("Variable \"{}\" already exists!", identifier);
                }

                compile_expression(&mut state, expression);
                state.variables.insert(identifier.clone(), state.stack_size);
                state.assembly.push_str("push rax\n");
                state.stack_size += 1;
            }
        }
    }

    return state.assembly;
}

fn compile_expression(state: &mut CompilationState, expression: &Expression) {
    match expression {
        Expression::IntLiteral(value) => {
            let to_append = format!("mov rax, {}\n", value);
            state.assembly.push_str(&to_append);
        },
        Expression::Identifier(other) => {
            if !state.variables.contains_key(other) {
                panic!("Undeclared identifier: \"{}\"", other);
            }
            let stack_location = state.variables.get(other).unwrap();
            let to_append = format!("mov rax, QWORD [rsp + {}]\n", (state.stack_size - stack_location - 1) * 8);
            state.assembly.push_str(&to_append);
        },
        Expression::Add { left, right } => {
            compile_expression(state, right);
            state.assembly.push_str("push rax\n");
            state.stack_size += 1;
            compile_expression(state, left);
            state.assembly.push_str("pop rbx\n add rax, rbx\n");
            state.stack_size -= 1;
        },
        Expression::Multiply { left, right } => {
            compile_expression(state, right);
            state.assembly.push_str("push rax\n");
            state.stack_size += 1;
            compile_expression(state, left);
            state.assembly.push_str("pop rbx\n imul rbx\n");
            state.stack_size -= 1;
        },
        Expression::Subtract{ left, right } => {
            compile_expression(state, right);
            state.assembly.push_str("push rax\n");
            state.stack_size += 1;
            compile_expression(state, left);
            state.assembly.push_str("pop rbx\n sub rax, rbx\n");
            state.stack_size -= 1;
        },
        Expression::Divide{ left, right } => {
            compile_expression(state, right);
            state.assembly.push_str("push rax\n");
            state.stack_size += 1;
            compile_expression(state, left);
            state.assembly.push_str("pop rbx\n idiv rbx\n");
            state.stack_size -= 1;
        },
    }
}
