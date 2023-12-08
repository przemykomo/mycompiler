use crate::parser::*;
use crate::tokenizer::DataType;
use std::collections::HashMap;
use std::iter::Peekable;
use std::slice::Iter;
use indoc::formatdoc;
use indoc::indoc;

struct CompilationState<> {
    assembly: String,
}

struct ScopeState<'a> {
    iter: Peekable<Iter<'a, Statement>>,
    stack_size: i32,
    variables: HashMap<String, Variable>,
    assembly: String
}

struct Variable {
    stack_location: i32,
    data_type: DataType
}

pub fn sizeof(data_type: &DataType) -> i32 {
    match data_type {
        DataType::Int => 8,
        DataType::Char => 1,
        DataType::Array { data_type: _, size: _ } => todo!()
    }
}

pub fn sizeofword(data_type: &DataType) -> &str {
    match data_type {
        DataType::Int => "QWORD",
        DataType::Char => "BYTE",
        DataType::Array { data_type, size: _ } => sizeofword(data_type)
    }
}

pub fn reg_from_size(size: i32) -> &'static str {
    match size {
        8 => "rax",
        4 => "eax",
        2 => "ax",
        1 => "al",
        _ => panic!("Wrong value!")
    }
}

pub fn compile_to_assembly(parsed_unit: &ParsedUnit) -> String {
    let mut compilation_state = CompilationState {
        assembly: String::new()
    };

    let mut extern_dec_iter = parsed_unit.extern_declarations.iter();
    while let Some(extern_delcaration) = extern_dec_iter.next() {
        let to_append = formatdoc!("extern {}\n", extern_delcaration);
        compilation_state.assembly.push_str(&to_append);
    }

    let mut function_iter = parsed_unit.functions.iter();

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
            variables: HashMap::<String, Variable>::new(), // Identifier -> stack location
            assembly: String::new()
        };
        compile_scope(&mut scope_state);
        compilation_state.assembly.push_str(&format!("sub rsp, {}\n", ((scope_state.stack_size + 15) / 16) * 16)); // align the stack frame to 2 bytes
        compilation_state.assembly.push_str(&scope_state.assembly);
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

fn compile_scope(state: &mut ScopeState) {
    while let Some(statement) = state.iter.next() {
        match statement {
            Statement::Exit(expression) => {
                compile_expression(state, &expression);
                state.assembly.push_str("mov rdi, rax\n mov rax, 60\n syscall\n");
            },
            Statement::VariableDefinition { identifier, expression: expression_opt, data_type } => {
                if state.variables.contains_key(identifier) {
                    panic!("Variable \"{}\" already exists!", identifier);
                }

                let size = if let DataType::Array { data_type, size } = data_type {
                    sizeof(data_type) * size
                } else {
                    sizeof(data_type)
                };

                state.stack_size = ((state.stack_size + size - 1) / size) * size + size;

                if let Some(expression) = expression_opt {
                    compile_expression(state, &expression);
                    state.assembly.push_str(&format!("mov {} [rbp - {}], {}\n", sizeofword(data_type), state.stack_size, reg_from_size(size)));
                }
                state.variables.insert(identifier.clone(), Variable { stack_location: state.stack_size, data_type: data_type.clone() });
            },
            Statement::VariableAssigment { identifier, expression } => {
                compile_expression(state, &expression);
                let variable = state.variables.get(identifier).expect(&format!("Undeclared identifier: \"{}\"", identifier));
                let size = sizeof(&variable.data_type);
                state.assembly.push_str(&format!("mov {} [rbp - {}], {}\n", sizeofword(&variable.data_type), variable.stack_location, reg_from_size(size)));
            },
            Statement::ArrayElementAssigment { identifier, element, expression } => {
                compile_expression(state, &element);
                let variable = state.variables.get(identifier).expect(&format!("Undeclared identifier: \"{}\"", identifier));
                if let DataType::Array { data_type, size: _size } = variable.data_type.clone() {
                    let size = sizeof(&data_type);
                    let stack_location = variable.stack_location.clone();
                    state.assembly.push_str("push rax\n");
                    state.stack_size += 8;
                    compile_expression(state, expression);
                    state.assembly.push_str(&format!("pop rbx\nmov {} [rbp - {} + rbx * {}], {}\n", sizeofword(&data_type), stack_location, size, reg_from_size(size)));
                    state.stack_size -= 8;
                } else {
                    panic!("{} isn't an array!", identifier);
                }
            },
            Statement::Increment(identifier) => {
                let variable = state.variables.get(identifier).expect(&format!("Undeclared identifier: \"{}\"", identifier));
                state.assembly.push_str(&format!("inc {} [rbp - {}]\n", sizeofword(&variable.data_type), variable.stack_location));
            },
            Statement::Decrement(identifier) => {
                let variable = state.variables.get(identifier).expect(&format!("Undeclared identifier: \"{}\"", identifier));
                state.assembly.push_str(&format!("dec {} [rbp - {}]\n", sizeofword(&variable.data_type), variable.stack_location));
            },
            Statement::FunctionCall { identifier, arguments } => {
                let mut arguments_to_append : String = String::new();
                for (i, argument) in arguments.iter().enumerate().rev()  {
                    let variable = state.variables.get(argument).expect(&format!("Undeclared identifier: \"{}\"", argument));

                    let instruction = if let DataType::Array { data_type: _, size: _ } = &variable.data_type {
                        "lea"
                    } else if sizeof(&variable.data_type) == 8 {
                        "mov"
                    } else {
                        "movzx"
                    };

                    if i > 5 {
                        let to_append = format!("{} rax, {} [rbp - {}]\n", instruction, sizeofword(&variable.data_type), variable.stack_location);

                        arguments_to_append.push_str(&to_append);
                    } else {
                        let register = match i {
                            0 => "rdi",
                            1 => "rsi",
                            2 => "rdx",
                            3 => "rcx",
                            4 => "r8",
                            5 => "r9",
                            _ => unreachable!()
                        };
                        let to_append = format!("{} {}, {} [rbp - {}]\n", instruction, register, sizeofword(&variable.data_type), variable.stack_location);
                        arguments_to_append.push_str(&to_append);
                    }
                }
                let to_append = formatdoc!("push rax
                                            push rdi
                                            push rsi
                                            push rdx
                                            push rcx
                                            push r8
                                            push r9
                                            push r10
                                            push r11
                                            {}
                                            call {}
                                            pop r11
                                            pop r10
                                            pop r9
                                            pop r8
                                            pop rcx
                                            pop rdx
                                            pop rsi
                                            pop rdi
                                            pop rax\n", arguments_to_append, identifier);
                state.assembly.push_str(&to_append);
            }
        }
    }
}

fn compile_expression(state: &mut ScopeState, expression: &Expression) {
    match expression {
        Expression::IntLiteral(value) => {
            let to_append = format!("mov rax, {}\n", value);
            state.assembly.push_str(&to_append);
        },
        Expression::CharacterLiteral(c) => {
            let to_append = format!("mov rax, '{}'\n", c);
            state.assembly.push_str(&to_append);
        },
        Expression::Identifier(identifier) => {
            let variable = state.variables.get(identifier).expect(&format!("Undeclared identifier: \"{}\"", identifier));
            let size = sizeof(&variable.data_type);
            state.assembly.push_str(&format!("{} {}, {} [rbp - {}]\n", if size == 8 {"mov"} else {"movzx"}, reg_from_size(size), sizeofword(&variable.data_type), variable.stack_location));
        },
        Expression::AccessArrayElement { identifier, element } => {
            let variable = state.variables.get(identifier).expect(&format!("Undeclared identifier: \"{}\"", identifier));
            if let DataType::Array { data_type, size: _ } = variable.data_type.clone() {
                let stack_location = variable.stack_location.clone();
                compile_expression(state, element);
                let size = sizeof(&data_type);
                state.assembly.push_str(&format!("{} {}, {} [rbp - {} + rda]\n", if size == 8 {"mov"} else {"movzx"}, reg_from_size(size), sizeofword(&data_type), stack_location));
            } else {
                panic!("{} is not an array type!", identifier);
            }
        },
        Expression::Add { left, right } => {
            compile_expression(state, right);
            state.assembly.push_str("push rax\n");
            state.stack_size += 8;
            compile_expression(state, left);
            state.assembly.push_str("pop rbx\n add rax, rbx\n");
            state.stack_size -= 8;
        },
        Expression::Multiply { left, right } => {
            compile_expression(state, right);
            state.assembly.push_str("push rax\n");
            state.stack_size += 8;
            compile_expression(state, left);
            state.assembly.push_str("pop rbx\n imul rbx\n");
            state.stack_size -= 8;
        },
        Expression::Subtract{ left, right } => {
            compile_expression(state, right);
            state.assembly.push_str("push rax\n");
            state.stack_size += 8;
            compile_expression(state, left);
            state.assembly.push_str("pop rbx\n sub rax, rbx\n");
            state.stack_size -= 8;
        },
        Expression::Divide{ left, right } => {
            compile_expression(state, right);
            state.assembly.push_str("push rax\n");
            state.stack_size += 8;
            compile_expression(state, left);
            state.assembly.push_str("pop rbx\n idiv rbx\n");
            state.stack_size -= 8;
        },
        Expression::Dereference(expression) => {
            compile_expression(state, expression);
            state.assembly.push_str("mov rax, QWORD [rax]\n");
        },
        Expression::Reference(variable) => {
            let variable = state.variables.get(variable).expect(&format!("Undeclared identifier: \"{}\"", variable));
            let to_append = format!("mov rax, rbp - {}\n", variable.stack_location);
            state.assembly.push_str(&to_append);
        },
        Expression::StringLiteral(_) => todo!(),
    }
}
