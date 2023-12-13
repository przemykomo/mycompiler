use crate::parser::*;
use crate::tokenizer::DataType;
use std::collections::HashMap;
use std::iter::Peekable;
use std::slice::Iter;

struct CompilationState<'a> {
    assembly: String,
    string_constants: HashMap<String, usize>,
    float_constants: HashMap<String, usize>,
    unique_label_id: i32,
    parsed_unit: &'a ParsedUnit
}

struct ScopeState<'a> {
    iter: Peekable<Iter<'a, Statement>>,
    stack_size_current: i32,
    max_stack_size: i32,
    variables: &'a mut Vec<Variable>,
    current_function: &'a FunctionDefinition,
    assembly: String
}

struct Variable {
    identifier: String,
    stack_location: i32,
    data_type: DataType
}

#[must_use]
#[derive(Debug)]
struct ExpressionResult {
    data_type: DataType,
    result_container: ResultContainer
}

#[derive(Debug)]
enum ResultContainer {
    Register,
    FloatRegister,
    Flag(Flag)
}

#[derive(Debug)]
enum Flag {
    EQUAL,
    LARGER,
    SMALLER
}

fn can_increment(data_type: &DataType) -> bool {
    match data_type {
        DataType::Int | DataType::Char | DataType::Pointer(_) => true,
        DataType::Array { data_type: _, size: _ } | DataType::Boolean | DataType::Void | DataType::Float => false
    }
}

pub fn sizeof(data_type: &DataType) -> i32 {
    match data_type {
        DataType::Int | DataType::Pointer { .. } => 8,
        DataType::Float => 4,
        DataType::Char | DataType::Boolean => 1,
        DataType::Array { data_type: _, size: _ } => todo!(),
        DataType::Void => panic!("sizeof(void) is invalid!")
    }
}

pub fn sizeofword(data_type: &DataType) -> &str {
    match data_type {
        DataType::Int | DataType::Pointer { .. } => "QWORD",
        DataType::Float => "DWORD",
        DataType::Char | DataType::Boolean => "BYTE",
        DataType::Array { data_type, size: _ } => sizeofword(data_type),
        DataType::Void => panic!("sizeofword(void) is invalid!")
    }
}

pub fn reg_from_size(size: i32, reg: &str) -> &'static str {
    match reg {
        "rax" => match size {
                    8 => "rax",
                    4 => "eax",
                    2 => "ax",
                    1 => "al",
                    _ => panic!("Wrong value!")
                },
        "rbx" => match size {
                    8 => "rbx",
                    4 => "ebx",
                    2 => "bx",
                    1 => "bl",
                    _ => panic!("Wrong value!")
                },
        _ => panic!("Wrong register name: {}", reg)
    }
}

fn get_function<'a>(compilation_state: &'a mut CompilationState, identifier: &str) -> Option<&'a FunctionPrototype> {
    if let Some(function) = compilation_state.parsed_unit.functions.iter().find(|f| f.prototype.name.eq(identifier)) {
        Some(&function.prototype)
    } else {
        compilation_state.parsed_unit.function_declarations.iter().find(|f| f.name.eq(identifier))
    }
}

fn compile_function_call(compilation_state: &mut CompilationState, state: &mut ScopeState, function_call: &FunctionCall) -> ExpressionResult {
    if let Some(function) = get_function(compilation_state, &function_call.identifier) {
        if function_call.arguments.len() != function.arguments.len() {
            panic!("Passed wrong amoung of parameters to function \"{}\"", function.name);
        }

        let mut arguments_to_append : String = String::new();
        let mut floats_index : i64 = function.arguments.iter().filter(|data_type| is_float(data_type)).count() as i64;
        let mut integer_index : i64 = function.arguments.len() as i64 - floats_index;
        
        floats_index -= 1;
        integer_index -= 1;

        for (i, argument) in function_call.arguments.iter().enumerate().rev() {
            let variable = state.variables.iter().rev().find(|var| var.identifier.eq(argument)).expect(&format!("Undeclared variable: \"{}\"", argument));
            if !variable.data_type.eq(function.arguments.get(i).expect("Already checked for a number of arguments, should never happen.")) {
                panic!("Passed argument of a wrong type!");
            }

            if is_float(&variable.data_type) {
                if floats_index > 7 {
                    arguments_to_append.push_str(&format!("movss xmm0, DWORD [rbp - {}]\npush xmm0\n", variable.stack_location));
                } else {
                    let register = match floats_index {
                        0 => "xmm0",
                        1 => "xmm1",
                        2 => "xmm2",
                        3 => "xmm3",
                        4 => "xmm4",
                        5 => "xmm5",
                        6 => "xmm6",
                        7 => "xmm7",
                        _ => unreachable!()
                    };

                    arguments_to_append.push_str(&format!("movss {}, DWORD [rbp - {}]\n", register, variable.stack_location));
                }

                floats_index -= 1;
            } else {
                let instruction;
                let register;
                if let DataType::Array { data_type: _, size: _ } = &variable.data_type {
                    instruction = "lea";
                    register = "rax";
                } else if sizeof(&variable.data_type) == 8 {
                    instruction = "mov";
                    register = "rax";
                } else {
                    instruction = "movzx";
                    register = "eax";
                }

                if integer_index > 5 {
                    arguments_to_append.push_str(&format!("{} {}, {} [rbp - {}]\npush rax\n", instruction, register, sizeofword(&variable.data_type), variable.stack_location));
                } else {
                    let register = match integer_index {
                        0 => "rdi",
                        1 => "rsi",
                        2 => "rax",
                        3 => "rcx",
                        4 => "r8",
                        5 => "r9",
                        _ => unreachable!()
                    };
                    arguments_to_append.push_str(&format!("{} {}, {} [rbp - {}]\n", instruction, register, sizeofword(&variable.data_type), variable.stack_location));
                }

                integer_index -= 1;
            }
        }

        /* TODO: do some more inteligent saving of registers, commenting this out since I read
         * variables from stack every time I need them anyway and I might need rax register for the
         * return value.
        let to_append = formatdoc!("push rax
                                    push rdi
                                    push rsi
                                    push rax
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
                                    pop rax
                                    pop rsi
                                    pop rdi
                                    pop rax\n", arguments_to_append, function_call.identifier);
        state.assembly.push_str(&to_append); */
        state.assembly.push_str(&format!("{}call {}\n", arguments_to_append, function_call.identifier));
        return ExpressionResult { data_type:  function.return_type.clone(), result_container: ResultContainer::Register };
    } else {
        panic!("Cannot find function with a name \"{}\"", function_call.identifier);
    }

}

fn is_float(data_type: &DataType) -> bool {
    data_type.eq(&DataType::Float)
}

pub fn compile_to_assembly(parsed_unit: &ParsedUnit) -> String {
    let mut compilation_state = CompilationState {
        assembly: String::new(),
        string_constants: HashMap::new(),
        float_constants: HashMap::new(),
        unique_label_id: 0,
        parsed_unit
    };

    for function_declaration in parsed_unit.function_declarations.iter() {
        compilation_state.assembly.push_str(&format!("extern {}\n", function_declaration.name));
    }

    for function in parsed_unit.functions.iter() {
        if function.public {
            compilation_state.assembly.push_str(&format!("global {}\n", function.prototype.name));
        }
        //TODO: make compile_scope return information about used registries and only push those.
        //Use scratch registries only if possible.
        /*
        let mut to_append = formatdoc!("{}:
                                push rbx
                                push r12
                                push r13
                                push r14
                                push r15
                                push rbp
                                mov rbp, rsp\n", function.prototype.name);*/
        compilation_state.assembly.push_str(&format!("{}:\npush rbp\nmov rbp, rsp\n", function.prototype.name));
        let mut variables = Vec::new();
        let mut scope_state = ScopeState {
            iter: function.body.iter().peekable(),
            stack_size_current: 0,
            max_stack_size: 0,
            variables: &mut variables,
            current_function: &function,
            assembly: String::new()
        };
        compile_scope(&mut scope_state, &mut compilation_state);
        compilation_state.assembly.push_str(&format!("sub rsp, {}\n", ((scope_state.max_stack_size + 15) / 16) * 16)); // align the stack frame to 2 bytes
        compilation_state.assembly.push_str(&scope_state.assembly);
        /*
        to_append = formatdoc!(".{}.end:
                            mov rsp, rbp
                            pop rbp
                            pop r15
                            pop r14
                            pop r13
                            pop r12
                            pop rbx
                            ret\n", function.prototype.name); */
        compilation_state.assembly.push_str(&format!(".{}.end:\nmov rsp, rbp\npop rbp\nret\n", function.prototype.name));
    }

    for (text, id) in compilation_state.string_constants {
        compilation_state.assembly.insert_str(0, &format!(".String{}: db '{}', 0\n", id, text));
    }

    for (text, id) in compilation_state.float_constants {
        compilation_state.assembly.insert_str(0, &format!(".Float{}: dd {}\n", id, text));
    }

    return compilation_state.assembly;
}

fn compile_scope(state: &mut ScopeState, compilation_state: &mut CompilationState) {
    while let Some(statement) = state.iter.next() {
        match statement {
            Statement::VariableDefinition { identifier, expression: expression_opt, data_type } => {
                let size = if let DataType::Array { data_type, size } = data_type {
                    sizeof(data_type) * size
                } else {
                    sizeof(data_type)
                };

                state.stack_size_current = ((state.stack_size_current + size - 1) / size) * size + size;

                if let Some(expression) = expression_opt {
                    let result = compile_expression(state, compilation_state, &expression);
                    if result.data_type != *data_type {
                        panic!("Variable {} doesn't have the same type as the expression!", identifier);
                    }
                    match result.result_container {
                        ResultContainer::Register => {
                            state.assembly.push_str(&format!("mov {} [rbp - {}], {}\n", sizeofword(data_type), state.stack_size_current, reg_from_size(size, "rax")));
                        },
                        ResultContainer::FloatRegister => {
                            state.assembly.push_str(&format!("movss {} [rbp - {}], xmm0\n", sizeofword(data_type), state.stack_size_current));
                        },
                        ResultContainer::Flag(flag) => {
                            let set_instruction = set_instruction_from_flag(&flag);
                            let reg = reg_from_size(size, "rax");
                            state.assembly.push_str(&format!("{} {}\nmov {} [rbp - {}], {}\n", set_instruction, reg, sizeofword(data_type), state.stack_size_current, reg));
                        }
                    }
                }
                state.variables.push(Variable { identifier: identifier.clone(), stack_location: state.stack_size_current, data_type: data_type.clone() });
            },
            Statement::VariableAssigment { identifier, expression } => {
                let result = compile_expression(state, compilation_state, &expression);
                let variable = state.variables.iter().rev().find(|var| var.identifier.eq(identifier)).expect(&format!("Undeclared variable: \"{}\"", identifier));
                if result.data_type != variable.data_type {
                    panic!("Variable doesn't have the same type as the expression!");
                }
                let size = sizeof(&variable.data_type);
                match result.result_container {
                    ResultContainer::Register => {
                        state.assembly.push_str(&format!("mov {} [rbp - {}], {}\n", sizeofword(&variable.data_type), variable.stack_location, reg_from_size(size, "rax")));
                    },
                    ResultContainer::FloatRegister => {
                        state.assembly.push_str(&format!("mvss {} [rbp - {}], xmm0\n", sizeofword(&variable.data_type), variable.stack_location));
                    },
                    ResultContainer::Flag(flag) => {
                        let set_instruction = set_instruction_from_flag(&flag);
                        let reg = reg_from_size(size, "rax");
                        state.assembly.push_str(&format!("{} {}\nmov {} [rbp - {}], {}\n", set_instruction, reg, sizeofword(&variable.data_type), variable.stack_location, reg));
                    }
                }
            },
            Statement::Return(expression) => {
                let result = compile_expression(state, compilation_state, expression);
                
                if result.data_type != state.current_function.prototype.return_type {
                    panic!("Trying to return a different data type than in the function declaration!");
                }

                state.assembly.push_str(&format!("jmp .{}.end\n", state.current_function.prototype.name));
            },
            Statement::ArrayElementAssigment { identifier, element, expression } => {
                let element_result = compile_expression(state, compilation_state, &element);
                if element_result.data_type != DataType::Int {
                    panic!("Array index must be an integer!");
                }
                let variable = state.variables.iter().rev().find(|var| var.identifier.eq(identifier)).expect(&format!("Undeclared variable: \"{}\"", identifier));
                if let DataType::Array { data_type, size: _size } = variable.data_type.clone() {
                    let size = sizeof(&data_type);
                    let stack_location = variable.stack_location.clone();
                    state.assembly.push_str("push rax\n");
                    let result = compile_expression(state, compilation_state, expression);
                    if result.data_type != *data_type {
                        panic!("Element in the array doesn't have the same type as the expresssion!");
                    }
                    state.assembly.push_str(&format!("pop rbx\nmov {} [rbp - {} + rbx * {}], {}\n", sizeofword(&data_type), stack_location, size, reg_from_size(size, "rax")));
                } else {
                    panic!("{} isn't an array!", identifier);
                }
            },
            Statement::Increment(identifier) => {
                let variable = state.variables.iter().rev().find(|var| var.identifier.eq(identifier)).expect(&format!("Undeclared variable: \"{}\"", identifier));
                if !can_increment(&variable.data_type) {
                    panic!("Cannot increment the variable \"{}\"", &variable.identifier);
                }
                state.assembly.push_str(&format!("inc {} [rbp - {}]\n", sizeofword(&variable.data_type), variable.stack_location));
            },
            Statement::Decrement(identifier) => {
                let variable = state.variables.iter().rev().find(|var| var.identifier.eq(identifier)).expect(&format!("Undeclared variable: \"{}\"", identifier));
                if !can_increment(&variable.data_type) {
                    panic!("Cannot decrement the variable \"{}\"", &variable.identifier);
                }
                state.assembly.push_str(&format!("dec {} [rbp - {}]\n", sizeofword(&variable.data_type), variable.stack_location));
            },
            Statement::FunctionCall(function_call) => {
                let _ = compile_function_call(compilation_state, state, function_call);
            },
            Statement::If { expression, scope, else_scope } => {
                let result = compile_expression(state, compilation_state, expression);
                if result.data_type != DataType::Boolean {
                    panic!("If statement condition has to be a boolean!");
                }

                if let ResultContainer::Flag(flag) = result.result_container {
                    let label_id = compilation_state.unique_label_id;
                    compilation_state.unique_label_id += 1;
                    state.assembly.push_str(&format!("{} .L{}\n", jump_instruction_from_negated_flag(&flag), label_id));
                
                    let variables_len = state.variables.len();
                    let mut scope_state = ScopeState {
                        iter: scope.iter().peekable(),
                        stack_size_current: state.stack_size_current,
                        max_stack_size: state.stack_size_current,
                        variables: state.variables,
                        current_function: state.current_function,
                        assembly: String::new()
                    };
                    compile_scope(&mut scope_state, compilation_state);
                    state.max_stack_size = state.max_stack_size.max(scope_state.max_stack_size);
                    state.assembly.push_str(&scope_state.assembly);
                    state.variables.truncate(variables_len);

                    if let Some(else_scope) = else_scope {
                        let label_id = compilation_state.unique_label_id;
                        compilation_state.unique_label_id += 1;
                        state.assembly.push_str(&format!("jmp .L{}\n.L{}:\n", label_id, label_id - 1));
                    
                        let variables_len = state.variables.len();
                        let mut scope_state = ScopeState {
                            iter: else_scope.iter().peekable(),
                            stack_size_current: state.stack_size_current,
                            max_stack_size: state.stack_size_current,
                            current_function: state.current_function,
                            variables: state.variables,
                            assembly: String::new()
                        };
                        compile_scope(&mut scope_state, compilation_state);
                        state.max_stack_size = state.max_stack_size.max(scope_state.max_stack_size);
                        
                        state.assembly.push_str(&scope_state.assembly);
                        state.assembly.push_str(&format!(".L{}:\n", label_id));
                        state.variables.truncate(variables_len);
                    } else {
                        state.assembly.push_str(&format!(".L{}:\n", label_id));
                    }

                } else {
                    todo!();
                }
            }
        }
    }
    state.max_stack_size = state.max_stack_size.max(state.stack_size_current);
}

fn set_instruction_from_flag(flag: &Flag) -> &str {
    match flag {
        Flag::EQUAL => "sete",
        Flag::LARGER => "setnle",
        Flag::SMALLER => "setl"
    }
}

fn jump_instruction_from_negated_flag(flag: &Flag) -> &str {
    match flag {
        Flag::EQUAL => "jne",
        Flag::LARGER => "jle",
        Flag::SMALLER => "jge"
    }
}

fn compile_expression(state: &mut ScopeState, compilation_state: &mut CompilationState, expression: &Expression) -> ExpressionResult {
    match expression {
        Expression::IntLiteral(value) => {
            state.assembly.push_str(&format!("mov rax, {}\n", value));
            ExpressionResult { data_type: DataType::Int, result_container: ResultContainer::Register }
        },
        Expression::CharacterLiteral(c) => {
            state.assembly.push_str(&format!("mov rax, '{}'\n", c));
            ExpressionResult { data_type: DataType::Char, result_container: ResultContainer::Register }
        },
        Expression::Identifier(identifier) => {
            let variable = state.variables.iter().rev().find(|var| var.identifier.eq(identifier)).expect(&format!("Undeclared variable: \"{}\"", identifier));
            let instruction;
            let register;
            if sizeof(&variable.data_type) == 8 {
                instruction = "mov";
                register = "rax";
            } else {
                instruction = "movzx";
                register = "eax";
            }
            state.assembly.push_str(&format!("{} {}, {} [rbp - {}]\n", instruction, register, sizeofword(&variable.data_type), variable.stack_location));
            ExpressionResult { data_type: variable.data_type.clone(), result_container: ResultContainer::Register }
        },
        Expression::AccessArrayElement { identifier, element } => {
            let variable = state.variables.iter().rev().find(|var| var.identifier.eq(identifier)).expect(&format!("Undeclared variable: \"{}\"", identifier));
            if let DataType::Array { data_type, size: _ } = variable.data_type.clone() {
                let stack_location = variable.stack_location.clone();
                let result = compile_expression(state, compilation_state, element);
                if result.data_type != DataType::Int {
                    panic!("Array index must be an integer!");
                }
                let instruction;
                let register;
                if sizeof(&data_type) == 8 {
                    instruction = "mov";
                    register = "rax";
                } else {
                    instruction = "movzx";
                    register = "eax";
                }
                state.assembly.push_str(&format!("{} {}, {} [rbp - {} + rax]\n", instruction, register, sizeofword(&data_type), stack_location));
                ExpressionResult { data_type: *data_type, result_container: ResultContainer::Register }
            } else {
                panic!("{} is not an array type!", identifier);
            }
        },
        Expression::ArithmeticExpression { left, right, operator } => {
            let right_result = compile_expression(state, compilation_state, right);
            state.assembly.push_str("push rax\n");
            let left_result = compile_expression(state, compilation_state, left);
            match operator {
                ArithmeticOperator::Add => state.assembly.push_str("pop rbx\n add rax, rbx\n"),
                ArithmeticOperator::Subtract => state.assembly.push_str("pop rbx\n sub rax, rbx\n"),
                ArithmeticOperator::Multiply => state.assembly.push_str("pop rbx\n imul rbx\n"),
                ArithmeticOperator::Divide => state.assembly.push_str("pop rbx\n idiv rbx\n")
            }
            
            if let DataType::Int = right_result.data_type  {
                if let DataType::Int = left_result.data_type {
                    ExpressionResult { data_type: DataType::Int, result_container: ResultContainer::Register }
                } else {
                    panic!("Cannot add non integers!");
                }
            } else {
                panic!("Cannot add non integers!");
            }
        },
        Expression::ComparisonExpression { left, right, operator } => {
            let right_result = compile_expression(state, compilation_state, right);
            state.assembly.push_str("push rax\n");
            let left_result = compile_expression(state, compilation_state, left);
            if right_result.data_type != left_result.data_type {
                panic!("Cannot compare different data types!");
            }
            let size = sizeof(&left_result.data_type);
            state.assembly.push_str(&format!("pop rbx\n cmp {}, {}\n", reg_from_size(size, "rax"), reg_from_size(size, "rbx")));

            //todo load the reslts into registers to compare if they happen to be booleans

            match operator {
                ComparisonOperator::CompareEqual => ExpressionResult { data_type: DataType::Boolean, result_container: ResultContainer::Flag(Flag::EQUAL) },
                ComparisonOperator::CompareLarger => ExpressionResult { data_type: DataType::Boolean, result_container: ResultContainer::Flag(Flag::LARGER) },
                ComparisonOperator::CompareSmaller => ExpressionResult { data_type: DataType::Boolean, result_container: ResultContainer::Flag(Flag::SMALLER) }
            }
        },
        Expression::Dereference(expression) => {
            let result = compile_expression(state, compilation_state, expression);
            if let DataType::Pointer(data_type) = result.data_type {
                state.assembly.push_str("mov rax, QWORD [rax]\n");
                ExpressionResult { data_type: *data_type, result_container: ResultContainer::Register }
            } else {
                panic!("Cannot dereference a non pointer!");
            }
        },
        Expression::Reference(identifier) => {
            let variable = state.variables.iter().rev().find(|var| var.identifier.eq(identifier)).expect(&format!("Undeclared variable: \"{}\"", identifier));
            state.assembly.push_str(&format!("lea rax, [rbp - {}]\n", variable.stack_location));
            ExpressionResult { data_type: DataType::Pointer(Box::new(variable.data_type.clone())), result_container: ResultContainer::Register }
        },
        Expression::StringLiteral(text) => {
            let id : usize;
            if compilation_state.string_constants.contains_key(text) {
                id = *compilation_state.string_constants.get(text).expect("Missing string in the string_contants hash map. Should never happen.");
            } else {
                id = compilation_state.string_constants.len();
                compilation_state.string_constants.insert(text.clone(), id);
            }
            state.assembly.push_str(&format!("lea rax, [.String{}]\n", id));
            ExpressionResult { data_type: DataType::Pointer(Box::new(DataType::Char)), result_container: ResultContainer::Register }
        },
        Expression::FunctionCall(function_call) => {
            compile_function_call(compilation_state, state, function_call)
        },
        Expression::BoolLiteral(value) => {
            state.assembly.push_str(&format!("mov rax, {}\n", *value as i32));
            ExpressionResult { data_type: DataType::Boolean, result_container: ResultContainer::Register }
        },
        Expression::FloatLiteral(value) => {
            let id : usize;
            if compilation_state.float_constants.contains_key(value) {
                id = *compilation_state.float_constants.get(value).expect("Missing string in the float_constants hash map. Should never happen.");
            } else {
                id = compilation_state.float_constants.len();
                compilation_state.float_constants.insert(value.clone(), id);
            }
            state.assembly.push_str(&format!("movss xmm0, DWORD [.Float{}]\n", id));
            ExpressionResult { data_type: DataType::Float, result_container: ResultContainer::FloatRegister }
        }
    }
}
