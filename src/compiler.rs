use crate::parser::*;
use crate::tokenizer::DataType;
use std::collections::HashMap;
use std::iter::Peekable;
use std::slice::Iter;

struct CompilationState<'a> {
    ir_unit: IRUnit,
    string_constants: HashMap<String, usize>,
    float_constants: HashMap<String, usize>,
    unique_label_id: i32,
    parsed_unit: &'a ParsedUnit,
    struct_types: HashMap<String, StructType>
}

struct ScopeState<'a> {
    iter: Peekable<Iter<'a, Statement>>,
    stack_size_current: i32,
    max_stack_size: i32,
    variables: &'a mut Vec<Variable>,
    current_function: &'a FunctionDefinition,
    instructions: Vec<Instruction>,
    allocated_registers: usize
}

#[derive(Debug)]
struct Variable {
    identifier: String,
    stack_location: i32,
    data_type: DataType,
    virtual_reg: usize
}

struct StructType {
    members: Vec<StructMemberWithOffset>,
    size: i32
}

struct StructMemberWithOffset {
    member: StructMember,
    offset: i32
}

#[must_use]
#[derive(Debug)]
struct ExpressionResult {
    data_type: DataType,
    result_container: ResultContainer
}

#[derive(Debug)]
enum ResultContainer {
    Register(Register),
    Flag(Flag),
    IdentifierWithOffset { identifier: String, offset: i32 }
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
        DataType::Array { data_type: _, size: _ } | DataType::Boolean | DataType::Void | DataType::Float | DataType::Struct(_) => false
    }
}

fn sizeof(data_type: &DataType, compilation_state: &CompilationState) -> i32 {
    match data_type {
        DataType::Int | DataType::Pointer { .. } => 8,
        DataType::Float => 4,
        DataType::Char | DataType::Boolean => 1,
        DataType::Array { data_type: _, size: _ } => todo!(),
        DataType::Void => panic!("sizeof(void) is invalid!"),
        DataType::Struct(identifier) => {
            compilation_state.struct_types.get(identifier).expect("Struct type is missing!").size
        }
    }
}

pub fn word_from_type(data_type: &DataType) -> &str {
    match data_type {
        DataType::Int | DataType::Pointer { .. } => "QWORD",
        DataType::Float => "DWORD",
        DataType::Char | DataType::Boolean => "BYTE",
        DataType::Array { data_type, size: _ } => word_from_type(data_type),
        DataType::Void => panic!("word_from_type(void) is invalid!"),
        DataType::Struct(_) => panic!("word_from_type(struct) is invalid!")
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

fn get_function<'a>(compilation_state: &'a CompilationState, identifier: &str) -> Option<&'a FunctionPrototype> {
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

        let mut arguments_in_reg_asm : String = String::new();
        let mut arguments_in_stack_asm : String = String::new();
        let mut int_index : i32 = 0;
        let mut float_index : i32 = 0;
        let mut stack_size_for_args : i32 = 0;

        for (i, argument) in function_call.arguments.iter().enumerate() {
            let result = move_to_reg_if_needed(state, compile_expression(state, compilation_state, argument));

            if !result.data_type.eq(function.arguments.get(i).expect("Already checked for a number of arguments, should never happen.")) {
                panic!("Passed argument of a wrong type!");
            }

            if let DataType::Struct(struct_name) = result.data_type {
                let struct_type = compilation_state.struct_types.get(&struct_name).expect("Struct type is missing!");
                if let ResultContainer::IdentifierWithOffset { identifier, offset } = result.result_container {
                    let variable = state.variables.iter().rev().find(|var| var.identifier.eq(&identifier)).expect(&format!("Undeclared variable: \"{}\"", identifier));
                    arguments_in_stack_asm.push_str(&format!("sub rsp, {}", struct_type.size));
                    stack_size_for_args += struct_type.size;
                    let mut i = 0;
                    while i < struct_type.size {
                        match struct_type.size - i {
                            0 => unreachable!(),
                            1 => {
                                arguments_in_stack_asm.push_str(&format!("movzx eax, BYTE [rbp - {}]\nmov BYTE [rsp + {}], al\n", variable.stack_location - offset, i));
                                i += 1;
                            },
                            2 | 3 => {
                                arguments_in_stack_asm.push_str(&format!("movzx eax, WORD [rbp - {}]\nmov WORD [rsp + {}], ax\n", variable.stack_location - offset, i));
                                i += 2;
                            },
                            4..=7 => {
                                arguments_in_stack_asm.push_str(&format!("mov eax, DWORD [rbp - {}]\nmov DWORD [rsp + {}], eax\n", variable.stack_location - offset, i));
                                i += 4;
                            },
                            _ => {
                                arguments_in_stack_asm.push_str(&format!("mov rax, QWORD [rbp - {}]\nmov QWORD [rsp + {}], rax\n", variable.stack_location - offset, i));
                                i += 8;
                            }
                        }
                    }
                } else {
                    panic!();
                }
            } else {
                let size = sizeof(&result.data_type, compilation_state);
                if is_float(&result.data_type) {
                    if float_index > 7 {
                        arguments_in_stack_asm.push_str(&format!("movss {} [rsp + %stack_size% - {}], xmm0\n", word_from_type(&result.data_type), stack_size_for_args));
                        stack_size_for_args += size;
                    } else if float_index > 0 {
                        arguments_in_reg_asm.push_str(&format!("movd xmm{}, xmm0\n", float_index));
                    }

                    float_index += 1;
                } else {
                    if int_index > 5 {
                        arguments_in_stack_asm.push_str(&format!("mov {} [rsp + %stack_size% - {}, {}", word_from_type(&result.data_type), stack_size_for_args, reg_from_size(size, "rax")));
                        stack_size_for_args += size;
                    } else {
                        let register = match int_index {
                            0 => "rdi",
                            1 => "rsi",
                            2 => "rdx",
                            3 => "rcx",
                            4 => "r8",
                            5 => "r9",
                            _ => unreachable!()
                        };
                        todo!();
                    }

                    int_index -= 1;
                }
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
        state.instructions.push_str(&format!("{}call {}\n", arguments_to_append, function_call.identifier));
        arguments_in_stack_asm.push_str(&format!("add rsp, {}", stack_size_for_args));
        if is_float(&function.return_type) {
            return ExpressionResult { data_type: function.return_type.clone(), result_container: ResultContainer::FloatRegister };
        }
        return ExpressionResult { data_type: function.return_type.clone(), result_container: ResultContainer::Register };
    } else {
        panic!("Cannot find function with a name \"{}\"", function_call.identifier);
    }

}

fn is_float(data_type: &DataType) -> bool {
    data_type.eq(&DataType::Float)
}

#[derive(Debug)]
pub enum Register {
    Virtual(usize),
    RSP,
    RBP,
}

pub enum RegOrNum {
    Reg(Register),
    Num(i32)
}

pub enum RegOrMem {
    Reg(Register),
    Mem { pointer: Register, offset: i32 }
}

pub enum Instruction {
    SUB { left: Register, right: RegOrNum },
    PUSH(Register),
    POP(Register),
    MOV { left: RegOrMem, right: RegOrNum },
    SetFromFlag { left: RegOrMem, flag: Flag },
    VirtualRegisterAllocation { id: usize, reg_type: DataType },
    Label(String),
    RET(Option<Register>)
}

pub struct Procedure {
    name: String,
    instructions: Vec<Instruction>   
}

pub enum Directive {
    Extern(String),
    Global(String),
    DefineConstString { label: String, value: String },
    DefineConstFloat { label: String, value: String }
}

pub struct IRUnit {
    top_directives: Vec<Directive>,
    procedures: Vec<Procedure>
}

pub fn compile_to_ir(parsed_unit: &ParsedUnit) -> IRUnit {
    let mut compilation_state = CompilationState {
        ir_unit: IRUnit { top_directives: Vec::new(), procedures: Vec::new() },
        string_constants: HashMap::new(),
        float_constants: HashMap::new(),
        unique_label_id: 0,
        parsed_unit,
        struct_types: HashMap::new()
    };

    for struct_declaration in parsed_unit.struct_declarations.iter() {
        let mut members: Vec<StructMemberWithOffset> = Vec::new();
        let mut size = 0;

        for member in struct_declaration.members.iter() {
            let member_size = sizeof(&member.data_type, &compilation_state);
            size = ((size + member_size - 1) / member_size) * member_size + member_size;
            members.push(StructMemberWithOffset { member: member.clone(), offset: size });
        }

        compilation_state.struct_types.insert(struct_declaration.identifier.clone(), StructType { members, size });
    }

    for function_declaration in parsed_unit.function_declarations.iter() {
        compilation_state.ir_unit.top_directives.push(Directive::Extern(function_declaration.name));
        
    }

    for function in parsed_unit.functions.iter() {
        let mut instructions = Vec::<Instruction>::new();
        if function.public {
            compilation_state.ir_unit.top_directives.push(Directive::Global(function.prototype.name));
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
        instructions.push(Instruction::PUSH(Register::RBP));
        instructions.push(Instruction::MOV { left: Register::RBP, right: RegOrNum::Reg(Register::RSP) });
        let mut variables = Vec::new();
        let mut scope_state = ScopeState {
            iter: function.body.iter().peekable(),
            stack_size_current: 0,
            max_stack_size: 0,
            variables: &mut variables,
            current_function: &function,
            instructions: Vec::new()
        };
        compile_scope(&mut scope_state, &mut compilation_state);
        instructions.push(Instruction::SUB { left: Register::RSP, right: RegOrNum::Num(((scope_state.max_stack_size + 15) / 16) * 16) });
        instructions.append(&mut scope_state.instructions);
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
        instructions.push(Instruction::Label(format!(".{}.end", function.prototype.name)));
        instructions.push(Instruction::MOV { left: RegOrMem::Reg(Register::RSP), right: RegOrNum::Reg(Register::RBP) });
        instructions.push(Instruction::POP(Register::RBP));
        instructions.push(Instruction::RET);
    }

    for (text, id) in compilation_state.string_constants {
        compilation_state.ir_unit.top_directives.push(Directive::DefineConstString { label: format!(".String{}", id), value: text });
    }

    for (text, id) in compilation_state.float_constants {
        compilation_state.ir_unit.top_directives.push(Directive::DefineConstFloat { label: format!(".Float{}", id), value: text });
    }

    return compilation_state.ir_unit;
}

fn compile_scope(state: &mut ScopeState, compilation_state: &mut CompilationState) {
    while let Some(statement) = state.iter.next() {
        match statement {
            Statement::VariableDefinition { identifier, expression: expression_opt, data_type } => {
                let size = if let DataType::Array { data_type, size } = data_type {
                    sizeof(data_type, compilation_state) * size
                } else {
                    sizeof(data_type, compilation_state)
                };

                state.stack_size_current = ((state.stack_size_current + size - 1) / size) * size + size;

                let virtual_reg = state.allocated_registers;
                state.allocated_registers += 1;

                state.variables.push(Variable { identifier: identifier.clone(), stack_location: state.stack_size_current, data_type: data_type.clone(), virtual_reg });

                state.instructions.push(Instruction::VirtualRegisterAllocation { id: virtual_reg, reg_type: data_type.clone() });

                if let Some(expression) = expression_opt {
                    let result = compile_expression(state, compilation_state, &expression);
                    if result.data_type != *data_type {
                        panic!("Variable {} doesn't have the same type as the expression!", identifier);
                    }
                    match result.result_container {
                        ResultContainer::Register(reg) => {
                            state.instructions.push(Instruction::MOV { left: RegOrMem::Reg(Register::Virtual(virtual_reg)), right: RegOrNum::Reg(reg) });
                        },
                        ResultContainer::Flag(flag) => {
                            state.instructions.push(Instruction::SetFromFlag { left: RegOrMem::Reg(Register::Virtual(virtual_reg)), flag });
                        },
                        ResultContainer::IdentifierWithOffset { identifier, offset } => {},
                    }
                }
            }, /*
            Statement::VariableAssigment { identifier, expression } => {
                let result = compile_expression(state, compilation_state, &expression);
                let variable = state.variables.iter().rev().find(|var| var.identifier.eq(identifier.get(0).expect("Expected identifier"))).expect(&format!("Undeclared variable: \"{}\"", identifier.get(0).unwrap()));
                if result.data_type != variable.data_type {
                    panic!("Variable doesn't have the same type as the expression!");
                }
                let size = sizeof(&variable.data_type, compilation_state);
                match result.result_container {
                    ResultContainer::Register => {
                        state.assembly.push_str(&format!("mov {} [rbp - {}], {}\n", sizeofword(&variable.data_type), variable.stack_location, reg_from_size(size, "rax")));
                    },
                    ResultContainer::FloatRegister => {
                        state.assembly.push_str(&format!("movss {} [rbp - {}], xmm0\n", sizeofword(&variable.data_type), variable.stack_location));
                    },
                    ResultContainer::Flag(flag) => {
                        let set_instruction = set_instruction_from_flag(&flag);
                        let reg = reg_from_size(size, "rax");
                        state.assembly.push_str(&format!("{} {}\nmov {} [rbp - {}], {}\n", set_instruction, reg, sizeofword(&variable.data_type), variable.stack_location, reg));
                    }
                }
            }, */
            Statement::Return(expression) => {
                let result = compile_expression(state, compilation_state, expression);
                
                if result.data_type != state.current_function.prototype.return_type {
                    panic!("Trying to return a different data type than in the function declaration!");
                }

                match result.result_container {
                    ResultContainer::Register(reg) => state.instructions.push(Instruction::RET(Some(reg))),
                    ResultContainer::Flag(flag) => {
                        state.instructions.push(Instruction::SetFromFlag { left: RegOrMem::Reg(Register::Virtual(state.allocated_registers)), flag });
                        state.instructions.push(Instruction::RET(Some(Register::Virtual(state.allocated_registers))));
                        state.allocated_registers += 1;
                    },
                    ResultContainer::IdentifierWithOffset { identifier, offset } => todo!(),
                }
            }, /*
            Statement::ArrayElementAssigment { identifier, element, expression } => {
                let element_result = compile_expression(state, compilation_state, &element);
                if element_result.data_type != DataType::Int {
                    panic!("Array index must be an integer!");
                }
                let variable = state.variables.iter().rev().find(|var| var.identifier.eq(identifier.get(0).expect("Identifier expected"))).expect(&format!("Undeclared variable: \"{}\"", identifier.get(0).unwrap()));
                if let DataType::Array { data_type, size: _size } = variable.data_type.clone() {
                    let size = sizeof(&data_type, compilation_state);
                    let stack_location = variable.stack_location.clone();
                    state.assembly.push_str("push rax\n");
                    let result = compile_expression(state, compilation_state, expression);
                    if result.data_type != *data_type {
                        panic!("Element in the array doesn't have the same type as the expresssion!");
                    }
                    state.assembly.push_str(&format!("pop rbx\nmov {} [rbp - {} + rbx * {}], {}\n", sizeofword(&data_type), stack_location, size, reg_from_size(size, "rax")));
                } else {
                    panic!("{} isn't an array!", identifier.get(0).unwrap());
                }
            }, */
            Statement::Increment(identifier) => {
                let variable = state.variables.iter().rev().find(|var| var.identifier.eq(identifier)).expect(&format!("Undeclared variable: \"{}\"", identifier));
                if !can_increment(&variable.data_type) {
                    panic!("Cannot increment the variable \"{}\"", &variable.identifier);
                }
                state.instructions.push_str(&format!("inc {} [rbp - {}]\n", word_from_type(&variable.data_type), variable.stack_location));
            },
            Statement::Decrement(identifier) => {
                let variable = state.variables.iter().rev().find(|var| var.identifier.eq(identifier)).expect(&format!("Undeclared variable: \"{}\"", identifier));
                if !can_increment(&variable.data_type) {
                    panic!("Cannot decrement the variable \"{}\"", &variable.identifier);
                }
                state.instructions.push_str(&format!("dec {} [rbp - {}]\n", word_from_type(&variable.data_type), variable.stack_location));
            }, /*
            Statement::FunctionCall(function_call) => {
                let _ = compile_function_call(compilation_state, state, function_call);
            }, */
            Statement::If { expression, scope, else_scope } => {
                let result = compile_expression(state, compilation_state, expression);
                if result.data_type != DataType::Boolean {
                    panic!("If statement condition has to be a boolean!");
                }

                if let ResultContainer::Flag(flag) = result.result_container {
                    let label_id = compilation_state.unique_label_id;
                    compilation_state.unique_label_id += 1;
                    state.instructions.push_str(&format!("{} .L{}\n", jump_instruction_from_negated_flag(&flag), label_id));
                
                    let variables_len = state.variables.len();
                    let mut scope_state = ScopeState {
                        iter: scope.iter().peekable(),
                        stack_size_current: state.stack_size_current,
                        max_stack_size: state.stack_size_current,
                        variables: state.variables,
                        current_function: state.current_function,
                        instructions: String::new()
                    };
                    compile_scope(&mut scope_state, compilation_state);
                    state.max_stack_size = state.max_stack_size.max(scope_state.max_stack_size);
                    state.instructions.push_str(&scope_state.instructions);
                    state.variables.truncate(variables_len);

                    if let Some(else_scope) = else_scope {
                        let label_id = compilation_state.unique_label_id;
                        compilation_state.unique_label_id += 1;
                        state.instructions.push_str(&format!("jmp .L{}\n.L{}:\n", label_id, label_id - 1));
                    
                        let variables_len = state.variables.len();
                        let mut scope_state = ScopeState {
                            iter: else_scope.iter().peekable(),
                            stack_size_current: state.stack_size_current,
                            max_stack_size: state.stack_size_current,
                            current_function: state.current_function,
                            variables: state.variables,
                            instructions: String::new()
                        };
                        compile_scope(&mut scope_state, compilation_state);
                        state.max_stack_size = state.max_stack_size.max(scope_state.max_stack_size);
                        
                        state.instructions.push_str(&scope_state.instructions);
                        state.instructions.push_str(&format!(".L{}:\n", label_id));
                        state.variables.truncate(variables_len);
                    } else {
                        state.instructions.push_str(&format!(".L{}:\n", label_id));
                    }

                } else {
                    todo!();
                }
            },
            Statement::Expression(expression) => {
                let _ = compile_expression(state, compilation_state, expression);
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

fn move_to_reg_if_needed(state: &mut ScopeState, mut expression_result: ExpressionResult) -> ExpressionResult {

    match expression_result.result_container {
        ResultContainer::Register => return expression_result,
        ResultContainer::FloatRegister => return expression_result,
        ResultContainer::Flag(ref flag) => {
            state.instructions.push_str(&format!("{} rax\n", set_instruction_from_flag(flag)));
            expression_result.result_container = ResultContainer::Register;
            return expression_result;
        },
        ResultContainer::IdentifierWithOffset { identifier, offset } => {
            let variable = state.variables.iter().rev().find(|var| var.identifier.eq(&identifier)).expect(&format!("Undeclared variable: \"{}\"", identifier));
            if let DataType::Array { data_type, size } = expression_result.data_type {
                state.instructions.push_str(&format!("lea rax, [rbp - {}]\n", variable.stack_location - offset));
                expression_result.data_type = DataType::Pointer(Box::new(*data_type.clone()));
            } else {
                state.instructions.push_str(&format!("mov rax, [rbp - {}]\n", variable.stack_location - offset));
            }
            expression_result.result_container = ResultContainer::Register;
            return expression_result;
        },
    }
}

fn compile_expression(state: &mut ScopeState, compilation_state: &mut CompilationState, expression: &Expression) -> ExpressionResult {
    match expression {
        Expression::IntLiteral(value) => {
            state.instructions.push_str(&format!("mov rax, {}\n", value));
            ExpressionResult { data_type: DataType::Int, result_container: ResultContainer::Register }
        },
        Expression::CharacterLiteral(c) => {
            state.instructions.push_str(&format!("mov rax, '{}'\n", c));
            ExpressionResult { data_type: DataType::Char, result_container: ResultContainer::Register }
        },
        Expression::Identifier(identifier) => {
            let variable = state.variables.iter().rev().find(|var| var.identifier.eq(identifier)).expect(&format!("Undeclared variable: \"{}\"", identifier));
            ExpressionResult { data_type: variable.data_type.clone(), result_container: ResultContainer::IdentifierWithOffset { identifier: identifier.clone(), offset: 0 } }
            /*
            if lvalue {
                ExpressionResult { data_type: variable.data_type.clone(), result_container: ResultContainer::LValue(Box::new(LValue::Variable(identifier.clone()))) }
            } else {
                let instruction;
                let register;
                if is_float(&variable.data_type) {
                    instruction = "movss";
                    register = "xmm0";
                } else {
                    if sizeof(&variable.data_type, compilation_state) == 8 {
                        instruction = "mov";
                        register = "rax";
                    } else {
                        instruction = "movzx";
                        register = "eax";
                    }
                }
                state.assembly.push_str(&format!("{} {}, {} [rbp - {}]\n", instruction, register, sizeofword(&variable.data_type), variable.stack_location));
                ExpressionResult { data_type: variable.data_type.clone(), result_container: ResultContainer::Register }
            }
            */
        },
        Expression::ArraySubscript { identifier, element } => {
            let variable = state.variables.iter().rev().find(|var| var.identifier.eq(identifier)).expect(&format!("Undeclared variable: \"{}\"", identifier));
            if let DataType::Array { data_type, size: _ } = variable.data_type.clone() {
                let stack_location = variable.stack_location.clone();
                let result = compile_expression(state, compilation_state, element);
                if result.data_type != DataType::Int {
                    panic!("Array index must be an integer!");
                }
                todo!("Array subscripts");/*
                if lvalue {
                    ExpressionResult { data_type: *data_type, result_container: ResultContainer::LValue(Box::new(LValue::VariableWithOffset { identifier: identifier.clone(), offset: Box::new(result.result_container) })) }
                } else {
                    let instruction;
                    let register;
                    if is_float(&data_type) {
                        instruction = "movss";
                        register = "xmm0";
                    } else {
                        if sizeof(&data_type, compilation_state) == 8 {
                            instruction = "mov";
                            register = "rax";
                        } else {
                            instruction = "movzx";
                            register = "eax";
                        }
                    }
                    state.assembly.push_str(&format!("{} {}, {} [rbpl- {} + rax]\n", instruction, register, sizeofword(&data_type), stack_location));
                    ExpressionResult { data_type: *data_type, result_container: ResultContainer::Register }
                } */
            } else {
                panic!("{} is not an array type!", identifier);
            }
        },
        Expression::ArithmeticExpression { left, right, operator } => {
            let right_result = compile_expression(state, compilation_state, right);
            let right_result = move_to_reg_if_needed(state, right_result);
            match right_result.data_type {
                DataType::Int => {
                    state.instructions.push_str("push rax\n");
                },
                DataType::Float => {
                    state.instructions.push_str("movd eax, xmm0\npush rax\n");
                },
                _ => panic!("Cannot do an arithmetic operation!")
            }
            let left_result = compile_expression(state, compilation_state, left);
            let left_result = move_to_reg_if_needed(state, left_result);

            if !left_result.data_type.eq(&right_result.data_type) {
                panic!("Cannot do arithmetic operations on values of different data types!");
            }

            match left_result.data_type {
                DataType::Int => {
                    match operator {
                        ArithmeticOperator::Add => state.instructions.push_str("pop rbx\n add rax, rbx\n"),
                        ArithmeticOperator::Subtract => state.instructions.push_str("pop rbx\n sub rax, rbx\n"),
                        ArithmeticOperator::Multiply => state.instructions.push_str("pop rbx\n imul rbx\n"),
                        ArithmeticOperator::Divide => state.instructions.push_str("pop rbx\n idiv rbx\n")
                    }
                    ExpressionResult { data_type: DataType::Int, result_container: ResultContainer::Register }
                },
                DataType::Float => {
                    match operator {
                        ArithmeticOperator::Add => state.instructions.push_str("pop rax\nmovd xmm1, eax\naddss xmm0, xmm1\n"),
                        ArithmeticOperator::Subtract => state.instructions.push_str("pop rax\nmovd xmm1, eax\nsubss xmm0, xmm1\n"),
                        ArithmeticOperator::Multiply => state.instructions.push_str("pop rax\nmovd xmm1, eax\nmulss xmm0, xmm1\n"),
                        ArithmeticOperator::Divide => state.instructions.push_str("pop rax\nmovd xmm1, eax\ndivss xmm0, xmm1\n")
                    }
                    ExpressionResult { data_type: DataType::Float, result_container: ResultContainer::FloatRegister }
                },
                _ => panic!("Cannot do an arithmetic operation!")
            }
        },
        Expression::ComparisonExpression { left, right, operator } => {
            let right_result = compile_expression(state, compilation_state, right);
            let right_result = move_to_reg_if_needed(state, right_result);
            state.instructions.push_str("push rax\n");
            let left_result = compile_expression(state, compilation_state, left);
            let left_result = move_to_reg_if_needed(state, left_result);
            if right_result.data_type != left_result.data_type {
                panic!("Cannot compare different data types!");
            }
            let size = sizeof(&left_result.data_type, compilation_state);
            state.instructions.push_str(&format!("pop rbx\n cmp {}, {}\n", reg_from_size(size, "rax"), reg_from_size(size, "rbx")));

            //todo load the reslts into registers to compare if they happen to be booleans

            match operator {
                ComparisonOperator::CompareEqual => ExpressionResult { data_type: DataType::Boolean, result_container: ResultContainer::Flag(Flag::EQUAL) },
                ComparisonOperator::CompareLarger => ExpressionResult { data_type: DataType::Boolean, result_container: ResultContainer::Flag(Flag::LARGER) },
                ComparisonOperator::CompareSmaller => ExpressionResult { data_type: DataType::Boolean, result_container: ResultContainer::Flag(Flag::SMALLER) }
            }
        },
        Expression::Dereference(expression) => {
            let result = compile_expression(state, compilation_state, expression);
            let result = move_to_reg_if_needed(state, result);
            if let DataType::Pointer(data_type) = result.data_type {
                state.instructions.push_str("mov rax, QWORD [rax]\n");
                ExpressionResult { data_type: *data_type, result_container: ResultContainer::Register }
            } else {
                panic!("Cannot dereference a non pointer!");
            }
        },
        Expression::AddressOf(expression) => {
            let result = compile_expression(state, compilation_state, expression);

            if let ResultContainer::IdentifierWithOffset { identifier, offset } = result.result_container {
                let variable = state.variables.iter().rev().find(|var| var.identifier.eq(&identifier)).expect(&format!("Undeclared variable: \"{}\"", identifier));
                state.instructions.push_str(&format!("lea rax, [rbp - {}]\n", variable.stack_location - offset));
                ExpressionResult { data_type: DataType::Pointer(Box::new(variable.data_type.clone())), result_container: ResultContainer::Register }
            } else {
                panic!("Cannot take address of an rvalue!");
            }
        },
        Expression::StringLiteral(text) => {
            let id : usize;
            if compilation_state.string_constants.contains_key(text) {
                id = *compilation_state.string_constants.get(text).expect("Missing string in the string_contants hash map. Should never happen.");
            } else {
                id = compilation_state.string_constants.len();
                compilation_state.string_constants.insert(text.clone(), id);
            }
            state.instructions.push_str(&format!("lea rax, [.String{}]\n", id));
            ExpressionResult { data_type: DataType::Pointer(Box::new(DataType::Char)), result_container: ResultContainer::Register }
        },
        Expression::FunctionCall(function_call) => {
            compile_function_call(compilation_state, state, function_call)
        },
        Expression::BoolLiteral(value) => {
            state.instructions.push_str(&format!("mov rax, {}\n", *value as i32));
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
            state.instructions.push_str(&format!("movss xmm0, DWORD [.Float{}]\n", id));
            ExpressionResult { data_type: DataType::Float, result_container: ResultContainer::FloatRegister }
        }
        Expression::Assigment { left, right } => {
            /*
            if is_float(&right_result.data_type) {
                state.assembly.push_str("movd eax, xmm0\npush rax\n");
            } else {
                state.assembly.push_str("push rax\n");
            }*/

            let left_result = compile_expression(state, compilation_state, left);
            let right_result = compile_expression(state, compilation_state, right);
            let right_result = move_to_reg_if_needed(state, right_result);

            if right_result.data_type != left_result.data_type {
                panic!("Cannot assing value of a different data type!");
            }

            if let ExpressionResult { data_type, result_container: ResultContainer::IdentifierWithOffset { identifier, offset } } = left_result {
                let variable = state.variables.iter().rev().find(|var| var.identifier.eq(&identifier)).expect(&format!("Undeclared variable: \"{}\"", &identifier));

                let size = sizeof(&data_type, compilation_state);
                let stack_location = variable.stack_location.clone();
                
                if is_float(&data_type) {
                    state.instructions.push_str(&format!("movd eax, xmm0\nmov {} [rbp - {}], {}\n", word_from_type(&data_type), stack_location - offset, reg_from_size(size, "rax")));
                } else {
                    state.instructions.push_str(&format!("mov {} [rbp - {}], {}\n", word_from_type(&data_type), stack_location - offset, reg_from_size(size, "rax")));
                }

                right_result
            } else {
                panic!("Trying to assign value to rvalue!");
            }
        }
        Expression::MemberAccess { left, right } => {
            let left = compile_expression(state, compilation_state, left);

            if let DataType::Struct(left_identifier) = left.data_type {
                if let ResultContainer::IdentifierWithOffset { identifier, offset } = left.result_container {
                    let struct_type = compilation_state.struct_types.get(&left_identifier).expect("Expected struct to exist.");

                    match (*right).as_ref() {
                        Expression::Identifier(right_identifier) => {
                            let member = struct_type.members.iter().find(|member| member.member.identifier.eq(right_identifier)).expect("Expected member of struct to exist.");
                            ExpressionResult { data_type: member.member.data_type.clone(), result_container: ResultContainer::IdentifierWithOffset { identifier: identifier.clone(), offset: offset + member.offset } }
                        },
                        Expression::ArraySubscript { .. } => todo!(),
                        Expression::FunctionCall(_) => todo!(),
                        _ => panic!("Invalid member access expression!")
                    }
                } else {
                    panic!("Member access operator used on an rvalue!");
                }
            } else {
                panic!("Member access operator used on a non-struct variable!");
            }
        }
    }
}
