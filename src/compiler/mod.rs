use crate::parser::*;
use crate::tokenizer::DataType;
use std::cell::RefCell;
use std::collections::HashMap;
use std::iter::Peekable;
use std::rc::Rc;
use std::slice::Iter;

pub mod expression;
mod scope;
use scope::compile_scope;

pub struct CompilationState<'a> {
    assembly: String,
    string_constants: HashMap<String, usize>,
    float_constants: HashMap<String, usize>,
    unique_label_id: i32,
    parsed_unit: &'a ParsedUnit,
    struct_types: HashMap<String, StructType>
}

pub struct ScopeState<'a> {
    iter: Peekable<Iter<'a, Statement>>,
    stack_size_current: i32,
    max_stack_size: i32,
    variables: &'a mut Vec<Variable>,
    current_function: &'a FunctionDefinition,
    assembly: String,
    used_registers: HashMap<Register, Rc<RefCell<Register>>>
}

#[derive(Debug)]
struct Variable {
    identifier: String,
    stack_location: i32,
    data_type: DataType
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
pub struct ExpressionResult {
    data_type: DataType,
    result_container: ResultContainer
}

#[derive(Debug)]
enum ResultContainer {
    Register(Rc<RefCell<Register>>),
    FloatRegister,
    Flag(Flag),
    IdentifierWithOffset { identifier: String, offset: i32 }
}

#[derive(Debug, Eq, Hash, PartialEq, Clone, Copy)]
enum Register {
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    //RBP,
    //RSP,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}
use Register::*;

#[derive(Debug)]
enum Flag {
    EQUAL,
    LARGER,
    SMALLER
}

fn get_free_register(state: &ScopeState) -> Option<Register> {
    for reg in [RAX, RBX, RCX, RDX, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15] {
        if !state.used_registers.contains_key(&reg) {
            return Some(reg);
        }
    }
    return None;
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
            compilation_state.struct_types.get(identifier).expect("Struct type \"{}\" is missing!").size
        }
    }
}

pub fn sizeofword(data_type: &DataType) -> &str {
    match data_type {
        DataType::Int | DataType::Pointer { .. } => "QWORD",
        DataType::Float => "DWORD",
        DataType::Char | DataType::Boolean => "BYTE",
        DataType::Array { data_type, size: _ } => sizeofword(data_type),
        DataType::Void => panic!("sizeofword(void) is invalid!"),
        DataType::Struct(_) => panic!("sizeofword(struct) is invalid!")
    }
}

fn reg_from_size(size: i32, reg: Register) -> &'static str {
    match (reg, size) {
        (RAX, 8) => "rax", (RAX, 4) => "eax",  (RAX, 2) => "ax",   (RAX, 1) => "al",
        (RBX, 8) => "rbx", (RBX, 4) => "ebx",  (RBX, 2) => "bx",   (RBX, 1) => "bl",
        (RCX, 8) => "rcx", (RCX, 4) => "ecx",  (RCX, 2) => "cx",   (RCX, 1) => "cl",
        (RDX, 8) => "rdx", (RDX, 4) => "edx",  (RDX, 2) => "dx",   (RDX, 1) => "dl",
        (RSI, 8) => "rsi", (RSI, 4) => "esi",  (RSI, 2) => "si",   (RSI, 1) => "sil",
        (RDI, 8) => "rdi", (RDI, 4) => "edi",  (RDI, 2) => "di",   (RDI, 1) => "dil",
        (R8,  8) => "r8",  (R8,  4) => "r8d",  (R8,  2) => "r8w",  (R8,  1) => "r8b",
        (R9,  8) => "r9",  (R9,  4) => "r9d",  (R9,  2) => "r9w",  (R9,  1) => "r9b",
        (R10, 8) => "r10", (R10, 4) => "r10d", (R10, 2) => "r10w", (R10, 1) => "r10b",
        (R11, 8) => "r11", (R11, 4) => "r11d", (R11, 2) => "r11w", (R11, 1) => "r11b",
        (R12, 8) => "r12", (R12, 4) => "r12d", (R12, 2) => "r12w", (R12, 1) => "r12b",
        (R13, 8) => "r13", (R13, 4) => "r13d", (R13, 2) => "r13w", (R13, 1) => "r13b",
        (R14, 8) => "r14", (R14, 4) => "r14d", (R14, 2) => "r14w", (R14, 1) => "r14b",
        (R15, 8) => "r15", (R15, 4) => "r15d", (R15, 2) => "r15w", (R15, 1) => "r15b",
        _ => todo!("add more regs")
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
                } else if sizeof(&variable.data_type, &compilation_state) == 8 {
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
        if is_float(&function.return_type) {
            return ExpressionResult { data_type: function.return_type.clone(), result_container: ResultContainer::FloatRegister };
        }
        return ExpressionResult { data_type: function.return_type.clone(), result_container: ResultContainer::Register(Rc::new(RefCell::new(Register::RAX))) };
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
            assembly: String::new(),
            used_registers: HashMap::new()
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
