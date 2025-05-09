use crate::parser::*;
use crate::tokenizer::{DataType, Error, Span};
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::iter::Peekable;
use std::rc::Rc;
use std::slice::Iter;

pub mod expression;
mod scope;
use scope::compile_scope;

pub mod regmap;
use regmap::RegMap;

mod asmutil;
use asmutil::*;

pub struct CompilationState<'a> {
    asm: Asm,
    string_constants: HashMap<String, usize>,
    float_constants: HashMap<String, usize>,
    unique_label_id: i32,
    parsed_unit: &'a ParsedUnit,
    struct_types: HashMap<String, StructType>,
    errors: Vec<Error>,
    highlights: Vec<Highlight>,
}

pub struct ScopeState<'a> {
    iter: Peekable<Iter<'a, Statement>>,
    stack_size_current: i32,
    max_stack_size: i32,
    variables: &'a mut Vec<Variable>,
    current_function: &'a FunctionDefinition,
    asm: Asm,
    used_registers: RegMap,
}

#[derive(Debug)]
struct Variable {
    identifier: String,
    stack_location: i32,
    data_type: DataType,
}

#[derive(Clone)]
struct StructType {
    members: Vec<StructMemberWithOffset>,
    size: i32,
}

#[derive(Clone)]
struct StructMemberWithOffset {
    member: StructMember,
    offset: i32,
}

#[must_use]
#[derive(Debug)]
pub struct ExpressionResult {
    data_type: DataType,
    result_container: ResultContainer,
}

#[derive(Debug)]
enum ResultContainer {
    TempVariable(Rc<RefCell<TempVariable>>),
    FloatRegister,
    Flag(Flag),
    IdentifierWithOffset {
        identifier: String,
        offset: i32,
    },
    #[allow(dead_code)] // TODO: Possibly do some checking if the struct literal matches the
    // definition of the struct with the same identifier
    StructLiteral {
        identifier: String,
        members: Vec<(String, Option<ExpressionResult>)>,
    },
    ConstInt(i32),
    ConstChar(char),
}

#[derive(Debug, Eq, Hash, PartialEq, Clone, Copy)]
pub enum TempVariable {
    Register(Register),
    Stack(i32),
}

#[derive(Debug, Eq, Hash, PartialEq, Clone, Copy)]
pub enum Register {
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    RBP,
    RSP,
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
    SMALLER,
}

#[macro_export]
macro_rules! fmt {
    ($($arg:tt)*) => {{
        // let res = std::format!("; {}:{}\n{}", file!(), line!(), std::format!($($arg)*));
        // res
        std::format!($($arg)*)
    }}
}
pub(crate) use fmt;

const USED_REGISTERS: [Register; 14] = [
    R15, R14, R13, R12, R11, R10, RBX, RAX, R9, R8, RCX, RDX, RSI, RDI,
];

fn get_any_free_register(state: &ScopeState) -> Option<Register> {
    for reg in USED_REGISTERS {
        if !state.used_registers.contains_key(&reg) {
            return Some(reg);
        }
    }
    return None;
}

fn force_get_any_free_register(
    state: &mut ScopeState,
    protected_registers: &[Register],
    hint: Option<Register>,
) -> Register {
    if let Some(reg) = hint {
        if !state.used_registers.contains_key(&reg) {
            return reg;
        }
    }
    if let Some(reg) = get_any_free_register(state) {
        reg
    } else {
        let Some(oldest_temp) = state.used_registers.get_oldest(protected_registers) else {
            unreachable!()
        };
        let TempVariable::Register(reg) = oldest_temp.borrow().clone() else {
            unreachable!()
        };
        state.stack_size_current += 8;
        state.asm.mov(
            RegPointer {
                reg: RBP,
                offset: -state.stack_size_current,
            },
            reg,
            Word::QWORD,
        );
        state.used_registers.remove(&reg);
        *oldest_temp.borrow_mut() = TempVariable::Stack(state.stack_size_current);
        reg
    }
}

fn free_register(state: &mut ScopeState, target_reg: Register, protected_registers: &[Register]) {
    if !state.used_registers.contains_key(&target_reg) {
        return;
    };
    for free_reg in USED_REGISTERS {
        if !protected_registers.contains(&free_reg) && !state.used_registers.contains_key(&free_reg)
        {
            state.asm.mov(free_reg, target_reg, Word::QWORD);
            if let Some(temp) = state.used_registers.remove(&target_reg) {
                *temp.borrow_mut() = TempVariable::Register(free_reg);
                state.used_registers.insert(free_reg, temp);
            }
            return;
        }
    }
    let Some(temp) = state.used_registers.remove(&target_reg) else {
        unreachable!()
    };
    let TempVariable::Register(reg) = temp.borrow().clone() else {
        unreachable!()
    };
    state.stack_size_current += 8;
    state.asm.mov(
        RegPointer {
            reg: RBP,
            offset: -state.stack_size_current,
        },
        reg,
        Word::QWORD,
    );
    state.used_registers.remove(&reg);
    *temp.borrow_mut() = TempVariable::Stack(state.stack_size_current);
}

fn sizeof(data_type: &DataType, compilation_state: &mut CompilationState) -> i32 {
    match data_type {
        DataType::Int | DataType::Pointer { .. } => 8,
        DataType::Float => 4,
        DataType::Char | DataType::Boolean => 1,
        DataType::Array { data_type, size } => sizeof(&data_type, compilation_state) * size,
        DataType::Void => panic!("sizeof(void) is invalid!"),
        DataType::Struct(identifier) => {
            compilation_state.highlights.push(Highlight {
                span: identifier.span,
                kind: HighlightKind::Struct,
            });
            compilation_state
                .struct_types
                .get(&identifier.identifier)
                .expect("Struct type \"{}\" is missing!")
                .size
        }
    }
}

pub fn sizeofword(data_type: &DataType) -> Word {
    match data_type {
        DataType::Int | DataType::Pointer { .. } => Word::QWORD,
        DataType::Float => Word::DWORD,
        DataType::Char | DataType::Boolean => Word::BYTE,
        DataType::Array { data_type, size: _ } => sizeofword(data_type),
        DataType::Void => panic!("sizeofword(void) is invalid!"),
        DataType::Struct(_) => panic!("sizeofword(struct) is invalid!"),
    }
}

#[rustfmt::skip]
fn reg_from_size(size: i32, reg: Register) -> &'static str {
    match (reg, size) {
        (RAX, 8) => "rax", (RAX, 4) => "eax",  (RAX, 2) => "ax",   (RAX, 1) => "al",
        (RBX, 8) => "rbx", (RBX, 4) => "ebx",  (RBX, 2) => "bx",   (RBX, 1) => "bl",
        (RCX, 8) => "rcx", (RCX, 4) => "ecx",  (RCX, 2) => "cx",   (RCX, 1) => "cl",
        (RDX, 8) => "rdx", (RDX, 4) => "edx",  (RDX, 2) => "dx",   (RDX, 1) => "dl",
        (RSI, 8) => "rsi", (RSI, 4) => "esi",  (RSI, 2) => "si",   (RSI, 1) => "sil",
        (RDI, 8) => "rdi", (RDI, 4) => "edi",  (RDI, 2) => "di",   (RDI, 1) => "dil",
        (RBP, 8) => "rbp", (RBP, 4) => "ebp",  (RBP, 2) => "bp",   (RBP, 1) => "bpl",
        (RSP, 8) => "rsp", (RSP, 4) => "esp",  (RSP, 2) => "sp",   (RSP, 1) => "spl",
        (R8,  8) => "r8",  (R8,  4) => "r8d",  (R8,  2) => "r8w",  (R8,  1) => "r8b",
        (R9,  8) => "r9",  (R9,  4) => "r9d",  (R9,  2) => "r9w",  (R9,  1) => "r9b",
        (R10, 8) => "r10", (R10, 4) => "r10d", (R10, 2) => "r10w", (R10, 1) => "r10b",
        (R11, 8) => "r11", (R11, 4) => "r11d", (R11, 2) => "r11w", (R11, 1) => "r11b",
        (R12, 8) => "r12", (R12, 4) => "r12d", (R12, 2) => "r12w", (R12, 1) => "r12b",
        (R13, 8) => "r13", (R13, 4) => "r13d", (R13, 2) => "r13w", (R13, 1) => "r13b",
        (R14, 8) => "r14", (R14, 4) => "r14d", (R14, 2) => "r14w", (R14, 1) => "r14b",
        (R15, 8) => "r15", (R15, 4) => "r15d", (R15, 2) => "r15w", (R15, 1) => "r15b",
        _ => unreachable!()
    }
}

fn get_function(
    compilation_state: &CompilationState,
    identifier: &str,
) -> Option<FunctionPrototype> {
    if let Some(function) = compilation_state
        .parsed_unit
        .functions
        .iter()
        .find(|f| f.prototype.name.identifier.eq(identifier))
    {
        Some(function.prototype.clone())
    } else {
        compilation_state
            .parsed_unit
            .function_declarations
            .iter()
            .find(|f| f.name.identifier.eq(identifier))
            .cloned()
    }
}

fn compile_function_call(
    compilation_state: &mut CompilationState,
    state: &mut ScopeState,
    function_call: &FunctionCall,
) -> Option<ExpressionResult> {
    let Some(function) = get_function(compilation_state, &function_call.identifier.identifier)
    else {
        compilation_state.errors.push(Error {
            span: function_call.span,
            msg: format!(
                "Cannot find function \"{}\"",
                function_call.identifier.identifier
            ),
        });
        return None;
    };
    compilation_state.highlights.push(Highlight {
        span: function_call.identifier.span,
        kind: HighlightKind::Function,
    });
    if function_call.arguments.len() != function.arguments.len() {
        compilation_state.errors.push(Error {
            span: function_call.span,
            msg: format!(
                "Passed wrong amount of parameters to function \"{}\"",
                function.name.identifier
            ),
        });
        return None;
    }

    const SCRATCH_REGS: [Register; 9] = [RDI, RSI, RDX, RCX, R8, R9, RAX, R10, R11];

    let floats_index = function
        .arguments
        .iter()
        .filter(|data_type| is_float(data_type))
        .count();
    let mut integer_amount = function.arguments.len() - floats_index;
    let mut integer_index = integer_amount;

    let results: Option<Vec<ExpressionResult>> = function_call
        .arguments
        .iter()
        .enumerate()
        .rev()
        .map(|(i, argument)| {
            let data_type = function
                .arguments
                .get(i)
                .expect("Already checked for a number of arguments, should never happen.")
                .clone();
            let result = expression::compile_expression(
                state,
                compilation_state,
                argument,
                if i <= 5 { Some(SCRATCH_REGS[i]) } else { None },
            )?;

            if result.data_type != data_type {
                compilation_state.errors.push(Error {
                    span: argument.span,
                    msg: format!(
                        "Passed argument of a wrong type! Expected: {:?}, got: {:?}",
                        data_type, result.data_type
                    ),
                });
                return None;
            }
            Some(result)
        })
        .collect();
    let results = results?;

    for result in results.iter() {
        if is_float(&result.data_type) {
            //floats_index -= 1;
            todo!();
        } else {
            integer_index -= 1;
            if integer_index > 5 {
                if let ResultContainer::TempVariable(temp) = &result.result_container {
                    if let TempVariable::Register(reg) = temp.borrow().clone() {
                        state.asm.push(reg);
                        state.used_registers.remove(&reg);
                    } else {
                        todo!();
                    }
                } else {
                    todo!();
                }
            } else {
                let target_reg = match integer_index {
                    0 => RDI,
                    1 => RSI,
                    2 => RDX,
                    3 => RCX,
                    4 => R8,
                    5 => R9,
                    _ => unreachable!(),
                };
                match &result.result_container {
                    ResultContainer::TempVariable(temp) => {
                        let TempVariable::Register(reg) = temp.borrow().clone() else {
                            todo!()
                        };
                        if reg != target_reg {
                            free_register(state, target_reg, &SCRATCH_REGS);
                            state.asm.mov(target_reg, reg, Word::QWORD);
                        }
                    }
                    ResultContainer::IdentifierWithOffset { identifier, offset } => {
                        free_register(state, target_reg, &SCRATCH_REGS);
                        let Some(variable) = state
                            .variables
                            .iter()
                            .rev()
                            .find(|var| var.identifier.eq(identifier))
                        else {
                            compilation_state.errors.push(Error {
                                span: function_call.span,
                                msg: format!("Undeclared variable: \"{}\"", identifier),
                            });
                            return None;
                        };
                        state.asm.mov(
                            target_reg,
                            RegPointer {
                                reg: RBP,
                                offset: -(variable.stack_location - offset),
                            },
                            Word::QWORD,
                        );
                    }
                    _ => todo!("{:?}", result.result_container),
                }
            }
        }
    }

    if integer_amount > 6 {
        integer_amount = 6;
    }

    for i in integer_amount..SCRATCH_REGS.len() {
        free_register(state, SCRATCH_REGS[i], &SCRATCH_REGS);
    }

    state.asm.call(&function_call.identifier.identifier);
    if is_float(&function.return_type) {
        return Some(ExpressionResult {
            data_type: function.return_type.clone(),
            result_container: ResultContainer::FloatRegister,
        });
    }
    return Some(ExpressionResult {
        data_type: function.return_type.clone(),
        result_container: ResultContainer::TempVariable(Rc::new(RefCell::new(
            TempVariable::Register(Register::RAX),
        ))),
    });
}

fn is_float(data_type: &DataType) -> bool {
    data_type.eq(&DataType::Float)
}

// The compiler generates highlights for identifiers since they need context information to
// determine the semantic token type in the LSP. Might add more highlights later.
#[derive(Debug)]
pub enum HighlightKind {
    Parameter,
    Property,
    Variable,
    Struct,
    Function,
}

#[derive(Debug)]
pub struct Highlight {
    pub span: Span,
    pub kind: HighlightKind,
}

pub struct CompiledUnit {
    pub asm: String,
    pub errors: Vec<Error>,
    pub highlighs: Vec<Highlight>,
}

pub fn compile_to_assembly(parsed_unit: &ParsedUnit) -> CompiledUnit {
    let mut compilation_state = CompilationState {
        asm: Asm::new(),
        string_constants: HashMap::new(),
        float_constants: HashMap::new(),
        unique_label_id: 0,
        parsed_unit,
        struct_types: HashMap::new(),
        errors: Vec::new(),
        highlights: Vec::new(),
    };

    for struct_declaration in parsed_unit.struct_declarations.iter() {
        let mut members: Vec<StructMemberWithOffset> = Vec::new();
        let mut size = 0;

        for member in struct_declaration.members.iter() {
            members.push(StructMemberWithOffset {
                member: member.clone(),
                offset: size,
            });
            let member_size = sizeof(&member.data_type, &mut compilation_state);
            size = ((size + member_size - 1) / member_size) * member_size + member_size;
        }

        compilation_state.highlights.push(Highlight {
            span: struct_declaration.identifier.span,
            kind: HighlightKind::Struct,
        });

        compilation_state.struct_types.insert(
            struct_declaration.identifier.identifier.clone(),
            StructType { members, size },
        );
    }

    for function_declaration in parsed_unit.function_declarations.iter() {
        compilation_state.highlights.push(Highlight {
            span: function_declaration.name.span,
            kind: HighlightKind::Function,
        });
        compilation_state
            .asm
            .extern_label(&function_declaration.name.identifier);
    }

    for function in parsed_unit.functions.iter() {
        compilation_state.highlights.push(Highlight {
            span: function.prototype.name.span,
            kind: HighlightKind::Function,
        });
        if function.public {
            compilation_state
                .asm
                .global_label(&function.prototype.name.identifier);
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
        compilation_state
            .asm
            .label(&function.prototype.name.identifier);
        compilation_state.asm.push(RBP);
        compilation_state.asm.mov(RBP, RSP, Word::QWORD);
        let mut variables = Vec::new();
        let mut scope_state = ScopeState {
            iter: function.body.iter().peekable(),
            stack_size_current: 0,
            max_stack_size: 0,
            variables: &mut variables,
            current_function: &function,
            asm: Asm::new(),
            used_registers: RegMap::new(),
        };
        compile_scope(&mut scope_state, &mut compilation_state);
        compilation_state.asm.sub(
            RSP,
            ((scope_state.max_stack_size + 15) / 16) * 16,
            Word::QWORD,
        );
        compilation_state.asm.append(scope_state.asm);
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
        // TODO: When I make the instructions stored in a vector instead of generating assembly
        // directly I might check if the last instruction was a jmp to this label and remove it
        compilation_state
            .asm
            .label(&fmt!(".{}.end", function.prototype.name.identifier));
        // compilation_state.asm.mov(RSP, RBP, Word::QWORD);
        // compilation_state.asm.pop(RBP);
        compilation_state.asm.leave();
        compilation_state.asm.ret();
    }

    for (text, id) in compilation_state.string_constants {
        compilation_state.asm.label(&fmt!(".String{}", id));
        compilation_state.asm.db(text);
    }

    for (text, id) in compilation_state.float_constants {
        compilation_state.asm.label(&fmt!(".Float{}", id));
        compilation_state.asm.db(text);
    }

    CompiledUnit {
        asm: compilation_state.asm.assembly,
        errors: compilation_state.errors,
        highlighs: compilation_state.highlights,
    }
}

fn set_instruction_from_flag(flag: &Flag) -> &str {
    match flag {
        Flag::EQUAL => "sete",
        Flag::LARGER => "setg",
        Flag::SMALLER => "setl",
    }
}

fn jump_instruction_from_negated_flag(flag: &Flag) -> &str {
    match flag {
        Flag::EQUAL => "jne",
        Flag::LARGER => "jle",
        Flag::SMALLER => "jge",
    }
}
