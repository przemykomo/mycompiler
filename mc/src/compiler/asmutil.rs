use std::fmt::Display;

use crate::compiler::jump_instruction_from_negated_flag;

use super::{fmt, reg_from_size, set_instruction_from_flag, Register};

pub struct Asm {
    pub assembly: String
}

pub enum Word {
    QWORD,
    DWORD,
    WORD,
    BYTE
}

#[allow(dead_code)]
pub enum FloatRegister {
    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
    XMM8,
    XMM9,
    XMM10,
    XMM11,
    XMM12,
    XMM13,
    XMM14,
    XMM15
}

pub enum Memory {
    Reg(RegPointer),
    Label(String)
}

pub struct RegPointer {
    pub reg: Register,
    pub offset: i32
}

impl Display for Word {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let word = match self {
            Word::QWORD => "QWORD".to_string(),
            Word::DWORD => "DWORD".to_string(),
            Word::WORD => "WORD".to_string(),
            Word::BYTE => "BYTE".to_string(),
        };
        write!(f, "{}", word)
    }
}

pub trait Operand {
    fn asm_from_size(&self, word: &Word) -> String;
}

pub trait FloatOperand {
    fn asm(&self) -> String;
}

pub trait FloatOrIntRegOrMemOperand {
    fn asm(&self) -> String;
}

impl Operand for RegPointer {
    fn asm_from_size(&self, word: &Word) -> String {
        fmt!("{} [{} {:+}]", word, reg_from_size(8, self.reg), self.offset)
    }
}

impl FloatOperand for String {
    fn asm(&self) -> String {
        fmt!("DWORD [{}]", self)
    }
}

impl FloatOperand for RegPointer {
    fn asm(&self) -> String {
        self.asm_from_size(&Word::DWORD) // assuming only f32 for now
    }
}

impl FloatOrIntRegOrMemOperand for FloatRegister {
    fn asm(&self) -> String {
        FloatOperand::asm(self)
    }
}

impl FloatOrIntRegOrMemOperand for Register {
    fn asm(&self) -> String {
        self.asm_from_size(&Word::DWORD)
    }
}

impl FloatOperand for FloatRegister {
    fn asm(&self) -> String {
        match self {
            XMM0 => "XMM0",
            XMM1 => "XMM1",
            XMM2 => "XMM2",
            XMM3 => "XMM3",
            XMM4 => "XMM4",
            XMM5 => "XMM5",
            XMM6 => "XMM6",
            XMM7 => "XMM7",
            XMM8 => "XMM8",
            XMM9 => "XMM9",
            XMM10 => "XMM10",
            XMM11 => "XMM11",
            XMM12 => "XMM12",
            XMM13 => "XMM13",
            XMM14 => "XMM14",
            XMM15 => "XMM15",
        }.to_string()
    }
}

use Register::*;
use FloatRegister::*;

impl Operand for Memory {
    fn asm_from_size(&self, word: &Word) -> String {
        match self {
            Memory::Reg(reg_pointer) => reg_pointer.asm_from_size(word),
            Memory::Label(label) => label.clone(),
        }
    }
}

impl Operand for Register {
    fn asm_from_size(&self, word: &Word) -> String {
        match (self, word) {
            (RAX, Word::QWORD) => "rax", (RAX, Word::DWORD) => "eax",  (RAX, Word::WORD) => "ax",   (RAX, Word::BYTE) => "al",
            (RBX, Word::QWORD) => "rbx", (RBX, Word::DWORD) => "ebx",  (RBX, Word::WORD) => "bx",   (RBX, Word::BYTE) => "bl",
            (RCX, Word::QWORD) => "rcx", (RCX, Word::DWORD) => "ecx",  (RCX, Word::WORD) => "cx",   (RCX, Word::BYTE) => "cl",
            (RDX, Word::QWORD) => "rdx", (RDX, Word::DWORD) => "edx",  (RDX, Word::WORD) => "dx",   (RDX, Word::BYTE) => "dl",
            (RSI, Word::QWORD) => "rsi", (RSI, Word::DWORD) => "esi",  (RSI, Word::WORD) => "si",   (RSI, Word::BYTE) => "sil",
            (RDI, Word::QWORD) => "rdi", (RDI, Word::DWORD) => "edi",  (RDI, Word::WORD) => "di",   (RDI, Word::BYTE) => "dil",
            (RBP, Word::QWORD) => "rbp", (RBP, Word::DWORD) => "ebp",  (RBP, Word::WORD) => "bp",   (RBP, Word::BYTE) => "bpl",
            (RSP, Word::QWORD) => "rsp", (RSP, Word::DWORD) => "esp",  (RSP, Word::WORD) => "sp",   (RSP, Word::BYTE) => "spl",
            (R8,  Word::QWORD) => "r8",  (R8,  Word::DWORD) => "r8d",  (R8,  Word::WORD) => "r8w",  (R8,  Word::BYTE) => "r8b",
            (R9,  Word::QWORD) => "r9",  (R9,  Word::DWORD) => "r9d",  (R9,  Word::WORD) => "r9w",  (R9,  Word::BYTE) => "r9b",
            (R10, Word::QWORD) => "r10", (R10, Word::DWORD) => "r10d", (R10, Word::WORD) => "r10w", (R10, Word::BYTE) => "r10b",
            (R11, Word::QWORD) => "r11", (R11, Word::DWORD) => "r11d", (R11, Word::WORD) => "r11w", (R11, Word::BYTE) => "r11b",
            (R12, Word::QWORD) => "r12", (R12, Word::DWORD) => "r12d", (R12, Word::WORD) => "r12w", (R12, Word::BYTE) => "r12b",
            (R13, Word::QWORD) => "r13", (R13, Word::DWORD) => "r13d", (R13, Word::WORD) => "r13w", (R13, Word::BYTE) => "r13b",
            (R14, Word::QWORD) => "r14", (R14, Word::DWORD) => "r14d", (R14, Word::WORD) => "r14w", (R14, Word::BYTE) => "r14b",
            (R15, Word::QWORD) => "r15", (R15, Word::DWORD) => "r15d", (R15, Word::WORD) => "r15w", (R15, Word::BYTE) => "r15b",
        }.to_string()
    }
}

impl Operand for i32 {
    fn asm_from_size(&self, _word: &Word) -> String {
        self.to_string()
    }
}

impl Operand for &char {
    fn asm_from_size(&self, _word: &Word) -> String {
        fmt!("'{}'", self)
    }
}

impl Asm {
    pub fn new() -> Self {
        Self { assembly: String::new() }
    }

    pub fn label(&mut self, label: &str) {
        self.assembly.push_str(&fmt!("{}:\n", label));
    }

    pub fn extern_label(&mut self, label: &str) {
        self.assembly.push_str(&fmt!("extern {}\n", label));
    }

    pub fn global_label(&mut self, label: &str) {
        self.assembly.push_str(&fmt!("global {}\n", label));
    }

    pub fn push(&mut self, reg: Register) {
        self.assembly.push_str(&fmt!("push {}\n", reg_from_size(8, reg)));
    }

    pub fn mov(&mut self, left: impl Operand, right: impl Operand, size: Word) {
        self.assembly.push_str(&fmt!("mov {}, {}\n", left.asm_from_size(&size), right.asm_from_size(&size)));
    }

    pub fn subss(&mut self, left: impl FloatOperand, right: impl FloatOperand) {
        self.assembly.push_str(&fmt!("subss {}, {}\n", left.asm(), right.asm()));
    }

    pub fn mulss(&mut self, left: impl FloatOperand, right: impl FloatOperand) {
        self.assembly.push_str(&fmt!("mulss {}, {}\n", left.asm(), right.asm()));
    }

    pub fn divss(&mut self, left: impl FloatOperand, right: impl FloatOperand) {
        self.assembly.push_str(&fmt!("divss {}, {}\n", left.asm(), right.asm()));
    }

    pub fn addss(&mut self, left: impl FloatOperand, right: impl FloatOperand) {
        self.assembly.push_str(&fmt!("addss {}, {}\n", left.asm(), right.asm()));
    }

    pub fn movss(&mut self, left: impl FloatOperand, right: impl FloatOperand) {
        self.assembly.push_str(&fmt!("movss {}, {}\n", left.asm(), right.asm()));
    }

    pub fn movd(&mut self, left: impl FloatOrIntRegOrMemOperand, right: impl FloatOrIntRegOrMemOperand) {
        self.assembly.push_str(&fmt!("movd {}, {}\n", left.asm(), right.asm()));
    }

    pub fn append(&mut self, other: Asm) {
        self.assembly.push_str(&other.assembly);
    }

    pub fn pop(&mut self, reg: Register) {
        self.assembly.push_str(&fmt!("pop {}\n", reg_from_size(8, reg)));
    }

    pub fn ret(&mut self) {
        self.assembly.push_str("ret\n");
    }

    pub fn db(&mut self, text: String) {
        self.assembly.push_str(&fmt!("db '{}', 0\n", text));
    }

    pub fn set(&mut self, flag: &super::Flag, reg: Register) {
        self.assembly.push_str(&fmt!("{} {}\n", set_instruction_from_flag(flag), reg.asm_from_size(&Word::BYTE)));
    }

    pub fn jmp(&mut self, label: &str) {
        self.assembly.push_str(&fmt!("jmp {}\n", label));
    }

    pub fn jmp_negated_flag(&mut self, flag: super::Flag, label: &str) {
        self.assembly.push_str(&fmt!("{} {}\n", jump_instruction_from_negated_flag(&flag), label));
    }

    pub fn call(&mut self, label: &str) {
        self.assembly.push_str(&fmt!("call {}\n", label));
    }

    pub fn add(&mut self, left: impl Operand, right: impl Operand, size: Word) {
        self.assembly.push_str(&fmt!("add {}, {}\n", left.asm_from_size(&size), right.asm_from_size(&size)));
    }

    pub fn sub(&mut self, left: impl Operand, right: impl Operand, size: Word) {
        self.assembly.push_str(&fmt!("sub {}, {}\n", left.asm_from_size(&size), right.asm_from_size(&size)));
    }

    pub fn cmp(&mut self, left: impl Operand, right: impl Operand, size: Word) {
        self.assembly.push_str(&fmt!("cmp {}, {}\n", left.asm_from_size(&size), right.asm_from_size(&size)));
    }

    pub fn imul(&mut self, left: impl Operand, right: impl Operand, size: Word) {
        self.assembly.push_str(&fmt!("imul {}, {}\n", left.asm_from_size(&size), right.asm_from_size(&size)));
    }

    pub fn idiv(&mut self, op: impl Operand, size: Word) {
        self.assembly.push_str(&fmt!("idiv {}\n", op.asm_from_size(&size)));
    }

    pub fn cqo(&mut self) {
        self.assembly.push_str("cqo\n");
    }

    pub fn lea(&mut self, left: Register, right: Memory, size: Word) {
        self.assembly.push_str(&fmt!("lea {}, {}\n", left.asm_from_size(&size), right.asm_from_size(&Word::QWORD)));
    }

    pub fn leave(&mut self) {
        self.assembly.push_str("leave\n");
    }
}
