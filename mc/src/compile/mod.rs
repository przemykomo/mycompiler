// #![allow(unused)]
use std::{fs, mem::transmute};

use object::{
    build::{
        Bytes,
        elf::{Builder, Relocation, SectionData, Symbol, SymbolId},
    },
    elf::{
        EM_X86_64, ET_REL, R_X86_64_PC32, SHF_ALLOC, SHF_EXECINSTR, SHT_PROGBITS, SHT_RELA,
        SHT_STRTAB, SHT_SYMTAB, STB_GLOBAL, STB_LOCAL, STT_NOTYPE, STT_SECTION,
    },
    write::StreamingBuffer,
};

use crate::{
    ast::ArithmeticOp,
    ir::{IRGen, Instruction, Value},
    tokenizer::DataType,
};
mod regmap;
use regmap::RegMap;

#[derive(Debug, Clone, Copy)]
pub enum TempLoc {
    Reg(Register),
    Mem(i32),
    None,
}

const EXTERN_SYMBOL_ADDEND: i64 = -0x04; // All extern symbol relocations generated using NASM have
// this addend, no idea why.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub enum Register {
    RAX = 0,
    RCX = 1,
    RDX = 2,
    RBX = 3,
    RSP = 4,
    RBP = 5,
    RSI = 6,
    RDI = 7,
    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    R13 = 13,
    R14 = 14,
    R15 = 15,
}

const ALL_REGISTERS: [Register; 16] = {
    use Register::*;

    [
        RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15,
    ]
};

struct Assembler {
    pub data: Vec<u8>,
}

const REX_W: u8 = 0b01001000;
const REX_R: u8 = 0b01000100;
const REX_X: u8 = 0b01000010;
const REX_B: u8 = 0b01000001;

const MOD_PTR: u8 = 0b00000000;
const MOD_DISP8: u8 = 0b01000000;
const MOD_DISP32: u8 = 0b10000000;
const MOD_REG: u8 = 0b11000000;

impl Assembler {
    pub fn mov(&mut self, reg: Register, val: i64) {
        if reg < Register::R8 {
            self.data.push(REX_W); // rex.w - 64bit version of mov
            self.data.push(0xb8 + reg as u8);
        } else {
            self.data.push(REX_W | REX_B); // rex.wb - 64bit version of mov
            self.data.push(0xb8 + reg as u8 - 8);
        }
        let val: &[u8; 8] = unsafe { transmute(&val) };
        self.data.extend_from_slice(val);
    }

    pub fn ret(&mut self) {
        self.data.push(0xc3);
    }

    pub fn call(&mut self) {
        self.data.extend_from_slice(&[0xe8, 0x00, 0x00, 0x00, 0x00]);
    }

    /// Returns the offset of where to patch in the stack size
    pub fn stackframe_setup(&mut self) -> usize {
        self.push(Register::RBP);
        self.mov_reg(Register::RBP, Register::RSP);
        self.sub(Register::RSP, 0);
        return self.data.len() - 1;
    }

    pub fn stackframe_cleanup(&mut self) {
        self.mov_reg(Register::RSP, Register::RBP);
        self.pop(Register::RBP);
    }

    pub fn push(&mut self, reg: Register) {
        if reg < Register::R8 {
            self.data.push(0x50 + reg as u8);
        } else {
            self.data.push(REX_B);
            self.data.push(0x50 + reg as u8 - 8);
        }
    }

    pub fn pop(&mut self, reg: Register) {
        if reg < Register::R8 {
            self.data.push(0x58 + reg as u8);
        } else {
            self.data.push(REX_B);
            self.data.push(0x58 + reg as u8 - 8);
        }
    }

    pub fn mov_reg(&mut self, left: Register, right: Register) {
        if left < Register::R8 {
            if right < Register::R8 {
                self.data.extend_from_slice(&[REX_W, 0x89]);
                self.data.push(0xc0 + left as u8 + (right as u8 * 8));
            } else {
                self.data.extend_from_slice(&[REX_W | REX_R, 0x89]);
                self.data.push(0xc0 + left as u8 + ((right as u8 - 8) * 8));
            }
        } else {
            if right < Register::R8 {
                self.data.extend_from_slice(&[REX_W | REX_B, 0x89]);
                self.data.push(0xc0 + left as u8 - 8 + (right as u8 * 8));
            } else {
                self.data.extend_from_slice(&[REX_W | REX_R | REX_B, 0x89]);
                self.data
                    .push(0xc0 + left as u8 - 8 + ((right as u8 - 8) * 8));
            }
        }
    }

    /// Sign extends the value to 64 bits
    pub fn move_to_mem_sign_extend(&mut self, ptr: Register, offset: i32, val: i32) {
        // if reg < register::r8 {
        //     self.data.push(rex_w); // rex.w - 64bit version of mov
        //     self.data.push(0xb8 + reg as u8);
        // } else {
        //     self.data.push(rex_w | rex_b); // rex.wb - 64bit version of mov
        //     self.data.push(0xb8 + reg as u8 - 8);
        // }
        let offset: u8 = i8::cast_unsigned(offset as i8); //TODO?
        self.data.push(REX_W);
        self.data.push(0xc7);
        self.data.push(0x45);
        self.data.push(offset);
        let val: &[u8; 4] = unsafe { transmute(&val) };
        self.data.extend_from_slice(val);
    }

    pub fn sub(&mut self, reg: Register, val: u8) {
        if reg < Register::R8 {
            self.data.push(REX_W);
            self.data.push(0x83);
            self.data.push(0xe8 + reg as u8);
        } else {
            self.data.push(REX_W | REX_B);
            self.data.push(0x83);
            self.data.push(0xe8 + reg as u8 - 8);
        }
        self.data.push(val);
    }

    pub fn add(&mut self, left: Register, right: Register) {
        if left < Register::R8 {
            if right < Register::R8 {
                self.data.extend_from_slice(&[REX_W, 0x01]);
                self.data.push(0xc0 + left as u8 + (right as u8 * 8));
            } else {
                self.data.extend_from_slice(&[REX_W | REX_R, 0x01]);
                self.data.push(0xc0 + left as u8 + ((right as u8 - 8) * 8));
            }
        } else {
            if right < Register::R8 {
                self.data.extend_from_slice(&[REX_W | REX_B, 0x01]);
                self.data.push(0xc0 + left as u8 - 8 + (right as u8 * 8));
            } else {
                self.data.extend_from_slice(&[REX_W | REX_R | REX_B, 0x01]);
                self.data
                    .push(0xc0 + left as u8 - 8 + ((right as u8 - 8) * 8));
            }
        }
    }

    /// Calculates the REX prefix, ModR/M byte & optionaly SIB or displacement
    // ------ REX prefix ---------
    // Bit     7 6 5 4 3 2 1 0
    // Usage   0 1 0 0 W R X B
    // - W changes the operand size to 64 bits
    // - R expands Reg to 4 bits
    // - X expands SIB index to 4 bits
    // - B expands R/M or SIB base to 4 bits
    //
    // ------ ModR/M byte --------
    // Bit     7 6 | 5 4 3 | 2 1 0
    // Usage   Mod |  Reg  |  R/M
    //
    // - Adressing Modes:
    // Mod value    00    |      01     |      10      | 11
    // R/M = XXX   [RAX]  | [RAX+disp8] | [RAX+disp32] | RAX
    // R/M = 100   [SIB]  | [SIB+disp8] | [SIB+disp32] | RSP
    // R/M = 101 [disp32] | [RBP+disp8] | [RBP+disp32] | RBP
    //            ^--[RIP+disp32] in 64-bit mode,
    //            [disp32] can be obtained with SIB where Base=101 and Index=100
    //
    // ------- SIB byte ----------
    // Bit     7 6 | 5 4 3 | 2 1 0
    // Usage Scale | Index | Base
    // (scale * index) + base + disp
    //  - Scale is 1, 2, 4, or 8
    //  - Base and Index encode registers
    //  - Disp is a normal displacement as specified in the adressing modes, encoded after SIB
    //
    fn mod_rm_ptr(reg: Register, rm: Register, offset: i32) -> (u8, u8, Option<u8>, Vec<u8>) {
        let mut rex = REX_W;
        if reg >= Register::R8 {
            rex |= REX_R;
        }
        if rm >= Register::R8 {
            rex |= REX_B;
        }

        let mut mod_rm = (reg as u8 & 0b111) << 3; // Reg
        let mut sib: Option<u8> = None;
        let mut disp: Vec<u8> = Vec::with_capacity(4);

        if offset != 0 {
            if let Ok(byte) = <i32 as TryInto<i8>>::try_into(offset) {
                mod_rm |= MOD_DISP8;
                disp.push(i8::cast_unsigned(byte));
            } else {
                mod_rm |= MOD_DISP32;
                disp.extend_from_slice(&offset.to_ne_bytes());
            }
        }

        match rm as u8 & 0b111 {
            4 => {
                // R/M = 4 is a special value which specifies that a SIB byte will follow this one
                mod_rm |= 4;
                sib = Some(0b00_100_100); // Scale 1, index 0, reg rsp
            }
            5 => {
                // R/M = 5 still means that we use RBP as expected, but a displacement always
                // has to follow
                mod_rm |= 5;
                if offset == 0 {
                    mod_rm |= MOD_DISP8;
                    disp.push(0);
                }
            }
            other => {
                // all other values just mean a specific register
                mod_rm |= other;
            }
        }
        (rex, mod_rm, sib, disp)
    }

    pub fn add_mem(&mut self, reg: Register, ptr: Register, offset: i32) {
        let (rex, mod_rm, sib, disp) = Self::mod_rm_ptr(reg, ptr, offset);
        self.data.push(rex);
        self.data.push(0x03);
        self.data.push(mod_rm);
        if let Some(sib) = sib {
            self.data.push(sib);
        }
        self.data.extend_from_slice(disp.as_slice());
    }

    pub fn imul_mem(&mut self, reg: Register, ptr: Register, offset: i32) {
        let (rex, mod_rm, sib, disp) = Self::mod_rm_ptr(reg, ptr, offset);
        self.data.push(rex);
        self.data.extend_from_slice(&[0x0f, 0xaf]);
        self.data.push(mod_rm);
        if let Some(sib) = sib {
            self.data.push(sib);
        }
        self.data.extend_from_slice(disp.as_slice());
    }

    pub fn mov_reg_from_mem(&mut self, reg: Register, ptr: Register, offset: i32) {
        let (rex, mod_rm, sib, disp) = Self::mod_rm_ptr(reg, ptr, offset);
        self.data.push(rex);
        self.data.push(0x8b);
        self.data.push(mod_rm);
        if let Some(sib) = sib {
            self.data.push(sib);
        }
        self.data.extend_from_slice(disp.as_slice());
    }

    pub fn mov_to_mem(&mut self, reg: Register, ptr: Register, offset: i32) {
        let (rex, mod_rm, sib, disp) = Self::mod_rm_ptr(reg, ptr, offset);
        self.data.push(rex);
        self.data.push(0x89);
        self.data.push(mod_rm);
        if let Some(sib) = sib {
            self.data.push(sib);
        }
        self.data.extend_from_slice(disp.as_slice());
    }
}

pub fn compile_elf_object(ir: &IRGen, out_file: &str) {
    let mut externs: Vec<(String, u64)> = Vec::new();
    let mut symbols_to_export: Vec<(String, u64)> = Vec::new();

    let mut assembler = Assembler { data: Vec::new() };

    for function in &ir.functions {
        let mut temp_locations: Vec<TempLoc> = vec![TempLoc::None; function.scope.temps.len()];
        let mut var_locations: Vec<i32> = vec![0; function.scope.vars.len()];
        let mut regmap = RegMap::new();
        let mut stack_size: i32 = 0;

        for (var_index, var) in function.scope.vars.iter().enumerate() {
            if var.argument {
                todo!();
            } else {
                stack_size += sizeof(&var.data_type) as i32;
                var_locations[var_index] = -stack_size;
            }
        }

        symbols_to_export.push((function.ident.clone(), assembler.data.len() as u64));
        let stack_size_patch_offset = assembler.stackframe_setup();
        for instruction in &function.scope.instructions {
            match instruction {
                Instruction::ArithmeticInt {
                    op,
                    lhs,
                    rhs,
                    result,
                } => match (temp_locations[*lhs], temp_locations[*rhs]) {
                    (TempLoc::Reg(left), TempLoc::Reg(right)) => match op {
                        ArithmeticOp::Add => {
                            assembler.add(left, right);
                            temp_locations[*lhs] = TempLoc::None;
                            temp_locations[*rhs] = TempLoc::None;
                            temp_locations[*result] = TempLoc::Reg(left);
                        }
                        ArithmeticOp::Sub => todo!(),
                        ArithmeticOp::Mul => todo!(),
                        ArithmeticOp::Div => todo!(),
                    },
                    (TempLoc::Reg(left), TempLoc::Mem(right)) => match op {
                        ArithmeticOp::Add => {
                            assembler.add_mem(left, Register::RBP, right);
                            temp_locations[*lhs] = TempLoc::None;
                            temp_locations[*rhs] = TempLoc::None;
                            temp_locations[*result] = TempLoc::Reg(left);
                        }
                        ArithmeticOp::Sub => todo!(),
                        ArithmeticOp::Mul => {
                            assembler.imul_mem(left, Register::RBP, right);
                            temp_locations[*lhs] = TempLoc::None;
                            temp_locations[*rhs] = TempLoc::None;
                            temp_locations[*result] = TempLoc::Reg(left);
                        }
                        ArithmeticOp::Div => todo!(),
                    },
                    (TempLoc::Mem(left), TempLoc::Reg(reg)) => match op {
                        ArithmeticOp::Add => {
                            assembler.add_mem(reg, Register::RBP, left);
                            temp_locations[*lhs] = TempLoc::None;
                            temp_locations[*rhs] = TempLoc::None;
                            temp_locations[*result] = TempLoc::Reg(reg);
                        }
                        ArithmeticOp::Sub => todo!(),
                        ArithmeticOp::Mul => todo!(),
                        ArithmeticOp::Div => todo!(),
                    },
                    (TempLoc::Mem(left), TempLoc::Mem(right)) => {
                        match regmap.get_any_free_register() {
                            Some(reg) => {
                                assembler.mov_reg_from_mem(reg, Register::RBP, left);
                                match op {
                                    ArithmeticOp::Add => {
                                        assembler.add_mem(reg, Register::RBP, right);
                                        temp_locations[*lhs] = TempLoc::None;
                                        temp_locations[*rhs] = TempLoc::None;
                                        temp_locations[*result] = TempLoc::Reg(reg);
                                    }
                                    ArithmeticOp::Sub => todo!(),
                                    ArithmeticOp::Mul => todo!(),
                                    ArithmeticOp::Div => todo!(),
                                }
                            }
                            None => todo!(),
                        }
                    }
                    (TempLoc::None, loc) => unreachable!("{} {} {:?}", lhs, rhs, loc),
                    (loc, TempLoc::None) => unreachable!("{} {} {:?}", lhs, rhs, loc),
                },
                Instruction::ArithmeticFloat {
                    op,
                    lhs,
                    rhs,
                    result,
                } => todo!(),
                Instruction::Comparison {
                    op,
                    lhs,
                    rhs,
                    result,
                } => todo!(),
                Instruction::LoadValue(index, val) => match val {
                    Value::ImmediateInt(val) => match temp_locations[*index] {
                        TempLoc::Reg(reg) => {
                            assembler.mov(reg, *val);
                        }
                        TempLoc::Mem(offset) => {
                            if let Ok(val) = (*val).try_into() {
                                assembler.move_to_mem_sign_extend(Register::RBP, offset, val);
                            } else {
                                todo!();
                            }
                        }
                        TempLoc::None => match regmap.get_any_free_register() {
                            Some(reg) => {
                                regmap.insert(reg, *index);
                                temp_locations[*index] = TempLoc::Reg(reg);
                                assembler.mov(reg, *val);
                            }
                            None => {
                                if let Ok(val) = (*val).try_into() {
                                    stack_size += 8;
                                    assembler.move_to_mem_sign_extend(
                                        Register::RBP,
                                        -stack_size,
                                        val,
                                    );
                                } else {
                                    todo!();
                                }
                            }
                        },
                    },
                    Value::ImmediateFloat(_) => todo!(),
                    Value::ImmediateString(_) => todo!(),
                    Value::StructLiteral(values) => todo!(),
                    Value::ArrayAccess {
                        var_index,
                        array_index,
                    } => match &function.scope.vars[*var_index].data_type {
                        DataType::I64 => {
                            assert!(matches!(&**array_index, Value::ImmediateInt(0)));
                            temp_locations[*index] = TempLoc::Mem(var_locations[*var_index]);
                        }
                        DataType::Char => todo!(),
                        DataType::Array { data_type, size } => todo!(),
                        DataType::Pointer(data_type) => todo!(),
                        DataType::Boolean => todo!(),
                        DataType::Void => todo!(),
                        DataType::F32 => todo!(),
                        DataType::Struct(identifier_spanned) => todo!(),
                    },
                    Value::MemberAccess => todo!(),
                    Value::Temporary(_) => todo!(),
                    Value::Call(_, values) => todo!(),
                    Value::Variable(var_index) => {
                        match &function.scope.vars[*var_index].data_type {
                            DataType::I64 => {
                                temp_locations[*index] = TempLoc::Mem(var_locations[*var_index]);
                            }
                            DataType::Char => todo!(),
                            DataType::Array { data_type, size } => todo!(),
                            DataType::Pointer(data_type) => todo!(),
                            DataType::Boolean => todo!(),
                            DataType::Void => todo!(),
                            DataType::F32 => todo!(),
                            DataType::Struct(identifier_spanned) => todo!(),
                        }
                    }
                },
                Instruction::Call(ident, items) => {
                    call_function(
                        ir,
                        ident,
                        items,
                        &mut assembler,
                        &mut externs,
                        &mut temp_locations,
                        &mut var_locations,
                        &mut regmap,
                        &mut stack_size,
                    );
                }
                Instruction::AssignTemp { lhs, rhs } => {
                    match (temp_locations[*lhs], temp_locations[*rhs]) {
                        (TempLoc::Reg(left), TempLoc::Reg(right)) => {
                            assembler.mov_reg(left, right);
                        }
                        (TempLoc::None, TempLoc::Reg(right)) => {
                            match regmap.get_any_free_register() {
                                Some(left) => {
                                    regmap.insert(left, *lhs);
                                    assembler.mov_reg(left, right);
                                }
                                None => todo!(),
                            }
                        }
                        (TempLoc::Reg(left), TempLoc::Mem(right)) => {
                            assembler.mov_reg_from_mem(left, Register::RBP, right);
                        }
                        (TempLoc::Mem(_), TempLoc::Reg(right)) => todo!(),
                        (TempLoc::Mem(_), TempLoc::Mem(_)) => todo!(),
                        (TempLoc::None, TempLoc::Mem(_)) => todo!(),
                        (_, TempLoc::None) => unreachable!(),
                    }
                }
                Instruction::Return(value) => {
                    match value {
                        Value::ImmediateInt(val) => {
                            assembler.mov(Register::RAX, *val);
                        }
                        Value::ImmediateFloat(_) => todo!(),
                        Value::ImmediateString(_) => todo!(),
                        Value::StructLiteral(values) => todo!(),
                        Value::ArrayAccess {
                            var_index,
                            array_index,
                        } => todo!(),
                        Value::MemberAccess => todo!(),
                        Value::Temporary(index) => match temp_locations[*index] {
                            TempLoc::Reg(reg) => {
                                if reg != Register::RAX {
                                    assembler.mov_reg(Register::RAX, reg);
                                }
                            }
                            TempLoc::Mem(_) => todo!(),
                            TempLoc::None => unreachable!(),
                        },
                        Value::Call(_, values) => todo!(),
                        Value::Variable(_) => todo!(),
                    }

                    assembler.stackframe_cleanup();
                    assembler.ret();
                }
                Instruction::AssignVar { var, temp } => {
                    let to = var_locations[*var];
                    match temp_locations[*temp] {
                        TempLoc::Reg(from) => {
                            assembler.mov_to_mem(from, Register::RBP, to);
                        }
                        TempLoc::Mem(from) => match regmap.get_any_free_register() {
                            Some(reg) => {
                                assembler.mov_reg_from_mem(reg, Register::RBP, from);
                                assembler.mov_to_mem(reg, Register::RBP, to);
                            }
                            None => todo!(),
                        },
                        TempLoc::None => unreachable!(),
                    }
                }
            }
        }
        if let Ok(stack_size) = stack_size.try_into() {
            assembler.data[stack_size_patch_offset] = stack_size;
        } else {
            todo!();
        }
    }

    let mut builder = Builder::new(object::Endianness::Little, true);

    builder.header.e_type = ET_REL;
    builder.header.e_machine = EM_X86_64;

    let text = builder.sections.add();
    text.name = ".text".into();
    text.sh_type = SHT_PROGBITS;
    text.sh_flags = (SHF_ALLOC | SHF_EXECINSTR) as u64;
    text.data = SectionData::Data(Bytes::from(assembler.data));
    text.sh_addralign = 16;
    let text_id = text.id();

    // let data_sec = builder.sections.add();
    // data_sec.name = ".data".into();
    // data_sec.sh_type = SHT_PROGBITS;
    // data_sec.sh_flags = (SHF_WRITE | SHF_ALLOC) as u64;

    // let mut data: Vec<u8> = Vec::new();
    // let msg_addend = 0;
    // data.extend_from_slice(msg.to_bytes_with_nul());
    // let fmt_addend = data.len() as i64;
    // data.extend_from_slice(fmt.to_bytes_with_nul());
    //
    // data_sec.data = SectionData::Data(Bytes::from(data));
    // data_sec.sh_addralign = 4;
    // let data_id = data_sec.id();

    let shstrtab = builder.sections.add();
    shstrtab.name = ".shstrtab".into();
    shstrtab.sh_type = SHT_STRTAB;
    shstrtab.data = SectionData::SectionString;
    shstrtab.sh_addralign = 1;

    let symtab = builder.sections.add();
    symtab.name = ".symtab".into();
    symtab.sh_type = SHT_SYMTAB;
    symtab.data = SectionData::Symbol;
    symtab.sh_addralign = 8;
    symtab.sh_entsize = 0x18;
    let symtab_id = symtab.id();

    let strtab = builder.sections.add();
    strtab.name = ".strtab".into();
    strtab.sh_type = SHT_STRTAB;
    strtab.data = SectionData::String;

    let text_sym = builder.symbols.add();
    text_sym.section = Some(text_id);
    text_sym.set_st_info(STB_LOCAL, STT_SECTION);

    // let data_sym = builder.symbols.add();
    // data_sym.section = Some(data_id);
    // data_sym.set_st_info(STB_LOCAL, STT_SECTION);
    // let data_sym_id = data_sym.id();

    for (ident, offset) in symbols_to_export {
        let sym = builder.symbols.add();
        let ident: Vec<u8> = ident.into();
        sym.name = ident.into();
        sym.section = Some(text_id);
        sym.st_value = offset;
        sym.set_st_info(STB_GLOBAL, STT_NOTYPE);
    }

    let externs: Vec<(SymbolId, u64)> = externs
        .iter()
        .map(|(ident, offset)| {
            let name = ident.as_str().into();
            let sym = builder.symbols.iter_mut().find(|s| s.name == name);
            let sym: &mut Symbol = if let Some(sym) = sym {
                sym
            } else {
                builder.symbols.add()
            };

            sym.name = name;
            sym.set_st_info(STB_GLOBAL, STT_NOTYPE);
            (sym.id(), *offset)
        })
        .collect();

    let rela = builder.sections.add();
    rela.name = ".rela.text".into();
    rela.sh_type = SHT_RELA;
    rela.sh_link_section = Some(symtab_id);
    rela.sh_info_section = Some(text_id);
    rela.sh_entsize = 0x18; // readelf expexted 18 so...

    let extern_relocations = externs
        .iter()
        .map(|(id, offset)| Relocation {
            r_offset: *offset,
            symbol: Some(*id),
            r_type: R_X86_64_PC32,
            r_addend: EXTERN_SYMBOL_ADDEND,
        })
        .collect();

    // let relocations = vec![
    //     Relocation {
    //         // "%s"
    //         r_offset: msg_offset,
    //         symbol: Some(data_sym_id),
    //         r_type: R_X86_64_64,
    //         r_addend: msg_addend,
    //     },
    //     Relocation {
    //         // "Hello World!"
    //         r_offset: fmt_offset,
    //         symbol: Some(data_sym_id),
    //         r_type: R_X86_64_64,
    //         r_addend: fmt_addend,
    //     },
    //     Relocation {
    //         // extern printf
    //         r_offset: printf_offset,
    //         symbol: Some(printf_sym_id),
    //         r_type: R_X86_64_PC32,
    //         r_addend: EXTERN_SYMBOL_ADDEND,
    //     },
    // ];
    rela.data = SectionData::Relocation(extern_relocations);
    rela.sh_addralign = 8;

    let mut open_options = fs::OpenOptions::new();
    let file = open_options
        .write(true)
        .create(true)
        .truncate(true)
        .open(out_file)
        .unwrap();
    let mut buffer = StreamingBuffer::new(file);
    builder.write(&mut buffer).unwrap();
    buffer.result().unwrap()
}

const ARG_REGS: [Register; 6] = [
    Register::R9,
    Register::R8,
    Register::RCX,
    Register::RDX,
    Register::RSI,
    Register::RDI,
];

const SCRATCH_REGS: [Register; 9] = [
    Register::RAX,
    Register::RDI,
    Register::RSI,
    Register::RDX,
    Register::RCX,
    Register::R8,
    Register::R9,
    Register::R10,
    Register::R11,
];

fn call_function(
    ir: &IRGen,
    ident: &str,
    items: &[Value],
    assembler: &mut Assembler,
    externs: &mut Vec<(String, u64)>,
    temp_locations: &mut Vec<TempLoc>,
    var_locations: &mut Vec<i32>,
    regmap: &mut RegMap,
    stack_size: &mut i32,
) {
    let func = ir.parser.get_function(ident).unwrap();
    let classes: Vec<ArgumentClass> = func
        .arguments
        .iter()
        .map(|(_, arg)| classify(ir, arg))
        .collect();
    // Once arguments are classified, the registers get assigned (in left-to-right order) for passing as follows
    let mut arg_regs = Vec::from(ARG_REGS);
    let mut used_arg_regs = Vec::new();
    for (i, class) in classes.iter().enumerate() {
        let arg = &items[i];
        match class {
            // 1. If the class is MEMORY, pass the argument on the stack at an address respecting the
            // arguments alignment (which might be more than its natural alignement).
            ArgumentClass::MEMORY => todo!(),
            // 2. If the class is INTEGER, the next available register of the sequence %rdi, %rsi, %rdx,
            // %rcx, %r8 and %r9 is used
            ArgumentClass::INTEGER => match arg {
                Value::ImmediateInt(_) => todo!(),
                Value::ImmediateFloat(_) => todo!(),
                Value::ImmediateString(_) => todo!(),
                Value::StructLiteral(values) => todo!(),
                Value::ArrayAccess {
                    var_index,
                    array_index,
                } => todo!(),
                Value::Variable(var) => {
                    let loc = var_locations[*var];
                    let reg = arg_regs.pop().unwrap();
                    //TODO: put them on stack if no available regs
                    regmap.free_register(assembler, reg, &ARG_REGS, stack_size, temp_locations);
                    assembler.mov_reg_from_mem(reg, Register::RBP, loc);
                    used_arg_regs.push(reg);
                }
                Value::MemberAccess => todo!(),
                Value::Temporary(_) => todo!(),
                Value::Call(_, values) => todo!(),
            },
            // 3. If the class is SSE, the next available vector register is used, the registers are taken
            // in the order from %xmm0 to %xmm7.
            ArgumentClass::SSE => todo!(),
            // 4. If the class is SSEUP, the eightbyte is passed in the next available eightbyte chunk
            // of the last used vector register.
            ArgumentClass::SSEUP => todo!(),
            // 5. If the class is X87, X87UP or COMPLEX_X87, it is passed in memory.
            ArgumentClass::X87 => todo!(),
            ArgumentClass::X87UP => todo!(),
            ArgumentClass::COMPLEX_X87 => todo!(),
            ArgumentClass::NO_CLASS => unreachable!(),
            ArgumentClass::STRUCT(items) => todo!(),
        }
    }

    //TODO: Won't work if some arguments get passed on the stack. Can I fix this without 2 passes?
    for reg in SCRATCH_REGS {
        if !used_arg_regs.contains(&reg) {
            regmap.free_register(assembler, reg, &SCRATCH_REGS, stack_size, temp_locations);
        }
    }

    //TODO: this only works with extern functions
    externs.push((ident.to_string(), assembler.data.len() as u64 + 1));
    assembler.call();
}

// I'm 100% sure I'm doing this wrong
fn classify(ir: &IRGen, arg: &DataType) -> ArgumentClass {
    match arg {
        DataType::I64 => ArgumentClass::INTEGER,
        DataType::Char => ArgumentClass::INTEGER,
        DataType::Array { data_type, size } => todo!(),
        DataType::Pointer(data_type) => ArgumentClass::INTEGER,
        DataType::Boolean => ArgumentClass::INTEGER,
        DataType::Void => unreachable!(),
        DataType::F32 => ArgumentClass::SSE,
        DataType::Struct(ident) => {
            let size = sizeof(arg);
            let struct_dec = ir
                .parser
                .struct_declarations
                .iter()
                .find(|dec| dec.ident.ident == ident.ident)
                .unwrap();
            // If the size of an object is larger than eight eightbytes, or it contains unaligned fields, it has class MEMORY.
            if size == 0 {
                ArgumentClass::NO_CLASS
            } else if size > 8 * 8 {
                ArgumentClass::MEMORY
            } else {
                // If the size of the aggregate exceeds a single eightbyte, each is classified separately.
                let mut classes = Vec::new();
                let mut member_size;
                let mut members = struct_dec.members.iter();
                while let Some(member) = members.next() {
                    // Each field of an object is classified recursively so that always two fields are con-
                    // sidered. The resulting class is calculated according to the classes of the fields in the
                    // eightbyte
                    let mut left_class = classify(ir, &member.data_type);
                    member_size = sizeof(&member.data_type);
                    if member_size >= 8 {
                        classes.push(left_class);
                        continue;
                    }

                    let mut class_size = member_size;
                    while class_size < 8 {
                        left_class = if let Some(right) = members.next() {
                            class_size += sizeof(&right.data_type);
                            let right_class = classify(ir, &right.data_type);
                            if left_class == right_class {
                                left_class // (a) If both classes are equal, this is the resulting class.
                            } else if left_class == ArgumentClass::NO_CLASS {
                                right_class // (b) If one of the classes is NO_CLASS, the resulting class is the other class.
                            } else if right_class == ArgumentClass::NO_CLASS {
                                left_class
                            } else if left_class == ArgumentClass::MEMORY
                                || right_class == ArgumentClass::MEMORY
                            {
                                ArgumentClass::MEMORY // (c) If one of the classes is MEMORY, the result is the MEMORY class.
                            } else if left_class == ArgumentClass::INTEGER
                                || right_class == ArgumentClass::INTEGER
                            {
                                ArgumentClass::INTEGER // (d) If one of the classes is INTEGER, the result is the INTEGER.
                            } else if left_class == ArgumentClass::X87
                                || left_class == ArgumentClass::X87UP
                                || left_class == ArgumentClass::COMPLEX_X87
                                || right_class == ArgumentClass::X87
                                || right_class == ArgumentClass::X87UP
                                || right_class == ArgumentClass::COMPLEX_X87
                            {
                                ArgumentClass::MEMORY // (e) If one of the classes is X87, X87UP, COMPLEX_X87 class, MEMORY is used as class.
                            } else {
                                ArgumentClass::SSE // (f) Otherwise class SSE is used.
                            }
                        } else {
                            left_class
                        };
                    }
                    classes.push(left_class);
                }

                assert!((size + 7) / 8 == classes.len()); // idk anymore

                // 5. Then a post merger cleanup is done:
                for class in &classes {
                    // (a) If one of the classes is MEMORY, the whole argument is passed in memory.
                    if *class == ArgumentClass::MEMORY {
                        return ArgumentClass::MEMORY;
                    }
                }
                // (b) If X87UP is not preceded by X87, the whole argument is passed in memory.
                for a in classes.windows(2) {
                    if a[1] == ArgumentClass::X87UP && a[0] != ArgumentClass::X87 {
                        return ArgumentClass::MEMORY;
                    }
                }
                if classes.len() == 1 && *classes.first().unwrap() == ArgumentClass::X87UP {
                    return ArgumentClass::MEMORY;
                }
                // (c) If the size of the aggregate exceeds two eightbytes and the first eightbyte isn’t
                // SSE or any other eightbyte isn’t SSEUP, the whole argument is passed in mem-
                // ory.
                if size > 2 * 8 {
                    let mut iter = classes.iter();
                    if *iter.next().unwrap() != ArgumentClass::SSE
                        || iter.any(|c| *c != ArgumentClass::SSEUP)
                    {
                        return ArgumentClass::MEMORY;
                    }
                }
                // (d) If SSEUP is not preceded by SSE or SSEUP, it is converted to SSE.
                let mut iter = classes.iter_mut().rev().peekable();
                while let Some(a) = iter.next() {
                    let p = iter.peek();
                    if *a == ArgumentClass::SSEUP
                        && p.is_none_or(|p| {
                            **p != ArgumentClass::SSE && **p != ArgumentClass::SSEUP
                        })
                    {
                        *a = ArgumentClass::SSE;
                    }
                }
                ArgumentClass::STRUCT(classes)
            }
        }
    }
}

/// According to the System V Application Binary Interface 3.2.3 Parameter Passing
#[derive(Clone, PartialEq, Eq)]
enum ArgumentClass {
    INTEGER, // This class consists of integral types that fit into one of the general purpose registers.
    SSE,     // The class consists of types that fit into a vector register.
    SSEUP, // The class consists of types that fit into a vector register and can be passed and returned in the upper bytes of it.
    X87,
    X87UP,       // These classes consists of types that will be returned via the x87 FPU.
    COMPLEX_X87, // This class consists of types that will be returned via the x87 FPU.
    NO_CLASS, // This class is used as initializer in the algorithms. It will be used for padding and empty structures and unions.
    MEMORY, // This class consists of types that will be passed and returned in memory via the stack.
    STRUCT(Vec<ArgumentClass>), // Utility
}

fn sizeof(data_type: &DataType) -> usize {
    match data_type {
        DataType::I64 => 8,
        DataType::Char => todo!(),
        DataType::Array { data_type, size } => todo!(),
        DataType::Pointer(data_type) => todo!(),
        DataType::Boolean => todo!(),
        DataType::Void => todo!(),
        DataType::F32 => todo!(),
        DataType::Struct(identifier_spanned) => todo!(),
    }
}
