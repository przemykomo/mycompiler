use std::{
    ffi::{CStr, CString},
    fs,
    mem::transmute,
    str::FromStr,
};

use object::{
    build::{
        elf::{Builder, Relocation, SectionData, Symbol, SymbolId},
        ByteString, Bytes,
    },
    elf::{
        EM_X86_64, ET_REL, R_X86_64_64, R_X86_64_PC32, SHF_ALLOC, SHF_EXECINSTR, SHF_WRITE,
        SHT_PROGBITS, SHT_RELA, SHT_STRTAB, SHT_SYMTAB, STB_GLOBAL, STB_LOCAL, STT_NOTYPE,
        STT_SECTION,
    },
    write::StreamingBuffer,
};

use crate::{
    ir::{IRGen, Instruction, Value},
    tokenizer::DataType,
};

const EXTERN_SYMBOL_ADDEND: i64 = -0x04; // All extern symbol relocations generated using NASM have
                                         // this addend, no idea why.
#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum Register {
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

struct Assembler {
    pub data: Vec<u8>,
}

impl Assembler {
    pub fn mov(&mut self, reg: Register, val: i64) {
        if reg < Register::R8 {
            self.data.push(0x48); // REX.W - 64bit version of mov
            self.data.push(0xb8 + reg as u8);
        } else {
            self.data.push(0x49); // REX.WB - 64bit version of mov
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

    pub fn stackframe_setup(&mut self) {
        self.push(Register::RBP);
        // self.mov_reg(Register::RBP, Register::RSP);
        // sub
    }

    pub fn stackframe_cleanup(&mut self) {
        // self.mov_reg(Register::RSP, Register::RBP);
        self.pop(Register::RBP);
    }

    pub fn push(&mut self, reg: Register) {
        if reg < Register::R8 {
            self.data.push(0x50 + reg as u8);
        } else {
            self.data.push(0x41);
            self.data.push(0x50 + reg as u8 - 8);
        }
    }

    pub fn pop(&mut self, reg: Register) {
        if reg < Register::R8 {
            self.data.push(0x58 + reg as u8);
        } else {
            self.data.push(0x41);
            self.data.push(0x58 + reg as u8 - 8);
        }
    }
}

pub fn compile_elf_object(ir: &IRGen, out_file: &str) {
    let mut externs: Vec<(String, u64)> = Vec::new();
    let mut symbols_to_export: Vec<(String, u64)> = Vec::new();

    let mut assembler = Assembler { data: Vec::new() };

    for function in &ir.functions {
        symbols_to_export.push((function.ident.clone(), assembler.data.len() as u64));
        assembler.stackframe_setup();
        for instruction in &function.scope.instructions {
            match instruction {
                Instruction::ArithmeticInt {
                    op,
                    lhs,
                    rhs,
                    result,
                } => todo!(),
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
                Instruction::LoadInt(_, _) => todo!(),
                Instruction::LoadFloat(_, _) => todo!(),
                Instruction::Call(ident, items) => {
                    assert!(items.is_empty());
                    //TODO: this only works with extern functions
                    externs.push((ident.clone(), assembler.data.len() as u64 + 1));
                    assembler.call();
                }
                Instruction::Assign { lhs, rhs } => todo!(),
                Instruction::Return(value) => match value {
                    Value::ImmediateInt(val) => {
                        assembler.stackframe_cleanup();
                        assembler.mov(Register::RAX, *val);
                        assembler.ret();
                    }
                    Value::ImmediateFloat(_) => todo!(),
                    Value::ImmediateString(_) => todo!(),
                    Value::StructLiteral(values) => todo!(),
                    Value::ArrayAccess { var, index } => todo!(),
                    Value::MemberAccess => todo!(),
                    Value::Temporary(_) => todo!(),
                    Value::Call(_, values) => todo!(),
                },
            }
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
