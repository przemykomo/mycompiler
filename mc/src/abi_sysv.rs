// Based on the C3 compiler implementation

#![allow(nonstandard_style)]
use inkwell::llvm_sys::target::{LLVMABISizeOfType, LLVMOffsetOfElement};

use crate::{
    ast::FunctionPrototype,
    llvm::LLVMGen,
    tokenizer::DataType::{self, *},
};

pub fn create_abi_info(llvmgen: &LLVMGen, fun: &FunctionPrototype) -> FuncABIInfo {
    let mut available_registers = Registers {
        int_registers: 6,
        ssa_registers: 8,
    };
    let ret_abi_info = classify_return(llvmgen, &fun.return_type);
    if let ABIArgInfo::Single(Piece {
        class: X64Class::CLASS_MEMORY,
        ..
    }) = ret_abi_info
    {
        // If the type has class MEMORY, then the caller provides space for the return value
        // and passes the address of this storage in %rdi as if it were the first argument to the
        // function.
        available_registers.int_registers -= 1;
    }

    let args: Vec<ABIArgInfo> = fun
        .arguments
        .iter()
        .map(|(_, arg)| classify_parameter(llvmgen, arg, &mut available_registers))
        .collect();

    FuncABIInfo {
        ret: ret_abi_info,
        args,
    }
}

fn classify_parameter(
    llvmgen: &LLVMGen,
    data_type: &DataType,
    available_registers: &mut Registers,
) -> ABIArgInfo {
    let mut pieces = [Piece::new(X64Class::CLASS_NO_CLASS); 8];

    let (ret, classes_to_assign) = match classify(llvmgen, data_type, &mut pieces, 0) {
        0 => (
            ABIArgInfo::Single(Piece::new(X64Class::CLASS_MEMORY)),
            &[] as &[Piece],
        ),
        1 => (ABIArgInfo::Single(pieces[0]), &pieces[0..0]),
        words => (ABIArgInfo::Aggregate(pieces, words), &pieces[0..words]),
    };

    let mut needed_regs = Registers {
        int_registers: 0,
        ssa_registers: 0,
    };

    for piece in classes_to_assign {
        match piece.class {
            X64Class::CLASS_INTEGER => {
                needed_regs.int_registers += 1;
            }
            X64Class::CLASS_SSE => {
                needed_regs.ssa_registers += 1;
            }
            _ => {}
        }
    }

    // If there are no registers available for any eightbyte of an argument, the whole argument is passed on the stack.
    if available_registers.int_registers < needed_regs.int_registers
        || available_registers.ssa_registers < needed_regs.ssa_registers
    {
        return ABIArgInfo::Single(Piece::new(X64Class::CLASS_MEMORY));
    }

    available_registers.int_registers -= needed_regs.int_registers;
    available_registers.ssa_registers -= needed_regs.ssa_registers;

    ret
}

fn classify_return(llvmgen: &LLVMGen, data_type: &DataType) -> ABIArgInfo {
    let mut pieces = [Piece::new(X64Class::CLASS_NO_CLASS); 8];

    match classify(llvmgen, data_type, &mut pieces, 0) {
        0 => ABIArgInfo::Single(Piece::new(X64Class::CLASS_MEMORY)),
        1 => ABIArgInfo::Single(pieces[0]),
        words => ABIArgInfo::Aggregate(pieces, words),
    }
}

// Returns the number of words, or 0 if the parameter should be passed in memory.
// As a special case for zero sized containers, classes[0] will be NO_CLASS and 1 is returned.
fn classify(
    llvmgen: &LLVMGen,
    data_type: &DataType,
    pieces: &mut [Piece; 8],
    byte_offset: usize,
) -> usize {
    match data_type {
        UnsizedInt | UnsizedFloat => unreachable!(),
        Void => 1,
        I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64 | Pointer(_) | Char | Boolean => {
            let bits_size = match data_type {
                Boolean | Char | I8 | U8 => 8,
                I16 | U16 => 16,
                I32 | U32 => 32,
                I64 | U64 | Pointer(_) => 64,
                _ => unreachable!(),
            };
            pieces[0] = Piece {
                class: X64Class::CLASS_INTEGER,
                meaningful_bits: byte_offset as u8 * 8 + bits_size,
                vec: false,
            };
            1
        }
        F32 => {
            pieces[0] = Piece {
                class: X64Class::CLASS_SSE,
                meaningful_bits: byte_offset as u8 * 8 + 32,
                // Only checked if this word ends up merged as SSE,
                // and 2 floats are then passed as a vec in LLVM IR
                vec: byte_offset == 4,
            };
            1
        }
        F64 => {
            pieces[0] = Piece {
                class: X64Class::CLASS_SSE,
                meaningful_bits: byte_offset as u8 * 8 + 64,
                vec: false,
            };
            1
        }
        Array {
            data_type: _,
            size: _,
        } => todo!(),
        Struct(ident) => unsafe {
            // https://github.com/gcc-mirror/gcc/blob/HEAD@%7B2021-05-05%7D/gcc/config/i386/i386.c#L2080
            let llvm_type = llvmgen.to_llvm_type(data_type);
            let bytes = LLVMABISizeOfType(llvmgen.data_layout, llvm_type) as usize;
            if bytes > 64 {
                return 0;
            }

            assert!(bytes > 0, "TODO: zero-sized structs");

            let mut subpieces = [Piece::new(X64Class::CLASS_NO_CLASS); 8];

            const UNITS_PER_WORD: usize = 8;
            //TODO: Why add byte offset here? Are we clearing the previously set classes for some
            //reason?
            let words = (bytes + byte_offset + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

            // They get initialized to NO CLASS anywawy
            // for i in 0..words {
            //     classes[i as usize] = X64Class::CLASS_NO_CLASS;
            // }

            let s = llvmgen
                .typechecker
                .parser
                .struct_declarations
                .iter()
                .find(|s| s.ident.ident == ident.ident)
                .unwrap();

            for (i, member) in s.members.iter().enumerate() {
                let offset = LLVMOffsetOfElement(llvmgen.data_layout, llvm_type, i as u32) as usize;

                let byte_offset = byte_offset + offset;
                // We get the remainder here since we consider the offset inside the word
                let num = classify(llvmgen, &member.data_type, &mut subpieces, byte_offset % 8);

                if num == 0 {
                    return 0;
                }

                let pos = byte_offset / 8;
                let mut i = 0;
                while i < num && (i + pos) < words {
                    let subpiece = &subpieces[i];
                    let piece = &mut pieces[i + pos];
                    piece.vec = (piece.vec || subpiece.vec)
                        && piece.class == X64Class::CLASS_SSE
                        && subpiece.class == X64Class::CLASS_SSE;
                    piece.class = merge_classes(subpiece.class, piece.class);
                    piece.meaningful_bits = piece.meaningful_bits.max(subpiece.meaningful_bits);
                    i += 1;
                }
            }

            if words > 2 {
                /* When size > 16 bytes, if the first one isn't
                X86_64_SSE_CLASS or any other ones aren't
                X86_64_SSEUP_CLASS, everything should be passed in
                memory.  */

                if pieces[0].class != X64Class::CLASS_SSE {
                    return 0;
                }

                //TODO remove this as I never use SSEUP
                for piece in &pieces[1..words] {
                    if piece.class != X64Class::CLASS_SSEUP {
                        return 0;
                    }
                }
            }

            // Final merger cleanup
            for i in 0..words {
                /* If one class is MEMORY, everything should be passed in
                memory.  */
                if pieces[i].class == X64Class::CLASS_MEMORY {
                    return 0;
                }

                /* The X86_64_SSEUP_CLASS should be always preceded by
                X86_64_SSE_CLASS or X86_64_SSEUP_CLASS.  */
                if i > 1
                    && pieces[i].class == X64Class::CLASS_SSEUP
                    && pieces[i - 1].class != X64Class::CLASS_SSE
                    && pieces[i - 1].class != X64Class::CLASS_SSEUP
                {
                    /* The first one should never be X86_64_SSEUP_CLASS.  */
                    // assert!(i != 0); // Redundant check since we have if i > 1
                    pieces[i].class = X64Class::CLASS_SSE;
                }
            }

            words
        },
    }
}

fn merge_classes(class1: X64Class, class2: X64Class) -> X64Class {
    /* Rule #1: If both classes are equal, this is the resulting class.  */
    if class1 == class2 {
        return class1;
    }

    /* Rule #2: If one of the classes is NO_CLASS, the resulting class is
    the other class.  */
    if class1 == X64Class::CLASS_NO_CLASS {
        return class2;
    }
    if class2 == X64Class::CLASS_NO_CLASS {
        return class1;
    }

    /* Rule #3: If one of the classes is MEMORY, the result is MEMORY.  */
    if class1 == X64Class::CLASS_MEMORY || class2 == X64Class::CLASS_MEMORY {
        return X64Class::CLASS_MEMORY;
    }

    /* Rule #4: If one of the classes is INTEGER, the result is INTEGER.  */
    if class1 == X64Class::CLASS_INTEGER || class2 == X64Class::CLASS_INTEGER {
        return X64Class::CLASS_INTEGER;
    }

    // /* Rule #5: If one of the classes is X87, X87UP, or COMPLEX_X87 class,
    //    MEMORY is used.  */
    // if (class1 == X86_64_X87_CLASS
    //     || class1 == X86_64_X87UP_CLASS
    //     || class1 == X86_64_COMPLEX_X87_CLASS
    //     || class2 == X86_64_X87_CLASS
    //     || class2 == X86_64_X87UP_CLASS
    //     || class2 == X86_64_COMPLEX_X87_CLASS)
    //   return X86_64_MEMORY_CLASS;

    /* Rule #6: Otherwise class SSE is used.  */
    X64Class::CLASS_SSE
}

#[derive(Debug)]
pub struct FuncABIInfo {
    pub ret: ABIArgInfo,
    pub args: Vec<ABIArgInfo>,
}

pub struct AbiType {}

struct Registers {
    int_registers: i32,
    ssa_registers: i32,
}

#[derive(Debug)]
pub enum ABIArgInfo {
    Single(Piece),
    Aggregate([Piece; 8], usize),
}

#[derive(PartialEq, Eq)]
pub enum ABIKind {
    ABI_ARG_IGNORE,
    ABI_ARG_DIRECT,
    ABI_ARG_DIRECT_PAIR,
    ABI_ARG_DIRECT_COERCE,
    ABI_ARG_DIRECT_COERCE_INT,
    ABI_ARG_DIRECT_SPLIT_STRUCT_I32,
    ABI_ARG_EXPAND_COERCE,
    ABI_ARG_INDIRECT,
    ABI_ARG_EXPAND,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct Piece {
    pub class: X64Class,
    pub meaningful_bits: u8,
    pub vec: bool,
}

impl Piece {
    pub fn new(class: X64Class) -> Piece {
        Piece {
            class,
            meaningful_bits: 0,
            vec: false,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum X64Class {
    CLASS_NO_CLASS,
    CLASS_MEMORY,
    CLASS_INTEGER,
    CLASS_SSE,
    CLASS_SSEUP,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum ABIPiece {
    Integer { bits: usize },
    Float { bits: usize },
    Vector { element_bits: usize, count: usize },
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum LLVMCoerceType {
    i8,
    i16,
    i24,
    i32,
    i40,
    i48,
    i56,
    i64,
    Float,
    Floatx2,
    Double,
}
