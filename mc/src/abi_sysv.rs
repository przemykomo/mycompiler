// Based on the GCC and C3 compiler implementation

#![allow(nonstandard_style)]
use std::ffi::CString;

use llvm_sys::{
    core::*,
    prelude::*,
    target::{LLVMABISizeOfType, LLVMOffsetOfElement},
};

use crate::{
    ast::FunctionPrototype,
    llvm::{LLVMGen, Scope, Variable},
    tokenizer::DataType::{self, *},
    typecheck::TypedExpr,
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
// byte_offset is an offset within the eightbyte word
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
            let words = (bytes + byte_offset + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

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
                return 0;
                // if pieces[0].class != X64Class::CLASS_SSE {
                //     return 0;
                // }
                //
                // //TODO remove this as I never use SSEUP
                // for piece in &pieces[1..words] {
                //     if piece.class != X64Class::CLASS_SSEUP {
                //         return 0;
                //     }
                // }
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
                // if i > 1
                //     && pieces[i].class == X64Class::CLASS_SSEUP
                //     && pieces[i - 1].class != X64Class::CLASS_SSE
                //     && pieces[i - 1].class != X64Class::CLASS_SSEUP
                // {
                //     /* The first one should never be X86_64_SSEUP_CLASS.  */
                //     // assert!(i != 0); // Redundant check since we have if i > 1
                //     pieces[i].class = X64Class::CLASS_SSE;
                // }
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
}

pub fn fun_prologue(
    llvmgen: &mut LLVMGen,
    function: &crate::typecheck::TypedFunc,
    fn_ref: *mut llvm_sys::LLVMValue,
    func_abi_info: &FuncABIInfo,
    scope: &mut Scope,
) {
    unsafe {
        let mut ir_param = 0;
        if let ABIArgInfo::Single(Piece {
            class: X64Class::CLASS_MEMORY,
            ..
        }) = func_abi_info.ret
        {
            ir_param = 1;
        }

        for (abi, (ident, data_type)) in
            func_abi_info.args.iter().zip(&function.prototype.arguments)
        {
            if let ABIArgInfo::Single(Piece {
                class: X64Class::CLASS_MEMORY,
                ..
            }) = abi
            {
                scope.vars.push(Variable {
                    ident: ident.clone(),
                    data_type: data_type.clone(),
                    ptr: LLVMGetParam(fn_ref, ir_param),
                });
                ir_param += 1;
                continue;
            }

            let ptr = LLVMBuildAlloca(
                llvmgen.builder,
                llvmgen.to_llvm_type(&data_type),
                CString::new(ident.ident.clone()).unwrap().as_ptr(),
            );

            scope.vars.push(Variable {
                ident: ident.clone(),
                data_type: data_type.clone(),
                ptr,
            });

            if data_type.is_scalar() {
                let param = LLVMGetParam(fn_ref, ir_param);
                LLVMBuildStore(llvmgen.builder, param, ptr);
                ir_param += 1;
                continue;
            }
            //Reconstruct the aggregate from ABI complaint IR args
            //There might be problems if the struct isn't aligned, but I think it always
            //is in my case
            match abi {
                ABIArgInfo::Single(piece) => match piece.class {
                    X64Class::CLASS_NO_CLASS => {}
                    X64Class::CLASS_MEMORY => unreachable!(),
                    X64Class::CLASS_INTEGER => {
                        let meaningful_bits_type =
                            LLVMIntTypeInContext(llvmgen.context, piece.meaningful_bits as u32);

                        let param = LLVMBuildTrunc(
                            llvmgen.builder,
                            LLVMGetParam(fn_ref, ir_param),
                            meaningful_bits_type,
                            c"int_bits".as_ptr(),
                        );
                        LLVMBuildStore(llvmgen.builder, param, ptr);
                        ir_param += 1;
                    }
                    X64Class::CLASS_SSE => {
                        // Simply use the IR param type
                        let param = LLVMGetParam(fn_ref, ir_param);
                        LLVMBuildStore(llvmgen.builder, param, ptr);
                        ir_param += 1;
                    }
                },
                ABIArgInfo::Aggregate(pieces, words) => {
                    // Index the alloca'd struct ptr as if it's an array of
                    // eightbytes and store the values piece by piece
                    let word_type = LLVMInt64TypeInContext(llvmgen.context);
                    for word in 0..*words {
                        let mut index =
                            LLVMConstInt(LLVMInt32TypeInContext(llvmgen.context), word as u64, 0);
                        let mut ptr = LLVMBuildGEP2(
                            llvmgen.builder,
                            word_type,
                            ptr,
                            &mut index,
                            1,
                            c"gep".as_ptr(),
                        );

                        match pieces[word].class {
                            X64Class::CLASS_NO_CLASS | X64Class::CLASS_MEMORY => {
                                unreachable!()
                            }
                            X64Class::CLASS_INTEGER => {
                                // IR param is always i64, but I might need to store less
                                // bits. TODO: Possibly just change the IR param type
                                // itllvmgen instead of truncating here.
                                let meaningful_bits_type = LLVMIntTypeInContext(
                                    llvmgen.context,
                                    pieces[word].meaningful_bits as u32,
                                );

                                ptr = LLVMBuildBitCast(
                                    llvmgen.builder,
                                    ptr,
                                    LLVMPointerType(meaningful_bits_type, 0),
                                    c"int_ptr".as_ptr(),
                                );

                                let param = LLVMBuildTrunc(
                                    llvmgen.builder,
                                    LLVMGetParam(fn_ref, ir_param),
                                    meaningful_bits_type,
                                    c"int_bits".as_ptr(),
                                );
                                LLVMBuildStore(llvmgen.builder, param, ptr);
                                ir_param += 1;
                            }
                            X64Class::CLASS_SSE => {
                                // Simply use the IR param type
                                let param = LLVMGetParam(fn_ref, ir_param);
                                LLVMBuildStore(llvmgen.builder, param, ptr);
                                ir_param += 1;
                            }
                        }
                    }
                }
            }
        }

        // LLVM docs recommend to put all alloca at the function beginning
        for var in &function.scope.vars[function.prototype.arguments.len()..] {
            let ptr = LLVMBuildAlloca(
                llvmgen.builder,
                llvmgen.to_llvm_type(&var.data_type),
                CString::new(var.ident.ident.clone()).unwrap().as_ptr(),
            );

            scope.vars.push(Variable {
                ident: var.ident.clone(),
                data_type: var.data_type.clone(),
                ptr,
            });
        }
    }
}

pub fn add_fn_decl(
    llvmgen: &mut LLVMGen,
    decl: &crate::ast::FunctionPrototype,
) -> (LLVMValueRef, FuncABIInfo) {
    unsafe {
        let func_abi_info = create_abi_info(llvmgen, decl);
        let mut params: Vec<LLVMTypeRef> = Vec::new();
        let mut attributes: Vec<(LLVMAttributeRef, u32)> = Vec::new();
        let retval = if decl.return_type.is_scalar() {
            llvmgen.to_llvm_type(&decl.return_type)
        } else {
            match func_abi_info.ret {
                ABIArgInfo::Single(piece) => match piece.class {
                    X64Class::CLASS_NO_CLASS => LLVMVoidTypeInContext(llvmgen.context),
                    X64Class::CLASS_MEMORY => {
                        //Add ptr as a first param
                        params.push(LLVMPointerTypeInContext(llvmgen.context, 0));
                        let attr = LLVMCreateTypeAttribute(
                            llvmgen.context,
                            LLVMGetEnumAttributeKindForName(c"sret".as_ptr(), 4),
                            llvmgen.to_llvm_type(&decl.return_type),
                        );
                        // Params are 1 indexed
                        attributes.push((attr, 1));
                        LLVMVoidTypeInContext(llvmgen.context)
                    }
                    X64Class::CLASS_INTEGER => LLVMInt64TypeInContext(llvmgen.context),
                    X64Class::CLASS_SSE => {
                        if piece.vec {
                            let f = LLVMFloatTypeInContext(llvmgen.context);
                            LLVMVectorType(f, 2)
                        } else if piece.meaningful_bits == 32 {
                            LLVMFloatTypeInContext(llvmgen.context)
                        } else {
                            LLVMDoubleTypeInContext(llvmgen.context)
                        }
                    }
                },
                ABIArgInfo::Aggregate(pieces, words) => {
                    assert!(words <= 2);

                    let mut members: Vec<LLVMTypeRef> = Vec::new();

                    for piece in &pieces[..words] {
                        match piece.class {
                            X64Class::CLASS_NO_CLASS | X64Class::CLASS_MEMORY => unreachable!(),
                            X64Class::CLASS_INTEGER => {
                                members.push(LLVMInt64TypeInContext(llvmgen.context));
                            }
                            X64Class::CLASS_SSE => {
                                let m = if piece.vec {
                                    let f = LLVMFloatTypeInContext(llvmgen.context);
                                    LLVMVectorType(f, 2)
                                } else if piece.meaningful_bits == 32 {
                                    LLVMFloatTypeInContext(llvmgen.context)
                                } else {
                                    LLVMDoubleTypeInContext(llvmgen.context)
                                };
                                members.push(m);
                            }
                        }
                    }
                    LLVMStructTypeInContext(
                        llvmgen.context,
                        members.as_mut_ptr(),
                        members.len() as u32,
                        0,
                    )
                }
            }
        };

        for (abi, (_, data_type)) in func_abi_info.args.iter().zip(&decl.arguments) {
            if data_type.is_scalar() {
                params.push(llvmgen.to_llvm_type(data_type));
                continue;
            }
            match abi {
                ABIArgInfo::Single(piece) => match piece.class {
                    X64Class::CLASS_NO_CLASS => {}
                    X64Class::CLASS_MEMORY => {
                        params.push(LLVMPointerTypeInContext(llvmgen.context, 0));
                        let attr = LLVMCreateTypeAttribute(
                            llvmgen.context,
                            LLVMGetEnumAttributeKindForName(c"byval".as_ptr(), 5),
                            llvmgen.to_llvm_type(data_type),
                        );
                        // Params are 1 indexed
                        attributes.push((attr, params.len() as u32));
                    }
                    X64Class::CLASS_INTEGER => params.push(LLVMInt64TypeInContext(llvmgen.context)),
                    X64Class::CLASS_SSE => {
                        let p = if piece.vec {
                            let f = LLVMFloatTypeInContext(llvmgen.context);
                            LLVMVectorType(f, 2)
                        } else if piece.meaningful_bits == 32 {
                            LLVMFloatTypeInContext(llvmgen.context)
                        } else {
                            LLVMDoubleTypeInContext(llvmgen.context)
                        };
                        params.push(p);
                    }
                },
                ABIArgInfo::Aggregate(pieces, words) => {
                    assert!(*words <= 2);
                    for piece in &pieces[..*words] {
                        match piece.class {
                            X64Class::CLASS_NO_CLASS | X64Class::CLASS_MEMORY => unreachable!(),
                            X64Class::CLASS_INTEGER => {
                                params.push(LLVMInt64TypeInContext(llvmgen.context));
                            }
                            X64Class::CLASS_SSE => {
                                let m = if piece.vec {
                                    let f = LLVMFloatTypeInContext(llvmgen.context);
                                    LLVMVectorType(f, 2)
                                } else if piece.meaningful_bits == 32 {
                                    LLVMFloatTypeInContext(llvmgen.context)
                                } else {
                                    LLVMDoubleTypeInContext(llvmgen.context)
                                };
                                params.push(m);
                            }
                        }
                    }
                }
            }
        }

        let fn_type = LLVMFunctionType(retval, params.as_mut_ptr(), params.len() as u32, 0);

        let name = CString::new(decl.ident.ident.as_bytes()).unwrap();
        let fn_val = LLVMAddFunction(llvmgen.module, name.as_ptr(), fn_type);

        for (attr, i) in attributes {
            LLVMAddAttributeAtIndex(fn_val, i, attr);
        }
        (fn_val, func_abi_info)
    }
}

pub fn build_ret(
    llvmgen: &mut LLVMGen,
    scope: &mut Scope,
    return_type: &DataType,
    fn_ref: *mut llvm_sys::LLVMValue,
    func_abi_info: &FuncABIInfo,
    expr: &Option<TypedExpr>,
) {
    unsafe {
        let Some(expr) = expr else {
            LLVMBuildRetVoid(llvmgen.builder);
            return;
        };
        let value = llvmgen.compile_expression(expr, scope);
        if return_type.is_scalar() {
            LLVMBuildRet(llvmgen.builder, value);
            return;
        }
        if let ABIArgInfo::Single(Piece {
            class: X64Class::CLASS_MEMORY,
            ..
        }) = func_abi_info.ret
        {
            LLVMBuildStore(llvmgen.builder, value, LLVMGetParam(fn_ref, 0));
            LLVMBuildRetVoid(llvmgen.builder);
            return;
        }
        let src_type = llvmgen.to_llvm_type(return_type);
        let src = LLVMBuildAlloca(llvmgen.builder, src_type, c"retval".as_ptr());
        let dst_type = LLVMGetReturnType(LLVMGlobalGetValueType(fn_ref));
        let dst = LLVMBuildAlloca(llvmgen.builder, dst_type, c"retval.coerce".as_ptr());
        LLVMBuildStore(llvmgen.builder, value, src);
        LLVMBuildMemCpy(
            llvmgen.builder,
            dst,
            LLVMGetAlignment(dst),
            src,
            LLVMGetAlignment(src),
            LLVMSizeOf(src_type),
        );
        LLVMBuildRet(
            llvmgen.builder,
            LLVMBuildLoad2(llvmgen.builder, dst_type, dst, c"coerce".as_ptr()),
        );

        // Clang casts it by doing memcpy so
    }
}

pub fn call(
    llvmgen: &LLVMGen,
    scope: &Scope,
    func: &String,
    args_exprs: &Vec<TypedExpr>,
) -> *mut llvm_sys::LLVMValue {
    unsafe {
        let (decl, _) = llvmgen.typechecker.parser.get_function(func).unwrap();
        let func_abi_info = create_abi_info(llvmgen, decl);
        let func_c_str = CString::new(func.clone()).unwrap();
        let fn_ref = LLVMGetNamedFunction(llvmgen.module, func_c_str.as_ptr());
        let fn_type = LLVMGlobalGetValueType(fn_ref);

        let mut args: Vec<LLVMValueRef> = Vec::new();
        let ret_ptr = if decl.return_type.is_scalar() {
            None
        } else {
            if let ABIArgInfo::Single(Piece {
                class: X64Class::CLASS_MEMORY,
                ..
            }) = func_abi_info.ret
            {
                let dst = LLVMBuildAlloca(
                    llvmgen.builder,
                    llvmgen.to_llvm_type(&decl.return_type),
                    c"ret".as_ptr(),
                );
                args.push(dst);
                Some(dst)
            } else {
                None
            }
        };

        for ((abi, (_, data_type)), expr) in func_abi_info
            .args
            .iter()
            .zip(&decl.arguments)
            .zip(args_exprs)
        {
            let val = llvmgen.compile_expression(expr, scope);
            if data_type.is_scalar() {
                args.push(val);
                continue;
            }

            match abi {
                ABIArgInfo::Single(piece) => match piece.class {
                    X64Class::CLASS_NO_CLASS => {}
                    X64Class::CLASS_MEMORY => {
                        let alloca = LLVMBuildAlloca(
                            llvmgen.builder,
                            llvmgen.to_llvm_type(data_type),
                            c"arg".as_ptr(),
                        );
                        LLVMBuildStore(llvmgen.builder, val, alloca);
                        args.push(alloca);
                    }
                    X64Class::CLASS_INTEGER => {
                        let ext = LLVMBuildZExtOrBitCast(
                            llvmgen.builder,
                            val,
                            LLVMInt64TypeInContext(llvmgen.context),
                            c"ext".as_ptr(),
                        );
                        args.push(ext);
                    }
                    X64Class::CLASS_SSE => {
                        args.push(val);
                    }
                },
                ABIArgInfo::Aggregate(pieces, words) => {
                    let alloca = LLVMBuildAlloca(
                        llvmgen.builder,
                        llvmgen.to_llvm_type(data_type),
                        c"arg".as_ptr(),
                    );
                    LLVMBuildStore(llvmgen.builder, val, alloca);

                    // Index the alloca'd struct ptr as if it's an array of
                    // eightbytes and load the values piece by piece
                    let word_type = LLVMInt64TypeInContext(llvmgen.context);
                    for word in 0..*words {
                        let mut index =
                            LLVMConstInt(LLVMInt32TypeInContext(llvmgen.context), word as u64, 0);
                        let ptr = LLVMBuildGEP2(
                            llvmgen.builder,
                            word_type,
                            alloca,
                            &mut index,
                            1,
                            c"gep".as_ptr(),
                        );
                        match pieces[word].class {
                            X64Class::CLASS_NO_CLASS | X64Class::CLASS_MEMORY => {
                                unreachable!()
                            }
                            X64Class::CLASS_INTEGER => {
                                // IR param is always i64, but I might need to store less
                                // bits. TODO: Possibly just change the IR param type
                                // itllvmgen instead of extending here.
                                let meaningful_bits_type = LLVMIntTypeInContext(
                                    llvmgen.context,
                                    pieces[word].meaningful_bits as u32,
                                );
                                let val = LLVMBuildLoad2(
                                    llvmgen.builder,
                                    meaningful_bits_type,
                                    ptr,
                                    c"int_val".as_ptr(),
                                );
                                let ext = LLVMBuildZExtOrBitCast(
                                    llvmgen.builder,
                                    val,
                                    LLVMInt64TypeInContext(llvmgen.context),
                                    c"ext".as_ptr(),
                                );
                                args.push(ext);
                            }
                            X64Class::CLASS_SSE => {
                                let p = LLVMGetParam(fn_ref, args.len() as u32);
                                let ty = LLVMTypeOf(p);
                                let val =
                                    LLVMBuildLoad2(llvmgen.builder, ty, ptr, c"float_val".as_ptr());
                                args.push(val);
                            }
                        }
                    }
                }
            }
        }

        if decl.return_type.is_scalar() {
            LLVMBuildCall2(
                llvmgen.builder,
                fn_type,
                fn_ref,
                args.as_mut_ptr(),
                args.len() as u32,
                c"".as_ptr(),
            )
        } else if let Some(dst) = ret_ptr {
            LLVMBuildCall2(
                llvmgen.builder,
                fn_type,
                fn_ref,
                args.as_mut_ptr(),
                args.len() as u32,
                c"".as_ptr(),
            );
            LLVMBuildLoad2(
                llvmgen.builder,
                llvmgen.to_llvm_type(&decl.return_type),
                dst,
                c"call_retval".as_ptr(),
            )
        } else {
            let call_val = LLVMBuildCall2(
                llvmgen.builder,
                fn_type,
                fn_ref,
                args.as_mut_ptr(),
                args.len() as u32,
                c"".as_ptr(),
            );

            // Clang casts it by doing memcpy so
            let src_type = LLVMGetReturnType(LLVMGlobalGetValueType(fn_ref));
            let src = LLVMBuildAlloca(llvmgen.builder, src_type, c"callval.coerce".as_ptr());
            let dst_type = llvmgen.to_llvm_type(&decl.return_type);
            let dst = LLVMBuildAlloca(llvmgen.builder, dst_type, c"callval".as_ptr());

            LLVMBuildStore(llvmgen.builder, call_val, src);
            LLVMBuildMemCpy(
                llvmgen.builder,
                dst,
                LLVMGetAlignment(dst),
                src,
                LLVMGetAlignment(src),
                LLVMSizeOf(dst_type),
            );

            LLVMBuildLoad2(
                llvmgen.builder,
                llvmgen.to_llvm_type(&decl.return_type),
                dst,
                c"call_retval".as_ptr(),
            )
        }
    }
}
