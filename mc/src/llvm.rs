use std::ffi::CString;
use std::ptr;

use inkwell::llvm_sys::analysis::LLVMVerifierFailureAction;
use inkwell::llvm_sys::analysis::LLVMVerifyFunction;
use inkwell::llvm_sys::core::*;
use inkwell::llvm_sys::prelude::*;
use inkwell::llvm_sys::target::*;
use inkwell::llvm_sys::target_machine::LLVMCodeGenFileType::LLVMObjectFile;
use inkwell::llvm_sys::target_machine::LLVMCodeGenOptLevel::LLVMCodeGenLevelNone;
use inkwell::llvm_sys::target_machine::*;

use crate::abi_sysv::ABIArgInfo;
use crate::abi_sysv::FuncABIInfo;
use crate::abi_sysv::Piece;
use crate::abi_sysv::X64Class;
use crate::abi_sysv::create_abi_info;
use crate::typecheck::BlockReturnActuality::AlwaysReturns;
use crate::typecheck::Place;
use crate::typecheck::PlaceKind;
use crate::{
    ast::{ArithmeticOp, IdentifierSpanned},
    tokenizer::{DataType, Error},
    typecheck::{TypeChecker, TypedBlock, TypedExpr, TypedExprKind, TypedStatement, TypedStmtKind},
};

#[derive(Debug)]
pub struct Variable {
    ident: IdentifierSpanned,
    pub data_type: DataType,
    pub ptr: LLVMValueRef,
}

#[derive(Debug)]
pub struct Scope {
    pub vars: Vec<Variable>,
}

impl Scope {
    fn find(&self, ident: &str) -> &Variable {
        self.vars.iter().find(|v| v.ident.ident == ident).unwrap()
    }
}

pub struct LLVMGen<'a> {
    pub typechecker: &'a TypeChecker<'a>,
    pub errors: Vec<Error>,
    context: LLVMContextRef,
    builder: LLVMBuilderRef,
    pub module: LLVMModuleRef,
    id_count: usize,
    pub data_layout: LLVMTargetDataRef,
    machine: LLVMTargetMachineRef,
}

impl<'a> LLVMGen<'a> {
    pub fn new(typechecker: &'a TypeChecker<'a>, context: LLVMContextRef) -> LLVMGen<'a> {
        unsafe {
            let builder = LLVMCreateBuilderInContext(context);
            let module = LLVMModuleCreateWithNameInContext(c"tmp".as_ptr(), context);

            LLVMInitializeX86Target();
            LLVMInitializeX86TargetInfo();
            LLVMInitializeX86AsmPrinter();
            LLVMInitializeX86AsmParser();
            LLVMInitializeX86Disassembler();
            LLVMInitializeX86TargetMC();

            let mut target = ptr::null_mut();
            let mut err_string = ::core::ptr::null_mut();

            let triple = c"x86_64-pc-linux-gnu".as_ptr();
            let ret = LLVMGetTargetFromTriple(triple, &mut target, &mut err_string);
            if ret == 1 {
                panic!("get target");
            }

            let opt = LLVMCreateTargetMachineOptions();
            LLVMTargetMachineOptionsSetCPU(opt, c"x86-64".as_ptr());
            LLVMTargetMachineOptionsSetABI(opt, c"sysv".as_ptr());
            LLVMTargetMachineOptionsSetCodeGenOptLevel(opt, LLVMCodeGenLevelNone);

            let machine = LLVMCreateTargetMachineWithOptions(target, triple, opt);

            let data_layout = LLVMCreateTargetDataLayout(machine);
            LLVMSetDataLayout(module, LLVMCopyStringRepOfTargetData(data_layout));

            LLVMSetTarget(module, triple);

            Self {
                typechecker,
                errors: Vec::new(),
                context,
                builder,
                module,
                id_count: 0,
                data_layout,
                machine,
            }
        }
    }

    fn alloc_id(&mut self) -> usize {
        let l = self.id_count;
        self.id_count += 1;
        l
    }

    pub fn generate_ir(&mut self) {
        for decl in &self.typechecker.parser.function_declarations {
            self.add_fn_decl(decl);
        }

        for function in &self.typechecker.typed_functions {
            let (fn_ref, func_abi_info) = self.add_fn_decl(&function.prototype);
            unsafe {
                let entry_block =
                    LLVMAppendBasicBlockInContext(self.context, fn_ref, c"entry".as_ptr());
                LLVMPositionBuilderAtEnd(self.builder, entry_block);

                let mut scope = Scope { vars: Vec::new() };

                let mut ir_param = 0;

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
                        self.builder,
                        self.to_llvm_type(&data_type),
                        CString::new(ident.ident.clone()).unwrap().as_ptr(),
                    );

                    scope.vars.push(Variable {
                        ident: ident.clone(),
                        data_type: data_type.clone(),
                        ptr,
                    });

                    if data_type.is_scalar() {
                        let param = LLVMGetParam(fn_ref, ir_param);
                        LLVMBuildStore(self.builder, param, ptr);
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
                                let meaningful_bits_type = LLVMIntTypeInContext(
                                    self.context,
                                    piece.meaningful_bits as u32,
                                );

                                let param = LLVMBuildTrunc(
                                    self.builder,
                                    LLVMGetParam(fn_ref, ir_param),
                                    meaningful_bits_type,
                                    c"int_bits".as_ptr(),
                                );
                                LLVMBuildStore(self.builder, param, ptr);
                                ir_param += 1;
                            }
                            X64Class::CLASS_SSE => {
                                // Simply use the IR param type
                                let param = LLVMGetParam(fn_ref, ir_param);
                                LLVMBuildStore(self.builder, param, ptr);
                                ir_param += 1;
                            }
                            X64Class::CLASS_SSEUP => todo!(),
                        },
                        ABIArgInfo::Aggregate(pieces, words) => {
                            // Index the alloca'd struct ptr as if it's an array of
                            // eightbytes and store the values piece by piece
                            let word_type = LLVMInt64TypeInContext(self.context);
                            for word in 0..*words {
                                let mut index = LLVMConstInt(
                                    LLVMInt32TypeInContext(self.context),
                                    word as u64,
                                    0,
                                );
                                let mut ptr = LLVMBuildGEP2(
                                    self.builder,
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
                                        // itself instead of truncating here.
                                        let meaningful_bits_type = LLVMIntTypeInContext(
                                            self.context,
                                            pieces[word].meaningful_bits as u32,
                                        );

                                        ptr = LLVMBuildBitCast(
                                            self.builder,
                                            ptr,
                                            LLVMPointerType(meaningful_bits_type, 0),
                                            c"int_ptr".as_ptr(),
                                        );

                                        let param = LLVMBuildTrunc(
                                            self.builder,
                                            LLVMGetParam(fn_ref, ir_param),
                                            meaningful_bits_type,
                                            c"int_bits".as_ptr(),
                                        );
                                        LLVMBuildStore(self.builder, param, ptr);
                                        ir_param += 1;
                                    }
                                    X64Class::CLASS_SSE => {
                                        // Simply use the IR param type
                                        let param = LLVMGetParam(fn_ref, ir_param);
                                        LLVMBuildStore(self.builder, param, ptr);
                                        ir_param += 1;
                                    }
                                    X64Class::CLASS_SSEUP => todo!(),
                                }
                            }
                        }
                    }
                }

                // LLVM docs recommend to put all alloca at the function beginning
                for var in &function.scope.vars[function.prototype.arguments.len()..] {
                    let ptr = LLVMBuildAlloca(
                        self.builder,
                        self.to_llvm_type(&var.data_type),
                        CString::new(var.ident.ident.clone()).unwrap().as_ptr(),
                    );

                    scope.vars.push(Variable {
                        ident: var.ident.clone(),
                        data_type: var.data_type.clone(),
                        ptr,
                    });
                }

                for statement in &function.block.statements {
                    self.compile_statement(
                        statement,
                        &mut scope,
                        &function.prototype.return_type,
                        fn_ref,
                    );
                }

                if LLVMVerifyFunction(fn_ref, LLVMVerifierFailureAction::LLVMPrintMessageAction)
                    == 1
                {
                    LLVMDeleteFunction(fn_ref);
                    panic!("fn verify");
                }
            }
        }
    }

    pub fn build(&self, out_file: String) {
        unsafe {
            let mut err_string = ::core::ptr::null_mut();
            let ret = LLVMTargetMachineEmitToFile(
                self.machine,
                self.module,
                CString::new(out_file).unwrap().as_ptr(),
                LLVMObjectFile,
                &mut err_string,
            );

            if ret == 1 {
                panic!("emit object file");
                // panic!("{}", LLVMString::new(err_string));
            }
        }
    }

    fn add_fn_decl(&mut self, decl: &crate::ast::FunctionPrototype) -> (LLVMValueRef, FuncABIInfo) {
        unsafe {
            let func_abi_info = create_abi_info(self, decl);
            let mut params: Vec<LLVMTypeRef> = Vec::new();
            let mut attributes: Vec<(LLVMAttributeRef, u32)> = Vec::new();
            let retval = if decl.return_type.is_scalar() {
                self.to_llvm_type(&decl.return_type)
            } else {
                match func_abi_info.ret {
                    ABIArgInfo::Single(piece) => match piece.class {
                        X64Class::CLASS_NO_CLASS => LLVMVoidTypeInContext(self.context),
                        X64Class::CLASS_MEMORY => {
                            //Add ptr as a first param
                            params.push(LLVMPointerTypeInContext(self.context, 0));
                            LLVMVoidTypeInContext(self.context)
                        }
                        X64Class::CLASS_INTEGER => LLVMInt64TypeInContext(self.context),
                        X64Class::CLASS_SSE => {
                            if piece.vec {
                                let f = LLVMFloatTypeInContext(self.context);
                                LLVMVectorType(f, 2)
                            } else if piece.meaningful_bits == 32 {
                                LLVMFloatTypeInContext(self.context)
                            } else {
                                LLVMDoubleTypeInContext(self.context)
                            }
                        }
                        X64Class::CLASS_SSEUP => todo!(),
                    },
                    ABIArgInfo::Aggregate(pieces, words) => {
                        assert!(words <= 2);

                        let mut members: Vec<LLVMTypeRef> = Vec::new();

                        for piece in &pieces[..words] {
                            match piece.class {
                                X64Class::CLASS_NO_CLASS | X64Class::CLASS_MEMORY => unreachable!(),
                                X64Class::CLASS_INTEGER => {
                                    members.push(LLVMInt64TypeInContext(self.context));
                                }
                                X64Class::CLASS_SSE => {
                                    let m = if piece.vec {
                                        let f = LLVMFloatTypeInContext(self.context);
                                        LLVMVectorType(f, 2)
                                    } else if piece.meaningful_bits == 32 {
                                        LLVMFloatTypeInContext(self.context)
                                    } else {
                                        LLVMDoubleTypeInContext(self.context)
                                    };
                                    members.push(m);
                                }
                                X64Class::CLASS_SSEUP => todo!(),
                            }
                        }
                        LLVMStructTypeInContext(
                            self.context,
                            members.as_mut_ptr(),
                            members.len() as u32,
                            0,
                        )
                    }
                }
            };

            for (abi, (_, data_type)) in func_abi_info.args.iter().zip(&decl.arguments) {
                if data_type.is_scalar() {
                    params.push(self.to_llvm_type(data_type));
                    continue;
                }
                match abi {
                    ABIArgInfo::Single(piece) => match piece.class {
                        X64Class::CLASS_NO_CLASS => {}
                        X64Class::CLASS_MEMORY => {
                            params.push(LLVMPointerTypeInContext(self.context, 0));
                            let attr = LLVMCreateTypeAttribute(
                                self.context,
                                LLVMGetEnumAttributeKindForName(c"byval".as_ptr(), 5),
                                self.to_llvm_type(data_type),
                            );
                            // Params are 1 indexed
                            attributes.push((attr, params.len() as u32));
                        }
                        X64Class::CLASS_INTEGER => {
                            params.push(LLVMInt64TypeInContext(self.context))
                        }
                        X64Class::CLASS_SSE => {
                            let p = if piece.vec {
                                let f = LLVMFloatTypeInContext(self.context);
                                LLVMVectorType(f, 2)
                            } else if piece.meaningful_bits == 32 {
                                LLVMFloatTypeInContext(self.context)
                            } else {
                                LLVMDoubleTypeInContext(self.context)
                            };
                            params.push(p);
                        }
                        X64Class::CLASS_SSEUP => unreachable!(),
                    },
                    ABIArgInfo::Aggregate(pieces, words) => {
                        assert!(*words <= 2);
                        for piece in &pieces[..*words] {
                            match piece.class {
                                X64Class::CLASS_NO_CLASS | X64Class::CLASS_MEMORY => unreachable!(),
                                X64Class::CLASS_INTEGER => {
                                    params.push(LLVMInt64TypeInContext(self.context));
                                }
                                X64Class::CLASS_SSE => {
                                    let m = if piece.vec {
                                        let f = LLVMFloatTypeInContext(self.context);
                                        LLVMVectorType(f, 2)
                                    } else if piece.meaningful_bits == 32 {
                                        LLVMFloatTypeInContext(self.context)
                                    } else {
                                        LLVMDoubleTypeInContext(self.context)
                                    };
                                    params.push(m);
                                }
                                X64Class::CLASS_SSEUP => todo!(),
                            }
                        }
                    }
                }
            }

            let fn_type = LLVMFunctionType(retval, params.as_mut_ptr(), params.len() as u32, 0);

            let name = CString::new(decl.ident.ident.as_bytes()).unwrap();
            let fn_val = LLVMAddFunction(self.module, name.as_ptr(), fn_type);

            for (attr, i) in attributes {
                LLVMAddAttributeAtIndex(fn_val, i, attr);
            }
            (fn_val, func_abi_info)
        }
    }

    pub fn to_llvm_type(&self, data_type: &DataType) -> LLVMTypeRef {
        unsafe {
            match data_type {
                DataType::UnsizedInt => unreachable!(),
                DataType::I8 | DataType::U8 => LLVMInt8TypeInContext(self.context),
                DataType::I16 | DataType::U16 => LLVMInt16TypeInContext(self.context),
                DataType::I32 | DataType::U32 => LLVMInt32TypeInContext(self.context),
                DataType::U64 | DataType::I64 => LLVMInt64TypeInContext(self.context),

                DataType::UnsizedFloat => unreachable!(),
                DataType::F32 => LLVMFloatTypeInContext(self.context),
                DataType::F64 => LLVMDoubleTypeInContext(self.context),

                DataType::Char => LLVMInt8TypeInContext(self.context),
                DataType::Array { data_type, size } => {
                    LLVMArrayType2(self.to_llvm_type(data_type), *size as u64)
                }
                DataType::Pointer(_) => LLVMPointerTypeInContext(self.context, 0),
                DataType::Boolean => LLVMInt8TypeInContext(self.context),
                DataType::Void => LLVMVoidTypeInContext(self.context),
                DataType::Struct(ident) => {
                    let s = self
                        .typechecker
                        .parser
                        .struct_declarations
                        .iter()
                        .find(|s| s.ident.ident == ident.ident)
                        .unwrap();

                    let mut members: Vec<LLVMTypeRef> = s
                        .members
                        .iter()
                        .map(|m| self.to_llvm_type(&m.data_type))
                        .collect();

                    LLVMStructTypeInContext(
                        self.context,
                        members.as_mut_ptr(),
                        members.len() as u32,
                        0,
                    )
                }
            }
        }
    }

    fn compile_statement(
        &mut self,
        statement: &TypedStatement,
        scope: &mut Scope,
        return_type: &DataType,
        fn_ref: LLVMValueRef,
    ) {
        unsafe {
            match &statement.kind {
                TypedStmtKind::If {
                    expr,
                    then_block: then_scope,
                    else_block: else_scope,
                } => {
                    let condition = self.compile_expression(expr, scope);
                    let then_block = LLVMCreateBasicBlockInContext(
                        self.context,
                        CString::new(format!("if.then{}", self.alloc_id()))
                            .unwrap()
                            .as_ptr(),
                    );

                    let else_block = LLVMCreateBasicBlockInContext(
                        self.context,
                        CString::new(format!("if.else{}", self.alloc_id()))
                            .unwrap()
                            .as_ptr(),
                    );

                    let end_block = LLVMCreateBasicBlockInContext(
                        self.context,
                        CString::new(format!("if.end{}", self.alloc_id()))
                            .unwrap()
                            .as_ptr(),
                    );

                    let br_block = if else_scope.is_some() {
                        else_block
                    } else {
                        end_block
                    };

                    LLVMBuildCondBr(self.builder, condition, then_block, br_block);

                    LLVMAppendExistingBasicBlock(fn_ref, then_block);
                    LLVMPositionBuilderAtEnd(self.builder, then_block);
                    self.subscope(scope, return_type, then_scope, fn_ref);

                    if then_scope.return_actuality != AlwaysReturns {
                        LLVMBuildBr(self.builder, end_block);
                    }

                    if let Some(else_scope) = else_scope {
                        LLVMAppendExistingBasicBlock(fn_ref, else_block);
                        LLVMPositionBuilderAtEnd(self.builder, else_block);
                        self.subscope(scope, return_type, else_scope, fn_ref);
                        if else_scope.return_actuality != AlwaysReturns {
                            LLVMBuildBr(self.builder, end_block);
                        }
                    }

                    if statement.return_actuality != AlwaysReturns {
                        LLVMAppendExistingBasicBlock(fn_ref, end_block);
                        LLVMPositionBuilderAtEnd(self.builder, end_block);
                    }
                }
                TypedStmtKind::Return(expr) => {
                    if let Some(expr) = expr {
                        let value = self.compile_expression(expr, scope);
                        LLVMBuildRet(self.builder, value);
                    } else {
                        LLVMBuildRetVoid(self.builder);
                    }
                }
                TypedStmtKind::Expression(expr) => {
                    self.compile_expression(expr, scope);
                }
                TypedStmtKind::While { expr: _, block: _ } => {
                    todo!();
                    // let label_begin = self.alloc_label();
                    // let label_end = self.alloc_label();
                    // scope.instructions.push(IRInstruction {
                    //     id: self.alloc_inst(),
                    //     r#type: IRType::Void,
                    //     operation: Operation::Label(label_begin),
                    // });
                    //
                    // let condition = expression
                    //     .as_ref()
                    //     .map(|expr| self.compile_expression(expr, scope))
                    //     .flatten();
                    //
                    // if let Some((cond, data_type)) = condition {
                    //     self.err_if_mismatched(
                    //         &expression.as_ref().unwrap().span,
                    //         data_type,
                    //         DataType::Boolean,
                    //     );
                    //     scope.instructions.push(IRInstruction {
                    //         id: self.alloc_inst(),
                    //         r#type: IRType::Void,
                    //         operation: Operation::JumpLabelIfNot {
                    //             cond: cond[0],
                    //             label: label_end,
                    //         },
                    //     });
                    // }
                    //
                    // self.subscope(scope, return_type, current_frame_size, while_scope);
                    // scope.instructions.push(IRInstruction {
                    //     id: self.alloc_inst(),
                    //     r#type: IRType::Void,
                    //     operation: Operation::JumpLabel(label_begin),
                    // });
                    // scope.instructions.push(IRInstruction {
                    //     id: self.alloc_inst(),
                    //     r#type: IRType::Void,
                    //     operation: Operation::Label(label_end),
                    // });
                }

                TypedStmtKind::VariableDefinition {
                    ident,
                    expr,
                    data_type: _,
                } => {
                    //TODO: move alloca to the function beginning
                    let ptr = scope
                        .vars
                        .iter()
                        .find(|v| v.ident.ident == *ident)
                        .unwrap()
                        .ptr;

                    if let Some(expr) = expr {
                        let value = self.compile_expression(expr, scope);
                        LLVMBuildStore(self.builder, value, ptr);
                        //TODO: handle large structs
                    }
                }
            }
        }
    }

    fn subscope(
        &mut self,
        scope: &mut Scope,
        return_type: &DataType,
        typed_block: &TypedBlock,
        fn_ref: LLVMValueRef,
    ) {
        for statement in &typed_block.statements {
            self.compile_statement(statement, scope, return_type, fn_ref);
        }
    }

    fn compile_expression<'b>(&self, expr: &TypedExpr, scope: &Scope) -> LLVMValueRef {
        unsafe {
            match &expr.kind {
                TypedExprKind::NumerLiteral(val) => {
                    //TODO: maybe move it to typechecker?
                    if expr.inferred_type.is_int() {
                        LLVMConstInt(self.to_llvm_type(&expr.inferred_type), *val as u64, 1)
                    } else if expr.inferred_type.is_float() {
                        LLVMConstReal(self.to_llvm_type(&expr.inferred_type), *val as f64)
                    } else {
                        unreachable!();
                    }
                }
                TypedExprKind::CharLiteral(_) => todo!(),
                TypedExprKind::BoolLiteral(_) => todo!(),
                TypedExprKind::FloatLiteral(val) => {
                    LLVMConstReal(self.to_llvm_type(&expr.inferred_type), *val)
                }
                TypedExprKind::StringLiteral(string) => LLVMBuildGlobalString(
                    self.builder,
                    CString::new(string.clone()).unwrap().as_ptr(),
                    c"str".as_ptr(),
                ),
                TypedExprKind::Negation(_typed_expr) => todo!(),
                TypedExprKind::Not(_typed_expr) => todo!(),
                TypedExprKind::UnaryAddressOf(_place) => todo!(),
                TypedExprKind::Dereference(_typed_expr) => todo!(),
                TypedExprKind::Arithmetic(op, lhs, rhs) => {
                    let ty = &lhs.inferred_type;
                    let lhs = self.compile_expression(lhs, scope);
                    let rhs = self.compile_expression(rhs, scope);

                    use DataType::*;
                    match ty {
                        I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64 => match op {
                            ArithmeticOp::Add => {
                                LLVMBuildAdd(self.builder, lhs, rhs, c"add".as_ptr())
                            }
                            ArithmeticOp::Sub => {
                                LLVMBuildSub(self.builder, lhs, rhs, c"sub".as_ptr())
                            }
                            ArithmeticOp::Mul => {
                                LLVMBuildMul(self.builder, lhs, rhs, c"mul".as_ptr())
                            }
                            ArithmeticOp::Div => {
                                if matches!(ty, U8 | U16 | U32 | U64) {
                                    LLVMBuildUDiv(self.builder, lhs, rhs, c"div".as_ptr())
                                } else {
                                    LLVMBuildSDiv(self.builder, lhs, rhs, c"div".as_ptr())
                                }
                            }
                        },
                        F32 | F64 => match op {
                            ArithmeticOp::Add => {
                                LLVMBuildFAdd(self.builder, lhs, rhs, c"add".as_ptr())
                            }
                            ArithmeticOp::Sub => {
                                LLVMBuildFSub(self.builder, lhs, rhs, c"sub".as_ptr())
                            }
                            ArithmeticOp::Mul => {
                                LLVMBuildFMul(self.builder, lhs, rhs, c"mul".as_ptr())
                            }
                            ArithmeticOp::Div => {
                                LLVMBuildFDiv(self.builder, lhs, rhs, c"div".as_ptr())
                            }
                        },
                        Struct(_ident) => todo!(),
                        _ => unreachable!(),
                    }
                }
                TypedExprKind::Comparison(op, lhs, rhs) => {
                    let lhs_v = self.compile_expression(lhs, scope);
                    let rhs_v = self.compile_expression(rhs, scope);
                    if lhs.inferred_type.is_int() {
                        LLVMBuildICmp(self.builder, (*op).into(), lhs_v, rhs_v, c"cmp".as_ptr())
                    } else if lhs.inferred_type.is_float() {
                        LLVMBuildFCmp(self.builder, (*op).into(), lhs_v, rhs_v, c"cmp".as_ptr())
                    } else {
                        todo!();
                    }
                }
                TypedExprKind::Assignment(place, expr) => {
                    //TODO: move alloca to the function beginning
                    let ptr = self.compile_place(place, scope);
                    let value = self.compile_expression(expr, scope);

                    LLVMBuildStore(self.builder, value, ptr);
                    value
                    //TODO: handle large structs
                }
                TypedExprKind::MemberAccess(place, member) => {
                    let ptr = self.compile_member_access(place, scope, member);
                    LLVMBuildLoad2(
                        self.builder,
                        self.to_llvm_type(&expr.inferred_type),
                        ptr,
                        CString::new(member.clone()).unwrap().as_ptr(),
                    )
                }
                TypedExprKind::Identifier(ident) => {
                    // Some((var.ptr, DataType::Pointer(Box::new(var.data_type.clone()))))
                    let var = scope.find(ident);
                    LLVMBuildLoad2(
                        self.builder,
                        self.to_llvm_type(&var.data_type),
                        var.ptr,
                        CString::new(ident.clone()).unwrap().as_ptr(),
                    )
                }
                TypedExprKind::StructLiteral(exprs) => {
                    let mut val = LLVMGetPoison(self.to_llvm_type(&expr.inferred_type));

                    for (i, expr) in exprs.iter().enumerate() {
                        val = LLVMBuildInsertValue(
                            self.builder,
                            val,
                            self.compile_expression(expr, scope),
                            i as u32,
                            c"".as_ptr(),
                        );
                    }

                    val
                    // todo!(
                    //     "Make struct literals special case as I think they are only valid for assignment, args and ==, or just find a way to get LLVMValueRef for an entire struct"
                    // );
                }
                TypedExprKind::Call(func, args) => {
                    let func = CString::new(func.clone()).unwrap();
                    let fn_ref = LLVMGetNamedFunction(self.module, func.as_ptr());
                    let fn_type = LLVMGlobalGetValueType(fn_ref);

                    let mut args: Vec<LLVMValueRef> = args
                        .iter()
                        .map(|expr| self.compile_expression(expr, scope))
                        .collect();

                    LLVMBuildCall2(
                        self.builder,
                        fn_type,
                        fn_ref,
                        args.as_mut_ptr(),
                        args.len() as u32,
                        c"".as_ptr(),
                    )
                }
            }
        }
    }

    fn compile_member_access(&self, place: &Place, scope: &Scope, member: &str) -> LLVMValueRef {
        let DataType::Struct(ident) = &place.inferred_type else {
            unreachable!()
        };

        let struct_dec = self
            .typechecker
            .parser
            .struct_declarations
            .iter()
            .find(|s| s.ident.ident == ident.ident)
            .unwrap();

        let ptr = self.compile_place(place, scope);
        let ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                self.to_llvm_type(&place.inferred_type),
                ptr,
                struct_dec
                    .members
                    .iter()
                    .position(|m| m.ident.ident == *member)
                    .unwrap() as u32,
                c"gep".as_ptr(),
            )
        };

        ptr
    }

    fn compile_place(&self, place: &Place, scope: &Scope) -> LLVMValueRef {
        match &place.kind {
            PlaceKind::Deref(expr) => self.compile_expression(expr, scope),
            PlaceKind::Variable(ident) => scope.find(ident).ptr,
            PlaceKind::Index(_ptr, _idx) => todo!(),
            PlaceKind::Member(place, member) => self.compile_member_access(place, scope, member),
        }
    }
}

impl<'a> Drop for LLVMGen<'a> {
    fn drop(&mut self) {
        unsafe {
            LLVMContextDispose(self.context);
            // LLVMDisposeBuilder(self.builder);
            // LLVMDisposeModule(self.module);
        }
    }
}
