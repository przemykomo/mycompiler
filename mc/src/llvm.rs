use std::ffi::CString;
use std::ptr;

use llvm_sys::analysis::LLVMVerifierFailureAction;
use llvm_sys::analysis::LLVMVerifyFunction;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::LLVMCodeGenFileType::LLVMObjectFile;
use llvm_sys::target_machine::LLVMCodeGenOptLevel::LLVMCodeGenLevelNone;
use llvm_sys::target_machine::*;

use crate::abi_sysv::FuncABIInfo;
use crate::abi_sysv::add_fn_decl;
use crate::abi_sysv::build_ret;
use crate::abi_sysv::call;
use crate::abi_sysv::fun_prologue;
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
    pub ident: IdentifierSpanned,
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
    pub context: LLVMContextRef,
    pub builder: LLVMBuilderRef,
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
            add_fn_decl(self, decl);
        }

        for function in &self.typechecker.typed_functions {
            let (fn_ref, func_abi_info) = add_fn_decl(self, &function.prototype);
            unsafe {
                let entry_block =
                    LLVMAppendBasicBlockInContext(self.context, fn_ref, c"entry".as_ptr());
                LLVMPositionBuilderAtEnd(self.builder, entry_block);

                let mut scope = Scope { vars: Vec::new() };

                fun_prologue(self, function, fn_ref, &func_abi_info, &mut scope);

                for statement in &function.block.statements {
                    self.compile_statement(
                        statement,
                        &mut scope,
                        &function.prototype.return_type,
                        fn_ref,
                        &func_abi_info,
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
                DataType::Boolean => LLVMInt1TypeInContext(self.context),
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
        func_abi_info: &FuncABIInfo,
    ) {
        unsafe {
            match &statement.kind {
                TypedStmtKind::If {
                    expr,
                    then_block: then_scope,
                    else_block: else_scope,
                } => {
                    let condition = self.compile_expression(expr, scope, fn_ref);
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
                    self.subscope(scope, return_type, then_scope, fn_ref, func_abi_info);

                    if then_scope.return_actuality != AlwaysReturns {
                        LLVMBuildBr(self.builder, end_block);
                    }

                    if let Some(else_scope) = else_scope {
                        LLVMAppendExistingBasicBlock(fn_ref, else_block);
                        LLVMPositionBuilderAtEnd(self.builder, else_block);
                        self.subscope(scope, return_type, else_scope, fn_ref, func_abi_info);
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
                    build_ret(self, scope, return_type, fn_ref, func_abi_info, expr);
                }
                TypedStmtKind::Expression(expr) => {
                    self.compile_expression(expr, scope, fn_ref);
                }
                TypedStmtKind::While { expr: _, block: _ } => {
                    todo!();
                }

                TypedStmtKind::VariableDefinition {
                    ident,
                    expr,
                    data_type: _,
                } => {
                    let ptr = scope
                        .vars
                        .iter()
                        .find(|v| v.ident.ident == *ident)
                        .unwrap()
                        .ptr;

                    if let Some(expr) = expr {
                        let value = self.compile_expression(expr, scope, fn_ref);
                        LLVMBuildStore(self.builder, value, ptr);
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
        func_abi_info: &FuncABIInfo,
    ) {
        for statement in &typed_block.statements {
            self.compile_statement(statement, scope, return_type, fn_ref, func_abi_info);
        }
    }

    pub fn compile_expression<'b>(
        &self,
        expr: &TypedExpr,
        scope: &Scope,
        fn_ref: LLVMValueRef,
    ) -> LLVMValueRef {
        unsafe {
            match &expr.kind {
                TypedExprKind::NumerLiteral(val) => {
                    if expr.inferred_type.is_int() {
                        LLVMConstInt(self.to_llvm_type(&expr.inferred_type), *val as u64, 1)
                    } else if expr.inferred_type.is_float() {
                        LLVMConstReal(self.to_llvm_type(&expr.inferred_type), *val as f64)
                    } else {
                        unreachable!();
                    }
                }
                TypedExprKind::CharLiteral(_) => todo!(),
                TypedExprKind::BoolLiteral(b) => {
                    LLVMConstInt(LLVMInt1TypeInContext(self.context), *b as u64, 0)
                }
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
                    let lhs = self.compile_expression(lhs, scope, fn_ref);
                    let rhs = self.compile_expression(rhs, scope, fn_ref);

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
                        _ => unreachable!(),
                    }
                }
                TypedExprKind::Comparison(op, lhs, rhs) => {
                    let lhs_v = self.compile_expression(lhs, scope, fn_ref);
                    let rhs_v = self.compile_expression(rhs, scope, fn_ref);
                    if lhs.inferred_type.is_int() {
                        LLVMBuildICmp(self.builder, (*op).into(), lhs_v, rhs_v, c"cmp".as_ptr())
                    } else if lhs.inferred_type.is_float() {
                        LLVMBuildFCmp(self.builder, (*op).into(), lhs_v, rhs_v, c"cmp".as_ptr())
                    } else {
                        unreachable!();
                    }
                }
                TypedExprKind::Assignment(place, expr) => {
                    let ptr = self.compile_place(place, scope, fn_ref);
                    let value = self.compile_expression(expr, scope, fn_ref);

                    LLVMBuildStore(self.builder, value, ptr);
                    value
                }
                TypedExprKind::MemberAccess(place, member) => {
                    let ptr = self.compile_member_access(place, scope, member, fn_ref);
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
                            self.compile_expression(expr, scope, fn_ref),
                            i as u32,
                            c"".as_ptr(),
                        );
                    }

                    val
                }
                TypedExprKind::Call(func, args_exprs) => {
                    call(self, fn_ref, scope, func, args_exprs)
                }
            }
        }
    }

    fn compile_member_access(
        &self,
        place: &Place,
        scope: &Scope,
        member: &str,
        fn_ref: LLVMValueRef,
    ) -> LLVMValueRef {
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

        let ptr = self.compile_place(place, scope, fn_ref);
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

    fn compile_place(&self, place: &Place, scope: &Scope, fn_ref: LLVMValueRef) -> LLVMValueRef {
        match &place.kind {
            PlaceKind::Deref(expr) => self.compile_expression(expr, scope, fn_ref),
            PlaceKind::Variable(ident) => scope.find(ident).ptr,
            PlaceKind::Index(_ptr, _idx) => todo!(),
            PlaceKind::Member(place, member) => {
                self.compile_member_access(place, scope, member, fn_ref)
            }
        }
    }

    pub fn entry_alloca(
        self: &Self,
        fn_ref: LLVMValueRef,
        llvm_type: LLVMTypeRef,
        name: *const std::ffi::c_char,
    ) -> LLVMValueRef {
        unsafe {
            let entry_builder = LLVMCreateBuilderInContext(self.context);
            let entry_block = LLVMGetEntryBasicBlock(fn_ref);
            let instr = LLVMGetFirstInstruction(entry_block);
            if instr.is_null() {
                LLVMPositionBuilderAtEnd(entry_builder, entry_block);
            } else {
                LLVMPositionBuilderBefore(entry_builder, instr);
            }
            let val = LLVMBuildAlloca(entry_builder, llvm_type, name);
            LLVMDisposeBuilder(entry_builder);
            val
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
