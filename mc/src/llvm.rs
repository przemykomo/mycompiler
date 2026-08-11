use std::ffi::CString;
use std::path::Path;
use std::ptr;

use inkwell::llvm_sys::analysis::LLVMVerifierFailureAction;
use inkwell::llvm_sys::analysis::LLVMVerifyFunction;
use inkwell::llvm_sys::core::*;
use inkwell::llvm_sys::prelude::*;
use inkwell::llvm_sys::target::*;
use inkwell::llvm_sys::target_machine::LLVMCodeGenFileType::LLVMObjectFile;
use inkwell::llvm_sys::target_machine::LLVMCodeGenOptLevel::LLVMCodeGenLevelNone;
use inkwell::llvm_sys::target_machine::*;
use inkwell::{
    OptimizationLevel,
    context::Context,
    targets::{FileType, InitializationConfig, Target, TargetMachineOptions, TargetTriple},
};

use crate::typecheck::Place;
use crate::typecheck::PlaceKind;
use crate::{
    ast::{ArithmeticOp, IdentifierSpanned},
    tokenizer::{DataType, Error, Span},
    typecheck::{TypeChecker, TypedBlock, TypedExpr, TypedExprKind, TypedStatement, TypedStmtKind},
};

pub fn run<'a>(out_file: &String) {
    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("tmp");

    let ret_type = context.i64_type();
    let fn_type = ret_type.fn_type(&[], false);
    let function = module.add_function("main", fn_type, None);
    let fn_type2 = context.void_type().fn_type(&[], false);
    let function2 = module.add_function("mytest", fn_type2, None);

    let entry = context.append_basic_block(function, "entry");
    builder.position_at_end(entry);

    let str_val = builder
        .build_global_string_ptr("Hello compiler!\n", "hello_str")
        .unwrap();
    // let alloca = builder.build_alloca(context.i64_type(), "var_alloca").unwrap();
    // builder.build_store(alloca, ).unwrap();

    let mytest = module.get_function("mytest").unwrap();
    builder.build_direct_call(mytest, &[], "").unwrap();
    /*
    let ret_val = builder.build_call(fn_value, &[i32_arg.into(), md_string.into()], "call").unwrap()
        .try_as_basic_value()
        .unwrap_basic();
    */
    let const_int = context.i64_type().const_int(23, true);
    let const_intb = context.i64_type().const_int(4, true);
    let add_result = builder
        .build_int_add(const_int, const_intb, "tmpadd")
        .unwrap();

    builder.build_return(Some(&add_result)).unwrap();

    // function.print_to_stderr();

    if !function.verify(true) {
        unsafe { function.delete() };
        panic!("function verify");
    }

    // let triple = TargetMachine::get_default_triple();
    // let target = Target::create_target_machine_from_options(&self, &triple, options);

    Target::initialize_x86(&InitializationConfig::default());

    let triple = TargetTriple::create("x86_64-pc-linux-gnu");
    let target = Target::from_triple(&triple).unwrap();
    let options = TargetMachineOptions::default()
        .set_cpu("x86-64")
        // .set_features("+avx2")
        .set_abi("sysv")
        .set_level(OptimizationLevel::None);

    let target_machine = target
        .create_target_machine_from_options(&triple, options)
        .unwrap();

    module.set_data_layout(&target_machine.get_target_data().get_data_layout());
    module.set_triple(&triple);

    module.print_to_stderr();

    target_machine
        .write_to_file(&module, FileType::Object, Path::new(out_file))
        .unwrap();
}

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
}

impl<'a> LLVMGen<'a> {
    pub fn new(typechecker: &'a TypeChecker<'a>, context: LLVMContextRef) -> LLVMGen<'a> {
        unsafe {
            let builder = LLVMCreateBuilderInContext(context);
            let module = LLVMModuleCreateWithNameInContext(c"tmp".as_ptr(), context);

            Self {
                typechecker,
                errors: Vec::new(),
                context,
                builder,
                module,
                id_count: 0,
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
            let fn_ref = self.add_fn_decl(&function.prototype);
            unsafe {
                let entry_block =
                    LLVMAppendBasicBlockInContext(self.context, fn_ref, c"entry".as_ptr());
                LLVMPositionBuilderAtEnd(self.builder, entry_block);

                let mut scope = Scope { vars: Vec::new() };

                for (n, (ident, data_type)) in function.prototype.arguments.iter().enumerate() {
                    let ptr = LLVMBuildAlloca(
                        self.builder,
                        self.to_llvm_type(&data_type),
                        CString::new(ident.ident.clone()).unwrap().as_ptr(),
                    );

                    let param = LLVMGetParam(fn_ref, n as u32);
                    LLVMBuildStore(self.builder, param, ptr);

                    scope.vars.push(Variable {
                        ident: ident.clone(),
                        data_type: data_type.clone(),
                        ptr,
                    });
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

            LLVMSetDataLayout(
                self.module,
                LLVMCopyStringRepOfTargetData(LLVMCreateTargetDataLayout(machine)),
            );

            LLVMSetTarget(self.module, triple);

            let ret = LLVMTargetMachineEmitToFile(
                machine,
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

    fn add_fn_decl(&mut self, decl: &crate::ast::FunctionPrototype) -> LLVMValueRef {
        let ret_type = self.to_llvm_type(&decl.return_type);
        let mut param_types: Vec<LLVMTypeRef> = decl
            .arguments
            .iter()
            .map(|(_, ty)| self.to_llvm_type(ty))
            .collect();
        unsafe {
            let fn_type = LLVMFunctionType(
                ret_type,
                param_types.as_mut_ptr(),
                param_types.len() as u32,
                0,
            );
            let name = CString::new(decl.ident.ident.as_bytes()).unwrap();
            LLVMAddFunction(self.module, name.as_ptr(), fn_type)
        }
    }

    fn to_llvm_type(&self, data_type: &DataType) -> LLVMTypeRef {
        unsafe {
            match data_type {
                DataType::I64 => LLVMInt64TypeInContext(self.context),
                DataType::Char => LLVMInt8TypeInContext(self.context),
                DataType::Array { data_type, size } => {
                    LLVMArrayType2(self.to_llvm_type(data_type), *size as u64)
                }
                DataType::Pointer(_) => LLVMPointerTypeInContext(self.context, 0),
                DataType::Boolean => LLVMInt8TypeInContext(self.context),
                DataType::Void => LLVMVoidTypeInContext(self.context),
                DataType::F32 => LLVMFloatTypeInContext(self.context),
                DataType::Struct(_identifier) => todo!(), //LLVMStructTypeInContext(self.context, n, 1, 1),
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

                    LLVMBuildBr(self.builder, end_block);

                    if let Some(else_scope) = else_scope {
                        self.subscope(scope, return_type, else_scope, fn_ref);
                        LLVMBuildBr(self.builder, end_block);
                    }

                    LLVMAppendExistingBasicBlock(fn_ref, end_block);
                    LLVMPositionBuilderAtEnd(self.builder, end_block);
                }
                TypedStmtKind::Return(expr) => {
                    let value = self.compile_expression(expr, scope);
                    LLVMBuildRet(self.builder, value);
                }
                TypedStmtKind::Expression(expr) => {
                    self.compile_expression(expr, scope);
                }
                TypedStmtKind::While { expr, block } => {
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
                    data_type,
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
        // current_frame_size: &mut i32,
        typed_block: &TypedBlock,
        fn_ref: LLVMValueRef,
    ) {
        // let outer_scope_frame = *current_frame_size;
        // let outer_vars_len = scope.vars.len();
        for statement in &typed_block.statements {
            self.compile_statement(statement, scope, return_type, fn_ref);
        }

        // scope.frame_size = scope.frame_size.max(*current_frame_size);
        // *current_frame_size = outer_scope_frame;
        // scope.truncate_reachable_vars(outer_vars_len);
    }

    fn err_if_mismatched(&mut self, span: &Span, expected: DataType, got: DataType) {
        //TODO maybe move typechecking into its own pass as this is duplicate code in both backends
        if got != expected {
            self.errors.push(Error {
                span: *span,
                msg: format!("Mismatched types. Expected `{expected:?}`, got `{got:?}`."),
            });
        }
    }

    fn compile_expression<'b>(&self, expr: &TypedExpr, scope: &Scope) -> LLVMValueRef {
        unsafe {
            match &expr.kind {
                TypedExprKind::NumerLiteral(val) => {
                    LLVMConstInt(LLVMInt64TypeInContext(self.context), *val as u64, 1)
                }
                TypedExprKind::CharLiteral(_) => todo!(),
                TypedExprKind::BoolLiteral(_) => todo!(),
                TypedExprKind::FloatLiteral(_) => todo!(),
                TypedExprKind::StringLiteral(_) => todo!(),
                TypedExprKind::Negation(typed_expr) => todo!(),
                TypedExprKind::Not(typed_expr) => todo!(),
                TypedExprKind::UnaryAddressOf(place) => todo!(),
                TypedExprKind::Dereference(typed_expr) => todo!(),
                TypedExprKind::Arithmetic(op, lhs, rhs) => {
                    let ty = &lhs.inferred_type;
                    let lhs = self.compile_expression(lhs, scope);
                    let rhs = self.compile_expression(rhs, scope);

                    match ty {
                        DataType::I64 => match op {
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
                                LLVMBuildSDiv(self.builder, lhs, rhs, c"div".as_ptr())
                            }
                        },
                        DataType::F32 => todo!(),
                        DataType::Struct(ident) => todo!(),
                        _ => unreachable!(),
                    }
                }
                TypedExprKind::Comparison(op, lhs, rhs) => {
                    let lhs = self.compile_expression(lhs, scope);
                    let rhs = self.compile_expression(rhs, scope);
                    LLVMBuildICmp(self.builder, (*op).into(), lhs, rhs, c"cmp".as_ptr())
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
                TypedExprKind::StructLiteral(typed_exprs) => todo!(),
                TypedExprKind::Call(_, typed_exprs) => todo!(),
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
            PlaceKind::Index(ptr, idx) => todo!(),
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
