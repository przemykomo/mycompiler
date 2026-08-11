use std::ffi::CString;
use std::path::Path;
use std::ptr::null;

use inkwell::llvm_sys::core::*;
use inkwell::llvm_sys::prelude::*;
use inkwell::{
    OptimizationLevel,
    context::Context,
    targets::{FileType, InitializationConfig, Target, TargetMachineOptions, TargetTriple},
};

use crate::ast::ArithmeticOp::Add;
use crate::ast::ArithmeticOp::Div;
use crate::ast::ArithmeticOp::Mul;
use crate::ast::ArithmeticOp::Sub;
use crate::ast::BinaryOp;
use crate::ast::Expression;
use crate::ast::ExpressionSpanned;
use crate::ast::Statement;
use crate::ast::UnaryOperator;
use crate::tokenizer::Span;
use crate::{
    ast::IdentifierSpanned,
    parser::Parser,
    tokenizer::{DataType, Error},
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
    initialized: bool,
    pub argument: bool,
    // pub frame_pos: i32,
    pub reachable: bool,
    pub ptr: LLVMValueRef,
}

#[derive(Debug)]
pub struct Scope {
    pub vars: Vec<Variable>,
}

impl Scope {
    fn truncate_reachable_vars(&mut self, starting: usize) {
        for var in &mut self.vars[starting..] {
            var.reachable = false;
        }
    }
}

pub struct LLVMGen<'a> {
    pub parser: &'a Parser<'a>,
    pub errors: Vec<Error>,
    context: LLVMContextRef,
    builder: LLVMBuilderRef,
    pub module: LLVMModuleRef,
    id_count: usize,
}

impl<'a> LLVMGen<'a> {
    pub fn new(parser: &'a Parser<'a>, context: LLVMContextRef) -> LLVMGen<'a> {
        unsafe {
            let builder = LLVMCreateBuilderInContext(context);
            let module = LLVMModuleCreateWithNameInContext(c"tmp".as_ptr(), context);

            Self {
                parser,
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
        // TODO
        // for decl in &self.parser.struct_declarations {
        //
        // }

        for decl in &self.parser.function_declarations {
            self.add_fn_decl(decl);
        }

        for function in &self.parser.functions {
            let fn_ref = self.add_fn_decl(&function.prototype);
            unsafe {
                let entry_block =
                    LLVMAppendBasicBlockInContext(self.context, fn_ref, c"entry".as_ptr());
                LLVMPositionBuilderAtEnd(self.builder, entry_block);

                let mut scope = Scope { vars: Vec::new() };

                for statement in &function.body {
                    self.compile_statement(
                        statement,
                        &mut scope,
                        &function.prototype.return_type,
                        fn_ref,
                    );
                }

                // let c = LLVMConstInt(LLVMInt64TypeInContext(self.context), 43, 0);
                // LLVMBuildRet(self.builder, c);
            }
            //     let mut vars: Vec<Variable> = Vec::new();
            //     for arg in &function.prototype.arguments {
            //         todo!();
            //         // vars.push(Variable {
            //         //     ident: arg.0.clone(),
            //         //     data_type: arg.1.clone(),
            //         //     initialized: true,
            //         //     argument: true,
            //         //     frame_pos: 0, //TODO
            //         //     reachable: true,
            //         // });
            //     }
            //
            //     let mut scope = Scope {
            //         vars,
            //         instructions: Vec::new(),
            //         // frame_size: 0,
            //     };
            //
            //     let mut current_frame_size = 0;
            //     for statement in &function.body {
            //         self.compile_statement(
            //             statement,
            //             &mut scope,
            //             &function.prototype.return_type,
            //             &mut current_frame_size,
            //         );
            //     }
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
        statement: &Statement,
        scope: &mut Scope,
        return_type: &DataType,
        fn_ref: LLVMValueRef,
        // current_frame_size: &mut i32,
    ) {
        unsafe {
            match statement {
                Statement::If {
                    expression,
                    scope: then_scope,
                    else_scope,
                } => {
                    let condition = expression
                        .as_ref()
                        .map(|expr| self.compile_expression(expr, scope))
                        .flatten();
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

                    if let Some((cond, data_type)) = condition {
                        self.err_if_mismatched(
                            &expression.as_ref().unwrap().span,
                            data_type,
                            DataType::Boolean,
                        );
                        let br_block = if else_scope.is_some() {
                            else_block
                        } else {
                            end_block
                        };
                        LLVMBuildCondBr(self.builder, cond, then_block, br_block);
                    }

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
                Statement::Return(expr) => {
                    if let Some((value, expr_type)) = self.compile_expression(expr, scope) {
                        self.err_if_mismatched(&expr.span, return_type.clone(), expr_type.clone());
                        LLVMBuildRet(self.builder, value);
                    }
                }
                Statement::Expression(expr) => {
                    self.compile_expression(expr, scope);
                }
                Statement::While {
                    expression: _,
                    scope: _,
                } => {
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
                Statement::For {
                    inital_statement: _,
                    condition_expr: _,
                    iteration_expr: _,
                    scope: _,
                } => todo!(),
                Statement::VariableDefinition {
                    ident,
                    expression,
                    data_type,
                } => {
                    let ptr = LLVMBuildAlloca(
                        self.builder,
                        self.to_llvm_type(data_type),
                        CString::new(format!("if.then{}", self.alloc_id()))
                            .unwrap()
                            .as_ptr(),
                    );

                    let initialized = if let Some(expr) = expression {
                        if let Some((value, expr_type)) = self.compile_expression(expr, scope) {
                            self.err_if_mismatched(&expr.span, data_type.clone(), expr_type);
                            LLVMBuildStore(self.builder, value, ptr);
                            //TODO: handle large structs
                        }
                        true
                    } else {
                        false
                    };

                    scope.vars.push(Variable {
                        ident: ident.clone(),
                        data_type: data_type.clone(),
                        initialized,
                        argument: false,
                        reachable: true,
                        ptr,
                    });
                }
            }
        }
    }

    fn subscope(
        &mut self,
        scope: &mut Scope,
        return_type: &DataType,
        // current_frame_size: &mut i32,
        statements: &Vec<Statement>,
        fn_ref: LLVMValueRef,
    ) {
        // let outer_scope_frame = *current_frame_size;
        let outer_vars_len = scope.vars.len();
        for statement in statements {
            self.compile_statement(statement, scope, return_type, fn_ref);
        }

        // scope.frame_size = scope.frame_size.max(*current_frame_size);
        // *current_frame_size = outer_scope_frame;
        scope.truncate_reachable_vars(outer_vars_len);
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

    fn compile_expression<'b>(
        &mut self,
        expr: &ExpressionSpanned,
        scope: &mut Scope,
    ) -> Option<(LLVMValueRef, DataType)> {
        unsafe {
            match &expr.expression {
                Expression::IntLiteral(val) => {
                    let val = LLVMConstInt(LLVMInt64TypeInContext(self.context), *val as u64, 1);
                    Some((val, DataType::I64))
                }
                Expression::CharacterLiteral(val) => {
                    todo!();
                    // Some((vec![id], DataType::Char))
                }
                Expression::BoolLiteral(val) => {
                    todo!();
                    // let id = self.alloc_inst();
                    // scope.instructions.push(IRInstruction {
                    //     id,
                    //     r#type: IRType::i8,
                    //     operation: Operation::ConstInt(*val as i64),
                    // });
                    // Some((vec![id], DataType::Boolean))
                }
                Expression::FloatLiteral(val) => {
                    todo!();
                    // Some((vec![id], DataType::F32))
                }
                Expression::StringLiteral(val) => {
                    todo!();
                    // (
                    //     Value::ImmediateString(val.clone()),
                    //     DataType::Array {
                    //         data_type: Box::new(DataType::Char),
                    //         size: val.len() as i32,
                    //     },
                    // )
                }
                Expression::StructLiteral { ident, members } => {
                    todo!();
                    // let Some(struct_dec) = self
                    //     .parser
                    //     .struct_declarations
                    //     .iter()
                    //     .find(|dec| dec.ident.ident == ident.ident)
                    // else {
                    //     self.errors.push(Error {
                    //         span: ident.span,
                    //         msg: format!("Cannot find a struct `{}`", ident.ident),
                    //     });
                    //     return None;
                    // };
                    //
                    // let mut values: Vec<usize> = Vec::new();
                    //
                    // for (i, (ident, expr)) in members.iter().enumerate() {
                    //     let m = struct_dec.members.get(i);
                    //     if let Some(m) = m {
                    //         if m.ident.ident != ident.ident {
                    //             self.errors.push(Error {
                    //                 span: ident.span,
                    //                 msg: format!("`{}` expected.", m.ident.ident),
                    //             });
                    //             return None;
                    //         }
                    //         if let Some(expr) = expr {
                    //             if let Some((mut value, data_type)) =
                    //                 self.compile_expression(expr, scope)
                    //             {
                    //                 self.err_if_mismatched(
                    //                     &expr.span,
                    //                     m.data_type.clone(),
                    //                     data_type,
                    //                 );
                    //                 // Hopefully flattening nested structs makes it easier
                    //                 values.append(&mut value);
                    //             }
                    //         }
                    //     } else {
                    //         self.errors.push(Error {
                    //             span: ident.span,
                    //             msg: format!("No such field `{}`.", ident.ident),
                    //         });
                    //         return None;
                    //     }
                    // }
                    //
                    // if struct_dec.members.len() > members.len() {
                    //     self.errors.push(Error {
                    //         span: ident.span,
                    //         msg: format!(
                    //             "Missing struct fields: `{}`",
                    //             List(&struct_dec.members[members.len()..])
                    //         ),
                    //     });
                    //
                    //     return None;
                    // }
                    //
                    // Some((values, DataType::Struct(struct_dec.ident.clone())))
                }
                Expression::FunctionCall(call) => {
                    // let Some((func, _)) = self.parser.get_function(&call.ident.ident) else {
                    //     self.errors.push(Error {
                    //         span: call.ident.span,
                    //         msg: format!("Cannot find a function `{}`", call.ident.ident),
                    //     });
                    //     return None;
                    // };
                    //
                    // let mut values: Vec<usize> = Vec::new();
                    // for (i, expr) in call.arguments.iter().enumerate() {
                    //     let arg = func.arguments.get(i);
                    //     let expr_result = self.compile_expression(expr, scope);
                    //     if let (Some((_ident, arg_type)), Some((mut value, data_type))) =
                    //         (arg, expr_result)
                    //     {
                    //         self.err_if_mismatched(&expr.span, arg_type.clone(), data_type);
                    //         values.append(&mut value);
                    //     }
                    // }
                    //
                    // if func.arguments.len() != call.arguments.len() {
                    //     self.errors.push(Error {
                    //         span: expr.span,
                    //         msg: format!(
                    //             "Expected {} arguments, got {}.",
                    //             func.arguments.len(),
                    //             call.arguments.len()
                    //         ),
                    //     });
                    //     return None;
                    // }

                    todo!("Function calls IR gen");
                    // let result = self.alloc_inst();
                    // scope.instructions.push(Instruction {
                    //     id: result,
                    //     r#type: (),
                    //     operation: Operation::Call,
                    // });
                    //
                    // Some((vec![result], func.return_type.clone()))
                }
                Expression::Identifier(ident) => {
                    if let Some((ptr, data_type)) = self.use_var(scope, ident) {
                        // let val = LLVMBuildLoad2(
                        //     self.builder,
                        //     self.to_llvm_type(&data_type),
                        //     ptr,
                        //     null(),
                        // );
                        Some((ptr, DataType::Pointer(Box::new(data_type))))
                    } else {
                        None
                    }
                }
                Expression::ArraySubscript { ident, element } => {
                    todo!();
                    // let element = self.compile_expression(element, scope);
                    // self.use_var(scope, ident, |(instructions, errors, var_index, var)| {
                    //     let DataType::Array { data_type, size } = &var.data_type else {
                    //         errors.push(Error {
                    //             span: ident.span,
                    //             msg: format!("Expected an array, got {:?}.", &var.data_type),
                    //         });
                    //         return None;
                    //     };
                    //
                    //     todo!();
                    //     // if let Some((value, data_type)) = element {
                    //     //     Some((
                    //     //         Value::ArrayAccess {
                    //     //             var_index,
                    //     //             array_index: Box::new(value),
                    //     //         },
                    //     //         data_type.clone(),
                    //     //     ))
                    //     // } else {
                    //     //     None
                    //     // }
                    // })
                }
                Expression::Binary {
                    lhs,
                    rhs,
                    operator: op,
                } => {
                    let left = self.compile_expression(lhs, scope);
                    let right = self.compile_expression(rhs, scope);

                    let (Some(left), Some(right)) = (left, right) else {
                        return None;
                    };

                    if left.1 != right.1 {
                        self.errors.push(Error {
                            span: lhs.span, //TODO
                            msg: format!("Cannot {:?} a {:?} and {:?}", op, left.1, right.1),
                        });
                        return None;
                    }

                    match left.1 {
                        DataType::I64 => {
                            match op {
                                BinaryOp::Arithmetic(op) => {
                                    let res = match op {
                                        Add => LLVMBuildAdd(self.builder, left.0, right.0, null()),
                                        Sub => LLVMBuildSub(self.builder, left.0, right.0, null()),
                                        Mul => LLVMBuildMul(self.builder, left.0, right.0, null()),
                                        Div => LLVMBuildSDiv(self.builder, left.0, right.0, null()),
                                    };
                                    Some((res, DataType::I64))
                                }
                                BinaryOp::Comparison(op) => {
                                    let res = LLVMBuildICmp(
                                        self.builder,
                                        (*op).into(),
                                        left.0,
                                        right.0,
                                        null(),
                                    );
                                    Some((res, DataType::Boolean))
                                }
                                BinaryOp::Assign => {
                                    //TODO: large structs
                                    let val = LLVMBuildLoad2(
                                        self.builder,
                                        LLVMInt64TypeInContext(self.context),
                                        PointerVal,
                                        null(),
                                    );
                                    LLVMBuildStore(self.builder, Val, Ptr);
                                    let value = self.alloc_inst();
                                    scope.instructions.push(IRInstruction {
                                        id: value,
                                        r#type: IRType::i64,
                                        operation: Operation::Load { ptr: right.0[0] },
                                    });
                                    let result = self.alloc_inst();
                                    scope.instructions.push(IRInstruction {
                                        id: result,
                                        r#type: IRType::Void,
                                        operation: Operation::Store {
                                            ptr: left.0[0],
                                            value,
                                        },
                                    });
                                    Some((vec![result], DataType::I64))
                                }
                                BinaryOp::MemberAccess => todo!(),
                            }
                        }
                        DataType::Char => todo!(),
                        DataType::Array { data_type, size } => todo!(),
                        DataType::Pointer(data_type) => todo!(),
                        DataType::Boolean => todo!(),
                        DataType::Void => {
                            self.errors.push(Error {
                                span: lhs.span, //TODO
                                msg: format!("Cannot {:?} a {:?} and {:?}", op, left.1, right.1),
                            });
                            None
                        }
                        DataType::F32 => todo!(),
                        DataType::Struct(identifier_spanned) => todo!(),
                    }
                }

                Expression::Unary { expr, operator } => {
                    todo!();
                }
            }
        }
    }

    fn use_var(
        &mut self,
        scope: &mut Scope,
        ident: &IdentifierSpanned,
        // f: impl FnOnce(
        //     (&mut Vec<IRInstruction>, &mut Vec<Error>, usize, &Variable),
        // ) -> Option<(Vec<usize>, DataType)>,
        // ) -> Option<(Vec<usize>, DataType)> {
    ) -> Option<(LLVMValueRef, DataType)> {
        let mut found_unreachable = None;
        if let Some((_var_index, var)) = scope
            .vars
            .iter()
            .enumerate()
            .rev()
            .find(|(i, var)| {
                if var.ident.ident == ident.ident {
                    if !var.reachable && found_unreachable.is_none() {
                        found_unreachable = Some((*i, *var));
                        false
                    } else {
                        true
                    }
                } else {
                    false
                }
            })
            .or(found_unreachable)
        {
            if !var.reachable {
                self.errors.push(Error {
                    span: ident.span,
                    msg: format!(
                        "Unreachable variable `{}`, possible candidate: `{:?}`",
                        ident.ident, var.ident.span
                    ),
                });
            }
            Some((var.ptr, var.data_type.clone()))
            // f((&mut scope.instructions, &mut self.errors, var_index, var))
        } else {
            self.errors.push(Error {
                span: ident.span,
                msg: format!("Undefined variable `{}`", ident.ident),
            });
            None
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
