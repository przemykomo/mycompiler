use crate::tokenizer::DataType;
use std::cell::RefCell;
use std::rc::Rc;

use super::*;
use asmutil::FloatRegister::*;

pub fn compile_expression(
    state: &mut ScopeState,
    compilation_state: &mut CompilationState,
    expression: &Expression,
    result_hint: Option<Register>,
) -> ExpressionResult {
    match expression {
        Expression::IntLiteral(value) => {
            // let reg = force_get_any_free_register(state, &[], result_hint);
            // state.asm.mov(reg, *value, Word::QWORD);
            // let temp = Rc::new(RefCell::new(TempVariable::Register(reg)));
            // state.used_registers.insert(reg, temp.clone());
            ExpressionResult {
                data_type: DataType::Int,
                result_container: ResultContainer::ConstInt(*value),
            }
        }
        Expression::CharacterLiteral(c) => {
            // let reg = force_get_any_free_register(state, &[], result_hint);
            // state.asm.mov(reg, c, Word::BYTE);
            // let temp = Rc::new(RefCell::new(TempVariable::Register(reg)));
            // state.used_registers.insert(reg, temp.clone());
            ExpressionResult {
                data_type: DataType::Char,
                result_container: ResultContainer::ConstChar(*c),
            }
        }
        Expression::Identifier(identifier) => {
            let variable = state
                .variables
                .iter()
                .rev()
                .find(|var| var.identifier.eq(identifier))
                .expect(&fmt!("Undeclared variable: \"{}\"", identifier));
            ExpressionResult {
                data_type: variable.data_type.clone(),
                result_container: ResultContainer::IdentifierWithOffset {
                    identifier: identifier.clone(),
                    offset: 0,
                },
            }
        }
        Expression::ArraySubscript {
            identifier,
            element,
        } => {
            let variable = state
                .variables
                .iter()
                .rev()
                .find(|var| var.identifier.eq(identifier))
                .expect(&fmt!("Undeclared variable: \"{}\"", identifier));
            if let DataType::Array {
                data_type: _,
                size: _,
            } = variable.data_type.clone()
            {
                //let stack_location = variable.stack_location.clone();
                let result = compile_expression(state, compilation_state, element, None);
                if result.data_type != DataType::Int {
                    panic!("Array index must be an integer!");
                }
                todo!("Array subscripts");
            } else {
                panic!("{} is not an array type!", identifier);
            }
        }
        Expression::ArithmeticExpression {
            left,
            right,
            operator,
        } => {
            let right_result = compile_expression(state, compilation_state, right, None);
            let right_result = move_to_reg_if_needed(state, right_result);
            let left_result = compile_expression(state, compilation_state, left, result_hint);
            let left_result = move_to_reg_if_needed(state, left_result);

            if !left_result.data_type.eq(&right_result.data_type) {
                panic!("Cannot do arithmetic operations on values of different data types!");
            }

            match left_result.data_type {
                DataType::Int => {
                    let ResultContainer::TempVariable(left_temp) = left_result.result_container
                    else {
                        unreachable!()
                    };


                    match right_result.result_container {
                        ResultContainer::TempVariable(right_temp) => {
                            let TempVariable::Register(right_reg) = right_temp.borrow().clone() else {
                                unreachable!()
                            };
                            compile_arithmetic_int_expression(state, left_temp.clone(), right_reg, operator);
                            state.used_registers.remove(&right_reg);
                        },
                        ResultContainer::ConstInt(right_const) => {
                            compile_arithmetic_int_expression(state, left_temp.clone(), right_const, operator);
                        },
                        _ => unreachable!()
                    }


                    ExpressionResult {
                        data_type: DataType::Int,
                        result_container: ResultContainer::TempVariable(left_temp),
                    }
                }
                DataType::Float => {
                    match operator {
                        ArithmeticOperator::Add => {
                            state.asm.pop(RAX);
                            state.asm.movd(XMM1, RAX);
                            state.asm.addss(XMM0, XMM1);
                        }
                        ArithmeticOperator::Subtract => {
                            state.asm.pop(RAX);
                            state.asm.movd(XMM1, RAX);
                            state.asm.subss(XMM0, XMM1);
                        }
                        ArithmeticOperator::Multiply => {
                            state.asm.pop(RAX);
                            state.asm.movd(XMM1, RAX);
                            state.asm.mulss(XMM0, XMM1);
                        }
                        ArithmeticOperator::Divide => {
                            state.asm.pop(RAX);
                            state.asm.movd(XMM1, RAX);
                            state.asm.divss(XMM0, XMM1);
                        }
                    }
                    ExpressionResult {
                        data_type: DataType::Float,
                        result_container: ResultContainer::FloatRegister,
                    }
                }
                _ => panic!("Cannot do an arithmetic operation!"),
            }
        }
        Expression::ComparisonExpression {
            left,
            right,
            operator,
        } => {
            let right_result = compile_expression(state, compilation_state, right, None);
            let right_result = move_to_reg_if_needed(state, right_result);
            let left_result = compile_expression(state, compilation_state, left, None);
            let left_result = move_to_reg_if_needed(state, left_result);
            if right_result.data_type != left_result.data_type {
                panic!("Cannot compare different data types!");
            }
            let size = sizeofword(&left_result.data_type);

            let ResultContainer::TempVariable(left_temp) = left_result.result_container else {
                unreachable!()
            };
            let ResultContainer::TempVariable(right_temp) = right_result.result_container else {
                unreachable!()
            };
            let TempVariable::Register(left_reg) = left_temp.borrow().clone() else {
                unreachable!()
            };
            let TempVariable::Register(right_reg) = right_temp.borrow().clone() else {
                unreachable!()
            };
            state.asm.cmp(left_reg, right_reg, size);
            state.used_registers.remove(&left_reg);
            state.used_registers.remove(&right_reg);

            //todo load the reslts into registers to compare if they happen to be booleans

            match operator {
                ComparisonOperator::CompareEqual => ExpressionResult {
                    data_type: DataType::Boolean,
                    result_container: ResultContainer::Flag(Flag::EQUAL),
                },
                ComparisonOperator::CompareLarger => ExpressionResult {
                    data_type: DataType::Boolean,
                    result_container: ResultContainer::Flag(Flag::LARGER),
                },
                ComparisonOperator::CompareSmaller => ExpressionResult {
                    data_type: DataType::Boolean,
                    result_container: ResultContainer::Flag(Flag::SMALLER),
                },
            }
        }
        Expression::Dereference(expression) => {
            let result = compile_expression(state, compilation_state, expression, result_hint);
            let result = move_to_reg_if_needed(state, result);
            let ResultContainer::TempVariable(temp) = result.result_container else {
                unreachable!()
            };
            let TempVariable::Register(reg) = temp.borrow().clone() else {
                unreachable!()
            };

            if let DataType::Pointer(data_type) = result.data_type {
                state
                    .asm
                    .mov(reg, RegPointer { reg, offset: 0 }, Word::QWORD);
                ExpressionResult {
                    data_type: *data_type,
                    result_container: ResultContainer::TempVariable(temp),
                }
            } else {
                panic!("Cannot dereference a non pointer!");
            }
        }
        Expression::AddressOf(expression) => {
            let result = compile_expression(state, compilation_state, expression, None);

            if let ResultContainer::IdentifierWithOffset { identifier, offset } =
                result.result_container
            {
                let reg = force_get_any_free_register(state, &[], result_hint);
                let variable = state
                    .variables
                    .iter()
                    .rev()
                    .find(|var| var.identifier.eq(&identifier))
                    .expect(&fmt!("Undeclared variable: \"{}\"", identifier));

                state.asm.lea(
                    reg,
                    Memory::Reg(RegPointer {
                        reg: RBP,
                        offset: -(variable.stack_location - offset),
                    }),
                    Word::QWORD,
                );
                ExpressionResult {
                    data_type: DataType::Pointer(Box::new(variable.data_type.clone())),
                    result_container: ResultContainer::TempVariable(Rc::new(RefCell::new(
                        TempVariable::Register(reg),
                    ))),
                }
            } else {
                panic!("Cannot take address of an rvalue!");
            }
        }
        Expression::StringLiteral(text) => {
            let id: usize;
            if compilation_state.string_constants.contains_key(text) {
                id = *compilation_state
                    .string_constants
                    .get(text)
                    .expect("Missing string in the string_contants hash map. Should never happen.");
            } else {
                id = compilation_state.string_constants.len();
                compilation_state.string_constants.insert(text.clone(), id);
            }
            let reg = force_get_any_free_register(state, &[], result_hint);

            state
                .asm
                .lea(reg, Memory::Label(fmt!(".String{}", id)), Word::QWORD);
            ExpressionResult {
                data_type: DataType::Pointer(Box::new(DataType::Char)),
                result_container: ResultContainer::TempVariable(Rc::new(RefCell::new(
                    TempVariable::Register(reg),
                ))),
            }
        }
        Expression::FunctionCall(function_call) => {
            compile_function_call(compilation_state, state, function_call)
        }
        Expression::BoolLiteral(value) => {
            let reg = force_get_any_free_register(state, &[], result_hint);
            state.asm.mov(reg, *value as i32, Word::QWORD);
            ExpressionResult {
                data_type: DataType::Boolean,
                result_container: ResultContainer::TempVariable(Rc::new(RefCell::new(
                    TempVariable::Register(reg),
                ))),
            }
        }
        Expression::FloatLiteral(value) => {
            let id: usize;
            if compilation_state.float_constants.contains_key(value) {
                id = *compilation_state
                    .float_constants
                    .get(value)
                    .expect("Missing string in the float_constants hash map. Should never happen.");
            } else {
                id = compilation_state.float_constants.len();
                compilation_state.float_constants.insert(value.clone(), id);
            }
            state.asm.movss(XMM0, fmt!(".Float{}", id));
            ExpressionResult {
                data_type: DataType::Float,
                result_container: ResultContainer::FloatRegister,
            }
        }
        Expression::Assigment { left, right } => {
            let left_result = compile_expression(state, compilation_state, left, None);
            let right_result = compile_expression(state, compilation_state, right, None);
            compile_assigment(state, left_result, right_result)
        }
        Expression::MemberAccess { left, right } => {
            let left = compile_expression(state, compilation_state, left, None);

            if let DataType::Struct(left_identifier) = left.data_type {
                if let ResultContainer::IdentifierWithOffset { identifier, offset } =
                    left.result_container
                {
                    let struct_type = compilation_state
                        .struct_types
                        .get(&left_identifier)
                        .expect("Expected struct to exist.");

                    match (*right).as_ref() {
                        Expression::Identifier(right_identifier) => {
                            let member = struct_type
                                .members
                                .iter()
                                .find(|member| member.member.identifier.eq(right_identifier))
                                .expect("Expected member of struct to exist.");
                            ExpressionResult {
                                data_type: member.member.data_type.clone(),
                                result_container: ResultContainer::IdentifierWithOffset {
                                    identifier: identifier.clone(),
                                    offset: offset + member.offset,
                                },
                            }
                        }
                        Expression::ArraySubscript { .. } => todo!(),
                        Expression::FunctionCall(_) => todo!(),
                        _ => panic!("Invalid member access expression!"),
                    }
                } else {
                    panic!("Member access operator used on an rvalue!");
                }
            } else {
                panic!("Member access operator used on a non-struct variable!");
            }
        }
        Expression::Increment(expression) => {
            let result = compile_expression(state, compilation_state, expression, None);

            if let ExpressionResult {
                ref data_type,
                result_container:
                    ResultContainer::IdentifierWithOffset {
                        ref identifier,
                        offset,
                    },
            } = result
            {
                let variable = state
                    .variables
                    .iter()
                    .rev()
                    .find(|var| var.identifier.eq(identifier))
                    .expect(&fmt!("Undeclared variable: \"{}\"", &identifier));
                let stack_location = variable.stack_location.clone();

                if is_float(&data_type) {
                    todo!();
                } else {
                    state.asm.add(
                        RegPointer {
                            reg: RBP,
                            offset: -(stack_location - offset),
                        },
                        1,
                        sizeofword(&data_type),
                    );
                }

                result
            } else {
                panic!("Trying to increment an rvalue!");
            }
        }
        Expression::Decrement(expression) => {
            let result = compile_expression(state, compilation_state, expression, None);

            if let ExpressionResult {
                ref data_type,
                result_container:
                    ResultContainer::IdentifierWithOffset {
                        ref identifier,
                        offset,
                    },
            } = result
            {
                let variable = state
                    .variables
                    .iter()
                    .rev()
                    .find(|var| var.identifier.eq(identifier))
                    .expect(&fmt!("Undeclared variable: \"{}\"", &identifier));
                let stack_location = variable.stack_location.clone();

                if is_float(&data_type) {
                    todo!();
                } else {
                    state.asm.sub(
                        RegPointer {
                            reg: RBP,
                            offset: -(stack_location - offset),
                        },
                        1,
                        sizeofword(&data_type),
                    );
                }

                result
            } else {
                panic!("Trying to decrement an rvalue!");
            }
        }
        Expression::StructLiteral {
            identifier,
            members,
        } => {
            let mut results: Vec<(String, ExpressionResult)> = Vec::new();
            for (member, expression) in members {
                results.push((
                    member.clone(),
                    compile_expression(state, compilation_state, expression, None),
                ));
            }

            ExpressionResult {
                data_type: DataType::Struct(identifier.clone()),
                result_container: ResultContainer::StructLiteral {
                    identifier: identifier.clone(),
                    members: results,
                },
            }
        }
    }
}

fn compile_arithmetic_int_expression(state: &mut ScopeState<'_>, left_temp: Rc<RefCell<TempVariable>>, right: impl Operand, operator: &ArithmeticOperator) {
    let TempVariable::Register(left_reg) = left_temp.borrow().clone() else {
        unreachable!()
    };

    match operator {
        ArithmeticOperator::Add => state.asm.add(left_reg, right, Word::QWORD),
        ArithmeticOperator::Subtract => {
            state.asm.sub(left_reg, right, Word::QWORD)
        }
        ArithmeticOperator::Multiply => {
            state.asm.imul(left_reg, right, Word::QWORD)
        }
        ArithmeticOperator::Divide => {
            if left_reg != RAX {
                free_register(state, RAX, &[]); //TODO: protect
                                                                                 //right register
                state.asm.mov(RAX, left_reg, Word::QWORD);
                state.used_registers.remove(&left_reg);
                *left_temp.borrow_mut() = TempVariable::Register(RAX);
                state.used_registers.insert(RAX, left_temp.clone());
            }
            if state.used_registers.contains_key(&RDX) {
                free_register(state, RDX, &[]);
            }
            state.asm.cqo();
            state.asm.idiv(right, Word::QWORD); //TODO: possibly make sure right is reg
        }
    }
}

pub fn compile_assigment(
    state: &mut ScopeState,
    left_result: ExpressionResult,
    right_result: ExpressionResult,
) -> ExpressionResult {
    let right_result = move_to_reg_if_needed(state, right_result);

    if right_result.data_type != left_result.data_type {
        panic!("Cannot assing value of a different data type!");
    }

    if let ExpressionResult {
        data_type,
        result_container: ResultContainer::IdentifierWithOffset { identifier, offset },
    } = left_result {
        let variable = state
            .variables
            .iter()
            .rev()
            .find(|var| var.identifier.eq(&identifier))
            .expect(&fmt!("Undeclared variable: \"{}\"", &identifier));

        let stack_location = variable.stack_location.clone();

        /*
        if let ResultContainer::Register = *offset {
            if is_float(&data_type) {
                state.assembly.push_str(&format!("pop rbx\nmovd xmm0, ebx\nmov {} [rbp - {} + rax * {}], {}\n", sizeofword(&data_type), stack_location, size, reg_from_size(size, "rax")));
            } else {
                state.assembly.push_str(&format!("pop rbx\nmov {} [rbp - {} + rax * {}], {}\n", sizeofword(&data_type), stack_location, size, reg_from_size(size, "rax")));
            }
        } else {
            panic!();
        }*/

        if is_float(&data_type) {
            todo!();
            //state.assembly.push_str(&format!("movd eax, xmm0\nmov {} [rbp - {}], {}\n", sizeofword(&data_type), stack_location - offset, reg_from_size(size, )));
        } else {

            match right_result.result_container {
                ResultContainer::TempVariable(ref temp) => {
                    let TempVariable::Register(reg) = temp.borrow().clone() else {
                        unreachable!()
                    };
                    state.asm.mov(
                        RegPointer {
                            reg: RBP,
                            offset: -(stack_location - offset),
                        },
                        reg,
                        sizeofword(&data_type),
                    );
                },
                ResultContainer::ConstInt(value) => {
                    state.asm.mov(
                        RegPointer {
                            reg: RBP,
                            offset: -(stack_location - offset),
                        },
                        value,
                        sizeofword(&data_type),
                    );
                },
                ResultContainer::ConstChar(value) => {
                    state.asm.mov(
                        RegPointer {
                            reg: RBP,
                            offset: -(stack_location - offset),
                        },
                        &value,
                        sizeofword(&data_type),
                    );
                },
                _ => unreachable!()
            }
        }

        right_result
    } else {
        panic!("Trying to assign value to rvalue!");
    }
}

fn move_to_reg_if_needed(
    state: &mut ScopeState,
    mut expression_result: ExpressionResult,
) -> ExpressionResult {
    match expression_result.result_container {
        ResultContainer::TempVariable(ref temp) => {
                let temp = temp.clone();
                let temp = temp.borrow();
                match *temp {
                    TempVariable::Register(_) => expression_result,
                    TempVariable::Stack(_) => todo!(),
                }
            }
        ResultContainer::FloatRegister => todo!(),
        ResultContainer::Flag(_) => todo!(),
        ResultContainer::IdentifierWithOffset { identifier, offset } => {
                let variable = state
                    .variables
                    .iter()
                    .rev()
                    .find(|var| var.identifier.eq(&identifier))
                    .expect(&fmt!("Undeclared variable: \"{}\"", identifier));
                if let Some(reg) = get_any_free_register(state) {
                    state.asm.mov(
                        reg,
                        RegPointer {
                            reg: RBP,
                            offset: -(variable.stack_location - offset),
                        },
                        Word::QWORD,
                    );
                    let temp = Rc::new(RefCell::new(TempVariable::Register(reg)));
                    expression_result.result_container = ResultContainer::TempVariable(temp.clone());
                    state.used_registers.insert(reg, temp);
                    expression_result
                } else {
                    todo!();
                }
            }
        ResultContainer::StructLiteral {
                identifier: _,
                members: _,
            } => todo!(),
        ResultContainer::ConstInt(_) => expression_result,
        ResultContainer::ConstChar(_) => expression_result,
    }
}
