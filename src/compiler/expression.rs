use crate::tokenizer::DataType;
use std::cell::RefCell;
use std::rc::Rc;

use super::*;

pub fn compile_expression(state: & mut ScopeState, compilation_state: &mut CompilationState, expression: &Expression) -> ExpressionResult {
    match expression {
        Expression::IntLiteral(value) => {
            if let Some(reg) = get_free_register(state) {
                state.assembly.push_str(&format!("mov {}, {}\n", reg_from_size(8, reg), value));
                let reg_rc = Rc::new(RefCell::new(reg));
                state.used_registers.insert(reg, reg_rc.clone());
                ExpressionResult { data_type: DataType::Int, result_container: ResultContainer::Register(reg_rc) }
            } else {
                todo!()
            }
        },
        Expression::CharacterLiteral(c) => {
            if let Some(reg) = get_free_register(state) {
                state.assembly.push_str(&format!("mov {}, {}\n", reg_from_size(1, reg), c));
                let reg_rc = Rc::new(RefCell::new(reg));
                state.used_registers.insert(reg, reg_rc.clone());
                ExpressionResult { data_type: DataType::Char, result_container: ResultContainer::Register(reg_rc) }
            } else {
                todo!()
            }
        },
        Expression::Identifier(identifier) => {
            let variable = state.variables.iter().rev().find(|var| var.identifier.eq(identifier)).expect(&format!("Undeclared variable: \"{}\"", identifier));
            ExpressionResult { data_type: variable.data_type.clone(), result_container: ResultContainer::IdentifierWithOffset { identifier: identifier.clone(), offset: 0 } }
        },
        Expression::ArraySubscript { identifier, element } => {
            let variable = state.variables.iter().rev().find(|var| var.identifier.eq(identifier)).expect(&format!("Undeclared variable: \"{}\"", identifier));
            if let DataType::Array { data_type: _, size: _ } = variable.data_type.clone() {
                //let stack_location = variable.stack_location.clone();
                let result = compile_expression(state, compilation_state, element);
                if result.data_type != DataType::Int {
                    panic!("Array index must be an integer!");
                }
                todo!("Array subscripts");
            } else {
                panic!("{} is not an array type!", identifier);
            }
        },
        Expression::ArithmeticExpression { left, right, operator } => {
            let right_result = compile_expression(state, compilation_state, right);
            let right_result = move_to_reg_if_needed(state, right_result);
            let left_result = compile_expression(state, compilation_state, left);
            let left_result = move_to_reg_if_needed(state, left_result);

            if !left_result.data_type.eq(&right_result.data_type) {
                panic!("Cannot do arithmetic operations on values of different data types!");
            }

            match left_result.data_type {
                DataType::Int => {
                    let ResultContainer::Register(left_reg) = left_result.result_container else { unreachable!() };
                    let ResultContainer::Register(right_reg) = right_result.result_container else { unreachable!() };
                    match operator {
                        ArithmeticOperator::Add => state.assembly.push_str(&format!("add {}, {}\n",
                                reg_from_size(8, *left_reg.borrow()), reg_from_size(8, *right_reg.borrow()))),
                        ArithmeticOperator::Subtract => state.assembly.push_str(&format!("sub {}, {}\n",
                                reg_from_size(8, *left_reg.borrow()), reg_from_size(8, *right_reg.borrow()))),
                        //ArithmeticOperator::Multiply => state.assembly.push_str("imul rbx\n"),
                        ArithmeticOperator::Divide => {
                            if *left_reg.borrow() != RAX {
                                if let Some(other_rc) = state.used_registers.remove(&RAX) {
                                    if let Some(reg) = get_free_register(state) {
                                        state.assembly.push_str(&format!("mov {}, {}\n", reg_from_size(8, reg), reg_from_size(8, *other_rc.borrow())));
                                        *other_rc.borrow_mut() = reg;
                                        state.used_registers.insert(reg, other_rc);
                                    } else {
                                        todo!();
                                    }
                                }
                                state.assembly.push_str(&format!("mov {}, {}\n", reg_from_size(8, RAX), reg_from_size(8, *left_reg.borrow())));
                                state.used_registers.remove(&left_reg.borrow());
                                *left_reg.borrow_mut() = RAX;
                                state.used_registers.insert(RAX, left_reg.clone());
                            }
                            if let Some(other_rc) = state.used_registers.remove(&RDX) {
                                if let Some(reg) = get_free_register(state) {
                                    state.assembly.push_str(&format!("mov {}, {}\n", reg_from_size(8, reg), reg_from_size(8, *other_rc.borrow())));
                                    *other_rc.borrow_mut() = reg;
                                    state.used_registers.insert(reg, other_rc);
                                } else {
                                    todo!();
                                }
                            }
                            state.assembly.push_str(&format!("cqo\nidiv {}\n", reg_from_size(8, *right_reg.borrow())));
                        },
                        _ => todo!()
                    }
                    state.used_registers.remove(&right_reg.borrow());
                    ExpressionResult { data_type: DataType::Int, result_container: ResultContainer::Register(left_reg) }
                },
                DataType::Float => {
                    match operator {
                        ArithmeticOperator::Add => state.assembly.push_str("pop rax\nmovd xmm1, eax\naddss xmm0, xmm1\n"),
                        ArithmeticOperator::Subtract => state.assembly.push_str("pop rax\nmovd xmm1, eax\nsubss xmm0, xmm1\n"),
                        ArithmeticOperator::Multiply => state.assembly.push_str("pop rax\nmovd xmm1, eax\nmulss xmm0, xmm1\n"),
                        ArithmeticOperator::Divide => state.assembly.push_str("pop rax\nmovd xmm1, eax\ndivss xmm0, xmm1\n")
                    }
                    ExpressionResult { data_type: DataType::Float, result_container: ResultContainer::FloatRegister }
                },
                _ => panic!("Cannot do an arithmetic operation!")
            }
        },
        Expression::ComparisonExpression { left, right, operator } => {
            let right_result = compile_expression(state, compilation_state, right);
            let right_result = move_to_reg_if_needed(state, right_result);
            let left_result = compile_expression(state, compilation_state, left);
            let left_result = move_to_reg_if_needed(state, left_result);
            if right_result.data_type != left_result.data_type {
                panic!("Cannot compare different data types!");
            }
            let size = sizeof(&left_result.data_type, compilation_state);
            
            let ResultContainer::Register(left_reg) = left_result.result_container else { unreachable!() };
            let ResultContainer::Register(right_reg) = right_result.result_container else { unreachable!() };
            state.assembly.push_str(&format!("cmp {}, {}\n", reg_from_size(size, *left_reg.borrow()), reg_from_size(size, *right_reg.borrow())));
            state.used_registers.remove(&left_reg.borrow());
            state.used_registers.remove(&right_reg.borrow());

            //todo load the reslts into registers to compare if they happen to be booleans

            match operator {
                ComparisonOperator::CompareEqual => ExpressionResult { data_type: DataType::Boolean, result_container: ResultContainer::Flag(Flag::EQUAL) },
                ComparisonOperator::CompareLarger => ExpressionResult { data_type: DataType::Boolean, result_container: ResultContainer::Flag(Flag::LARGER) },
                ComparisonOperator::CompareSmaller => ExpressionResult { data_type: DataType::Boolean, result_container: ResultContainer::Flag(Flag::SMALLER) }
            }
        },
        Expression::Dereference(expression) => {
            let result = compile_expression(state, compilation_state, expression);
            let result = move_to_reg_if_needed(state, result);
            let ResultContainer::Register(reg) = result.result_container else { unreachable!() };
            if let DataType::Pointer(data_type) = result.data_type {
                let reg_name = reg_from_size(8, *reg.borrow());
                state.assembly.push_str(&format!("mov {reg_name}, QWORD [{reg_name}]\n"));
                ExpressionResult { data_type: *data_type, result_container: ResultContainer::Register(reg) }
            } else {
                panic!("Cannot dereference a non pointer!");
            }
        },
        Expression::AddressOf(expression) => {
            let result = compile_expression(state, compilation_state, expression);

            if let ResultContainer::IdentifierWithOffset { identifier, offset } = result.result_container {
                let variable = state.variables.iter().rev().find(|var| var.identifier.eq(&identifier)).expect(&format!("Undeclared variable: \"{}\"", identifier));
                if let Some(reg) = get_free_register(state) {
                    state.assembly.push_str(&format!("lea {}, [rbp - {}]\n", reg_from_size(8, reg), variable.stack_location - offset));
                    ExpressionResult { data_type: DataType::Pointer(Box::new(variable.data_type.clone())), result_container: ResultContainer::Register(Rc::new(RefCell::new(reg))) }
                } else {
                    todo!();
                }
            } else {
                panic!("Cannot take address of an rvalue!");
            }
        },
        Expression::StringLiteral(text) => {
            let id : usize;
            if compilation_state.string_constants.contains_key(text) {
                id = *compilation_state.string_constants.get(text).expect("Missing string in the string_contants hash map. Should never happen.");
            } else {
                id = compilation_state.string_constants.len();
                compilation_state.string_constants.insert(text.clone(), id);
            }
            if let Some(reg) = get_free_register(state) {
                state.assembly.push_str(&format!("lea {}, [.String{}]\n", reg_from_size(8, reg), id));
                ExpressionResult { data_type: DataType::Pointer(Box::new(DataType::Char)), result_container: ResultContainer::Register(Rc::new(RefCell::new(reg))) }
            } else {
                todo!();
            }
        },
        Expression::FunctionCall(function_call) => {
            compile_function_call(compilation_state, state, function_call)
        },
        Expression::BoolLiteral(value) => {
            if let Some(reg) = get_free_register(state) {
                state.assembly.push_str(&format!("mov {}, {}\n", reg_from_size(8, reg), *value as i32));
                ExpressionResult { data_type: DataType::Boolean, result_container: ResultContainer::Register(Rc::new(RefCell::new(reg))) }
            } else {
                todo!();
            }
        },
        Expression::FloatLiteral(value) => {
            let id : usize;
            if compilation_state.float_constants.contains_key(value) {
                id = *compilation_state.float_constants.get(value).expect("Missing string in the float_constants hash map. Should never happen.");
            } else {
                id = compilation_state.float_constants.len();
                compilation_state.float_constants.insert(value.clone(), id);
            }
            state.assembly.push_str(&format!("movss xmm0, DWORD [.Float{}]\n", id));
            ExpressionResult { data_type: DataType::Float, result_container: ResultContainer::FloatRegister }
        }
        Expression::Assigment { left, right } => {
            let left_result = compile_expression(state, compilation_state, left);
            let right_result = compile_expression(state, compilation_state, right);
            let right_result = move_to_reg_if_needed(state, right_result);
            let ResultContainer::Register(ref reg) = right_result.result_container else { unreachable!() };

            if right_result.data_type != left_result.data_type {
                panic!("Cannot assing value of a different data type!");
            }

            if let ExpressionResult { data_type, result_container: ResultContainer::IdentifierWithOffset { identifier, offset } } = left_result {
                let variable = state.variables.iter().rev().find(|var| var.identifier.eq(&identifier)).expect(&format!("Undeclared variable: \"{}\"", &identifier));

                let size = sizeof(&data_type, compilation_state);
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
                    state.assembly.push_str(&format!("mov {} [rbp - {}], {}\n", sizeofword(&data_type), stack_location - offset, reg_from_size(size, *reg.borrow())));
                }

                right_result
            } else {
                panic!("Trying to assign value to rvalue!");
            }
        }
        Expression::MemberAccess { left, right } => {
            let left = compile_expression(state, compilation_state, left);

            if let DataType::Struct(left_identifier) = left.data_type {
                if let ResultContainer::IdentifierWithOffset { identifier, offset } = left.result_container {
                    let struct_type = compilation_state.struct_types.get(&left_identifier).expect("Expected struct to exist.");

                    match (*right).as_ref() {
                        Expression::Identifier(right_identifier) => {
                            let member = struct_type.members.iter().find(|member| member.member.identifier.eq(right_identifier)).expect("Expected member of struct to exist.");
                            ExpressionResult { data_type: member.member.data_type.clone(), result_container: ResultContainer::IdentifierWithOffset { identifier: identifier.clone(), offset: offset + member.offset } }
                        },
                        Expression::ArraySubscript { .. } => todo!(),
                        Expression::FunctionCall(_) => todo!(),
                        _ => panic!("Invalid member access expression!")
                    }
                } else {
                    panic!("Member access operator used on an rvalue!");
                }
            } else {
                panic!("Member access operator used on a non-struct variable!");
            }
        }
    }
}

fn move_to_reg_if_needed(state: &mut ScopeState, mut expression_result: ExpressionResult) -> ExpressionResult {
    match expression_result.result_container {
        ResultContainer::Register(_) => return expression_result,
        ResultContainer::FloatRegister => todo!(),
        ResultContainer::Flag(_) => todo!(),
        ResultContainer::IdentifierWithOffset { identifier, offset } => {
            let variable = state.variables.iter().rev().find(|var| var.identifier.eq(&identifier)).expect(&format!("Undeclared variable: \"{}\"", identifier));
            if let Some(reg) = get_free_register(state) {
                state.assembly.push_str(&format!("mov {}, [rbp - {}]\n", reg_from_size(8, reg), variable.stack_location - offset));
                let reg_rc = Rc::new(RefCell::new(reg));
                expression_result.result_container = ResultContainer::Register(reg_rc.clone());
                state.used_registers.insert(reg, reg_rc);
                return expression_result;
            } else {
                todo!();
            }
        },
    }
}
