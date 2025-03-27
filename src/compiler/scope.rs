use crate::tokenizer::DataType;

use expression::*;

use super::*;

pub fn compile_scope(state: &mut ScopeState, compilation_state: &mut CompilationState) {
    while let Some(statement) = state.iter.next() {
        compile_statement(state, compilation_state, statement);
    }
    state.max_stack_size = state.max_stack_size.max(state.stack_size_current);
}

pub fn compile_statement(state: &mut ScopeState, compilation_state: &mut CompilationState, statement: &Statement) {
    match statement {
        Statement::VariableDefinition { identifier, expression, data_type } => {
            let size = if let DataType::Array { data_type, size } = data_type {
                sizeof(data_type, compilation_state) * size
            } else {
                sizeof(data_type, compilation_state)
            };

            state.stack_size_current = ((state.stack_size_current + size - 1) / size) * size + size;
            state.variables.push(Variable { identifier: identifier.clone(), stack_location: state.stack_size_current, data_type: data_type.clone() });

            if let Some(expression) = expression {
                let result = compile_expression(state, compilation_state, &expression, None);
                if result.data_type != *data_type {
                    panic!("Variable {} doesn't have the same type as the expression!", identifier);
                }
                match result.result_container {
                    ResultContainer::TempVariable(ref temp) => {
                        match *temp.borrow() {
                            TempVariable::Register(reg) => {
                                state.asm.mov(RegPointer { reg: RBP, offset: -state.stack_size_current }, reg, sizeofword(data_type));
                                state.used_registers.remove(&reg);
                            },
                            TempVariable::Stack(_stack_location) => {
                                todo!()
                            }
                        }
                    },
                    ResultContainer::FloatRegister => {
                        state.asm.movss(RegPointer { reg: RBP, offset: -state.stack_size_current }, FloatRegister::XMM0);
                    },
                    ResultContainer::Flag(ref flag) => {
                        let reg = force_get_any_free_register(state, &[], None);
                        state.asm.set(flag, reg);
                        state.asm.mov(RegPointer { reg: RBP, offset: -state.stack_size_current }, reg, Word::BYTE);
                    },
                    ResultContainer::IdentifierWithOffset { identifier: _, offset: _ } => {},
                    ResultContainer::StructLiteral { identifier: _, members } => {
                        for (member, result) in members {
                            let left_result = compile_expression(state, compilation_state, &Expression::MemberAccess {
                                left: Box::new(Expression::Identifier(identifier.clone())),
                                right: Box::new(Expression::Identifier(member))
                            }, None);
                            compile_assigment(state, left_result, result);
                        }
                    }
                }
            }
        }
        Statement::Return(expression) => {
            let result = compile_expression(state, compilation_state, expression, Some(RAX));
            
            if result.data_type != state.current_function.prototype.return_type {
                panic!("Trying to return a different data type than in the function declaration!");
            }

            state.asm.jmp(&fmt!(".{}.end", state.current_function.prototype.name));
        },
        Statement::If { expression, scope, else_scope } => {
            let result = compile_expression(state, compilation_state, expression, None);
            if result.data_type != DataType::Boolean {
                panic!("If statement condition has to be a boolean!");
            }

            if let ResultContainer::Flag(flag) = result.result_container {
                let label_id = compilation_state.unique_label_id;
                compilation_state.unique_label_id += 1;
                state.asm.jmp_negated_flag(flag, &fmt!(".L{}", label_id));
            
                let variables_len = state.variables.len();
                let mut scope_state = ScopeState {
                    iter: scope.iter().peekable(),
                    stack_size_current: state.stack_size_current,
                    max_stack_size: state.stack_size_current,
                    variables: state.variables,
                    current_function: state.current_function,
                    asm: Asm::new(),
                    used_registers: state.used_registers.clone()
                };
                compile_scope(&mut scope_state, compilation_state);
                state.max_stack_size = state.max_stack_size.max(scope_state.max_stack_size);
                state.asm.append(scope_state.asm);
                state.variables.truncate(variables_len);

                if let Some(else_scope) = else_scope {
                    let label_id = compilation_state.unique_label_id;
                    compilation_state.unique_label_id += 1;
                    state.asm.jmp(&fmt!(".L{}", label_id));
                    state.asm.label(&(label_id - 1).to_string());
                
                    let variables_len = state.variables.len();
                    let mut scope_state = ScopeState {
                        iter: else_scope.iter().peekable(),
                        stack_size_current: state.stack_size_current,
                        max_stack_size: state.stack_size_current,
                        current_function: state.current_function,
                        variables: state.variables,
                        asm: Asm::new(),
                        used_registers: state.used_registers.clone()
                    };
                    compile_scope(&mut scope_state, compilation_state);
                    state.max_stack_size = state.max_stack_size.max(scope_state.max_stack_size);
                    
                    state.asm.append(scope_state.asm);
                    state.asm.label(&fmt!(".L{}", label_id));
                    state.variables.truncate(variables_len);
                } else {
                    state.asm.label(&fmt!(".L{}", label_id));
                }

            } else {
                todo!();
            }
        },
        Statement::While { expression, scope } => {
            let begin_label_id = compilation_state.unique_label_id;
            compilation_state.unique_label_id += 1;
            state.asm.label(&fmt!(".L{}", begin_label_id));

            let result = compile_expression(state, compilation_state, expression, None);

            if result.data_type != DataType::Boolean {
                panic!("While statement condition has to be a boolean!");
            }

            if let ResultContainer::Flag(flag) = result.result_container {
                let end_label_id = compilation_state.unique_label_id;
                compilation_state.unique_label_id += 1;
                state.asm.jmp_negated_flag(flag, &fmt!(".L{}", end_label_id));

                let variables_len = state.variables.len();
                let mut scope_state = ScopeState {
                    iter: scope.iter().peekable(),
                    stack_size_current: state.stack_size_current,
                    max_stack_size: state.stack_size_current,
                    variables: state.variables,
                    current_function: state.current_function,
                    asm: Asm::new(),
                    used_registers: state.used_registers.clone()
                };
                compile_scope(&mut scope_state, compilation_state);
                state.max_stack_size = state.max_stack_size.max(scope_state.max_stack_size);
                state.asm.append(scope_state.asm);
                state.asm.jmp(&fmt!(".L{}", begin_label_id));
                state.asm.label(&fmt!(".L{}", end_label_id));
                state.variables.truncate(variables_len);
            } else {
                todo!();
            }
        },
        Statement::For { inital_statement, condition_expr, iteration_expr, scope } => {
            let variables_len = state.variables.len();

            let begin_label_id = compilation_state.unique_label_id;
            compilation_state.unique_label_id += 1;
            state.asm.label(&fmt!(".L{}", begin_label_id));

            compile_statement(state, compilation_state, inital_statement);
            let mut scope_state = ScopeState {
                iter: scope.iter().peekable(),
                stack_size_current: state.stack_size_current,
                max_stack_size: state.stack_size_current,
                variables: state.variables,
                current_function: state.current_function,
                asm: Asm::new(),
                used_registers: state.used_registers.clone()
            };
            let result = compile_expression(&mut scope_state, compilation_state, condition_expr, None);

            if result.data_type != DataType::Boolean {
                panic!("While statement condition has to be a boolean!");
            }

            if let ResultContainer::Flag(flag) = result.result_container {
                let end_label_id = compilation_state.unique_label_id;
                compilation_state.unique_label_id += 1;
                state.asm.jmp_negated_flag(flag, &fmt!(".L{}", end_label_id));

                compile_scope(&mut scope_state, compilation_state);
                let _ = compile_expression(&mut scope_state, compilation_state, iteration_expr, None);
                state.max_stack_size = state.max_stack_size.max(scope_state.max_stack_size);
                state.asm.append(scope_state.asm);
                state.asm.jmp(&fmt!(".L{}", begin_label_id));
                state.asm.label(&fmt!(".L{}", end_label_id));
                state.variables.truncate(variables_len);
            } else {
                todo!();
            }
        },
        Statement::Expression(expression) => {
            let _ = compile_expression(state, compilation_state, expression, None);
        }
    }
}
