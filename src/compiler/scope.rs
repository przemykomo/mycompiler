use crate::tokenizer::DataType;

use expression::compile_expression;

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

            if let Some(expression) = expression {
                let result = compile_expression(state, compilation_state, &expression, None);
                if result.data_type != *data_type {
                    panic!("Variable {} doesn't have the same type as the expression!", identifier);
                }
                match result.result_container {
                    ResultContainer::TempVariable(ref temp) => {
                        match *temp.borrow() {
                            TempVariable::Register(reg) => {
                                state.assembly.push_str(&fmt!("mov {} [rbp - {}], {}\n",
                                    sizeofword(data_type), state.stack_size_current, reg_from_size(size, reg)));
                                state.used_registers.remove(&reg);
                            },
                            TempVariable::Stack(_stack_location) => {
                                todo!()
                            }
                        }
                    },
                    ResultContainer::FloatRegister => {
                        state.assembly.push_str(&fmt!("movss {} [rbp - {}], xmm0\n", sizeofword(data_type), state.stack_size_current));
                    },
                    ResultContainer::Flag(ref flag) => {
                        let reg = force_get_any_free_register(state, &[], None);
                        let set_instruction = set_instruction_from_flag(&flag);
                        let reg_str = reg_from_size(size, reg);
                        state.assembly.push_str(&fmt!("{} {}\nmov {} [rbp - {}], {}\n", set_instruction, reg_str, sizeofword(data_type), state.stack_size_current, reg_str));
                    },
                    ResultContainer::IdentifierWithOffset { identifier: _, offset: _ } => {}
                }
            }
            state.variables.push(Variable { identifier: identifier.clone(), stack_location: state.stack_size_current, data_type: data_type.clone() });
        }
        Statement::Return(expression) => {
            let result = compile_expression(state, compilation_state, expression, Some(RAX));
            
            if result.data_type != state.current_function.prototype.return_type {
                panic!("Trying to return a different data type than in the function declaration!");
            }

            state.assembly.push_str(&fmt!("jmp .{}.end\n", state.current_function.prototype.name));
        },
        Statement::If { expression, scope, else_scope } => {
            let result = compile_expression(state, compilation_state, expression, None);
            if result.data_type != DataType::Boolean {
                panic!("If statement condition has to be a boolean!");
            }

            if let ResultContainer::Flag(flag) = result.result_container {
                let label_id = compilation_state.unique_label_id;
                compilation_state.unique_label_id += 1;
                state.assembly.push_str(&fmt!("{} .L{}\n", jump_instruction_from_negated_flag(&flag), label_id));
            
                let variables_len = state.variables.len();
                let mut scope_state = ScopeState {
                    iter: scope.iter().peekable(),
                    stack_size_current: state.stack_size_current,
                    max_stack_size: state.stack_size_current,
                    variables: state.variables,
                    current_function: state.current_function,
                    assembly: String::new(),
                    used_registers: state.used_registers.clone()
                };
                compile_scope(&mut scope_state, compilation_state);
                state.max_stack_size = state.max_stack_size.max(scope_state.max_stack_size);
                state.assembly.push_str(&scope_state.assembly);
                state.variables.truncate(variables_len);

                if let Some(else_scope) = else_scope {
                    let label_id = compilation_state.unique_label_id;
                    compilation_state.unique_label_id += 1;
                    state.assembly.push_str(&fmt!("jmp .L{}\n.L{}:\n", label_id, label_id - 1));
                
                    let variables_len = state.variables.len();
                    let mut scope_state = ScopeState {
                        iter: else_scope.iter().peekable(),
                        stack_size_current: state.stack_size_current,
                        max_stack_size: state.stack_size_current,
                        current_function: state.current_function,
                        variables: state.variables,
                        assembly: String::new(),
                        used_registers: state.used_registers.clone()
                    };
                    compile_scope(&mut scope_state, compilation_state);
                    state.max_stack_size = state.max_stack_size.max(scope_state.max_stack_size);
                    
                    state.assembly.push_str(&scope_state.assembly);
                    state.assembly.push_str(&fmt!(".L{}:\n", label_id));
                    state.variables.truncate(variables_len);
                } else {
                    state.assembly.push_str(&fmt!(".L{}:\n", label_id));
                }

            } else {
                todo!();
            }
        },
        Statement::While { expression, scope } => {
            let begin_label_id = compilation_state.unique_label_id;
            compilation_state.unique_label_id += 1;
            state.assembly.push_str(&fmt!(".L{}:\n", begin_label_id));

            let result = compile_expression(state, compilation_state, expression, None);

            if result.data_type != DataType::Boolean {
                panic!("While statement condition has to be a boolean!");
            }

            if let ResultContainer::Flag(flag) = result.result_container {
                let end_label_id = compilation_state.unique_label_id;
                compilation_state.unique_label_id += 1;
                state.assembly.push_str(&fmt!("{} .L{}\n", jump_instruction_from_negated_flag(&flag), end_label_id));

                let variables_len = state.variables.len();
                let mut scope_state = ScopeState {
                    iter: scope.iter().peekable(),
                    stack_size_current: state.stack_size_current,
                    max_stack_size: state.stack_size_current,
                    variables: state.variables,
                    current_function: state.current_function,
                    assembly: String::new(),
                    used_registers: state.used_registers.clone()
                };
                compile_scope(&mut scope_state, compilation_state);
                state.max_stack_size = state.max_stack_size.max(scope_state.max_stack_size);
                state.assembly.push_str(&scope_state.assembly);
                state.assembly.push_str(&fmt!("jmp .L{}\n.L{}:\n", begin_label_id, end_label_id));
                state.variables.truncate(variables_len);
            } else {
                todo!();
            }
        },
        Statement::For { inital_statement, condition_expr, iteration_expr, scope } => {
            let variables_len = state.variables.len();

            let begin_label_id = compilation_state.unique_label_id;
            compilation_state.unique_label_id += 1;
            state.assembly.push_str(&fmt!(".L{}:\n", begin_label_id));

            compile_statement(state, compilation_state, inital_statement);
            let mut scope_state = ScopeState {
                iter: scope.iter().peekable(),
                stack_size_current: state.stack_size_current,
                max_stack_size: state.stack_size_current,
                variables: state.variables,
                current_function: state.current_function,
                assembly: String::new(),
                used_registers: state.used_registers.clone()
            };
            let result = compile_expression(&mut scope_state, compilation_state, condition_expr, None);

            if result.data_type != DataType::Boolean {
                panic!("While statement condition has to be a boolean!");
            }

            if let ResultContainer::Flag(flag) = result.result_container {
                let end_label_id = compilation_state.unique_label_id;
                compilation_state.unique_label_id += 1;
                state.assembly.push_str(&fmt!("{} .L{}\n", jump_instruction_from_negated_flag(&flag), end_label_id));

                compile_scope(&mut scope_state, compilation_state);
                let _ = compile_expression(&mut scope_state, compilation_state, iteration_expr, None);
                state.max_stack_size = state.max_stack_size.max(scope_state.max_stack_size);
                state.assembly.push_str(&scope_state.assembly);
                state.assembly.push_str(&fmt!("jmp .L{}\n.L{}:\n", begin_label_id, end_label_id));
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
