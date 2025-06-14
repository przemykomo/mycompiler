#![allow(warnings)]
use std::fmt::Display;
use std::rc::Rc;
use std::usize;

use itertools::Itertools;

use crate::ast::{
    ArithmeticOp, BinaryOp, BoolOp, Expression, ExpressionSpanned, Statement, UnaryOperator,
};
use crate::parser::Parser;

use crate::tokenizer::DataType;
use crate::tokenizer::Error;

struct List<'a, T: ToString>(&'a [T]);

impl<'a, T: ToString> Display for List<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.0.iter().map(|i| i.to_string()).join(", "))
    }
}

#[derive(Debug)]
pub enum Value {
    ImmediateInt(i64),
    ImmediateFloat(f32),
    ImmediateString(String),
    StructLiteral(Vec<Value>),
    ArrayAccess {
        var: Rc<Variable>,
        index: Box<Value>,
    },
    MemberAccess,
    Temporary(usize),
    Call(String, Vec<Value>),
}

#[derive(Debug)]
pub struct Variable {
    ident: String,
    data_type: DataType,
    initialized: bool,
}

pub struct IRGen<'a> {
    parser: &'a Parser<'a>,
    pub errors: Vec<Error>,
    pub functions: Vec<IRFunc>,
}

#[derive(Debug)]
pub struct IRFunc {
    pub ident: String,
    pub scope: Scope,
}

#[derive(Debug)]
enum IRType {
    I64,
    F32,
}

#[derive(Debug)]
pub struct TempVar {
    ir_type: IRType,
}

#[derive(Debug)]
pub struct Scope {
    pub vars: Vec<Rc<Variable>>,
    pub temps: Vec<TempVar>,
    pub instructions: Vec<Instruction>,
}

impl<'a> Scope {
    fn temp(&mut self, ir_type: IRType) -> usize {
        self.temps.push(TempVar { ir_type });
        self.temps.len() - 1
    }

    fn move_to_temp(&'a mut self, val: Value) -> usize {
        match val {
            Value::ImmediateInt(val) => {
                let temp = self.temp(IRType::I64);
                self.instructions.push(Instruction::LoadInt(temp, val));
                temp
            }
            Value::ImmediateFloat(_) => todo!(),
            Value::ImmediateString(_) => todo!(),
            Value::StructLiteral(values) => todo!(),
            Value::ArrayAccess { var, index } => todo!(),
            Value::MemberAccess => todo!(),
            Value::Temporary(temp_var) => temp_var,
            Value::Call(_, values) => todo!(),
        }
    }
}

#[derive(Debug)]
pub enum Instruction {
    ArithmeticInt {
        op: ArithmeticOp,
        lhs: usize,
        rhs: usize,
        result: usize,
    },
    ArithmeticFloat {
        op: ArithmeticOp,
        lhs: usize,
        rhs: usize,
        result: usize,
    },
    Comparison {
        op: BoolOp,
        lhs: usize,
        rhs: usize,
        result: usize,
    },
    LoadInt(usize, i64),
    LoadFloat(usize, f32),
    Call(String, Vec<usize>),
    Assign {
        lhs: usize,
        rhs: usize,
    },
    Return(Value),
}

impl<'a> IRGen<'a> {
    pub fn new(parser: &'a Parser<'a>) -> IRGen<'a> {
        IRGen {
            parser,
            errors: Vec::new(),
            functions: Vec::new(),
        }
    }

    pub fn generate_ir(&mut self) {
        //Since I don't know the structs alignments and the calling convention,
        //all operations in IR must operate on struct members, not the memory directly.
        for function in &self.parser.functions {
            let mut vars: Vec<Rc<Variable>> = Vec::new();
            for arg in &function.prototype.arguments {
                vars.push(Rc::new(Variable {
                    ident: arg.0.ident.clone(),
                    data_type: arg.1.clone(),
                    initialized: true,
                }));
            }

            let mut scope = Scope {
                vars,
                temps: Vec::new(),
                instructions: Vec::new(),
            };

            for statement in &function.body {
                self.compile_statement(statement, &mut scope, &function.prototype.return_type);
            }

            self.functions.push(IRFunc {
                ident: function.prototype.ident.ident.clone(),
                scope,
            });
        }
    }

    fn compile_statement(
        &mut self,
        statement: &Statement,
        scope: &mut Scope,
        return_type: &DataType,
    ) {
        match statement {
            Statement::If {
                expression,
                scope,
                else_scope,
            } => todo!(),
            Statement::Return(expr) => {
                if let Some((value, expr_type)) = self.compile_expression(expr, scope) {
                    if expr_type != *return_type {
                        self.errors.push(Error {
                            span: expr.span,
                            msg: format!(
                            "Mismatched types. Expected `{return_type:?}`, got `{expr_type:?}`."),
                        });
                    }
                    scope.instructions.push(Instruction::Return(value));
                }
            }
            Statement::Expression(expr) => {
                if let Some((Value::Call(ident, items), _)) = self.compile_expression(expr, scope) {
                    let instruction = Instruction::Call(
                        ident,
                        items
                            .into_iter()
                            .map(|val| scope.move_to_temp(val))
                            .collect(),
                    );
                    scope.instructions.push(instruction);
                }
            }
            Statement::While { expression, scope } => todo!(),
            Statement::For {
                inital_statement,
                condition_expr,
                iteration_expr,
                scope,
            } => todo!(),
            Statement::VariableDefinition {
                ident,
                expression,
                data_type,
            } => {
                let initialized = if let Some(expr) = expression {
                    if let Some((value, expr_type)) = self.compile_expression(expr, scope) {
                        if *data_type != expr_type {
                            self.errors.push(Error {
                                span: expr.span,
                                msg: format!(
                                "Mismatched types. Expected `{data_type:?}`, got `{expr_type:?}`."),
                            });
                        }
                    }
                    true
                } else {
                    false
                };

                scope.vars.push(Rc::new(Variable {
                    ident: ident.ident.clone(),
                    data_type: data_type.clone(),
                    initialized,
                }));
            }
        }
    }

    fn compile_expression<'b>(
        &mut self,
        expr: &ExpressionSpanned,
        scope: &mut Scope,
    ) -> Option<(Value, DataType)> {
        match &expr.expression {
            Expression::IntLiteral(val) => Some((Value::ImmediateInt(*val), DataType::I64)),
            Expression::CharacterLiteral(val) => {
                Some((Value::ImmediateInt(*val as i64), DataType::Char))
            }
            Expression::BoolLiteral(val) => {
                Some((Value::ImmediateInt(*val as i64), DataType::Boolean))
            }
            Expression::FloatLiteral(val) => {
                Some((Value::ImmediateFloat(val.clone()), DataType::F32))
            }
            Expression::StringLiteral(val) => Some((
                Value::ImmediateString(val.clone()),
                DataType::Array {
                    data_type: Box::new(DataType::Char),
                    size: val.len() as i32,
                },
            )),
            Expression::StructLiteral { ident, members } => {
                let Some(struct_dec) = self
                    .parser
                    .struct_declarations
                    .iter()
                    .find(|dec| dec.ident.ident == ident.ident)
                else {
                    self.errors.push(Error {
                        span: ident.span,
                        msg: format!("Cannot find a struct `{}`", ident.ident),
                    });
                    return None;
                };

                let mut values: Vec<Value> = Vec::new();

                for (i, (ident, expr)) in members.iter().enumerate() {
                    let m = struct_dec.members.get(i);
                    if let Some(m) = m {
                        if m.ident.ident != ident.ident {
                            self.errors.push(Error {
                                span: ident.span,
                                msg: format!("`{}` expected.", m.ident.ident),
                            });
                            return None;
                        }
                        if let Some(expr) = expr {
                            if let Some((value, data_type)) = self.compile_expression(expr, scope) {
                                if m.data_type != data_type {
                                    self.errors.push(Error {
                                        span: expr.span,
                                        msg: format!(
                                        "Mismatched types. Expected `{:?}`, got `{data_type:?}`.", m.data_type),
                                    });
                                    continue;
                                }
                                values.push(value);
                            }
                        }
                    } else {
                        self.errors.push(Error {
                            span: ident.span,
                            msg: format!("No such field `{}`.", ident.ident),
                        });
                        return None;
                    }
                }

                if struct_dec.members.len() > members.len() {
                    self.errors.push(Error {
                        span: ident.span,
                        msg: format!(
                            "Missing struct fields: `{}`",
                            List(&struct_dec.members[members.len()..])
                        ),
                    });

                    return None;
                }

                Some((
                    Value::StructLiteral(values),
                    DataType::Struct(struct_dec.ident.clone()),
                ))
            }
            Expression::FunctionCall(call) => {
                let Some(func) = self
                    .parser
                    .function_declarations
                    .iter()
                    .find(|dec| dec.ident.ident == call.ident.ident)
                    .or_else(|| {
                        self.parser
                            .functions
                            .iter()
                            .find(|dec| dec.prototype.ident.ident == call.ident.ident)
                            .map(|dec| &dec.prototype)
                    })
                else {
                    self.errors.push(Error {
                        span: call.ident.span,
                        msg: format!("Cannot find a function `{}`", call.ident.ident),
                    });
                    return None;
                };

                let mut values: Vec<Value> = Vec::new();
                for (i, expr) in call.arguments.iter().enumerate() {
                    let arg = func.arguments.get(i);
                    let expr_result = self.compile_expression(expr, scope);
                    if let (Some((_ident, arg_type)), Some((value, data_type))) = (arg, expr_result)
                    {
                        if *arg_type != data_type {
                            self.errors.push(Error {
                                span: expr.span,
                                msg: format!(
                                    "Mismatched types. Expected `{arg_type:?}`, got `{data_type:?}`."
                                ),
                            });
                        } else {
                            values.push(value);
                        }
                    }
                }

                if func.arguments.len() != call.arguments.len() {
                    self.errors.push(Error {
                        span: expr.span,
                        msg: format!(
                            "Expected {} arguments, got {}.",
                            func.arguments.len(),
                            call.arguments.len()
                        ),
                    });
                    return None;
                }

                Some((
                    Value::Call(call.ident.ident.clone(), values),
                    func.return_type.clone(),
                ))
            }
            Expression::Identifier(ident) => {
                if let Some(var) = scope.vars.iter().rev().find(|var| var.ident == ident.ident) {
                    Some((
                        Value::ArrayAccess {
                            var: var.clone(),
                            index: Box::new(Value::ImmediateInt(0)),
                        },
                        var.data_type.clone(),
                    ))
                } else {
                    self.errors.push(Error {
                        span: ident.span,
                        msg: format!("Undefined variable `{}`", ident.ident),
                    });
                    None
                }
            }
            Expression::ArraySubscript { ident, element } => {
                let element = self.compile_expression(element, scope);
                if let Some(var) = scope.vars.iter().rev().find(|var| var.ident == ident.ident) {
                    let DataType::Array { data_type, size } = &var.data_type else {
                        return None;
                    };

                    if let Some((value, data_type)) = element {
                        Some((
                            Value::ArrayAccess {
                                var: var.clone(),
                                index: Box::new(value),
                            },
                            data_type.clone(),
                        ))
                    } else {
                        None
                    }
                } else {
                    self.errors.push(Error {
                        span: ident.span,
                        msg: format!("Undefined variable `{}`", ident.ident),
                    });
                    None
                }
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
                        let lhs = scope.move_to_temp(left.0);
                        let rhs = scope.move_to_temp(right.0);
                        let result = scope.temp(IRType::I64);
                        match op {
                            BinaryOp::Arithmetic(op) => {
                                scope.instructions.push(Instruction::ArithmeticInt {
                                    op: *op,
                                    lhs,
                                    rhs,
                                    result,
                                });
                            }
                            BinaryOp::Bool(op) => {
                                scope.instructions.push(Instruction::Comparison {
                                    op: *op,
                                    lhs,
                                    rhs,
                                    result,
                                });
                            }
                            BinaryOp::Assign => {
                                scope.instructions.push(Instruction::Assign { lhs, rhs });
                            }
                            BinaryOp::MemberAccess => todo!(),
                        }
                        return Some((Value::Temporary(result), DataType::I64));
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
                // let (value, data_type) = self.compile_expression(expr, vars);
                match operator {
                    UnaryOperator::Dereference => Some(todo!()),
                    UnaryOperator::AddressOf => Some(todo!()),
                    UnaryOperator::LogicalNot => Some(todo!()),
                    UnaryOperator::Negation => Some(todo!()),
                }
            }
        }
    }
}
