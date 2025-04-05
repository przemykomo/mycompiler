use crate::tokenizer::*;
use itertools::Itertools;
use std::backtrace::Backtrace;
use std::iter::{Enumerate, Peekable};
use std::slice::Iter;
use std::usize::{self};

#[derive(Debug)]
pub struct ExpressionWithPos {
    pub expression: Expression,
    pub pos: TokenPos,
}

#[derive(Debug)]
pub enum Expression {
    IntLiteral(i32),
    CharacterLiteral(char),
    BoolLiteral(bool),
    FloatLiteral(String),
    StringLiteral(String),
    StructLiteral {
        identifier: String,
        members: Vec<(String, Option<ExpressionWithPos>)>,
    },
    Identifier(String),
    ArithmeticExpression {
        left: Box<ExpressionWithPos>,
        right: Box<ExpressionWithPos>,
        operator: ArithmeticOperator,
    },
    ComparisonExpression {
        left: Box<ExpressionWithPos>,
        right: Box<ExpressionWithPos>,
        operator: ComparisonOperator,
    },
    Dereference(Box<ExpressionWithPos>),
    AddressOf(Box<ExpressionWithPos>),
    ArraySubscript {
        identifier: String,
        element: Box<ExpressionWithPos>,
    },
    FunctionCall(FunctionCall),
    Assigment {
        left: Box<ExpressionWithPos>,
        right: Box<ExpressionWithPos>,
    },
    MemberAccess {
        left: Box<ExpressionWithPos>,
        right: Box<ExpressionWithPos>,
    },
    Increment(Box<ExpressionWithPos>),
    Decrement(Box<ExpressionWithPos>),
}

#[derive(Debug)]
pub struct StructDeclaration {
    pub identifier: String,
    pub members: Vec<StructMember>,
}

#[derive(Debug, Clone)]
pub struct StructMember {
    pub identifier: String,
    pub data_type: DataType,
}

#[derive(Debug)]
pub enum ArithmeticOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug)]
pub enum ComparisonOperator {
    CompareEqual,
    CompareLarger,
    CompareSmaller,
}

#[derive(Debug)]
pub enum Statement {
    If {
        expression: Option<ExpressionWithPos>,
        scope: Vec<Statement>,
        else_scope: Option<Vec<Statement>>,
    },
    Return(ExpressionWithPos),
    Expression(ExpressionWithPos),
    While {
        expression: Option<ExpressionWithPos>,
        scope: Vec<Statement>,
    },
    For {
        inital_statement: Box<Statement>,
        condition_expr: ExpressionWithPos,
        iteration_expr: ExpressionWithPos,
        scope: Vec<Statement>,
    },
    VariableDefinition {
        identifier: String,
        expression: Option<Box<ExpressionWithPos>>,
        data_type: DataType,
    },
}

#[derive(Debug)]
pub struct FunctionCall {
    pub identifier: String,
    pub arguments: Vec<ExpressionWithPos>,
    pub pos: TokenPos,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub prototype: FunctionPrototype,
    pub public: bool,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct FunctionPrototype {
    pub name: String,
    pub return_type: DataType,
    pub arguments: Vec<DataType>,
}

#[derive(Debug)]
pub struct ParsedUnit {
    pub function_declarations: Vec<FunctionPrototype>,
    pub functions: Vec<FunctionDefinition>,
    pub struct_declarations: Vec<StructDeclaration>,
    pub errors: Vec<Error>,
}

pub fn parse_arguments_declaration(
    iter: &mut Peekable<Enumerate<Iter<Token>>>,
    errors: &mut Vec<Error>,
    tokenized_file: &TokenizedFile,
) -> Vec<DataType> {
    let mut arguments = Vec::<DataType>::new();

    loop {
        match iter.next() {
            Some((i, Token::DataType(data_type))) => match iter.next() {
                Some((_, Token::Identifier(_))) => {
                    arguments.push(data_type.clone());
                }
                Some((i, Token::MultiplySign)) => match iter.next() {
                    Some((_, Token::Identifier(_))) => {
                        arguments.push(DataType::Pointer(Box::new(data_type.clone())));
                    }
                    Some((i, token)) => {
                        errors.push(Error {
                            pos: tokenized_file.positions[i],
                            msg: format!("Unexpected token `{:?}` in the function arguments, expected an indentifier.", token)
                        });
                    }
                    None => {
                        errors.push(Error {
                            pos: tokenized_file.positions[i],
                            msg: "Reached the end of file during parsing function arguments."
                                .to_string(),
                        });
                    }
                },
                Some((i, token)) => {
                    errors.push(Error {
                            pos: tokenized_file.positions[i],
                            msg: format!("Unexpected token `{:?}` in the function arguments, expected an indentifier or `*`.", token)
                        });
                }
                None => {
                    errors.push(Error {
                        pos: tokenized_file.positions[i],
                        msg: "Reached the end of file during parsing function arguments."
                            .to_string(),
                    });
                }
            },
            Some((i, Token::Identifier(identifier))) => match iter.next() {
                Some((_, Token::Identifier(_))) => {
                    arguments.push(DataType::Struct(identifier.clone()));
                }
                Some((i, Token::MultiplySign)) => match iter.next() {
                    Some((_, Token::Identifier(_))) => {
                        arguments.push(DataType::Pointer(Box::new(DataType::Struct(
                            identifier.clone(),
                        ))));
                    }
                    Some((i, token)) => {
                        errors.push(Error {
                            pos: tokenized_file.positions[i],
                            msg: format!("Unexpected token `{:?}` in the function arguments, expected an indentifier.", token)
                        });
                    }
                    None => {
                        errors.push(Error {
                            pos: tokenized_file.positions[i],
                            msg: "Reached the end of file during parsing function arguments."
                                .to_string(),
                        });
                    }
                },
                Some((i, token)) => {
                    errors.push(Error {
                            pos: tokenized_file.positions[i],
                            msg: format!("Unexpected token `{:?}` in the function arguments, expected an indentifier or `*`.", token)
                        });
                }
                None => {
                    errors.push(Error {
                        pos: tokenized_file.positions[i],
                        msg: "Reached the end of file during parsing function arguments."
                            .to_string(),
                    });
                }
            },
            Some((i, token)) => {
                errors.push(Error {
                    pos: tokenized_file.positions[i],
                    msg: format!(
                        "Unexpected token `{:?}` in the function arguments, expected a data type.",
                        token
                    ),
                });
            }
            None => {
                errors.push(Error {
                    pos: TokenPos {
                        pos: usize::MAX,
                        line: usize::MAX,
                        column: usize::MAX,
                    },
                    msg: "Reached the end of file during parsing function arguments.".to_string(),
                });
            }
        }

        if let Some((_, Token::Coma)) = iter.peek() {
            iter.next();
        } else {
            break;
        }
    }

    return arguments;
}

pub fn parse_arguments_passing(
    iter: &mut Peekable<Enumerate<Iter<Token>>>,
    errors: &mut Vec<Error>,
    tokenized_file: &TokenizedFile,
) -> Vec<ExpressionWithPos> {
    let mut arguments = Vec::<ExpressionWithPos>::new();

    loop {
        let i = iter.peek().unwrap().0.clone(); // I don't even know, I gotta get rid of
                                                // this system
        if let Some(expression) = parse_expression(iter, errors, tokenized_file) {
            arguments.push(ExpressionWithPos {
                expression,
                pos: tokenized_file.positions[i],
            });
        } else {
            break;
        }

        if let Some((_, Token::Coma)) = iter.peek() {
            iter.next();
        } else {
            break;
        }
    }

    return arguments;
}

pub fn parse(tokenized_file: &TokenizedFile) -> ParsedUnit {
    let mut parsed_unit = ParsedUnit {
        function_declarations: Vec::new(),
        functions: Vec::new(),
        struct_declarations: Vec::new(),
        errors: Vec::new(),
    };
    let mut iter = tokenized_file.tokens.iter().enumerate().peekable();

    while let Some((i, token)) = iter.next() {
        match token {
            Token::Struct => {
                if let Some(((_, Token::Identifier(identifier)), (_, Token::CurlyBracketOpen))) =
                    iter.next_tuple()
                {
                    let mut members: Vec<StructMember> = Vec::new();
                    while let Some((
                        (_, Token::DataType(data_type)),
                        (i, Token::Identifier(member_name)),
                    )) = iter.clone().next_tuple()
                    {
                        iter.next();
                        iter.next();

                        members.push(StructMember {
                            identifier: member_name.clone(),
                            data_type: data_type.clone(),
                        });
                        match iter.next() {
                            Some((_, Token::Coma)) => {}
                            Some((_, Token::CurlyBracketClose)) => {
                                break;
                            }
                            Some((i, token)) => {
                                parsed_unit.errors.push(Error {
                                    pos: tokenized_file.positions[i],
                                    msg: format!("Unexpected `{:?}`.", token),
                                });
                            }
                            None => {
                                parsed_unit.errors.push(Error {
                                    pos: tokenized_file.positions[i],
                                    msg: format!(
                                        "Reached the end of file in a middle of struct definition."
                                    ),
                                });
                            }
                        }
                    }

                    parsed_unit.struct_declarations.push(StructDeclaration {
                        identifier: identifier.clone(),
                        members,
                    })
                } else {
                    parsed_unit.errors.push(Error {
                        pos: tokenized_file.positions[i],
                        msg: "Expected struct declaration after the struct token!".to_string(),
                    });
                }
            }
            Token::DataType(return_type) => {
                if let Some(((_, Token::Identifier(identifier)), (_, Token::ParenthesisOpen))) =
                    iter.next_tuple()
                {
                    let arguments = if let Some((_, Token::ParenthesisClose)) = iter.peek() {
                        Vec::new()
                    } else {
                        parse_arguments_declaration(
                            &mut iter,
                            &mut parsed_unit.errors,
                            tokenized_file,
                        )
                    };

                    if let Some((_, Token::ParenthesisClose)) = iter.next() {
                        let function_prototype = FunctionPrototype {
                            name: identifier.clone(),
                            return_type: return_type.clone(),
                            arguments,
                        };
                        match iter.next() {
                            Some((_, Token::Semicolon)) => {
                                parsed_unit.function_declarations.push(function_prototype)
                            }
                            Some((_, Token::CurlyBracketOpen)) => {
                                let function = FunctionDefinition {
                                    prototype: function_prototype,
                                    public: false,
                                    body: parse_scope(
                                        &mut iter,
                                        &mut parsed_unit.errors,
                                        tokenized_file,
                                    ),
                                };
                                parsed_unit.functions.push(function);
                            }
                            Some((i, token)) => {
                                parsed_unit.errors.push(Error {
                                    pos: tokenized_file.positions[i],
                                    msg: format!("Unexpected `{:?}`. Expected `;` or `{{`.", token),
                                });
                            }
                            None => {
                                parsed_unit.errors.push(Error {
                                    pos: tokenized_file.positions[i],
                                    msg: "Expected `;` or `{{`.".to_string(),
                                });
                            }
                        }
                    } else {
                        parsed_unit.errors.push(Error {
                            pos: tokenized_file.positions[i],
                            msg: "Expected `)`.".to_string(),
                        });
                    }
                } else {
                    parsed_unit.errors.push(Error {
                        pos: tokenized_file.positions[i],
                        msg: "Expected function prototype.".to_string(),
                    });
                }
            }
            Token::Public => {
                if let Some((
                    (_, Token::DataType(return_type)),
                    (_, Token::Identifier(identifier)),
                    (_, Token::ParenthesisOpen),
                )) = iter.next_tuple()
                {
                    let arguments = if let Some((_, Token::ParenthesisClose)) = iter.peek() {
                        Vec::new()
                    } else {
                        parse_arguments_declaration(
                            &mut iter,
                            &mut parsed_unit.errors,
                            tokenized_file,
                        )
                    };

                    if let Some(((_, Token::ParenthesisClose), (_, Token::CurlyBracketOpen))) =
                        iter.next_tuple()
                    {
                        let function_prototype = FunctionPrototype {
                            name: identifier.clone(),
                            return_type: return_type.clone(),
                            arguments,
                        };

                        let function = FunctionDefinition {
                            prototype: function_prototype,
                            public: true,
                            body: parse_scope(&mut iter, &mut parsed_unit.errors, tokenized_file),
                        };
                        parsed_unit.functions.push(function);
                    } else {
                        parsed_unit.errors.push(Error {
                            pos: tokenized_file.positions[i],
                            msg: "Expected `)`.".to_string(),
                        });
                    }
                } else {
                    parsed_unit.errors.push(Error {
                        pos: tokenized_file.positions[i],
                        msg: "Expected function prototype.".to_string(),
                    });
                }
            }
            token => {
                parsed_unit.errors.push(Error {
                    pos: tokenized_file.positions[i],
                    msg: format!("Unexpected token `{:?}`.", token),
                });
            }
        }
    }

    return parsed_unit;
}

fn parse_variable_definition(
    iter: &mut Peekable<Enumerate<Iter<Token>>>,
    data_type: &DataType,
    errors: &mut Vec<Error>,
    tokenized_file: &TokenizedFile,
) -> Option<Statement> {
    match iter.next() {
        Some((i, Token::Identifier(identifier))) => {
            let my_expression: Option<Box<ExpressionWithPos>>;
            match iter.next() {
                Some((i, Token::EqualSign)) => {
                    my_expression =
                        parse_expression(iter, errors, tokenized_file).map(|expression| {
                            Box::new(ExpressionWithPos {
                                expression,
                                pos: tokenized_file.positions[i],
                            })
                        });
                }
                Some((_, Token::Semicolon)) => {
                    my_expression = None;
                }
                Some((i, token)) => {
                    errors.push(Error {
                        pos: tokenized_file.positions[i],
                        msg: format!("Unexpected token `{:?}`.", token),
                    });
                    return None;
                }
                None => {
                    errors.push(Error {
                        pos: tokenized_file.positions[i],
                        msg: format!("Reached the end of file. Expected `;` or `=`."),
                    });
                    return None;
                }
            }
            Some(Statement::VariableDefinition {
                identifier: identifier.clone(),
                expression: my_expression,
                data_type: data_type.clone(),
            })
        }
        Some((i, token)) => {
            errors.push(Error {
                pos: tokenized_file.positions[i],
                msg: format!("Unexpected token \'{:?}\'.", token),
            });
            None
        }
        None => {
            errors.push(Error {
                pos: TokenPos {
                    pos: usize::MAX,
                    line: usize::MAX,
                    column: usize::MAX,
                },
                msg: format!("Reached the end of file. Expected an identifier."),
            });
            None
        }
    }
}

fn parse_variable_definition_with_data_type(
    data_type: &DataType,
    iter: &mut Peekable<Enumerate<Iter<Token>>>,
    errors: &mut Vec<Error>,
    tokenized_file: &TokenizedFile,
) -> Option<Statement> {
    match iter.peek() {
        Some((_, Token::SquareParenthesisOpen)) => {
            let (i, _) = iter.next().unwrap();
            if let Some(((_, Token::IntLiteral(size)), (_, Token::SquareParenthesisClose))) =
                iter.next_tuple()
            {
                parse_variable_definition(
                    iter,
                    &DataType::Array {
                        data_type: Box::new(data_type.clone()),
                        size: *size,
                    },
                    errors,
                    tokenized_file,
                )
            } else {
                errors.push(Error {
                    pos: tokenized_file.positions[i],
                    msg: "Expected an int literal in an array definition!".to_string(),
                });
                None
            }
        }
        Some((_, Token::MultiplySign)) => {
            iter.next();
            parse_variable_definition(
                iter,
                &DataType::Pointer(Box::new(data_type.clone())),
                errors,
                tokenized_file,
            )
        }
        _ => parse_variable_definition(iter, data_type, errors, tokenized_file),
    }
}

pub fn parse_scope(
    iter: &mut Peekable<Enumerate<Iter<Token>>>,
    errors: &mut Vec<Error>,
    tokenized_file: &TokenizedFile,
) -> Vec<Statement> {
    let mut ast = Vec::<Statement>::new();

    while let Some((i, token)) = iter.peek() {
        match token {
            Token::DataType(data_type) => {
                iter.next();
                if let Some(statement) = parse_variable_definition_with_data_type(
                    data_type,
                    iter,
                    errors,
                    tokenized_file,
                ) {
                    ast.push(statement);
                }
            }
            Token::Identifier(_struct_name) => {
                // let (i, _) = iter.next().unwrap();
                let i = i.clone();
                let mut iter_clone = iter.clone();
                if let Some(((_, Token::Identifier(struct_name)), (_, Token::Identifier(_identifier)))) = iter_clone.next_tuple() {
                    iter.next();
                    if let Some(statement) = parse_variable_definition_with_data_type(
                        &DataType::Struct(struct_name.clone()),
                        iter,
                        errors,
                        tokenized_file,
                    ) {
                        ast.push(statement);
                    }
                } else {
                    if let Some(expression) = parse_expression(iter, errors, tokenized_file) {
                        ast.push(Statement::Expression(ExpressionWithPos {
                            expression,
                            pos: tokenized_file.positions[i],
                        }));
                    }
                }
            }
            Token::If => {
                let (i, _) = iter.next().unwrap();
                if let Some((i, Token::ParenthesisOpen)) = iter.next() {
                    let expression =
                        parse_expression(iter, errors, tokenized_file).map(|expression| {
                            ExpressionWithPos {
                                expression,
                                pos: tokenized_file.positions[i],
                            }
                        });
                    if let Some(((_, Token::ParenthesisClose), (_, Token::CurlyBracketOpen))) =
                        iter.next_tuple()
                    {
                        let scope = parse_scope(iter, errors, tokenized_file);
                        let else_scope = if let Some((_, Token::Else)) = iter.peek() {
                            let (i, _) = iter.next().unwrap();
                            if let Some((_, Token::CurlyBracketOpen)) = iter.next() {
                                Some(parse_scope(iter, errors, tokenized_file))
                            } else {
                                errors.push(Error {
                                    pos: tokenized_file.positions[i],
                                    msg: "Expected `{` after the else statement.".to_string(),
                                });
                                None
                            }
                        } else {
                            None
                        };
                        ast.push(Statement::If {
                            expression,
                            scope,
                            else_scope,
                        });
                    } else {
                        errors.push(Error {
                            pos: tokenized_file.positions[i],
                            msg: "Expected `) {` after the if condiction.".to_string(),
                        });
                    }
                } else {
                    errors.push(Error {
                        pos: tokenized_file.positions[i],
                        msg: "Expected `(` after the if token.".to_string(),
                    });
                }
            }
            Token::While => {
                let (i, _) = iter.next().unwrap();
                if let Some((i, Token::ParenthesisOpen)) = iter.next() {
                    let expression =
                        parse_expression(iter, errors, tokenized_file).map(|expression| {
                            ExpressionWithPos {
                                expression,
                                pos: tokenized_file.positions[i],
                            }
                        });
                    if let Some(((_, Token::ParenthesisClose), (_, Token::CurlyBracketOpen))) =
                        iter.next_tuple()
                    {
                        let scope = parse_scope(iter, errors, tokenized_file);
                        ast.push(Statement::While { expression, scope });
                    } else {
                        errors.push(Error {
                            pos: tokenized_file.positions[i],
                            msg: "Expected `) {` after the while condiction.".to_string(),
                        });
                    }
                } else {
                    errors.push(Error {
                        pos: tokenized_file.positions[i],
                        msg: "Expected `(` after the while token.".to_string(),
                    });
                }
            }
            Token::For => {
                iter.next();
                if let Some((_, Token::ParenthesisOpen)) = iter.next() {
                    todo!();
                    //let initial_expr = parse_(iter);
                    // let Some(Token::Semicolon) = iter.next() else { panic!("Expected semicolon!") };
                    // let condition_expr = parse_expression(iter);
                    // let Some(Token::Semicolon) = iter.next() else { panic!("Expected semicolon!") };
                    // let iteration_expr = parse_expression(iter);
                    // let Some(Token::Semicolon) = iter.next() else { panic!("Expected semicolon!") };
                    // if let Some((Token::ParenthesisClose, Token::CurlyBracketOpen)) = iter.next_tuple() {
                    // let scope = parse_scope(iter);
                    //abstract_syntax_tree.push(Statement::For { initial_expr, condition_expr, iteration_expr, scope });
                    // } else {
                    //     panic!("Expected closed parenthesis and opened curly brackets in the for statement!");
                    // }
                }
            }
            Token::CurlyBracketClose => {
                iter.next();
                return ast;
            }
            Token::Return => {
                let (i, _) = iter.next().unwrap();
                if let Some(expression) = parse_expression(iter, errors, tokenized_file) {
                    ast.push(Statement::Return(ExpressionWithPos {
                        expression,
                        pos: tokenized_file.positions[i],
                    }));
                }
            }
            Token::Semicolon => {
                iter.next();
            }
            _ => {
                let (i, _) = iter.next().unwrap();
                if let Some(expression) = parse_expression(iter, errors, tokenized_file) {
                    ast.push(Statement::Expression(ExpressionWithPos {
                        expression,
                        pos: tokenized_file.positions[i],
                    }));
                }
            }
        }
    }

    errors.push(Error {
        pos: TokenPos {
            pos: usize::MAX,
            line: usize::MAX,
            column: usize::MAX,
        },
        msg: "Expected `}`.".to_string(),
    });
    ast
}

fn parse_expression(
    iter: &mut Peekable<Enumerate<Iter<Token>>>,
    errors: &mut Vec<Error>,
    tokenized_file: &TokenizedFile,
) -> Option<Expression> {
    return parse_assigment(iter, errors, tokenized_file);
}

fn parse_assigment(
    iter: &mut Peekable<Enumerate<Iter<Token>>>,
    errors: &mut Vec<Error>,
    tokenized_file: &TokenizedFile,
) -> Option<Expression> {
    let mut left = parse_relational(iter, errors, tokenized_file)?;

    match iter.peek() {
        Some((_, Token::EqualSign)) => {
            let (i, _) = iter.next().unwrap();
            let right = parse_relational(iter, errors, tokenized_file)?;
            left = Expression::Assigment {
                left: Box::new(ExpressionWithPos {
                    expression: left,
                    pos: tokenized_file.positions[i],
                }),
                right: Box::new(ExpressionWithPos {
                    expression: right,
                    pos: tokenized_file.positions[i],
                }),
            }
        }
        _ => {
            return Some(left);
        }
    }

    return Some(left);
}

fn parse_relational(
    iter: &mut Peekable<Enumerate<Iter<Token>>>,
    errors: &mut Vec<Error>,
    tokenized_file: &TokenizedFile,
) -> Option<Expression> {
    let mut left = parse_addition(iter, errors, tokenized_file)?;

    match iter.peek() {
        Some((_, Token::CompareEqual)) => {
            let (i, _) = iter.next().unwrap();
            let right = parse_addition(iter, errors, tokenized_file)?;
            left = Expression::ComparisonExpression {
                left: Box::new(ExpressionWithPos {
                    expression: left,
                    pos: tokenized_file.positions[i],
                }),
                right: Box::new(ExpressionWithPos {
                    expression: right,
                    pos: tokenized_file.positions[i],
                }),
                operator: ComparisonOperator::CompareEqual,
            };
        }
        Some((_, Token::LargerThan)) => {
            let (i, _) = iter.next().unwrap();
            let right = parse_addition(iter, errors, tokenized_file)?;
            left = Expression::ComparisonExpression {
                left: Box::new(ExpressionWithPos {
                    expression: left,
                    pos: tokenized_file.positions[i],
                }),
                right: Box::new(ExpressionWithPos {
                    expression: right,
                    pos: tokenized_file.positions[i],
                }),
                operator: ComparisonOperator::CompareLarger,
            };
        }
        Some((_, Token::SmallerThan)) => {
            let (i, _) = iter.next().unwrap();
            let right = parse_addition(iter, errors, tokenized_file)?;
            left = Expression::ComparisonExpression {
                left: Box::new(ExpressionWithPos {
                    expression: left,
                    pos: tokenized_file.positions[i],
                }),
                right: Box::new(ExpressionWithPos {
                    expression: right,
                    pos: tokenized_file.positions[i],
                }),
                operator: ComparisonOperator::CompareSmaller,
            };
        }
        _ => {
            return Some(left);
        }
    }

    return Some(left);
}

fn parse_addition(
    iter: &mut Peekable<Enumerate<Iter<Token>>>,
    errors: &mut Vec<Error>,
    tokenized_file: &TokenizedFile,
) -> Option<Expression> {
    let mut left = parse_multiplication(iter, errors, tokenized_file)?;

    while let Some((_, token)) = iter.peek() {
        if let Token::PlusSign = token {
            let (i, _) = iter.next().unwrap();
            let right = parse_multiplication(iter, errors, tokenized_file)?;
            left = Expression::ArithmeticExpression {
                left: Box::new(ExpressionWithPos {
                    expression: left,
                    pos: tokenized_file.positions[i],
                }),
                right: Box::new(ExpressionWithPos {
                    expression: right,
                    pos: tokenized_file.positions[i],
                }),
                operator: ArithmeticOperator::Add,
            };
        } else if let Token::MinusSign = token {
            let (i, _) = iter.next().unwrap();
            let right = parse_multiplication(iter, errors, tokenized_file)?;
            left = Expression::ArithmeticExpression {
                left: Box::new(ExpressionWithPos {
                    expression: left,
                    pos: tokenized_file.positions[i],
                }),
                right: Box::new(ExpressionWithPos {
                    expression: right,
                    pos: tokenized_file.positions[i],
                }),
                operator: ArithmeticOperator::Subtract,
            };
        } else {
            return Some(left);
        }
    }

    return Some(left);
}

fn parse_multiplication(
    iter: &mut Peekable<Enumerate<Iter<Token>>>,
    errors: &mut Vec<Error>,
    tokenized_file: &TokenizedFile,
) -> Option<Expression> {
    let mut left = parse_literals_pointers(iter, errors, tokenized_file)?;

    while let Some((_, token)) = iter.peek() {
        if let Token::MultiplySign = token {
            let (i, _) = iter.next().unwrap();
            let right = parse_literals_pointers(iter, errors, tokenized_file)?;
            left = Expression::ArithmeticExpression {
                left: Box::new(ExpressionWithPos {
                    expression: left,
                    pos: tokenized_file.positions[i],
                }),
                right: Box::new(ExpressionWithPos {
                    expression: right,
                    pos: tokenized_file.positions[i],
                }),
                operator: ArithmeticOperator::Multiply,
            };
        } else if let Token::DivisionSign = token {
            let (i, _) = iter.next().unwrap();
            let right = parse_literals_pointers(iter, errors, tokenized_file)?;
            left = Expression::ArithmeticExpression {
                left: Box::new(ExpressionWithPos {
                    expression: left,
                    pos: tokenized_file.positions[i],
                }),
                right: Box::new(ExpressionWithPos {
                    expression: right,
                    pos: tokenized_file.positions[i],
                }),
                operator: ArithmeticOperator::Divide,
            };
        } else {
            return Some(left);
        }
    }

    return Some(left);
}

fn parse_literals_pointers(
    iter: &mut Peekable<Enumerate<Iter<Token>>>,
    errors: &mut Vec<Error>,
    tokenized_file: &TokenizedFile,
) -> Option<Expression> {
    let next = iter.peek();
    match next {
        Some((_, Token::IntLiteral(value))) => {
            iter.next();
            Some(Expression::IntLiteral(*value))
        }
        Some((_, Token::FloatLiteral(value))) => {
            iter.next();
            Some(Expression::FloatLiteral(value.clone()))
        }
        Some((_, Token::CharacterLiteral(c))) => {
            iter.next();
            Some(Expression::CharacterLiteral(c.clone()))
        }
        Some((_, Token::BoolLiteral(b))) => {
            iter.next();
            Some(Expression::BoolLiteral(b.clone()))
        }
        Some((_, Token::StringLiteral(value))) => {
            iter.next();
            Some(Expression::StringLiteral(value.clone()))
        }
        Some((_, Token::MultiplySign)) => {
            let (i, _) = iter.next().unwrap();
            Some(Expression::Dereference(Box::new(ExpressionWithPos {
                expression: parse_member_access(iter, errors, tokenized_file)?,
                pos: tokenized_file.positions[i],
            })))
        }
        Some((_, Token::Ampersand)) => {
            let (i, _) = iter.next().unwrap();
            Some(Expression::AddressOf(Box::new(ExpressionWithPos {
                expression: parse_member_access(iter, errors, tokenized_file)?,
                pos: tokenized_file.positions[i],
            })))
        }
        _ => parse_member_access(iter, errors, tokenized_file),
    }
}

fn parse_member_access(
    iter: &mut Peekable<Enumerate<Iter<Token>>>,
    errors: &mut Vec<Error>,
    tokenized_file: &TokenizedFile,
) -> Option<Expression> {
    let mut expression = parse_atom(iter, errors, tokenized_file)?;

    while let Some((_, Token::Period)) = iter.peek() {
        let (i, _) = iter.next().unwrap();
        expression = Expression::MemberAccess {
            left: Box::new(ExpressionWithPos {
                expression,
                pos: tokenized_file.positions[i],
            }),
            right: Box::new(ExpressionWithPos {
                expression: parse_atom(iter, errors, tokenized_file)?,
                pos: tokenized_file.positions[i],
            }),
        };
    }

    return Some(expression);
}

fn parse_atom(
    iter: &mut Peekable<Enumerate<Iter<Token>>>,
    errors: &mut Vec<Error>,
    tokenized_file: &TokenizedFile,
) -> Option<Expression> {
    match iter.next() {
        Some((_, Token::Identifier(identifier))) => {
            parse_identifier(iter, identifier.clone(), errors, tokenized_file)
        }
        Some((i, Token::ParenthesisOpen)) => {
            let expression = parse_expression(iter, errors, tokenized_file);
            if let Some((_, Token::ParenthesisClose)) = iter.next() {
                expression
            } else {
                errors.push(Error {
                    pos: tokenized_file.positions[i],
                    msg: "Expected `)`.".to_string(),
                });
                None
            }
        }
        Some((i, Token::PlusSign)) => {
            if let Some(((_, Token::PlusSign), (_, Token::Identifier(identifier)))) =
                iter.next_tuple()
            {
                Some(Expression::Increment(Box::new(ExpressionWithPos {
                    expression: parse_identifier(iter, identifier.clone(), errors, tokenized_file)?,
                    pos: tokenized_file.positions[i],
                })))
            } else {
                errors.push(Error {
                    pos: tokenized_file.positions[i],
                    msg: "Expected `++identifier`.".to_string(),
                });
                None
            }
        }
        Some((i, Token::MinusSign)) => {
            if let Some(((_, Token::MinusSign), (_, Token::Identifier(identifier)))) =
                iter.next_tuple()
            {
                Some(Expression::Decrement(Box::new(ExpressionWithPos {
                    expression: parse_identifier(iter, identifier.clone(), errors, tokenized_file)?,
                    pos: tokenized_file.positions[i],
                })))
            } else {
                errors.push(Error {
                    pos: tokenized_file.positions[i],
                    msg: "Expected `--identifier`.".to_string(),
                });
                None
            }
        }
        Some((i, token)) => {
            println!("{}", Backtrace::force_capture());
            errors.push(Error {
                pos: tokenized_file.positions[i],
                msg: format!("Expected `(` or an identifier, got: `{:?}`.", token),
            });
            None
        }
        None => {
            errors.push(Error {
                pos: TokenPos {
                    pos: usize::MAX,
                    line: usize::MAX,
                    column: usize::MAX,
                },
                msg: "Reached the end of file in a middle of an expression.".to_string(),
            });
            None
        }
    }
}

fn parse_identifier(
    iter: &mut Peekable<Enumerate<Iter<Token>>>,
    identifier: String,
    errors: &mut Vec<Error>,
    tokenized_file: &TokenizedFile,
) -> Option<Expression> {
    match iter.peek() {
        Some((_, Token::SquareParenthesisOpen)) => {
            let (i, _) = iter.next().unwrap();
            let expression = parse_expression(iter, errors, tokenized_file)?;
            match iter.next() {
                Some((_, Token::SquareParenthesisClose)) => {}
                _ => {
                    errors.push(Error {
                        pos: tokenized_file.positions[i],
                        msg: "Expected `]`.".to_string(),
                    });
                }
            }
            Some(Expression::ArraySubscript {
                identifier,
                element: Box::new(ExpressionWithPos {
                    expression,
                    pos: tokenized_file.positions[i],
                }),
            })
        }
        Some((_, Token::ParenthesisOpen)) => {
            let (i, _) = iter.next().unwrap();

            let arguments = if let Some((_, Token::ParenthesisClose)) = iter.peek() {
                Vec::new()
            } else {
                parse_arguments_passing(iter, errors, tokenized_file)
            };

            if let Some((_, Token::ParenthesisClose)) = iter.next() {
                Some(Expression::FunctionCall(FunctionCall {
                    identifier,
                    arguments,
                    pos: tokenized_file.positions[i],
                }))
            } else {
                errors.push(Error {
                    pos: tokenized_file.positions[i],
                    msg: "Expected `)` at the end of function call.".to_string(),
                });
                Some(Expression::FunctionCall(FunctionCall {
                    identifier,
                    arguments,
                    pos: tokenized_file.positions[i],
                }))
            }
        }
        Some((_, Token::CurlyBracketOpen)) => {
            let (i, _) = iter.next().unwrap();

            let mut members: Vec<(String, Option<ExpressionWithPos>)> = Vec::new();
            'outer: loop {
                if let Some((i, Token::Identifier(identifier))) = iter.next() {
                    match iter.next() {
                        Some((_, Token::Colon)) => {}
                        _ => {
                            errors.push(Error {
                                pos: tokenized_file.positions[i],
                                msg: "Expected `:` after a member identifier in a struct literal."
                                    .to_string(),
                            });
                        }
                    }
                    members.push((
                        identifier.clone(),
                        parse_expression(iter, errors, tokenized_file).map(|expression| {
                            ExpressionWithPos {
                                expression,
                                pos: tokenized_file.positions[i],
                            }
                        }),
                    ));
                    match iter.peek() {
                        Some((_, Token::Coma)) => {
                            iter.next();
                        }
                        Some((_, Token::CurlyBracketClose)) => break,
                        Some((i, _)) => {
                            errors.push(Error {
                                pos: tokenized_file.positions[*i],
                                msg: "Expected `,` or `}` in a struct literal.".to_string(),
                            });
                            while let Some((_, token)) = iter.peek() {
                                match token {
                                    Token::Coma => {
                                        iter.next();
                                        break;
                                    }
                                    Token::CurlyBracketClose => {
                                        break 'outer;
                                    }
                                    _ => iter.next(),
                                };
                            }
                        }
                        _ => break,
                    }
                } else {
                    errors.push(Error {
                        pos: tokenized_file.positions[i],
                        msg: "Expected a member identifier in a struct literal.".to_string(),
                    });
                }
            }

            if let Some((_, Token::CurlyBracketClose)) = iter.next() {
                Some(Expression::StructLiteral {
                    identifier,
                    members,
                })
            } else {
                errors.push(Error {
                    pos: tokenized_file.positions[i],
                    msg: "Expected `}}` at the end of a struct literal.".to_string(),
                });
                Some(Expression::StructLiteral {
                    identifier,
                    members,
                })
            }
        }
        _ => Some(Expression::Identifier(identifier.clone())),
    }
}
