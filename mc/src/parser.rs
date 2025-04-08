use crate::tokenizer::*;
use itertools::Itertools;
use std::iter::Peekable;
use std::slice::Iter;

#[derive(Debug)]
pub struct ExpressionSpanned {
    pub span: Span,
    pub expression: Expression,
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
        members: Vec<(String, Option<ExpressionSpanned>)>,
    },
    Identifier(String),
    ArithmeticExpression {
        left: Box<ExpressionSpanned>,
        right: Box<ExpressionSpanned>,
        operator: ArithmeticOperator,
    },
    ComparisonExpression {
        left: Box<ExpressionSpanned>,
        right: Box<ExpressionSpanned>,
        operator: ComparisonOperator,
    },
    Dereference(Box<ExpressionSpanned>),
    AddressOf(Box<ExpressionSpanned>),
    ArraySubscript {
        identifier: String,
        element: Box<ExpressionSpanned>,
    },
    FunctionCall(FunctionCall),
    Assigment {
        left: Box<ExpressionSpanned>,
        right: Box<ExpressionSpanned>,
    },
    MemberAccess {
        left: Box<ExpressionSpanned>,
        right: Box<ExpressionSpanned>,
    },
    Increment(Box<ExpressionSpanned>),
    Decrement(Box<ExpressionSpanned>),
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
        expression: Option<ExpressionSpanned>,
        scope: Vec<Statement>,
        else_scope: Option<Vec<Statement>>,
    },
    Return(ExpressionSpanned),
    Expression(ExpressionSpanned),
    While {
        expression: Option<ExpressionSpanned>,
        scope: Vec<Statement>,
    },
    For {
        inital_statement: Box<Statement>,
        condition_expr: ExpressionSpanned,
        iteration_expr: ExpressionSpanned,
        scope: Vec<Statement>,
    },
    VariableDefinition {
        identifier: String,
        expression: Option<Box<ExpressionSpanned>>,
        data_type: DataType,
    },
}

#[derive(Debug)]
pub struct FunctionCall {
    pub identifier: String,
    pub arguments: Vec<ExpressionSpanned>,
    pub span: Span,
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
    iter: &mut Peekable<Iter<TokenSpanned>>,
    errors: &mut Vec<Error>,
) -> Vec<DataType> {
    let mut arguments = Vec::<DataType>::new();

    loop {
        match iter.next() {
            Some(TokenSpanned {
                token: Token::DataType(data_type),
                span,
            }) => match iter.next() {
                Some(TokenSpanned {
                    token: Token::Identifier(_),
                    span: _,
                }) => {
                    arguments.push(data_type.clone());
                }
                Some(TokenSpanned {
                    token: Token::MultiplySign,
                    span,
                }) => match iter.next() {
                    Some(TokenSpanned {
                        token: Token::Identifier(_),
                        span: _,
                    }) => {
                        arguments.push(DataType::Pointer(Box::new(data_type.clone())));
                    }
                    Some(TokenSpanned { token, span }) => {
                        errors.push(Error {
                            span: *span,
                            msg: format!("Unexpected token `{:?}` in the function arguments, expected an indentifier.", token)
                        });
                    }
                    None => {
                        errors.push(Error {
                            span: span.max_end(),
                            msg: "Reached the end of file during parsing function arguments."
                                .to_string(),
                        });
                    }
                },
                Some(TokenSpanned { token, span }) => {
                    errors.push(Error {
                            span: *span,
                            msg: format!("Unexpected token `{:?}` in the function arguments, expected an indentifier or `*`.", token)
                        });
                }
                None => {
                    errors.push(Error {
                        span: span.max_end(),
                        msg: "Reached the end of file during parsing function arguments."
                            .to_string(),
                    });
                }
            },
            Some(TokenSpanned {
                token: Token::Identifier(identifier),
                span,
            }) => match iter.next() {
                Some(TokenSpanned {
                    token: Token::Identifier(_),
                    span: _,
                }) => {
                    arguments.push(DataType::Struct(identifier.clone()));
                }
                Some(TokenSpanned {
                    token: Token::MultiplySign,
                    span: _,
                }) => match iter.next() {
                    Some(TokenSpanned {
                        token: Token::Identifier(_),
                        span: _,
                    }) => {
                        arguments.push(DataType::Pointer(Box::new(DataType::Struct(
                            identifier.clone(),
                        ))));
                    }
                    Some(TokenSpanned { token, span }) => {
                        errors.push(Error {
                            span: *span,
                            msg: format!("Unexpected token `{:?}` in the function arguments, expected an indentifier.", token)
                        });
                    }
                    None => {
                        errors.push(Error {
                            span: span.max_end(),
                            msg: "Reached the end of file during parsing function arguments."
                                .to_string(),
                        });
                    }
                },
                Some(TokenSpanned { token, span }) => {
                    errors.push(Error {
                            span: *span,
                            msg: format!("Unexpected token `{:?}` in the function arguments, expected an indentifier or `*`.", token)
                        });
                }
                None => {
                    errors.push(Error {
                        span: span.max_end(),
                        msg: "Reached the end of file during parsing function arguments."
                            .to_string(),
                    });
                }
            },
            Some(TokenSpanned { token, span }) => {
                errors.push(Error {
                    span: *span,
                    msg: format!(
                        "Unexpected token `{:?}` in the function arguments, expected a data type.",
                        token
                    ),
                });
            }
            None => {
                errors.push(Error {
                    span: Span::eof(),
                    msg: "Reached the end of file during parsing function arguments.".to_string(),
                });
            }
        }

        if let Some(TokenSpanned {
            token: Token::Coma,
            span: _,
        }) = iter.peek()
        {
            iter.next();
        } else {
            break;
        }
    }

    return arguments;
}

pub fn parse_arguments_passing(
    iter: &mut Peekable<Iter<TokenSpanned>>,
    errors: &mut Vec<Error>,
) -> Vec<ExpressionSpanned> {
    let mut arguments = Vec::<ExpressionSpanned>::new();

    loop {
        if let Some(expression) = parse_expression(iter, errors) {
            arguments.push(expression);
        } else {
            break;
        }

        if let Some(TokenSpanned {
            token: Token::Coma,
            span: _,
        }) = iter.peek()
        {
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
    let mut iter = tokenized_file.tokens.iter().peekable();

    while let Some(TokenSpanned { token, span }) = iter.next() {
        match token {
            Token::Struct => {
                if let Some((
                    TokenSpanned {
                        token: Token::Identifier(identifier),
                        span: _,
                    },
                    TokenSpanned {
                        token: Token::CurlyBracketOpen,
                        span: _,
                    },
                )) = iter.next_tuple()
                {
                    let mut members: Vec<StructMember> = Vec::new();
                    while let Some((
                        TokenSpanned {
                            token: Token::DataType(data_type),
                            span: _,
                        },
                        TokenSpanned {
                            token: Token::Identifier(member_name),
                            span: _,
                        },
                    )) = iter.clone().next_tuple()
                    {
                        iter.next();
                        iter.next();

                        members.push(StructMember {
                            identifier: member_name.clone(),
                            data_type: data_type.clone(),
                        });
                        match iter.next() {
                            Some(TokenSpanned {
                                token: Token::Coma,
                                span: _,
                            }) => {}
                            Some(TokenSpanned {
                                token: Token::CurlyBracketClose,
                                span: _,
                            }) => {
                                break;
                            }
                            Some(TokenSpanned { token, span }) => {
                                parsed_unit.errors.push(Error {
                                    span: *span,
                                    msg: format!("Unexpected `{:?}`.", token),
                                });
                            }
                            None => {
                                parsed_unit.errors.push(Error {
                                    span: span.max_end(),
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
                        span: *span,
                        msg: "Expected struct declaration after the struct token!".to_string(),
                    });
                }
            }
            Token::DataType(return_type) => {
                if let Some((
                    TokenSpanned {
                        token: Token::Identifier(identifier),
                        span: _,
                    },
                    TokenSpanned {
                        token: Token::ParenthesisOpen,
                        span: _,
                    },
                )) = iter.next_tuple()
                {
                    let arguments = if let Some(TokenSpanned {
                        token: Token::ParenthesisClose,
                        span: _,
                    }) = iter.peek()
                    {
                        Vec::new()
                    } else {
                        parse_arguments_declaration(&mut iter, &mut parsed_unit.errors)
                    };

                    if let Some(TokenSpanned {
                        token: Token::ParenthesisClose,
                        span: _,
                    }) = iter.next()
                    {
                        let function_prototype = FunctionPrototype {
                            name: identifier.clone(),
                            return_type: return_type.clone(),
                            arguments,
                        };
                        match iter.next() {
                            Some(TokenSpanned {
                                token: Token::Semicolon,
                                span: _,
                            }) => parsed_unit.function_declarations.push(function_prototype),
                            Some(TokenSpanned {
                                token: Token::CurlyBracketOpen,
                                span: _,
                            }) => {
                                let function = FunctionDefinition {
                                    prototype: function_prototype,
                                    public: false,
                                    body: parse_scope(
                                        &mut iter,
                                        &mut parsed_unit.errors,
                                        
                                    ),
                                };
                                parsed_unit.functions.push(function);
                            }
                            Some(TokenSpanned { token, span }) => {
                                parsed_unit.errors.push(Error {
                                    span: *span,
                                    msg: format!("Unexpected `{:?}`. Expected `;` or `{{`.", token),
                                });
                            }
                            None => {
                                parsed_unit.errors.push(Error {
                                    span: *span,
                                    msg: "Expected `;` or `{{`.".to_string(),
                                });
                            }
                        }
                    } else {
                        parsed_unit.errors.push(Error {
                            span: *span,
                            msg: "Expected `)`.".to_string(),
                        });
                    }
                } else {
                    parsed_unit.errors.push(Error {
                        span: *span,
                        msg: "Expected function prototype.".to_string(),
                    });
                }
            }
            Token::Public => {
                if let Some((
                    TokenSpanned {
                        token: Token::DataType(return_type),
                        span: _,
                    },
                    TokenSpanned {
                        token: Token::Identifier(identifier),
                        span: _,
                    },
                    TokenSpanned {
                        token: Token::ParenthesisOpen,
                        span,
                    },
                )) = iter.next_tuple()
                {
                    let arguments = if let Some(TokenSpanned {
                        token: Token::ParenthesisClose,
                        span: _,
                    }) = iter.peek()
                    {
                        Vec::new()
                    } else {
                        parse_arguments_declaration(&mut iter, &mut parsed_unit.errors)
                    };

                    if let Some((
                        TokenSpanned {
                            token: Token::ParenthesisClose,
                            span: _,
                        },
                        TokenSpanned {
                            token: Token::CurlyBracketOpen,
                            span: _,
                        },
                    )) = iter.next_tuple()
                    {
                        let function_prototype = FunctionPrototype {
                            name: identifier.clone(),
                            return_type: return_type.clone(),
                            arguments,
                        };

                        let function = FunctionDefinition {
                            prototype: function_prototype,
                            public: true,
                            body: parse_scope(&mut iter, &mut parsed_unit.errors),
                        };
                        parsed_unit.functions.push(function);
                    } else {
                        parsed_unit.errors.push(Error {
                            span: *span,
                            msg: "Expected `)`.".to_string(),
                        });
                    }
                } else {
                    parsed_unit.errors.push(Error {
                        span: *span,
                        msg: "Expected function prototype.".to_string(),
                    });
                }
            }
            token => {
                parsed_unit.errors.push(Error {
                    span: *span,
                    msg: format!("Unexpected token `{:?}`.", token),
                });
            }
        }
    }

    return parsed_unit;
}

fn parse_variable_definition(
    iter: &mut Peekable<Iter<TokenSpanned>>,
    data_type: &DataType,
    errors: &mut Vec<Error>,
) -> Option<Statement> {
    match iter.next() {
        Some(TokenSpanned {
            token: Token::Identifier(identifier),
            span,
        }) => {
            let my_expression: Option<Box<ExpressionSpanned>>;
            match iter.next() {
                Some(TokenSpanned {
                    token: Token::EqualSign,
                    span: _,
                }) => {
                    my_expression = parse_expression(iter, errors).map(Box::new);
                }
                Some(TokenSpanned {
                    token: Token::Semicolon,
                    span: _,
                }) => {
                    my_expression = None;
                }
                Some(TokenSpanned { token, span }) => {
                    errors.push(Error {
                        span: *span,
                        msg: format!("Unexpected token `{:?}`.", token),
                    });
                    return None;
                }
                None => {
                    errors.push(Error {
                        span: span.max_end(),
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
        Some(TokenSpanned { token, span }) => {
            errors.push(Error {
                span: *span,
                msg: format!("Unexpected token `{:?}`.", token),
            });
            None
        }
        None => {
            errors.push(Error {
                span: Span::eof(),
                msg: format!("Reached the end of file. Expected an identifier."),
            });
            None
        }
    }
}

fn parse_variable_definition_with_data_type(
    data_type: &DataType,
    iter: &mut Peekable<Iter<TokenSpanned>>,
    errors: &mut Vec<Error>,
) -> Option<Statement> {
    match iter.peek() {
        Some(TokenSpanned {
            token: Token::SquareParenthesisOpen,
            span,
        }) => {
            if let Some((
                TokenSpanned {
                    token: Token::IntLiteral(size),
                    span: _,
                },
                TokenSpanned {
                    token: Token::SquareParenthesisClose,
                    span: _,
                },
            )) = iter.next_tuple()
            {
                parse_variable_definition(
                    iter,
                    &DataType::Array {
                        data_type: Box::new(data_type.clone()),
                        size: *size,
                    },
                    errors,
                    
                )
            } else {
                errors.push(Error {
                    span: *span,
                    msg: "Expected an int literal in an array definition!".to_string(),
                });
                None
            }
        }
        Some(TokenSpanned {
            token: Token::MultiplySign,
            span: _,
        }) => {
            iter.next();
            parse_variable_definition(
                iter,
                &DataType::Pointer(Box::new(data_type.clone())),
                errors,
                
            )
        }
        _ => parse_variable_definition(iter, data_type, errors),
    }
}

pub fn parse_scope(
    iter: &mut Peekable<Iter<TokenSpanned>>,
    errors: &mut Vec<Error>,
) -> Vec<Statement> {
    let mut ast = Vec::<Statement>::new();

    while let Some(TokenSpanned { token, span }) = iter.peek() {
        match token {
            Token::DataType(data_type) => {
                iter.next();
                if let Some(statement) = parse_variable_definition_with_data_type(
                    data_type,
                    iter,
                    errors,
                ) {
                    ast.push(statement);
                }
            }
            Token::Identifier(_struct_name) => {
                // let (i, _) = iter.next().unwrap();
                let mut iter_clone = iter.clone();
                if let Some((
                    TokenSpanned {
                        token: Token::Identifier(struct_name),
                        span: _,
                    },
                    TokenSpanned {
                        token: Token::Identifier(_identifier),
                        span: _,
                    },
                )) = iter_clone.next_tuple()
                {
                    iter.next();
                    if let Some(statement) = parse_variable_definition_with_data_type(
                        &DataType::Struct(struct_name.clone()),
                        iter,
                        errors,
                        
                    ) {
                        ast.push(statement);
                    }
                } else {
                    if let Some(expression) = parse_expression(iter, errors) {
                        ast.push(Statement::Expression(expression));
                    }
                }
            }
            Token::If => {
                if let Some(TokenSpanned {
                    token: Token::ParenthesisOpen,
                    span,
                }) = iter.next()
                {
                    let expression = parse_expression(iter, errors);
                    if let Some((
                        TokenSpanned {
                            token: Token::ParenthesisClose,
                            span: _,
                        },
                        TokenSpanned {
                            token: Token::CurlyBracketOpen,
                            span: _,
                        },
                    )) = iter.next_tuple()
                    {
                        let scope = parse_scope(iter, errors);
                        let else_scope = if let Some(TokenSpanned {
                            token: Token::Else,
                            span,
                        }) = iter.peek()
                        {
                            if let Some(TokenSpanned {
                                token: Token::CurlyBracketOpen,
                                span: _,
                            }) = iter.next()
                            {
                                Some(parse_scope(iter, errors))
                            } else {
                                errors.push(Error {
                                    span: *span,
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
                            span: *span,
                            msg: "Expected `) {` after the if condiction.".to_string(),
                        });
                    }
                } else {
                    errors.push(Error {
                        span: *span,
                        msg: "Expected `(` after the if token.".to_string(),
                    });
                }
            }
            Token::While => {
                if let Some(TokenSpanned {
                    token: Token::ParenthesisOpen,
                    span,
                }) = iter.next()
                {
                    let expression = parse_expression(iter, errors);
                    if let Some((
                        TokenSpanned {
                            token: Token::ParenthesisClose,
                            span: _,
                        },
                        TokenSpanned {
                            token: Token::CurlyBracketOpen,
                            span: _,
                        },
                    )) = iter.next_tuple()
                    {
                        let scope = parse_scope(iter, errors);
                        ast.push(Statement::While { expression, scope });
                    } else {
                        errors.push(Error {
                            span: *span,
                            msg: "Expected `) {` after the while condiction.".to_string(),
                        });
                    }
                } else {
                    errors.push(Error {
                        span: *span,
                        msg: "Expected `(` after the while token.".to_string(),
                    });
                }
            }
            Token::For => {
                iter.next();
                if let Some(TokenSpanned {
                    token: Token::ParenthesisOpen,
                    span: _,
                }) = iter.next()
                {
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
                iter.next();
                if let Some(expression) = parse_expression(iter, errors) {
                    ast.push(Statement::Return(expression));
                }
            }
            Token::Semicolon => {
                iter.next();
            }
            _ => {
                if let Some(expression) = parse_expression(iter, errors) {
                    ast.push(Statement::Expression(expression));
                }
            }
        }
    }

    errors.push(Error {
        span: Span::eof(),
        msg: "Expected `}`.".to_string(),
    });
    ast
}

fn parse_expression(
    iter: &mut Peekable<Iter<TokenSpanned>>,
    errors: &mut Vec<Error>,
) -> Option<ExpressionSpanned> {
    return parse_assigment(iter, errors);
}

fn parse_assigment(
    iter: &mut Peekable<Iter<TokenSpanned>>,
    errors: &mut Vec<Error>,
) -> Option<ExpressionSpanned> {
    let mut left = parse_relational(iter, errors)?;

    match iter.peek() {
        Some(TokenSpanned {
            token: Token::EqualSign,
            span: _,
        }) => {
            let right = parse_relational(iter, errors)?;
            left = ExpressionSpanned {
                span: Span::between(&left.span, &right.span),
                expression: Expression::Assigment {
                    left: Box::new(left),
                    right: Box::new(right),
                },
            }
        }
        _ => {
            return Some(left);
        }
    }

    return Some(left);
}

fn parse_relational(
    iter: &mut Peekable<Iter<TokenSpanned>>,
    errors: &mut Vec<Error>,
) -> Option<ExpressionSpanned> {
    let mut left = parse_addition(iter, errors)?;

    match iter.peek() {
        Some(TokenSpanned {
            token: Token::CompareEqual,
            span: _,
        }) => {
            let right = parse_addition(iter, errors)?;
            left = ExpressionSpanned {
                span: Span::between(&left.span, &right.span),
                expression: Expression::ComparisonExpression {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator: ComparisonOperator::CompareEqual,
                },
            };
        }
        Some(TokenSpanned {
            token: Token::LargerThan,
            span: _,
        }) => {
            let right = parse_addition(iter, errors)?;
            left = ExpressionSpanned {
                span: Span::between(&left.span, &right.span),
                expression: Expression::ComparisonExpression {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator: ComparisonOperator::CompareLarger,
                },
            };
        }
        Some(TokenSpanned {
            token: Token::SmallerThan,
            span: _,
        }) => {
            let right = parse_addition(iter, errors)?;
            left = ExpressionSpanned {
                span: Span::between(&left.span, &right.span),
                expression: Expression::ComparisonExpression {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator: ComparisonOperator::CompareSmaller,
                },
            };
        }
        _ => {
            return Some(left);
        }
    }

    return Some(left);
}

fn parse_addition(
    iter: &mut Peekable<Iter<TokenSpanned>>,
    errors: &mut Vec<Error>,
) -> Option<ExpressionSpanned> {
    let mut left = parse_multiplication(iter, errors)?;

    while let Some(TokenSpanned { token, span: _ }) = iter.peek() {
        match token {
            Token::PlusSign => {
                let right = parse_multiplication(iter, errors)?;
                left = ExpressionSpanned {
                    span: Span::between(&left.span, &right.span),
                    expression: Expression::ArithmeticExpression {
                        left: Box::new(left),
                        right: Box::new(right),
                        operator: ArithmeticOperator::Add,
                    },
                };
            }
            Token::MinusSign => {
                let right = parse_multiplication(iter, errors)?;
                left = ExpressionSpanned {
                    span: Span::between(&left.span, &right.span),
                    expression: Expression::ArithmeticExpression {
                        left: Box::new(left),
                        right: Box::new(right),
                        operator: ArithmeticOperator::Subtract,
                    },
                };
            }
            _ => return Some(left),
        }
    }

    return Some(left);
}

fn parse_multiplication(
    iter: &mut Peekable<Iter<TokenSpanned>>,
    errors: &mut Vec<Error>,
) -> Option<ExpressionSpanned> {
    let mut left = parse_literals_pointers(iter, errors)?;

    while let Some(TokenSpanned { token, span: _ }) = iter.peek() {
        match token {
            Token::MultiplySign => {
                let right = parse_literals_pointers(iter, errors)?;
                left = ExpressionSpanned {
                    span: Span::between(&left.span, &right.span),
                    expression: Expression::ArithmeticExpression {
                        left: Box::new(left),
                        right: Box::new(right),
                        operator: ArithmeticOperator::Multiply,
                    },
                };
            }
            Token::DivisionSign => {
                let right = parse_literals_pointers(iter, errors)?;
                left = ExpressionSpanned {
                    span: Span::between(&left.span, &right.span),
                    expression: Expression::ArithmeticExpression {
                        left: Box::new(left),
                        right: Box::new(right),
                        operator: ArithmeticOperator::Divide,
                    },
                };
            }
            _ => return Some(left),
        }
    }

    return Some(left);
}

fn parse_literals_pointers(
    iter: &mut Peekable<Iter<TokenSpanned>>,
    errors: &mut Vec<Error>,
) -> Option<ExpressionSpanned> {
    let Some(TokenSpanned { token, span }) = iter.peek() else {
        return None;
    };
    match token {
        Token::IntLiteral(value) => {
            iter.next();
            Some(ExpressionSpanned {
                span: *span,
                expression: Expression::IntLiteral(*value),
            })
        }
        Token::FloatLiteral(value) => {
            iter.next();
            Some(ExpressionSpanned {
                span: *span,
                expression: Expression::FloatLiteral(value.clone()),
            })
        }
        Token::CharacterLiteral(c) => {
            iter.next();
            Some(ExpressionSpanned {
                span: *span,
                expression: Expression::CharacterLiteral(*c),
            })
        }
        Token::BoolLiteral(b) => {
            iter.next();
            Some(ExpressionSpanned {
                span: *span,
                expression: Expression::BoolLiteral(*b),
            })
        }
        Token::StringLiteral(value) => {
            iter.next();
            Some(ExpressionSpanned {
                span: *span,
                expression: Expression::StringLiteral(value.clone()),
            })
        }
        Token::MultiplySign => Some(ExpressionSpanned {
            span: *span,
            expression: Expression::Dereference(Box::new(parse_member_access(
                iter,
                errors,
                
            )?)),
        }),
        Token::Ampersand => Some(ExpressionSpanned {
            span: *span,
            expression: Expression::AddressOf(Box::new(parse_member_access(
                iter,
                errors,
                
            )?)),
        }),
        _ => parse_member_access(iter, errors),
    }
}

fn parse_member_access(
    iter: &mut Peekable<Iter<TokenSpanned>>,
    errors: &mut Vec<Error>,
) -> Option<ExpressionSpanned> {
    let mut expression = parse_atom(iter, errors)?;

    while let Some(TokenSpanned {
        token: Token::Period,
        span,
    }) = iter.peek()
    {
        iter.next();
        expression = ExpressionSpanned {
            span: *span,
            expression: Expression::MemberAccess {
                left: Box::new(expression),
                right: Box::new(parse_atom(iter, errors)?),
            },
        };
    }

    return Some(expression);
}

fn parse_atom(
    iter: &mut Peekable<Iter<TokenSpanned>>,
    errors: &mut Vec<Error>,
) -> Option<ExpressionSpanned> {
    match iter.next() {
        Some(TokenSpanned {
            token: Token::Identifier(identifier),
            span,
        }) => parse_identifier(iter, identifier.clone(), errors, span),
        Some(TokenSpanned {
            token: Token::ParenthesisOpen,
            span,
        }) => {
            let expression = parse_expression(iter, errors);
            if let Some(TokenSpanned {
                token: Token::ParenthesisClose,
                span: _,
            }) = iter.next()
            {
                expression
            } else {
                errors.push(Error {
                    span: *span,
                    msg: "Expected `)`.".to_string(),
                });
                None
            }
        }
        Some(TokenSpanned {
            token: Token::PlusSign,
            span,
        }) => {
            if let Some((
                TokenSpanned {
                    token: Token::PlusSign,
                    span: _,
                },
                TokenSpanned {
                    token: Token::Identifier(identifier),
                    span: _,
                },
            )) = iter.next_tuple()
            {
                let identifier =
                    parse_identifier(iter, identifier.clone(), errors, span)?;
                Some(ExpressionSpanned {
                    span: Span::between(span, &identifier.span),
                    expression: Expression::Increment(Box::new(identifier)),
                })
            } else {
                errors.push(Error {
                    span: *span,
                    msg: "Expected `++identifier`.".to_string(),
                });
                None
            }
        }
        Some(TokenSpanned {
            token: Token::MinusSign,
            span,
        }) => {
            if let Some((
                TokenSpanned {
                    token: Token::MinusSign,
                    span: _,
                },
                TokenSpanned {
                    token: Token::Identifier(identifier),
                    span: _,
                },
            )) = iter.next_tuple()
            {
                let identifier =
                    parse_identifier(iter, identifier.clone(), errors, span)?;
                Some(ExpressionSpanned {
                    span: Span::between(span, &identifier.span),
                    expression: Expression::Decrement(Box::new(identifier)),
                })
            } else {
                errors.push(Error {
                    span: *span,
                    msg: "Expected `--identifier`.".to_string(),
                });
                None
            }
        }
        Some(TokenSpanned { token, span }) => {
            errors.push(Error {
                span: *span,
                msg: format!("Expected `(` or an identifier, got: `{:?}`.", token),
            });
            None
        }
        None => {
            errors.push(Error {
                span: Span::eof(),
                msg: "Reached the end of file in a middle of an expression.".to_string(),
            });
            None
        }
    }
}

fn parse_identifier(
    iter: &mut Peekable<Iter<TokenSpanned>>,
    identifier: String,
    errors: &mut Vec<Error>,
    span: &Span,
) -> Option<ExpressionSpanned> {
    match iter.peek() {
        Some(TokenSpanned {
            token: Token::SquareParenthesisOpen,
            span: _,
        }) => {
            let expression = parse_expression(iter, errors)?;
            let mut end = &expression.span;
            match iter.next() {
                Some(TokenSpanned {
                    token: Token::SquareParenthesisClose,
                    span,
                }) => end = span,
                _ => {
                    errors.push(Error {
                        span: *end,
                        msg: "Expected `]`.".to_string(),
                    });
                }
            }
            Some(ExpressionSpanned {
                span: Span::between(span, end),
                expression: Expression::ArraySubscript {
                    identifier,
                    element: Box::new(expression),
                },
            })
        }
        Some(TokenSpanned {
            token: Token::ParenthesisOpen,
            span,
        }) => {
            iter.next();
            let arguments = if let Some(TokenSpanned {
                token: Token::ParenthesisClose,
                span: _,
            }) = iter.peek()
            {
                Vec::new()
            } else {
                parse_arguments_passing(iter, errors)
            };

            if let Some(TokenSpanned {
                token: Token::ParenthesisClose,
                span: end,
            }) = iter.next()
            {
                let span = Span::between(span, end);
                Some(ExpressionSpanned {
                    span,
                    expression: Expression::FunctionCall(FunctionCall {
                        identifier,
                        arguments,
                        span,
                    }),
                })
            } else {
                errors.push(Error {
                    span: *span,
                    msg: "Expected `)` at the end of function call.".to_string(),
                });
                Some(ExpressionSpanned {
                    span: *span,
                    expression: Expression::FunctionCall(FunctionCall {
                        identifier,
                        arguments,
                        span: *span,
                    }),
                })
            }
        }
        Some(TokenSpanned {
            token: Token::CurlyBracketOpen,
            span,
        }) => {
            let mut members: Vec<(String, Option<ExpressionSpanned>)> = Vec::new();
            'outer: loop {
                if let Some(TokenSpanned {
                    token: Token::Identifier(identifier),
                    span,
                }) = iter.next()
                {
                    match iter.next() {
                        Some(TokenSpanned {
                            token: Token::Colon,
                            span: _,
                        }) => {}
                        _ => {
                            errors.push(Error {
                                span: *span,
                                msg: "Expected `:` after a member identifier in a struct literal."
                                    .to_string(),
                            });
                        }
                    }
                    members.push((
                        identifier.clone(),
                        parse_expression(iter, errors),
                    ));
                    match iter.peek() {
                        Some(TokenSpanned {
                            token: Token::Coma,
                            span: _,
                        }) => {
                            iter.next();
                        }
                        Some(TokenSpanned {
                            token: Token::CurlyBracketClose,
                            span: _,
                        }) => break,
                        Some(TokenSpanned { token: _, span }) => {
                            errors.push(Error {
                                span: *span,
                                msg: "Expected `,` or `}` in a struct literal.".to_string(),
                            });
                            while let Some(TokenSpanned { token, span: _ }) = iter.peek() {
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
                        span: *span,
                        msg: "Expected a member identifier in a struct literal.".to_string(),
                    });
                }
            }

            if let Some(TokenSpanned {
                token: Token::CurlyBracketClose,
                span: end,
            }) = iter.next()
            {
                Some(ExpressionSpanned {
                    span: Span::between(span, end),
                    expression: Expression::StructLiteral {
                        identifier,
                        members,
                    },
                })
            } else {
                errors.push(Error {
                    span: *span,
                    msg: "Expected `}}` at the end of a struct literal.".to_string(),
                });
                Some(ExpressionSpanned {
                    span: *span,
                    expression: Expression::StructLiteral {
                        identifier,
                        members,
                    },
                })
            }
        }
        _ => Some(ExpressionSpanned {
            span: *span,
            expression: Expression::Identifier(identifier.clone()),
        }),
    }
}
