use crate::tokenizer::*;
use std::iter::Peekable;
use std::slice::Iter;
use itertools::Itertools;

#[derive(Debug)]
pub enum Expression {
    IntLiteral(i32),
    CharacterLiteral(char),
    BoolLiteral(bool),
    FloatLiteral(String),
    Identifier(Vec<String>),
    ArithmeticExpression { left: Box<Expression>, right: Box<Expression>, operator: ArithmeticOperator },
    ComparisonExpression { left: Box<Expression>, right: Box<Expression>, operator: ComparisonOperator },
    StringLiteral(String),
    Dereference(Box<Expression>),
    Reference(Vec<String>),
    AccessArrayElement { identifier: Vec<String>, element: Box<Expression> },
    FunctionCall(FunctionCall)
}

#[derive(Debug)]
pub struct StructDeclaration {
    identifier: String,
    members: Vec<StructMember>
}

#[derive(Debug)]
pub struct StructMember {
    identifier: String,
    data_type: DataType
}

#[derive(Debug)]
pub enum ArithmeticOperator {
    Add,
    Subtract,
    Multiply,
    Divide
}

#[derive(Debug)]
pub enum ComparisonOperator {
    CompareEqual,
    CompareLarger,
    CompareSmaller
}

#[derive(Debug)]
pub enum Statement {
    VariableDefinition { identifier: String, expression: Option<Expression>, data_type: DataType },
    VariableAssigment { identifier: Vec<String>, expression: Expression },
    ArrayElementAssigment { identifier: Vec<String>, element: Expression, expression: Expression },
    Increment(Vec<String>),
    Decrement(Vec<String>),
    FunctionCall(FunctionCall),
    If { expression: Expression, scope: Vec<Statement>, else_scope: Option<Vec<Statement>> },
    Return(Expression)
}

#[derive(Debug)]
pub struct FunctionCall {
    pub identifier: String,
    pub arguments: Vec<String>
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub prototype: FunctionPrototype,
    pub public: bool,
    pub body: Vec<Statement>
}

#[derive(Debug)]
pub struct FunctionPrototype {
    pub name: String,
    pub return_type: DataType,
    pub arguments: Vec<DataType>
}

#[derive(Debug)]
pub struct ParsedUnit {
    pub function_declarations: Vec<FunctionPrototype>,
    pub functions: Vec<FunctionDefinition>,
    pub struct_declarations: Vec<StructDeclaration>
}

pub fn parse_arguments_declaration(iter: &mut Peekable<Iter<Token>>) -> Vec<DataType> {
    let mut arguments = Vec::<DataType>::new();

    loop {
        if let Some(Token::DataType(data_type)) = iter.next() {
            if let Some(Token::Identifier(_)) = iter.next() {
                arguments.push(data_type.clone());
            } else {
                panic!("Expected an identifier after a data type in the function");
            }
        } else {
            panic!("Unexpected token in the function arguments")
        }

        if let Some(Token::Coma) = iter.peek() {
            iter.next();
        } else {
            break;
        }
    }

    return arguments;
}

pub fn parse_arguments_passing(iter: &mut Peekable<Iter<Token>>) -> Vec<String> {
    let mut arguments = Vec::<String>::new();

    loop {
        if let Some(Token::Identifier(identifier)) = iter.next() {
            arguments.push(identifier.clone());
        } else {
            panic!("Expected an identifier as a function parameter!");
        }

        if let Some(Token::Coma) = iter.peek() {
            iter.next();
        } else {
            break;
        }
    }
    
    return arguments;
}

pub fn parse(tokens: &Vec<Token>) -> ParsedUnit {
    let mut parsed_unit = ParsedUnit {
        function_declarations: Vec::new(),
        functions: Vec::new(),
        struct_declarations: Vec::new()
    };
    let mut iter = tokens.iter().peekable();

    while let Some(token) = iter.next() {
        dbg!(token);
        match token {
            Token::Struct => {
                if let Some((Token::Identifier(identifier), Token::CurlyBracketOpen)) = iter.next_tuple() {
                    let mut members : Vec<StructMember> = Vec::new();
                    while let Some((Token::DataType(data_type), Token::Identifier(member_name))) = iter.next_tuple() {
                        members.push(StructMember { identifier: member_name.clone(), data_type: data_type.clone() });
                        let Some(Token::Coma) = iter.next() else {
                            if let Some(Token::CurlyBracketClose) = iter.next() {
                                break;
                            } else {
                                panic!("Expected a '}}' after last struct element!");
                            }
                        };
                    }

                    parsed_unit.struct_declarations.push(StructDeclaration { identifier: identifier.clone(), members })
                } else {
                    panic!("Expected struct declaration after the struct token!");
                }
            },
            Token::DataType(return_type) => {
                if let Some((Token::Identifier(identifier), Token::ParenthesisOpen)) = iter.next_tuple() {
                    let arguments = if let Some(Token::ParenthesisClose) = iter.peek() {
                        Vec::new()
                    } else {
                        parse_arguments_declaration(&mut iter)
                    };

                    if let Some(Token::ParenthesisClose) = iter.next() {
                        let function_prototype = FunctionPrototype { name: identifier.clone(), return_type: return_type.clone(), arguments };
                        match iter.next() {
                            Some(Token::Semicolon) => parsed_unit.function_declarations.push(function_prototype),
                            Some(Token::CurlyBracketOpen) => {
                                let function = FunctionDefinition {
                                    prototype: function_prototype,
                                    public: false,
                                    body: parse_scope(&mut iter)
                                };
                                parsed_unit.functions.push(function);
                            },
                            _ => panic!("Expected either \";\" or \"{{\" after the function prototype!")
                        }
                    } else {
                        panic!("Expected closed parenthesis after function arguments!");
                    }

                } else {
                    panic!("Syntax error while paring function signature.");
                }
            },
            Token::Public => {
                if let Some((Token::DataType(return_type), Token::Identifier(identifier), Token::ParenthesisOpen)) = iter.next_tuple() {
                    let arguments = if let Some(Token::ParenthesisClose) = iter.peek() {
                        Vec::new()
                    } else {
                        parse_arguments_declaration(&mut iter)
                    };

                    if let Some((Token::ParenthesisClose, Token::CurlyBracketOpen)) = iter.next_tuple() {
                        let function_prototype = FunctionPrototype { name: identifier.clone(), return_type: return_type.clone(), arguments };

                        let function = FunctionDefinition {
                            prototype: function_prototype,
                            public: true,
                            body: parse_scope(&mut iter)
                        };
                        parsed_unit.functions.push(function);
                    } else {
                        panic!("Expected closed parenthesis after function arguments!");
                    }
                } else {
                    panic!("Syntax error while paring a public function signature.");
                }
            },
            _ => {
                panic!("Syntax error.");
            }
        }
    }

    return parsed_unit;
}

fn variable_definition(iter: &mut Peekable<Iter<Token>>, abstract_syntax_tree: &mut Vec<Statement>, data_type: &DataType) {
    match iter.next() {
        Some(Token::Identifier(identifier)) => {
                let my_expression: Option<Expression>;
                match iter.next() {
                    Some(Token::EqualSign) => {
                        my_expression = Some(parse_expression(iter));
                    },
                    Some(Token::Semicolon) => {
                        my_expression = None;
                    },
                    _ => {
                        panic!("Syntax error.");
                    }
            }
            abstract_syntax_tree.push(Statement::VariableDefinition { identifier: identifier.clone(), expression: my_expression, data_type: data_type.clone() });
        },
        token => {
            dbg!(token);
            panic!("Unexpected token!");
        }
    }
}

pub fn parse_identifier(iter: &mut Peekable<Iter<Token>>, first: String) -> Vec<String> {
    let mut identifiers: Vec<String> = Vec::new();
    identifiers.push(first);
    while let Some((Token::Period, Token::Identifier(identifier))) = iter.next_tuple() {
        identifiers.push(identifier.clone());
    }
    return identifiers;
}

pub fn parse_scope(iter: &mut Peekable<Iter<Token>>) -> Vec::<Statement> {
    let mut abstract_syntax_tree = Vec::<Statement>::new();

    while let Some(token) = iter.next() {
        dbg!(token);
        match token {
            Token::DataType(data_type) => {
                match iter.peek() {
                    Some(Token::SquareParenthesisOpen) => {
                        iter.next();
                        if let Some((Token::IntLiteral(size), Token::SquareParenthesisClose)) = iter.next_tuple() {
                            variable_definition(iter, &mut abstract_syntax_tree, &DataType::Array { data_type: Box::new(data_type.clone()), size: *size });
                        } else {
                            panic!("Expected an int literal in an array definition!");
                        }
                    },
                    Some(Token::MultiplySign) => {
                        iter.next();
                        variable_definition(iter, &mut abstract_syntax_tree, &DataType::Pointer(Box::new(data_type.clone())));
                    },
                    _ => {
                        variable_definition(iter, &mut abstract_syntax_tree, data_type);
                    }
                }
            },
            Token::Identifier(first) => {
                let identifier = parse_identifier(iter, first.clone());
                match iter.next() {
                    Some(Token::PlusSign) => {
                        if let Some(Token::PlusSign) = iter.next() {
                            abstract_syntax_tree.push(Statement::Increment(identifier));
                        } else {
                            panic!();
                        }
                    },
                    Some(Token::MinusSign) => {
                        if let Some(Token::MinusSign) = iter.next() {
                            abstract_syntax_tree.push(Statement::Decrement(identifier));
                        } else {
                            panic!();
                        }
                    },
                    Some(Token::EqualSign) => {
                        abstract_syntax_tree.push(Statement::VariableAssigment { identifier, expression: parse_expression(iter) });
                    },
                    Some(Token::ParenthesisOpen) => {
                        let arguments = if let Some(Token::ParenthesisClose) = iter.peek() {
                            Vec::new()
                        } else {
                            parse_arguments_passing(iter)
                        };

                        if let Some(Token::ParenthesisClose) = iter.next() {
                            if identifier.len() == 1 {
                                abstract_syntax_tree.push(Statement::FunctionCall(FunctionCall { identifier: first.clone(), arguments }));
                            } else {
                                panic!("Trying to use a struct member as a function name!\n");
                            }
                        }
                    },
                    Some(Token::SquareParenthesisOpen) => {
                        let element = parse_expression(iter);
                        if let Some((Token::SquareParenthesisClose, Token::EqualSign)) = iter.next_tuple() {
                            let expression = parse_expression(iter);
                            abstract_syntax_tree.push(Statement::ArrayElementAssigment { identifier, element, expression });
                        }
                    },
                    other => {
                        dbg!(other);
                        panic!("Syntax error.");
                    }
                }
            },
            Token::If => {
                if let Some(Token::ParenthesisOpen) = iter.next() {
                    let expression = parse_expression(iter);
                    if let Some((Token::ParenthesisClose, Token::CurlyBracketOpen)) = iter.next_tuple() {
                        let scope = parse_scope(iter);
                        let else_scope = if let Some(Token::Else) = iter.peek() {
                            iter.next();
                            if let Some(Token::CurlyBracketOpen) = iter.next() {
                                Some(parse_scope(iter))
                            } else {
                                panic!("Expected opened curly brackets in th else statement!");
                            }
                        } else {
                            None
                        };
                        abstract_syntax_tree.push(Statement::If { expression, scope, else_scope });
                    } else {
                        panic!("Expected closed parenthesis and opened curly brackets in the if statement!");
                    }
                } else {
                    panic!("Expected parenthesis after an if token!");
                }
            },
            Token::CurlyBracketClose => {
                return abstract_syntax_tree;
            },
            Token::Return => {
                abstract_syntax_tree.push(Statement::Return(parse_expression(iter)));
            },
            _ => {}
        }
    }

    panic!("Cannot find close bracket of a scope.");
}

fn parse_expression(iter: &mut Peekable<Iter<Token>>) -> Expression {
    return parse_relational(iter);
}

fn parse_relational(iter: &mut Peekable<Iter<Token>>) -> Expression {
    let mut left = parse_addition(iter);

    match iter.peek() {
        Some(Token::EqualSign) => {
            iter.next();
            if let Some(Token::EqualSign) = iter.next() {
                let right = parse_addition(iter);
                left = Expression::ComparisonExpression { left: Box::new(left), right: Box::new(right), operator: ComparisonOperator::CompareEqual };
            } else {
                panic!("Cannot assign values in expressions!");
            }
        },
        Some(Token::LargerThan) => {
            iter.next();
            let right = parse_addition(iter);
            left = Expression::ComparisonExpression { left: Box::new(left), right: Box::new(right), operator: ComparisonOperator::CompareLarger };
        },
        Some(Token::SmallerThan) => {
            iter.next();
            let right = parse_addition(iter);
            left = Expression::ComparisonExpression { left: Box::new(left), right: Box::new(right), operator: ComparisonOperator::CompareSmaller };
        },        
        _ => {
            return left;
        }
    }

    return left;
}

fn parse_addition(iter: &mut Peekable<Iter<Token>>) -> Expression {
    let mut left = parse_multiplication(iter);

    while let Some(token) = iter.peek() {
        if let Token::PlusSign = token {
            iter.next();
            let primitive = parse_multiplication(iter);
            left = Expression::ArithmeticExpression { left: Box::new(left), right: Box::new(primitive), operator: ArithmeticOperator::Add };
        } else if let Token::MinusSign = token {
            iter.next();
            let primitive = parse_multiplication(iter);
            left = Expression::ArithmeticExpression { left: Box::new(left), right: Box::new(primitive), operator: ArithmeticOperator::Subtract };
        } else {
            return left;
        }
    }

    return left;
}

fn parse_multiplication(iter: &mut Peekable<Iter<Token>>) -> Expression {
    let mut left = parse_atom(iter);

    while let Some(token) = iter.peek() {
        if let Token::MultiplySign = token {
            iter.next();
            let primitive = parse_atom(iter);
            left = Expression::ArithmeticExpression { left: Box::new(left), right: Box::new(primitive), operator: ArithmeticOperator::Multiply };
        } else if let Token::DivisionSign = token {
            iter.next();
            let primitive = parse_atom(iter);
            left = Expression::ArithmeticExpression { left: Box::new(left), right: Box::new(primitive), operator: ArithmeticOperator::Divide };
        } else {
            return left;
        }
    }

    return left;
}

fn parse_atom(iter: &mut Peekable<Iter<Token>>) -> Expression {
    match iter.next() {
        Some(Token::IntLiteral(value)) => Expression::IntLiteral(*value),
        Some(Token::FloatLiteral(value)) => Expression::FloatLiteral(value.clone()),
        Some(Token::Identifier(first)) => {
            let mut identifier = parse_identifier(iter, first.clone());
            match iter.peek() {
                Some(Token::SquareParenthesisOpen) => {
                    iter.next();
                    let expression = parse_expression(iter);
                    let Some(Token::SquareParenthesisClose) = iter.next() else {
                        panic!("Expected closed square parenthesis.");
                    };
                    Expression::AccessArrayElement { identifier: identifier.clone(), element: Box::new(expression) }                    
                },
                Some(Token::ParenthesisOpen) => {
                    iter.next();

                    let arguments = if let Some(Token::ParenthesisClose) = iter.peek() {
                        Vec::new()
                    } else {
                        parse_arguments_passing(iter)
                    };
                    
                    if let Some(Token::ParenthesisClose) = iter.next() {
                        if identifier.len() == 1 {
                            Expression::FunctionCall(FunctionCall { identifier: first.clone() , arguments })
                        } else {
                            panic!("Trying to use a struct member as a function name!\n");
                        }
                    } else {
                        panic!("Expected ')' at the end of function call.");
                    }
                },
                _ => Expression::Identifier(identifier.clone()),
            }
        },
        Some(Token::CharacterLiteral(c)) => Expression::CharacterLiteral(c.clone()),
        Some(Token::BoolLiteral(b)) => Expression::BoolLiteral(b.clone()),
        Some(Token::MultiplySign) => Expression::Dereference(Box::new(parse_atom(iter))),
        Some(Token::Ampersand) => {
            if let Some(Token::Identifier(first)) = iter.next() {
                let mut identififer = parse_identifier(iter, first.clone());
                Expression::Reference(identififer)
            } else {
                panic!("Expected an identifier after a reference operator.");
            }
        },
        Some(Token::ParenthesisOpen) => {
            let expression = parse_expression(iter);
            let Some(Token::ParenthesisClose) = iter.next() else {
                panic!("Expecred closed parenthesis.");
            };
            expression
        },
        Some(Token::StringLiteral(value)) => Expression::StringLiteral(value.clone()),
        _ => {
            panic!("Cannot parse an atom expression!");
        }
    }
}
