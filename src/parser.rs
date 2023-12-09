use crate::tokenizer::*;
use std::iter::Peekable;
use std::slice::Iter;
use itertools::Itertools;

#[derive(Debug)]
pub enum Expression {
    IntLiteral(i32),
    CharacterLiteral(char),
    Identifier(String),
    ArithmeticExpression { left: Box<Expression>, right: Box<Expression>, operator: ArithmeticOperator },
    ComparisonExpression { left: Box<Expression>, right: Box<Expression>, operator: ComparisonOperator },
    StringLiteral(String),
    Dereference(Box<Expression>),
    Reference(String),
    AccessArrayElement { identifier: String, element: Box<Expression> }
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
    VariableAssigment { identifier: String, expression: Expression },
    ArrayElementAssigment { identifier: String, element: Expression, expression: Expression },
    Increment(String),
    Decrement(String),
    FunctionCall { identifier: String, arguments: Vec<String> },
    If { expression: Expression, scope: Vec::<Statement> }
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub public: bool,
    pub body: Vec::<Statement>
}

#[derive(Debug)]
pub struct ParsedUnit {
    pub extern_declarations: Vec::<String>,
    pub functions: Vec<Function>
}

pub fn parse(tokens: &Vec<Token>) -> ParsedUnit {
    let mut parsed_unit = ParsedUnit {
        extern_declarations: Vec::new(),
        functions: Vec::new()
    };
    let mut iter = tokens.iter().peekable();

    while let Some(token) = iter.next() {
        dbg!(token);
        match token {
            Token::Extern => {
                if let Some((Token::Identifier(identifier), Token::Semicolon)) = iter.next_tuple() {
                    parsed_unit.extern_declarations.push(identifier.to_string());
                } else {
                    panic!("Syntax error while parsing an extern statement.");
                }
            },
            Token::Function => {
                if let Some((Token::Identifier(identifier), Token::ParenthesisOpen,
                    Token::ParenthesisClose, Token::CurlyBracketOpen)) = iter.next_tuple() {
                    let function = Function {
                        name: identifier.clone(),
                        public: false,
                        body: parse_scope(&mut iter)
                    };
                    parsed_unit.functions.push(function);
                } else {
                    panic!("Syntax error while paring function signature.");
                }
            },
            Token::Public => {
                if let Some((Token::Function, Token::Identifier(identifier), Token::ParenthesisOpen,
                    Token::ParenthesisClose, Token::CurlyBracketOpen)) = iter.next_tuple() {
                    let function = Function {
                        name: identifier.clone(),
                        public: true,
                        body: parse_scope(&mut iter)
                    };
                    parsed_unit.functions.push(function);
                } else {
                    panic!("Syntax error while paring function signature.");
                }
            }
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

pub fn parse_scope(iter: &mut Peekable<Iter<Token>>) -> Vec::<Statement> {
    let mut abstract_syntax_tree = Vec::<Statement>::new();

    while let Some(token) = iter.next() {
        dbg!(token);
        match token {
            Token::VariableDefinition(data_type) => {
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
                        variable_definition(iter, &mut abstract_syntax_tree, &DataType::Pointer { data_type: Box::new(data_type.clone()) });
                    },
                    _ => {
                        variable_definition(iter, &mut abstract_syntax_tree, data_type);
                    }
                }
            },
            Token::Identifier(identifier) => {
                match iter.next() {
                    Some(Token::PlusSign) => {
                        if let Some(Token::PlusSign) = iter.next() {
                            abstract_syntax_tree.push(Statement::Increment(identifier.clone()));
                        } else {
                            panic!();
                        }
                    },
                    Some(Token::MinusSign) => {
                        if let Some(Token::MinusSign) = iter.next() {
                            abstract_syntax_tree.push(Statement::Decrement(identifier.clone()));
                        } else {
                            panic!();
                        }
                    },
                    Some(Token::EqualSign) => {
                        abstract_syntax_tree.push(Statement::VariableAssigment { identifier: identifier.clone(), expression: parse_expression(iter) });
                    },
                    Some(Token::ParenthesisOpen) => {
                        let mut arguments : Vec<String> = Vec::new();
                        while let Some(Token::Identifier(identifier)) = iter.peek() {
                            iter.next();
                            arguments.push(identifier.to_string());
                        }
                        if let Some(Token::ParenthesisClose) = iter.next() {
                            abstract_syntax_tree.push(Statement::FunctionCall { identifier: identifier.clone(), arguments });
                        }
                    },
                    Some(Token::SquareParenthesisOpen) => {
                        let element = parse_expression(iter);
                        if let Some((Token::SquareParenthesisClose, Token::EqualSign)) = iter.next_tuple() {
                            let expression = parse_expression(iter);
                            abstract_syntax_tree.push(Statement::ArrayElementAssigment { identifier: identifier.clone(), element, expression });
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
                        abstract_syntax_tree.push(Statement::If { expression, scope });
                    } else {
                        panic!("Expected closed parenthesis and opened curly brackets in the if statement!");
                    }
                } else {
                    panic!("Expected parenthesis after an if token!");
                }
            },
            Token::CurlyBracketClose => {
                return abstract_syntax_tree;
            }
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
        Some(Token::Identifier(other)) => {
            if let Some(Token::SquareParenthesisOpen) = iter.peek() {
                iter.next();
                let expression = parse_expression(iter);
                let Some(Token::SquareParenthesisClose) = iter.next() else {
                    panic!("Expected closed square parenthesis.");
                };
                Expression::AccessArrayElement { identifier: other.clone(), element: Box::new(expression) }                    
            } else {
                Expression::Identifier(other.clone())
            }
        },
        Some(Token::CharacterLiteral(c)) => Expression::CharacterLiteral(c.clone()),
        Some(Token::MultiplySign) => Expression::Dereference(Box::new(parse_atom(iter))),
        Some(Token::Ampersand) => {
            if let Some(Token::Identifier(variable)) = iter.next() {
                Expression::Reference(variable.clone())
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
