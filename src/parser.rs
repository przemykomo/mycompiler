use crate::tokenizer::*;
use std::iter::Peekable;
use std::slice::Iter;
use itertools::Itertools;

#[derive(Debug)] //todo: maybe put DataType as a value inside all Expression types?
pub enum Expression {
    IntLiteral(i32),
    CharacterLiteral(char),
    Identifier(String),
    Add { left: Box<Expression>, right: Box<Expression> },
    Multiply { left: Box<Expression>, right: Box<Expression> },
    Subtract { left: Box<Expression>, right: Box<Expression> },
    Divide { left: Box<Expression>, right: Box<Expression> },
    StringLiteral(String),
    Dereference(Box<Expression>),
    Reference(String)
}

#[derive(Debug)]
pub enum Statement {
    Exit(Expression),
    VariableDefinition { identifier: String, expression: Expression, data_type: DataType },
    VariableAssigment { identifier: String, expression: Expression },
    Increment(String),
    Decrement(String),
    FunctionCall { identifier: String, arguments: Vec<String> }
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
    if let Some(Token::Identifier(identifier)) = iter.next() {
            let my_expression: Expression;
            match iter.next() {
                Some(Token::EqualSign) => {
                    my_expression = parse_expression(iter);
                },
                Some(Token::Semicolon) => {
                    my_expression = Expression::IntLiteral(0);
                },
                _ => {
                    panic!("Syntax error.");
                }
        }
        abstract_syntax_tree.push(Statement::VariableDefinition { identifier: identifier.clone(), expression: my_expression, data_type: data_type.clone() });
    }
}

pub fn parse_scope(iter: &mut Peekable<Iter<Token>>) -> Vec::<Statement> {
    let mut abstract_syntax_tree = Vec::<Statement>::new();

    while let Some(token) = iter.next() {
        match token {
            /*
            Token::Exit => {
                let my_expression: Expression = parse_expression(iter);
                abstract_syntax_tree.push(Statement::Exit(my_expression));
            },*/
            Token::VariableDefinition(data_type) => {
                variable_definition(iter, &mut abstract_syntax_tree, data_type);
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
                    }
                    _ => {
                        panic!("Syntax error.");
                    }
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
    return parse_addition(iter);
}

fn parse_addition(iter: &mut Peekable<Iter<Token>>) -> Expression {
    let mut left = parse_multiplication(iter);

    while let Some(token) = iter.peek() {
        if let Token::PlusSign = token {
            iter.next();
            let primitive = parse_multiplication(iter);
            left = Expression::Add { left: Box::new(left), right: Box::new(primitive) };
        } else if let Token::MinusSign = token {
            iter.next();
            let primitive = parse_multiplication(iter);
            left = Expression::Subtract{ left: Box::new(left), right: Box::new(primitive) };
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
            left = Expression::Multiply { left: Box::new(left), right: Box::new(primitive) };
        } else if let Token::DivisionSign = token {
            iter.next();
            let primitive = parse_atom(iter);
            left = Expression::Divide { left: Box::new(left), right: Box::new(primitive) };
        } else {
            return left;
        }
    }

    return left;
}

fn parse_atom(iter: &mut Peekable<Iter<Token>>) -> Expression {
    match iter.next() {
        Some(Token::IntLiteral(value)) => Expression::IntLiteral(*value),
        Some(Token::Identifier(other)) => Expression::Identifier(other.clone()),
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
