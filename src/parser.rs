use crate::tokenizer::*;
use std::iter::Peekable;
use std::slice::Iter;

#[derive(Debug)]
pub enum Expression {
    IntLiteral(i32),
    Identifier(String),
    Add{ left: Box<Expression>, right: Box<Expression> },
    Multiply{ left: Box<Expression>, right: Box<Expression> },
    Subtract{ left: Box<Expression>, right: Box<Expression> },
    Divide{ left: Box<Expression>, right: Box<Expression> }
}

#[derive(Debug)]
pub enum Statement {
    Exit(Expression),
    VariableDefinition { identifier: String, expression: Expression }
}

pub fn parse(tokens: &Vec<Token>) -> Vec<Statement> {
    let mut abstract_syntax_tree = Vec::<Statement>::new();
    let mut iter = tokens.iter().peekable();

    while let Some(token) = iter.next() {
        match token {
            Token::Exit => {
                let my_expression : Expression = parse_expression(&mut iter);
                abstract_syntax_tree.push(Statement::Exit(my_expression));
            },
            Token::Let => {
                if let Some(Token::Identifier(identifier)) = iter.next() {
                    let my_expression : Expression;
                    match iter.next() {
                        Some(Token::EqualSign) => {
                            my_expression = parse_expression(&mut iter);
                        },
                        Some(Token::Semicolon) => {
                            my_expression = Expression::IntLiteral(0);
                        },
                        _ => {
                            panic!("Syntax error.");
                        }
                    }
                    abstract_syntax_tree.push(Statement::VariableDefinition { identifier: identifier.clone(), expression: my_expression });
                }
            },
            _ => {}
        }
    }

    return abstract_syntax_tree;
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
        _ => {
            panic!("NO!");
        }
    }
}
