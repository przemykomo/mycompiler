use crate::tokenizer::*;

#[derive(Debug)]
pub enum Expression {
    IntLiteral(i32),
    Identifier(String)
}

#[derive(Debug)]
pub enum Statement {
    Exit(Expression),
    VariableDefinition { identifier: String, value: i32 }
}


pub fn parse(tokens: &Vec<Token>) -> Vec<Statement> {
    let mut abstract_syntax_tree = Vec::<Statement>::new();
    let mut iter = tokens.iter().peekable();

    while let Some(token) = iter.next() {
        match token {
            Token::Exit => {
                match iter.next() {
                    Some(Token::IntLiteral(exit_value)) => {
                        abstract_syntax_tree.push(Statement::Exit(Expression::IntLiteral(*exit_value)));
                    },
                    Some(Token::Identifier(identifier)) => {
                        abstract_syntax_tree.push(Statement::Exit(Expression::Identifier(identifier.clone())));
                    },
                    _ => {
                        panic!("After exit should be either IntLiteral or Identifier token.");
                    }
                }
            },
            Token::Let => {
                if let Some(Token::Identifier(identifier)) = iter.next() {
                    if let Some(Token::EqualSign) = iter.next() {
                        if let Some(Token::IntLiteral(value)) = iter.next() {
                            abstract_syntax_tree.push(Statement::VariableDefinition { identifier: identifier.clone(), value: *value });
                        }
                    }
                }
            },
            _ => {}
        }
    }

    return abstract_syntax_tree;
}