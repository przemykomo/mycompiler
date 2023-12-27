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
    Identifier(String),
    ArithmeticExpression { left: Box<Expression>, right: Box<Expression>, operator: ArithmeticOperator },
    ComparisonExpression { left: Box<Expression>, right: Box<Expression>, operator: ComparisonOperator },
    StringLiteral(String),
    Dereference(Box<Expression>),
    AddressOf(Box<Expression>),
    ArraySubscript { identifier: String, element: Box<Expression> },
    FunctionCall(FunctionCall),
    Assigment { left: Box<Expression>, right: Box<Expression> },
    MemberAccess { left: Box<Expression>, right: Box<Expression> }
}

#[derive(Debug)]
pub struct StructDeclaration {
    pub identifier: String,
    pub members: Vec<StructMember>
}

#[derive(Debug, Clone)]
pub struct StructMember {
    pub identifier: String,
    pub data_type: DataType
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
    Increment(String),
    Decrement(String),
    If { expression: Expression, scope: Vec<Statement>, else_scope: Option<Vec<Statement>> },
    Return(Expression),
    Expression(Expression)
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
                    while let Some((Token::DataType(data_type), Token::Identifier(member_name))) = iter.clone().next_tuple() {
                        dbg!(iter.next());
                        dbg!(iter.next());

                        members.push(StructMember { identifier: member_name.clone(), data_type: data_type.clone() });
                        match iter.next() {
                            Some(Token::Coma) => {},
                            Some(Token::CurlyBracketClose) => {
                                break;
                            },
                            other => {
                                panic!("Unexpected \'{:?}\'! Expected a '}}' after last struct element!", other);
                            }
                        }
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

fn parse_variable_definition(iter: &mut Peekable<Iter<Token>>, abstract_syntax_tree: &mut Vec<Statement>, data_type: &DataType) {
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
                    token => {
                        dbg!(abstract_syntax_tree);
                        panic!("Unexpected token \'{:?}\'.", token);
                    }
            }
            abstract_syntax_tree.push(Statement::VariableDefinition { identifier: identifier.clone(), expression: my_expression, data_type: data_type.clone() });
        },
        token => {
            panic!("Unexpected token \'{:?}\'.", token);
        }
    }
}

fn parse_variable_definition_with_data_type(abstract_syntax_tree: &mut Vec<Statement>, data_type: &DataType, iter: &mut Peekable<Iter<Token>>) {
    match iter.peek() {
        Some(Token::SquareParenthesisOpen) => {
            iter.next();
            if let Some((Token::IntLiteral(size), Token::SquareParenthesisClose)) = iter.next_tuple() {
                parse_variable_definition(iter, abstract_syntax_tree, &DataType::Array { data_type: Box::new(data_type.clone()), size: *size });
            } else {
                panic!("Expected an int literal in an array definition!");
            }
        },
        Some(Token::MultiplySign) => {
            iter.next();
            parse_variable_definition(iter, abstract_syntax_tree, &DataType::Pointer(Box::new(data_type.clone())));
        },
        _ => {
            parse_variable_definition(iter, abstract_syntax_tree, data_type);
        }
    }
}

pub fn parse_scope(iter: &mut Peekable<Iter<Token>>) -> Vec::<Statement> {
    let mut abstract_syntax_tree = Vec::<Statement>::new();

    while let Some(token) = iter.peek() {
        dbg!(token);
        match token {
            Token::DataType(data_type) => {
                iter.next();
                parse_variable_definition_with_data_type(&mut abstract_syntax_tree, data_type, iter);
            },
            Token::Identifier(_struct_name) => {
                let mut iter_clone = iter.clone();
                dbg!(_struct_name);
                if let Some((Token::Identifier(struct_name), Token::Identifier(_identifier))) = iter_clone.next_tuple() {
                    dbg!(iter.next());
                    parse_variable_definition_with_data_type(&mut abstract_syntax_tree, &DataType::Struct(struct_name.clone()), iter);
                } else {
                    abstract_syntax_tree.push(Statement::Expression(parse_expression(iter)));
                }
            },
            Token::If => {
                iter.next();
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
                iter.next();
                return abstract_syntax_tree;
            },
            Token::Return => {
                iter.next();
                abstract_syntax_tree.push(Statement::Return(parse_expression(iter)));
            },
            Token::Semicolon => {
                iter.next();
            },
            _ => {
                abstract_syntax_tree.push(Statement::Expression(parse_expression(iter)));
            }
        }
    }

    panic!("Cannot find close bracket of a scope.");
}

fn parse_expression(iter: &mut Peekable<Iter<Token>>) -> Expression {
    return parse_assigment(iter);
}

fn parse_assigment(iter: &mut Peekable<Iter<Token>>) -> Expression {
    let mut left = parse_addition(iter);

    match iter.peek() {
        Some(Token::EqualSign) => {
            iter.next();
            let right = parse_relational(iter);
            left = Expression::Assigment { left: Box::new(left), right: Box::new(right) }
        },
        _ => {
            return left;
        }
    }
    

    return left;
}

fn parse_relational(iter: &mut Peekable<Iter<Token>>) -> Expression {
    let mut left = parse_addition(iter);

    match iter.peek() {
        Some(Token::CompareEqual) => {
            iter.next();
            let right = parse_addition(iter);
            left = Expression::ComparisonExpression { left: Box::new(left), right: Box::new(right), operator: ComparisonOperator::CompareEqual };
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
    let mut left = parse_literals_pointers(iter);

    while let Some(token) = iter.peek() {
        if let Token::MultiplySign = token {
            iter.next();
            let primitive = parse_literals_pointers(iter);
            left = Expression::ArithmeticExpression { left: Box::new(left), right: Box::new(primitive), operator: ArithmeticOperator::Multiply };
        } else if let Token::DivisionSign = token {
            iter.next();
            let primitive = parse_literals_pointers(iter);
            left = Expression::ArithmeticExpression { left: Box::new(left), right: Box::new(primitive), operator: ArithmeticOperator::Divide };
        } else {
            return left;
        }
    }

    return left;
}

fn parse_literals_pointers(iter: &mut Peekable<Iter<Token>>) -> Expression {
    let next = iter.peek();
    dbg!(next);
    match next {
        Some(Token::IntLiteral(value)) => {
            iter.next();
            Expression::IntLiteral(*value)
        },
        Some(Token::FloatLiteral(value)) => {
            iter.next();
            Expression::FloatLiteral(value.clone())
        },
        Some(Token::CharacterLiteral(c)) => {
            iter.next();
            Expression::CharacterLiteral(c.clone())
        },
        Some(Token::BoolLiteral(b)) => {
            iter.next();
            Expression::BoolLiteral(b.clone())
        },
        Some(Token::StringLiteral(value)) => {
            iter.next();
            Expression::StringLiteral(value.clone())
        },
        Some(Token::MultiplySign) => {
            iter.next();
            Expression::Dereference(Box::new(parse_member_access(iter)))
        },
        Some(Token::Ampersand) => {
            iter.next();
            Expression::AddressOf(Box::new(parse_member_access(iter)))
        },
        _ => parse_member_access(iter)
    }
}

fn parse_member_access(iter: &mut Peekable<Iter<Token>>) -> Expression {
    let mut expression = parse_atom(iter);

    while let Some(Token::Period) = iter.peek() {
        iter.next();
        expression = Expression::MemberAccess { left: Box::new(expression), right: Box::new(parse_atom(iter)) };
    }

    return expression;
}

fn parse_atom(iter: &mut Peekable<Iter<Token>>) -> Expression {
    if let Some(Token::Identifier(identifier)) = iter.next() {
        match iter.peek() {
            Some(Token::SquareParenthesisOpen) => {
                iter.next();
                let expression = parse_expression(iter);
                let Some(Token::SquareParenthesisClose) = iter.next() else {
                    panic!("Expected closed square parenthesis.");
                };
                Expression::ArraySubscript { identifier: identifier.clone(), element: Box::new(expression) }
            },
            Some(Token::ParenthesisOpen) => {
                iter.next();

                let arguments = if let Some(Token::ParenthesisClose) = iter.peek() {
                    Vec::new()
                } else {
                    parse_arguments_passing(iter)
                };
                
                if let Some(Token::ParenthesisClose) = iter.next() {
                    Expression::FunctionCall(FunctionCall { identifier: identifier.clone() , arguments })
                } else {
                    panic!("Expected ')' at the end of function call.");
                }
            },
            _ => Expression::Identifier(identifier.clone()),
        }
    } else {
        panic!("Expected an identifier.");
    }
}
