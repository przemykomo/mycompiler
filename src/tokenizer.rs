#[derive(Debug)]
pub enum Token {
    //Exit,
    IntLiteral(i32),
    CharacterLiteral(char),
    Semicolon,
    VariableDefinition(DataType),
    Identifier(String),
    EqualSign,
    PlusSign,
    MultiplySign,
    MinusSign,
    DivisionSign,
    ParenthesisOpen,
    ParenthesisClose,
    Function,
    CurlyBracketOpen,
    CurlyBracketClose,
    Public,
    String,
    StringLiteral(String),
    Extern,
    Ampersand
}

#[derive(Debug, Clone)]
pub enum DataType {
    Int,
    Char,
    Array{ data_type: Box<DataType>, size: i32 }
}

pub fn tokenize(contents: &str) -> Vec<Token> {
    let mut tokens = Vec::<Token>::new();
    let mut iter = contents.chars().peekable();
    let mut buffer;

    while let Some(mut c) = iter.next() {
        buffer = String::new();
        if c.is_alphabetic() || c == '_' {
            loop {
                buffer.push(c);
                if let Some(temp) = iter.peek() {
                    if temp.is_alphanumeric() || c == '_' {
                        c = iter.next().unwrap();
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }

            tokens.push(
                match buffer.as_str() {
                    //"exit" => Token::Exit,
                    "int" => Token::VariableDefinition(DataType::Int),
                    "char" => Token::VariableDefinition(DataType::Char),
                    "fn" => Token::Function,
                    "public" => Token::Public,
                    "string" => Token::String,
                    "extern" => Token::Extern,
                    _ => Token::Identifier(buffer)
                });
        } else if c.is_ascii_digit() || c == '-' {
            loop {
                buffer.push(c);
                if let Some(temp) = iter.peek() {
                    if temp.is_ascii_digit() {
                        c = iter.next().unwrap();
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }

            if let Ok(num) = buffer.parse::<i32>() {
                tokens.push(Token::IntLiteral(num));
            } else if buffer == "-" {
                tokens.push(Token::MinusSign);
            }
        } else if c == '"' {
            loop {
                if let Some(c) = iter.next() {
                    if c != '"' {
                        buffer.push(c);
                    } else {
                        tokens.push(Token::StringLiteral(buffer));
                        break;
                    }
                } else {
                    panic!("Expected second quote for string literal end.");
                }
            }

        } else if c == '\'' {
            if let Some(c) = iter.next() {
                if let Some('\'') = iter.next() {
                    if !c.is_ascii() {
                        panic!("Character {} isn't an ASCII character.", c);
                    }
                    tokens.push(Token::CharacterLiteral(c));
                } else {
                    panic!("Expected a ' at the end of a character literal.");
                }
            } else {
                panic!("Expected a character after '");
            }
        } else {
            tokens.push(
                    match c {
                        ';' => Token::Semicolon,
                        '=' => Token::EqualSign,
                        '+' => Token::PlusSign,
                        '*' => Token::MultiplySign,
                        '/' => Token::DivisionSign,
                        '(' => Token::ParenthesisOpen,
                        ')' => Token::ParenthesisClose,
                        '{' => Token::CurlyBracketOpen,
                        '}' => Token::CurlyBracketClose,
                        '&' => Token::Ampersand,
                        _ => continue
                });
        }
    }

    return tokens;
}
