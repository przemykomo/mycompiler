#[derive(Debug)]
pub enum Token {
    IntLiteral(i32),
    CharacterLiteral(char),
    Semicolon,
    DataType(DataType),
    Identifier(String),
    EqualSign,
    PlusSign,
    MultiplySign,
    MinusSign,
    DivisionSign,
    ParenthesisOpen,
    ParenthesisClose,
    CurlyBracketOpen,
    CurlyBracketClose,
    Public,
    String,
    StringLiteral(String),
    Extern,
    Ampersand,
    SquareParenthesisOpen,
    SquareParenthesisClose,
    LargerThan,
    SmallerThan,
    If,
    Else,
    Coma
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataType {
    Int,
    Char,
    Array{ data_type: Box<DataType>, size: i32 },
    Pointer{ data_type: Box<DataType> },
    Boolean,
    Void
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
                    "int" => Token::DataType(DataType::Int),
                    "char" => Token::DataType(DataType::Char),
                    "void" => Token::DataType(DataType::Void),
                    "public" => Token::Public,
                    "string" => Token::String,
                    "extern" => Token::Extern,
                    "if" => Token::If,
                    "else" => Token::Else,
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
                        '[' => Token::SquareParenthesisOpen,
                        ']' => Token::SquareParenthesisClose,
                        '<' => Token::SmallerThan,
                        '>' => Token::LargerThan,
                        ',' => Token::Coma,
                        _ => continue
                });
        }
    }

    return tokens;
}
