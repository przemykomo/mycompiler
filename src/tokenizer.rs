#[derive(Debug)]
pub enum Token {
    Exit,
    IntLiteral(i32),
    Semicolon,
    Let,
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
    Public
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
                    "exit" => Token::Exit,
                    "let" => Token::Let,
                    "fn" => Token::Function,
                    "public" => Token::Public,
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
                        _ => continue
                });
        }
    }

    return tokens;
}
