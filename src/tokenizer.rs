#[derive(Debug)]
pub enum Token {
    Exit,
    IntLiteral(i32),
    Semicolon,
    Let,
    Identifier(String),
    EqualSign,
    PlusSign,
    MultiplySign
}

pub fn tokenize(contents: &str) -> Vec<Token> {
    let mut tokens = Vec::<Token>::new();
    let mut iter = contents.chars().peekable();
    let mut buffer;

    while let Some(mut c) = iter.next() {
        buffer = String::new();
        if c.is_alphabetic() {
            loop {
                buffer.push(c);
                if let Some(temp) = iter.peek() {
                    if temp.is_alphanumeric() {
                        c = iter.next().unwrap();
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }

            if buffer == "exit" {
                tokens.push(Token::Exit);
            } else if buffer == "let" {
                tokens.push(Token::Let);
            } else {
                tokens.push(Token::Identifier(buffer));
            }
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
            }
        } else if c == ';' {
            tokens.push(Token::Semicolon);
        } else if c == '=' {
            tokens.push(Token::EqualSign);
        } else if c == '+' {
            tokens.push(Token::PlusSign);
        } else if c == '*' {
            tokens.push(Token::MultiplySign);
        }
    }

    return tokens;
}
