use itertools::WhileSome;

#[derive(Debug)]
pub enum Token {
    IntLiteral(i32),
    CharacterLiteral(char),
    BoolLiteral(bool),
    FloatLiteral(String),
    Semicolon,
    DataType(DataType),
    Identifier(String),
    EqualSign,
    CompareEqual,
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
    Coma,
    Colon,
    Return,
    Struct,
    Period,
    While,
    For,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataType {
    Int,
    Char,
    Array { data_type: Box<DataType>, size: i32 },
    Pointer(Box<DataType>),
    Boolean,
    Void,
    Float,
    Struct(String),
}

pub struct TokenizedFile {
    pub tokens: Vec<Token>,
    pub positions: Vec<TokenPos>,
    pub errors: Vec<TokenError>
}

#[derive(Debug)]
pub struct TokenPos {
    pub pos: usize,
    pub line: usize,
    pub column: usize
}

#[derive(Debug)]
pub struct TokenError {
    pub pos: TokenPos,
    pub msg: String
}

fn skip_line<T: Iterator<Item = (usize, char)>>(iter: &mut T, state: &mut TokenizationSate) {
    while let Some((pos, c)) = iter.next() {
        if c == '\n' {
            state.line += 1;
            state.line_begin_pos = pos;
            break;
        }
    }
}

fn error<T: Iterator<Item = (usize, char)>>(iter: &mut T, state: &mut TokenizationSate, msg: String, pos: usize) {
    state.errors.push(TokenError {
        pos: TokenPos {
            pos,
            line: state.line,
            column: pos - state.line_begin_pos
        },
        msg
    });
    skip_line(iter, state);
}

struct TokenizationSate {
    pub tokens: Vec<Token>,
    pub positions: Vec<TokenPos>,
    pub errors: Vec<TokenError>,
    pub line: usize,
    pub line_begin_pos: usize
}

pub fn tokenize(contents: &str) -> TokenizedFile {
    let mut state: TokenizationSate = TokenizationSate {
        tokens: Vec::<Token>::new(),
        positions: Vec::<TokenPos>::new(),
        errors: Vec::<TokenError>::new(),
        line: 0,
        line_begin_pos: 0,
    };
    let mut iter = contents.chars().enumerate().peekable();
    let mut buffer;

    while let Some((pos, mut c)) = iter.next() {
        buffer = String::new();
        if c.is_alphabetic() || c == '_' {
            loop {
                buffer.push(c);
                if let Some((_, temp)) = iter.peek() {
                    if temp.is_alphanumeric() || c == '_' {
                        c = iter.next().unwrap().1;
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }

            state.positions.push(TokenPos { pos, line: state.line, column: pos - state.line_begin_pos });
            state.tokens.push(match buffer.as_str() {
                "int" => Token::DataType(DataType::Int),
                "char" => Token::DataType(DataType::Char),
                "void" => Token::DataType(DataType::Void),
                "bool" => Token::DataType(DataType::Boolean),
                "float" => Token::DataType(DataType::Float),
                "public" => Token::Public,
                "string" => Token::String,
                "extern" => Token::Extern,
                "if" => Token::If,
                "else" => Token::Else,
                "return" => Token::Return,
                "true" => Token::BoolLiteral(true),
                "false" => Token::BoolLiteral(false),
                "struct" => Token::Struct,
                "while" => Token::While,
                "for" => Token::For,
                _ => Token::Identifier(buffer),
            });
        } else if c.is_ascii_digit() || c == '-' {
            let mut is_float = false;
            loop {
                buffer.push(c);
                if let Some((_, temp)) = iter.peek() {
                    if temp.is_ascii_digit() {
                        c = iter.next().unwrap().1;
                    } else if temp.eq(&'.') {
                        c = iter.next().unwrap().1;
                        is_float = true;
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }

            if is_float {
                if let Ok(_) = buffer.parse::<f32>() {
                    state.positions.push(TokenPos { pos, line: state.line, column: pos - state.line_begin_pos });
                    state.tokens.push(Token::FloatLiteral(buffer));
                } else {
                    error(&mut iter, &mut state, format!("Cannot parse a float constant: \"{}\"", buffer), pos);
                }
            } else {
                if let Ok(num) = buffer.parse::<i32>() {
                    state.positions.push(TokenPos { pos, line: state.line, column: pos - state.line_begin_pos });
                    state.tokens.push(Token::IntLiteral(num));
                } else if buffer == "-" {
                    state.positions.push(TokenPos { pos, line: state.line, column: pos - state.line_begin_pos });
                    state.tokens.push(Token::MinusSign);
                }
            }
        } else if c == '"' {
            loop {
                if let Some((_, c)) = iter.next() {
                    if c != '"' {
                        buffer.push(c);
                    } else {
                        state.positions.push(TokenPos { pos, line: state.line, column: pos - state.line_begin_pos });
                        state.tokens.push(Token::StringLiteral(buffer));
                        break;
                    }
                } else {
                    error(&mut iter, &mut state, "Expected second quote for string literal end.".to_string(), pos);
                }
            }
        } else if c == '\'' {
            if let Some((_, c)) = iter.next() {
                if let Some((_, '\'')) = iter.next() {
                    if !c.is_ascii() {
                        error(&mut iter, &mut state, format!("Character {} isn't an ASCII character.", c), pos);
                    }
                    state.positions.push(TokenPos { pos, line: state.line, column: pos - state.line_begin_pos });
                    state.tokens.push(Token::CharacterLiteral(c));
                } else {
                    error(&mut iter, &mut state, "Expected a ' at the end of a character literal.".to_string(), pos);
                }
            } else {
                error(&mut iter, &mut state, "Expected a character after '".to_string(), pos);
            }
        } else {
            state.tokens.push(match c {
                ';' => Token::Semicolon,
                '=' => {
                    if let Some((_, '=')) = iter.peek() {
                        iter.next();
                        Token::CompareEqual
                    } else {
                        Token::EqualSign
                    }
                }
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
                ':' => Token::Colon,
                '.' => Token::Period,
                other => {
                    if c.is_whitespace() {
                        if c == '\n' {
                            state.line += 1;
                            state.line_begin_pos = pos;
                        }
                    } else {
                        error(&mut iter, &mut state, format!("Unexpected character: `{}`", other), pos);
                    }
                    continue;
                },
            });
            state.positions.push(TokenPos { pos, line: state.line, column: pos - state.line_begin_pos });
        }
    }

    TokenizedFile {
        tokens: state.tokens,
        positions: state.positions,
        errors: state.errors
    }
}
