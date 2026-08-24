use std::usize::MAX;

use crate::ast::IdentifierSpanned;

#[derive(Debug, PartialEq)]
pub enum Token {
    IntLiteral(i128),
    CharacterLiteral(char),
    BoolLiteral(bool),
    FloatLiteral(f64),
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
    Fn,
    Arrow,
    Let,
    EOF,
    Bang,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataType {
    UnsizedInt,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,

    UnsizedFloat,
    F32,
    F64,

    Char,
    Array { data_type: Box<DataType>, size: i32 },
    Pointer(Box<DataType>),
    Boolean,
    Void,
    Struct(IdentifierSpanned), //TODO: Possibly change it to a Path/Vec<Identifier>
}

impl DataType {
    pub fn is_float(&self) -> bool {
        use DataType::*;
        // UnsizedInt can be promoted to a float
        matches!(self, UnsizedInt | UnsizedFloat | F32 | F64)
    }

    pub fn is_int(&self) -> bool {
        use DataType::*;
        matches!(
            self,
            UnsizedInt | I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64
        )
    }
}

#[derive(Debug)]
pub struct TokenizedFile {
    pub tokens: Vec<TokenSpanned>,
    pub errors: Vec<Error>,
}

#[derive(Debug)]
pub struct TokenSpanned {
    pub token: Token,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub line: usize,
    pub column: usize,
    pub endline: usize,
    pub endcolumn: usize,
}

impl Span {
    pub fn max_end(&self) -> Span {
        Span {
            line: self.line,
            column: self.column,
            endline: MAX,
            endcolumn: MAX,
        }
    }

    pub const EOF: Span = Span {
        line: MAX,
        column: MAX,
        endline: MAX,
        endcolumn: MAX,
    };

    pub fn between(begin: &Span, end: &Span) -> Span {
        Span {
            line: begin.line,
            column: begin.column,
            endline: end.endline,
            endcolumn: end.endcolumn,
        }
    }
}

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub msg: String,
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

fn error_till_newline<T: Iterator<Item = (usize, char)>>(
    iter: &mut T,
    state: &mut TokenizationSate,
    msg: String,
    pos: usize,
) {
    let line_being_pos = state.line_begin_pos;
    skip_line(iter, state);
    state.errors.push(Error {
        span: Span {
            line: state.line - 1,
            column: pos - line_being_pos,
            endline: state.line - 1,
            endcolumn: state.line_begin_pos - 1 - line_being_pos,
        },
        msg,
    });
}

struct TokenizationSate {
    pub tokens: Vec<TokenSpanned>,
    pub errors: Vec<Error>,
    pub line: usize,
    pub line_begin_pos: usize,
}

pub fn tokenize(contents: &str) -> TokenizedFile {
    let mut state: TokenizationSate = TokenizationSate {
        tokens: Vec::<TokenSpanned>::new(),
        errors: Vec::<Error>::new(),
        line: 0,
        line_begin_pos: 0,
    };
    let mut iter = contents.chars().enumerate().peekable();
    let mut buffer;
    let mut last_pos;

    while let Some((pos, mut c)) = iter.next() {
        last_pos = pos;
        buffer = String::new();
        if c.is_alphabetic() || c == '_' {
            loop {
                buffer.push(c);
                if let Some((_, temp)) = iter.peek() {
                    if temp.is_alphanumeric() || *temp == '_' {
                        (last_pos, c) = iter.next().unwrap();
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }

            let span = Span {
                line: state.line,
                column: pos - state.line_begin_pos,
                endline: state.line,
                endcolumn: last_pos - state.line_begin_pos,
            };
            let token = match buffer.as_str() {
                "i8" => Token::DataType(DataType::I8),
                "u8" => Token::DataType(DataType::U8),
                "i16" => Token::DataType(DataType::I16),
                "u16" => Token::DataType(DataType::U16),
                "i32" => Token::DataType(DataType::I32),
                "u32" => Token::DataType(DataType::U32),
                "i64" => Token::DataType(DataType::I64),
                "u64" => Token::DataType(DataType::U64),

                "f32" => Token::DataType(DataType::F32),
                "f64" => Token::DataType(DataType::F64),

                "char" => Token::DataType(DataType::Char),
                "void" => Token::DataType(DataType::Void),
                "bool" => Token::DataType(DataType::Boolean),
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
                "fn" => Token::Fn,
                "let" => Token::Let,
                _ => Token::Identifier(buffer),
            };
            if span.endline < span.line
                || (span.line == span.endline && span.endcolumn < span.column)
            {
                panic!("{:?} {:?}", &token, span);
            }
            state.tokens.push(TokenSpanned { token, span });
        } else if c.is_ascii_digit() || c == '-' {
            let mut is_float = false;
            loop {
                buffer.push(c);
                if let Some((pos, temp)) = iter.peek() {
                    if temp.is_ascii_digit() {
                        c = iter.next().unwrap().1;
                    } else if temp.eq(&'.') {
                        c = iter.next().unwrap().1;
                        is_float = true;
                    } else {
                        last_pos = *pos - 1;
                        break;
                    }
                } else {
                    break;
                }
            }

            if is_float {
                if let Ok(val) = buffer.parse::<f64>() {
                    let span = Span {
                        line: state.line,
                        column: pos - state.line_begin_pos,
                        endline: state.line,
                        endcolumn: last_pos - state.line_begin_pos,
                    };
                    state.tokens.push(TokenSpanned {
                        token: Token::FloatLiteral(val),
                        span,
                    });
                } else {
                    error_till_newline(
                        &mut iter,
                        &mut state,
                        format!("Cannot parse a float constant: \"{}\"", buffer),
                        pos,
                    );
                }
            } else {
                if let Ok(num) = buffer.parse::<i128>() {
                    let span = Span {
                        line: state.line,
                        column: pos - state.line_begin_pos,
                        endline: state.line,
                        endcolumn: last_pos - state.line_begin_pos,
                    };
                    state.tokens.push(TokenSpanned {
                        token: Token::IntLiteral(num),
                        span,
                    });
                } else if buffer == "-" {
                    if let Some((_, '>')) = iter.peek() {
                        iter.next();
                        state.tokens.push(TokenSpanned {
                            token: Token::Arrow,
                            span: Span {
                                line: state.line,
                                column: pos - state.line_begin_pos,
                                endline: state.line,
                                endcolumn: last_pos - state.line_begin_pos + 1,
                            },
                        });
                    } else {
                        state.tokens.push(TokenSpanned {
                            token: Token::MinusSign,
                            span: Span {
                                line: state.line,
                                column: pos - state.line_begin_pos,
                                endline: state.line,
                                endcolumn: last_pos - state.line_begin_pos,
                            },
                        });
                    }
                }
            }
        } else if c == '"' {
            loop {
                if let Some((last_pos, c)) = iter.next() {
                    if c != '"' {
                        buffer.push(c);
                    } else {
                        let span = Span {
                            line: state.line,
                            column: pos - state.line_begin_pos,
                            endline: state.line,
                            endcolumn: last_pos - state.line_begin_pos,
                        };
                        state.tokens.push(TokenSpanned {
                            token: Token::StringLiteral(buffer),
                            span,
                        });
                        break;
                    }
                } else {
                    state.errors.push(Error {
                        span: Span {
                            line: state.line,
                            column: pos - state.line_begin_pos,
                            endline: state.line,
                            endcolumn: pos - state.line_begin_pos + 1,
                        },
                        msg: "Expected a closing quote.".to_string(),
                    });
                    break;
                }
            }
        } else if c == '\'' {
            if let Some((_, c)) = iter.next() {
                if let Some((last_pos, '\'')) = iter.next() {
                    if !c.is_ascii() {
                        error_till_newline(
                            &mut iter,
                            &mut state,
                            format!("Character {} isn't an ASCII character.", c),
                            pos,
                        );
                    }
                    let span = Span {
                        line: state.line,
                        column: pos - state.line_begin_pos,
                        endline: state.line,
                        endcolumn: last_pos - state.line_begin_pos,
                    };
                    state.tokens.push(TokenSpanned {
                        token: Token::CharacterLiteral(c),
                        span,
                    });
                } else {
                    error_till_newline(
                        &mut iter,
                        &mut state,
                        "Expected a ' at the end of a character literal.".to_string(),
                        pos,
                    );
                }
            } else {
                error_till_newline(
                    &mut iter,
                    &mut state,
                    "Expected a character after '".to_string(),
                    pos,
                );
            }
        } else {
            let span = Span {
                line: state.line,
                column: pos - state.line_begin_pos,
                endline: state.line,
                endcolumn: pos - state.line_begin_pos,
            };
            let token = match c {
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
                '!' => Token::Bang,
                other => {
                    if c.is_whitespace() {
                        if c == '\n' {
                            state.line += 1;
                            state.line_begin_pos = pos + 1;
                        }
                    } else {
                        error_till_newline(
                            &mut iter,
                            &mut state,
                            format!("Unexpected character: `{}`", other),
                            pos,
                        );
                    }
                    continue;
                }
            };
            state.tokens.push(TokenSpanned { token, span });
        }
    }

    state.tokens.push(TokenSpanned {
        token: Token::EOF,
        span: Span::EOF,
    });

    TokenizedFile {
        tokens: state.tokens,
        errors: state.errors,
    }
}
