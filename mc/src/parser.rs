use crate::ast::*;
use crate::tokenizer::*;

pub struct Parser<'a> {
    tokens: &'a [TokenSpanned],
    index: usize,
    token: &'a TokenSpanned,
    pub function_declarations: Vec<FunctionPrototype>,
    pub functions: Vec<FunctionDefinition>,
    pub struct_declarations: Vec<StructDeclaration>,
    pub errors: Vec<Error>,
}

impl Parser<'_> {
    pub fn new(tokenized_file: &TokenizedFile) -> Parser {
        Parser {
            tokens: &tokenized_file.tokens, // must end with an EOF token
            index: 0,
            token: &tokenized_file.tokens[0],
            function_declarations: Vec::new(),
            functions: Vec::new(),
            struct_declarations: Vec::new(),
            errors: Vec::new(),
        }
    }

    //TODO: in some other loops I'm not checking for EOF
    pub fn parse(&mut self) {
        while self.token.token != Token::EOF {
            let public = self.eat(&Token::Public);

            if self.eat(&Token::Fn) {
                self.parse_fn(public);
            } else if self.eat(&Token::Struct) {
                self.parse_struct();
            }
        }
    }

    fn next(&mut self) -> &TokenSpanned {
        self.index += 1;
        self.token = &self.tokens[self.index];
        self.token
    }

    fn prev(&mut self) -> &TokenSpanned {
        &self.tokens[self.index - 1]
    }

    fn eat(&mut self, token: &Token) -> bool {
        let present = self.token.token == *token;
        if present {
            self.next();
        }
        present
    }

    fn err_expected(&mut self, expected: &str) {
        let got = &self.token.token;
        self.errors.push(Error {
            span: self.token.span,
            msg: format!("Expected {expected}, instead got {got:?}."),
        });
    }

    fn eat_or_err(&mut self, token: &Token) {
        if !self.eat(token) {
            self.err_expected(&format!("{token:?}"));
        }
    }

    fn parse_ident(&mut self) -> Option<IdentifierSpanned> {
        if let Token::Identifier(identifier) = &self.token.token {
            self.next();
            Some(IdentifierSpanned {
                ident: identifier.clone(),
                span: self.token.span,
            })
        } else {
            None
        }
    }

    fn parse_ident_or_err(&mut self) -> Option<IdentifierSpanned> {
        let ident = self.parse_ident();
        if ident.is_none() {
            self.err_expected("an identifier");
        }
        ident
    }

    fn parse_type(&mut self) -> Option<DataType> {
        match &self.token.token {
            Token::DataType(data_type) => {
                self.next();
                Some(data_type.clone())
            }
            Token::Identifier(identifier) => {
                self.next();
                Some(DataType::Struct(IdentifierSpanned {
                    ident: identifier.clone(),
                    span: self.token.span,
                }))
            }
            _ => {
                self.err_expected("a type");
                None
            }
        }
    }

    /// Parses coma separated definitions until `end`.
    /// `f` must consume everything between `:` and `,`.
    fn parse_definitions<T: FnMut(Option<IdentifierSpanned>, &mut Self)>(
        &mut self,
        end: &Token,
        mut f: T,
    ) {
        let mut first = true;

        while !self.eat(end) {
            if first {
                first = false;
            } else {
                self.eat_or_err(&Token::Coma);
            }
            let ident = self.parse_ident_or_err();
            self.eat_or_err(&Token::Colon);
            f(ident, self);
        }
    }

    /// Expects that `fn` was already consumed.
    fn parse_fn(&mut self, public: bool) {
        let ident = self.parse_ident_or_err();
        self.eat_or_err(&Token::ParenthesisOpen);
        let mut arguments: Vec<(IdentifierSpanned, DataType)> = Vec::new();
        self.parse_definitions(&Token::ParenthesisClose, |ident, this| {
            if let Some(data_type) = this.parse_type() {
                if let Some(ident) = ident {
                    arguments.push((ident, data_type));
                }
            }
        });

        let return_type = if self.eat(&Token::Arrow) {
            self.parse_type().unwrap_or(DataType::Void)
        } else {
            DataType::Void
        };

        if self.eat(&Token::Semicolon) {
            if let Some(ident) = ident {
                self.function_declarations.push(FunctionPrototype {
                    ident,
                    return_type,
                    arguments,
                });
            }
        } else if self.eat(&Token::CurlyBracketOpen) {
            let body = self.parse_scope();
            if let Some(name) = ident {
                self.functions.push(FunctionDefinition {
                    prototype: FunctionPrototype {
                        ident: name,
                        return_type,
                        arguments,
                    },
                    public,
                    body,
                });
            }
        }
    }

    // Expects that `struct` was already consumed.
    fn parse_struct(&mut self) {
        let ident = self.parse_ident_or_err();
        self.eat_or_err(&Token::CurlyBracketOpen);
        let mut members: Vec<StructMember> = Vec::new();

        self.parse_definitions(&Token::CurlyBracketClose, |ident, this| {
            if let Some(data_type) = this.parse_type() {
                if let Some(identifier) = ident {
                    members.push(StructMember {
                        ident: identifier,
                        data_type,
                    });
                }
            }
        });

        if let Some(ident) = ident {
            self.struct_declarations
                .push(StructDeclaration { ident, members });
        }
    }

    // Expects that `{` was already consumed. Consumes '}'.
    fn parse_scope(&mut self) -> Vec<Statement> {
        let mut statements: Vec<Statement> = Vec::new();
        while !self.eat(&Token::CurlyBracketClose) {
            statements.extend(self.parse_statement());
        }
        statements
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        if self.eat(&Token::Let) {
            let ident = self.parse_ident_or_err();
            self.eat_or_err(&Token::Colon);
            let data_type = self.parse_type();

            let expression = if self.eat(&Token::EqualSign) {
                let expr = self.parse_expression(0);
                expr
            } else {
                None
            };
            self.eat_or_err(&Token::Semicolon);
            Some(Statement::VariableDefinition {
                ident: ident?,
                expression,
                data_type: data_type?,
            })
        } else if self.eat(&Token::If) {
            let expression = self.parse_expression(0);
            self.eat_or_err(&Token::CurlyBracketOpen);
            let scope = self.parse_scope();
            let else_scope = if self.eat(&Token::Else) {
                self.eat_or_err(&Token::CurlyBracketOpen);
                Some(self.parse_scope())
            } else {
                None
            };

            Some(Statement::If {
                expression,
                scope,
                else_scope,
            })
        } else if self.eat(&Token::While) {
            let expression = self.parse_expression(0);
            self.eat_or_err(&Token::CurlyBracketOpen);
            let scope = self.parse_scope();
            Some(Statement::While { expression, scope })
        } else if self.eat(&Token::For) {
            todo!();
        } else if self.eat(&Token::Return) {
            let expression = self.parse_expression(0);
            self.eat_or_err(&Token::Semicolon);
            Some(Statement::Return(expression?))
        } else {
            let expression = self.parse_expression(0);
            self.eat_or_err(&Token::Semicolon);
            Some(Statement::Expression(expression?))
        }
    }

    fn parse_expression(&mut self, min_bp: u8) -> Option<ExpressionSpanned> {
        let unary_op = self.parse_unary_operator();
        let mut lhs = self.parse_literals_or_member_access()?;
        if let Some((operator, span)) = unary_op {
            lhs = ExpressionSpanned {
                span: Span::between(&span, &lhs.span),
                expression: Expression::Unary {
                    expr: Box::new(lhs),
                    operator,
                },
            }
        }

        while let Some(operator) = self.parse_binary_operator() {
            let (l_bp, r_bp) = operator.binding_power();
            if l_bp < min_bp {
                break;
            }
            self.next();

            let Some(rhs) = self.parse_expression(r_bp) else {
                self.err_expected("an expression");
                return Some(lhs);
            };

            lhs = ExpressionSpanned {
                span: Span::between(&lhs.span, &rhs.span),
                expression: Expression::Binary {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    operator,
                },
            };
        }

        Some(lhs)
    }

    fn parse_literals_or_member_access(&mut self) -> Option<ExpressionSpanned> {
        let expression = match &self.token.token {
            Token::IntLiteral(val) => Some(Expression::IntLiteral(*val)),
            Token::CharacterLiteral(val) => Some(Expression::CharacterLiteral(*val)),
            Token::BoolLiteral(val) => Some(Expression::BoolLiteral(*val)),
            Token::FloatLiteral(val) => Some(Expression::FloatLiteral(val.clone())),
            Token::StringLiteral(val) => Some(Expression::StringLiteral(val.clone())),
            _ => None,
        };

        if let Some(expression) = expression {
            let span = self.token.span;
            self.next();
            Some(ExpressionSpanned { span, expression })
        } else {
            self.parse_member_access()
        }
    }

    fn parse_member_access(&mut self) -> Option<ExpressionSpanned> {
        let mut lhs = self.parse_expression_atom()?;

        while self.eat(&Token::Period) {
            let rhs = self.parse_expression_atom()?;
            lhs = ExpressionSpanned {
                span: Span::between(&lhs.span, &rhs.span),
                expression: Expression::Binary {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    operator: BinaryOp::MemberAccess,
                },
            }
        }

        Some(lhs)
    }

    fn parse_expression_atom(&mut self) -> Option<ExpressionSpanned> {
        let span = self.token.span;
        match &self.token.token {
            Token::Identifier(ident) => {
                self.next();
                match self.token.token {
                    Token::SquareParenthesisOpen => {
                        self.next();
                        let expr = self.parse_expression(0);
                        self.eat_or_err(&Token::SquareParenthesisClose);
                        if let Some(expr) = expr {
                            Some(ExpressionSpanned {
                                span: Span::between(&span, &expr.span),
                                expression: Expression::ArraySubscript {
                                    ident: IdentifierSpanned {
                                        ident: ident.clone(),
                                        span,
                                    },
                                    element: Box::new(expr),
                                },
                            })
                        } else {
                            None
                        }
                    }
                    Token::ParenthesisOpen => {
                        self.next();
                        let mut arguments = Vec::new();
                        let mut first = true;

                        while !self.eat(&Token::ParenthesisClose) {
                            if first {
                                first = false;
                            } else {
                                self.eat_or_err(&Token::Coma);
                            }
                            if let Some(expr) = self.parse_expression(0) {
                                arguments.push(expr);
                            }
                        }

                        let call_span = Span::between(&span, &self.prev().span);
                        Some(ExpressionSpanned {
                            span: call_span,
                            expression: Expression::FunctionCall(FunctionCall {
                                ident: IdentifierSpanned {
                                    ident: ident.clone(),
                                    span,
                                },
                                arguments,
                                span: call_span,
                            }),
                        })
                    }
                    Token::CurlyBracketOpen => {
                        self.next();
                        let mut members = Vec::new();
                        self.parse_definitions(&Token::CurlyBracketClose, |ident, this| {
                            let expr = this.parse_expression(0);
                            if let Some(ident) = ident {
                                members.push((ident, expr));
                            }
                        });
                        Some(ExpressionSpanned {
                            span: Span::between(&span, &self.prev().span),
                            expression: Expression::StructLiteral {
                                ident: IdentifierSpanned {
                                    ident: ident.clone(),
                                    span,
                                },
                                members,
                            },
                        })
                    }
                    _ => Some(ExpressionSpanned {
                        span,
                        expression: Expression::Identifier(IdentifierSpanned {
                            ident: ident.clone(),
                            span,
                        }),
                    }),
                }
            }
            Token::ParenthesisOpen => {
                self.next();
                let expr = self.parse_expression(0);
                self.eat_or_err(&Token::ParenthesisClose);
                expr
            }
            _ => {
                self.err_expected("an expression");
                None
            }
        }
    }

    /// Does NOT consume the current token.
    fn parse_binary_operator(&mut self) -> Option<BinaryOp> {
        use ArithmeticOp::*;
        use BinaryOp::*;
        use BoolOp::*;
        match self.token.token {
            Token::EqualSign => Some(Assign),
            Token::CompareEqual => Some(Bool(Equal)),
            Token::PlusSign => Some(Arithmetic(Add)),
            Token::MultiplySign => Some(Arithmetic(Mul)),
            Token::MinusSign => Some(Arithmetic(Sub)),
            Token::DivisionSign => Some(Arithmetic(Div)),
            Token::LargerThan => Some(Bool(Larger)),
            Token::SmallerThan => Some(Bool(Smaller)),
            Token::Period => Some(MemberAccess),
            _ => None,
        }
    }

    /// Does consume the current token if parsed.
    fn parse_unary_operator(&mut self) -> Option<(UnaryOperator, Span)> {
        let op = match self.token.token {
            Token::MultiplySign => Some(UnaryOperator::Dereference),
            Token::Ampersand => Some(UnaryOperator::AddressOf),
            Token::Bang => Some(UnaryOperator::LogicalNot),
            Token::MinusSign => Some(UnaryOperator::Negation),
            _ => None,
        };

        if let Some(op) = op {
            let span = self.token.span;
            self.next();
            Some((op, span))
        } else {
            None
        }
    }

    pub fn get_function(&self, ident: &str) -> Option<&FunctionPrototype> {
        self.function_declarations
            .iter()
            .find(|dec| dec.ident.ident == ident)
            .or_else(|| {
                self.functions
                    .iter()
                    .find(|dec| dec.prototype.ident.ident == ident)
                    .map(|dec| &dec.prototype)
            })
    }
}
