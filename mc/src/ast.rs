use crate::tokenizer::*;

#[derive(Debug)]
pub struct ExpressionSpanned {
    pub span: Span,
    pub expression: Expression,
}

#[derive(Debug)]
pub enum Expression {
    IntLiteral(i32),
    CharacterLiteral(char),
    BoolLiteral(bool),
    FloatLiteral(String),
    StringLiteral(String),
    StructLiteral {
        identifier: IdentifierSpanned,
        members: Vec<(String, Option<ExpressionSpanned>)>,
    },
    FunctionCall(FunctionCall),
    Identifier(IdentifierSpanned),
    ArraySubscript {
        identifier: IdentifierSpanned,
        element: Box<ExpressionSpanned>,
    },
    Binary {
        lhs: Box<ExpressionSpanned>,
        rhs: Box<ExpressionSpanned>,
        operator: BinaryOperator,
    },
    Unary {
        exp: Box<ExpressionSpanned>,
        operator: UnaryOperator,
    },
}

#[derive(Debug)]
pub struct StructDeclaration {
    pub identifier: IdentifierSpanned,
    pub members: Vec<StructMember>,
}

#[derive(Debug, Clone)]
pub struct StructMember {
    pub identifier: IdentifierSpanned,
    pub data_type: DataType,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Dereference,
    AddressOf,
    LogicalNot,
    Negation, // Increment(Box<ExpressionSpanned>),
              // Decrement(Box<ExpressionSpanned>),
              // ErrorPropagation
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    CompareEqual,
    CompareLarger,
    CompareSmaller,
    Assign,
    MemberAccess,
}

impl BinaryOperator {
    pub fn binding_power(&self) -> (u8, u8) {
        match self {
            BinaryOperator::Add | BinaryOperator::Subtract => (7, 8),
            BinaryOperator::Multiply | BinaryOperator::Divide => (9, 10),
            BinaryOperator::CompareEqual
            | BinaryOperator::CompareLarger
            | BinaryOperator::CompareSmaller => (5, 6),
            BinaryOperator::Assign => (4, 3),
            BinaryOperator::MemberAccess => (1, 2),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IdentifierSpanned {
    pub identifier: String,
    pub span: Span,
}

impl PartialEq for IdentifierSpanned {
    fn eq(&self, other: &Self) -> bool {
        self.identifier.eq(&other.identifier)
    }
}

#[derive(Debug)]
pub enum Statement {
    If {
        expression: Option<ExpressionSpanned>,
        scope: Vec<Statement>,
        else_scope: Option<Vec<Statement>>,
    },
    Return(ExpressionSpanned),
    Expression(ExpressionSpanned),
    While {
        expression: Option<ExpressionSpanned>,
        scope: Vec<Statement>,
    },
    For {
        inital_statement: Box<Statement>,
        condition_expr: ExpressionSpanned,
        iteration_expr: ExpressionSpanned,
        scope: Vec<Statement>,
    },
    VariableDefinition {
        identifier: IdentifierSpanned,
        expression: Option<Box<ExpressionSpanned>>,
        data_type: DataType,
    },
}

#[derive(Debug)]
pub struct FunctionCall {
    pub identifier: IdentifierSpanned,
    pub arguments: Vec<ExpressionSpanned>,
    pub span: Span,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub prototype: FunctionPrototype,
    pub public: bool,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct FunctionPrototype {
    pub name: IdentifierSpanned,
    pub return_type: DataType,
    pub arguments: Vec<DataType>,
}

#[derive(Debug)]
pub struct ParsedUnit {
    pub function_declarations: Vec<FunctionPrototype>,
    pub functions: Vec<FunctionDefinition>,
    pub struct_declarations: Vec<StructDeclaration>,
    pub errors: Vec<Error>,
}
