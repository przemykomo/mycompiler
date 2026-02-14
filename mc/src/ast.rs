use std::fmt::Display;

use crate::tokenizer::*;

#[derive(Debug)]
pub struct ExpressionSpanned {
    pub span: Span,
    pub expression: Expression,
}

#[derive(Debug)]
pub enum Expression {
    IntLiteral(i64),
    CharacterLiteral(char),
    BoolLiteral(bool),
    FloatLiteral(f32),
    StringLiteral(String),
    StructLiteral {
        ident: IdentifierSpanned,
        members: Vec<(IdentifierSpanned, Option<ExpressionSpanned>)>,
    },
    FunctionCall(FunctionCall),
    Identifier(IdentifierSpanned),
    ArraySubscript {
        ident: IdentifierSpanned,
        element: Box<ExpressionSpanned>,
    },
    Binary {
        lhs: Box<ExpressionSpanned>,
        rhs: Box<ExpressionSpanned>,
        operator: BinaryOp,
    },
    Unary {
        expr: Box<ExpressionSpanned>,
        operator: UnaryOperator,
    },
}

#[derive(Debug)]
pub struct StructDeclaration {
    pub ident: IdentifierSpanned,
    pub members: Vec<StructMember>,
}

#[derive(Debug, Clone)]
pub struct StructMember {
    pub ident: IdentifierSpanned,
    pub data_type: DataType,
}

impl Display for StructMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.ident.ident.fmt(f)
    }
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

#[derive(Debug, Clone, Copy)]
pub enum ArithmeticOp {
    Add,
    Sub,
    Mul,
    Div,
}
impl ArithmeticOp {
    pub fn perform<
        T: std::ops::Add<Output = T>
            + std::ops::Sub<Output = T>
            + std::ops::Mul<Output = T>
            + std::ops::Div<Output = T>,
    >(
        &self,
        lhs: T,
        rhs: T,
    ) -> T {
        match self {
            ArithmeticOp::Add => lhs + rhs,
            ArithmeticOp::Sub => lhs - rhs,
            ArithmeticOp::Mul => lhs * rhs,
            ArithmeticOp::Div => lhs / rhs,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BoolOp {
    Equal,
    Larger,
    Smaller,
}

impl BoolOp {
    pub fn perform<T: std::cmp::PartialEq + std::cmp::PartialOrd>(&self, lhs: T, rhs: T) -> bool {
        match self {
            BoolOp::Equal => lhs == rhs,
            BoolOp::Larger => lhs > rhs,
            BoolOp::Smaller => lhs < rhs,
        }
    }
}

#[derive(Debug)]
pub enum BinaryOp {
    Arithmetic(ArithmeticOp),
    Bool(BoolOp),
    Assign,
    MemberAccess,
}

impl BinaryOp {
    pub fn binding_power(&self) -> (u8, u8) {
        use ArithmeticOp::*;
        use BinaryOp::*;
        match self {
            Arithmetic(Add) | Arithmetic(Sub) => (7, 8),
            Arithmetic(Mul) | Arithmetic(Div) => (9, 10),
            Bool(_) => (5, 6),
            Assign => (4, 3),
            MemberAccess => (1, 2),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IdentifierSpanned {
    pub ident: String,
    pub span: Span,
}

impl PartialEq for IdentifierSpanned {
    fn eq(&self, other: &Self) -> bool {
        self.ident.eq(&other.ident)
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
        ident: IdentifierSpanned,
        expression: Option<ExpressionSpanned>,
        data_type: DataType,
    },
}

#[derive(Debug)]
pub struct FunctionCall {
    pub ident: IdentifierSpanned,
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
    pub ident: IdentifierSpanned,
    pub return_type: DataType,
    pub arguments: Vec<(IdentifierSpanned, DataType)>,
}

#[derive(Debug)]
pub struct ParsedUnit {
    pub function_declarations: Vec<FunctionPrototype>,
    pub functions: Vec<FunctionDefinition>,
    pub struct_declarations: Vec<StructDeclaration>,
    pub errors: Vec<Error>,
}
