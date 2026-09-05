use std::fmt::Display;

use crate::{
    ast::{
        ArithmeticOp, BinaryOp, ComparisonOp, Expression, ExpressionSpanned, FunctionCall,
        FunctionDefinition, FunctionPrototype, IdentifierSpanned, Statement, StructDeclaration,
        UnaryOperator,
    },
    parser::Parser,
    tokenizer::{
        DataType::{self, *},
        Error, Span,
    },
    typecheck::{
        self,
        BlockReturnActuality::{AlwaysReturns, NeverReturns, SometimesReturns},
    },
};

pub struct List<'a, T: Display>(pub &'a [T]);

impl<'a, T: Display> Display for List<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let vec = &self.0;

        write!(f, "[")?;

        for (count, v) in vec.iter().enumerate() {
            if count != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", v)?;
        }
        write!(f, "]")
    }
}

#[derive(Debug)]
pub struct TypedStatement {
    pub kind: TypedStmtKind,
    pub return_actuality: BlockReturnActuality,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum BlockReturnActuality {
    NeverReturns,
    SometimesReturns,
    AlwaysReturns,
}

impl BlockReturnActuality {
    #[must_use]
    pub const fn join(a: Self, b: Self) -> Self {
        match (a, b) {
            (Self::NeverReturns, Self::NeverReturns) => Self::NeverReturns,

            (
                Self::NeverReturns | Self::SometimesReturns,
                Self::SometimesReturns | Self::AlwaysReturns,
            )
            | (
                Self::SometimesReturns | Self::AlwaysReturns,
                Self::NeverReturns | Self::SometimesReturns,
            ) => Self::SometimesReturns,

            (Self::AlwaysReturns, Self::AlwaysReturns) => Self::AlwaysReturns,
        }
    }

    fn demote(&self) -> Self {
        if *self != NeverReturns {
            SometimesReturns
        } else {
            NeverReturns
        }
    }
}

#[derive(Debug)]
pub enum TypedStmtKind {
    VariableDefinition {
        ident: String,
        expr: Option<TypedExpr>,
        data_type: DataType,
    },
    Expression(TypedExpr),
    If {
        expr: TypedExpr,
        then_block: TypedBlock,
        else_block: Option<TypedBlock>,
    },
    While {
        expr: TypedExpr,
        block: TypedBlock,
    },
    Return(Option<TypedExpr>),
}

#[derive(Debug)]
pub struct Variable {
    pub ident: IdentifierSpanned,
    pub data_type: DataType,
}

#[derive(Debug)]
pub struct Scope {
    pub vars: Vec<Variable>,
    pub visible_vars: Vec<usize>,
    // I've seen it done like that, but I don't think I need to do Rc and clone the map if I just
    // allow shadowing and truncate vec when exiting a scope
    // mappings: HashMap<&'input str, Rc<RefCell<ValueEntry<'input>>>>,
}

impl Scope {
    pub fn insert(&mut self, var: Variable) {
        let id = self.visible_vars.len();
        self.vars.push(var);
        self.visible_vars.push(id);
    }

    pub fn find(&self, key: &str, span: Span, errors: &mut Vec<Error>) -> Option<&Variable> {
        for visible in &self.visible_vars {
            if self.vars[*visible].ident.ident == key {
                return Some(&self.vars[*visible]);
            }
        }

        errors.push(Error {
            span,
            msg: format!("No variable named {key}"),
        });
        None
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockReturnAbility {
    MustNotReturn,
    MayReturn(DataType),
    MustReturn(DataType),
}

impl BlockReturnAbility {
    /// Determine the [`BlockReturnAbility`] of a sub-scope. `MustReturn`
    /// become`MayReturn`.
    #[must_use]
    pub fn demote(self) -> Self {
        match self {
            Self::MustNotReturn => Self::MustNotReturn,
            Self::MayReturn(x) | Self::MustReturn(x) => Self::MayReturn(x),
        }
    }
}

#[derive(Debug)]
pub struct TypedExpr {
    pub inferred_type: DataType,
    pub kind: TypedExprKind,
}

impl TypedExpr {
    fn coerce_to(&mut self, target: &DataType) -> bool {
        use DataType::*;
        if self.inferred_type == *target {
            return true;
        }

        if self.inferred_type == UnsizedInt {
            match target {
                UnsizedInt => todo!(),
                I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64 | F32 | F64 => {
                    self.inferred_type = target.clone();
                    return true;
                }
                _ => return false,
            }
        }

        if self.inferred_type == UnsizedFloat {
            match target {
                UnsizedFloat => todo!(),
                F32 | F64 => {
                    self.inferred_type = target.clone();
                    return true;
                }
                _ => return false,
            }
        }

        false
    }
}

#[derive(Debug)]
pub enum TypedExprKind {
    NumerLiteral(i128),
    CharLiteral(char),
    BoolLiteral(bool),
    FloatLiteral(f64),
    StringLiteral(String),
    Negation(Box<TypedExpr>),
    Not(Box<TypedExpr>),
    UnaryAddressOf(Box<Place>),
    Dereference(Box<TypedExpr>),
    Arithmetic(ArithmeticOp, Box<TypedExpr>, Box<TypedExpr>),
    Comparison(ComparisonOp, Box<TypedExpr>, Box<TypedExpr>),
    Assignment(Box<Place>, Box<TypedExpr>),
    MemberAccess(Box<Place>, String),
    Identifier(String),
    StructLiteral(Vec<TypedExpr>),
    Call(String, Vec<TypedExpr>),
}

#[derive(Debug)]
pub struct Place {
    pub inferred_type: DataType,
    pub kind: PlaceKind,
}

#[derive(Debug)]
pub enum PlaceKind {
    Deref(Box<TypedExpr>),
    Variable(String),
    Index(Box<TypedExpr>, Box<TypedExpr>),
    Member(Box<Place>, String),
}

pub struct TypeChecker<'a> {
    pub parser: &'a Parser<'a>,
    pub typed_functions: Vec<TypedFunc>,
    pub errors: Vec<Error>,
}

#[derive(Debug)]
pub struct TypedFunc {
    pub prototype: FunctionPrototype,
    pub block: TypedBlock,
    pub scope: Scope,
}

#[derive(Debug)]
pub struct TypedBlock {
    pub statements: Vec<TypedStatement>,
    pub return_actuality: BlockReturnActuality,
}

impl<'a> TypeChecker<'a> {
    pub fn typecheck(&mut self) {
        // for dec in &self.parser.struct_declarations {
        //     self.err_on_recursive_structs(dec, &mut Vec::new());
        // }

        for def in &self.parser.functions {
            self.type_func(def);
        }
    }

    fn err_on_recursive_structs<'s>(
        &mut self,
        dec: &'s StructDeclaration,
        stack: &mut Vec<&'s StructDeclaration>,
    ) where
        'a: 's,
    {
        stack.push(dec);
        for member in &dec.members {
            let member_dec = self
                .parser
                .struct_declarations
                .iter()
                .find(|s| s.ident.ident == member.ident.ident)
                .unwrap();
            if stack
                .iter()
                .find(|s| **s as *const StructDeclaration == member_dec as *const _)
                .is_some()
            {
                self.errors.push(Error {
                    span: member.ident.span,
                    msg: "Recursive struct definition.".to_owned(),
                });
            } else {
                self.err_on_recursive_structs(member_dec, stack);
            }
        }
    }

    pub fn type_func(&mut self, func: &FunctionDefinition) {
        let mut scope = Scope {
            vars: Vec::new(),
            visible_vars: Vec::new(),
        };

        for (ident, data_type) in &func.prototype.arguments {
            scope.insert(Variable {
                ident: ident.clone(),
                data_type: data_type.clone(),
            });
        }

        let mut block = self.type_block(
            &mut scope,
            &func.body,
            false,
            BlockReturnAbility::MustReturn(func.prototype.return_type.clone()),
        );

        if block.return_actuality != AlwaysReturns {
            if func.prototype.return_type == DataType::Void {
                block.statements.push(TypedStatement {
                    kind: TypedStmtKind::Return(None),
                    return_actuality: AlwaysReturns,
                });
                block.return_actuality = AlwaysReturns;
            } else {
                self.errors.push(Error {
                    span: func.prototype.ident.span,
                    msg: "Function doesn't always return.".to_owned(),
                });
            }
        }

        self.typed_functions.push(TypedFunc {
            prototype: func.prototype.clone(),
            block,
            scope,
        });
    }

    fn type_block(
        &mut self,
        scope: &mut Scope,
        statements: &Vec<Statement>,
        is_loop: bool,
        return_ability: BlockReturnAbility,
    ) -> TypedBlock {
        let mut typed_statements: Vec<TypedStatement> = Vec::new();
        let mut sometime_returns = false;
        let mut always_returns = false;

        for statement in statements {
            match statement {
                Statement::If {
                    expression,
                    scope: then_scope,
                    else_scope,
                } => {
                    let Some(expr) = expression else { continue };
                    let span = expr.span.clone();
                    let Some(expr) = self.type_expr(scope, expr, None) else {
                        continue;
                    };

                    if expr.inferred_type != DataType::Boolean {
                        self.errors.push(Error {
                            span,
                            msg: "Expected bool".to_owned(),
                        });
                    }

                    let then_block = self.type_block(
                        scope,
                        then_scope,
                        is_loop,
                        return_ability.clone().demote(),
                    );

                    let else_block = else_scope.as_ref().map(|else_scope| {
                        self.type_block(scope, else_scope, is_loop, return_ability.clone().demote())
                    });

                    let else_returned = else_block
                        .as_ref()
                        .map_or(BlockReturnActuality::NeverReturns, |block| {
                            block.return_actuality
                        });

                    let return_actuality =
                        BlockReturnActuality::join(then_block.return_actuality, else_returned);

                    match return_actuality {
                        BlockReturnActuality::NeverReturns => {}
                        BlockReturnActuality::SometimesReturns => sometime_returns = true,
                        BlockReturnActuality::AlwaysReturns => always_returns = true,
                    }

                    let kind = TypedStmtKind::If {
                        expr,
                        then_block,
                        else_block,
                    };
                    typed_statements.push(TypedStatement {
                        kind,
                        return_actuality,
                    });
                }
                Statement::Return(expr) => {
                    always_returns = true;
                    let span = expr.span.clone();
                    let Some(mut expr) = self.type_expr(scope, expr, None) else {
                        continue;
                    };

                    match &return_ability {
                        BlockReturnAbility::MustNotReturn => self.errors.push(Error {
                            span,
                            msg: "Must not return TODO where does it occur?".to_owned(),
                        }),
                        BlockReturnAbility::MayReturn(return_ty)
                        | BlockReturnAbility::MustReturn(return_ty) => {
                            if !expr.coerce_to(return_ty) {
                                self.errors.push(Error {
                                    span,
                                    msg: "Trying to return a wrong type".to_owned(),
                                });
                            }
                        }
                    }

                    let kind = TypedStmtKind::Return(Some(expr));
                    typed_statements.push(TypedStatement {
                        kind,
                        return_actuality: BlockReturnActuality::AlwaysReturns,
                    });
                }
                Statement::Expression(expr) => {
                    let expr = self.type_expr(scope, expr, None);
                    let Some(expr) = expr else { continue };

                    let kind = TypedStmtKind::Expression(expr);
                    typed_statements.push(TypedStatement {
                        kind,
                        return_actuality: BlockReturnActuality::NeverReturns,
                    });
                }
                Statement::While {
                    expression,
                    scope: then_scope,
                } => {
                    let Some(expr) = expression else { continue };
                    let span = expr.span.clone();

                    let Some(expr) = self.type_expr(scope, expr, None) else {
                        continue;
                    };

                    if expr.inferred_type != DataType::Boolean {
                        self.errors.push(Error {
                            span,
                            msg: "Expected bool".to_owned(),
                        });
                    }

                    let block = self.type_block(
                        scope,
                        then_scope,
                        is_loop,
                        return_ability.clone().demote(),
                    );
                    let return_actuality = block.return_actuality.demote();
                    let kind = TypedStmtKind::While { expr, block };

                    typed_statements.push(TypedStatement {
                        kind,
                        return_actuality,
                    });
                }
                Statement::For {
                    inital_statement: _,
                    condition_expr: _,
                    iteration_expr: _,
                    scope: _,
                } => todo!(),
                Statement::VariableDefinition {
                    ident,
                    expression,
                    data_type,
                } => {
                    scope.insert(Variable {
                        ident: ident.clone(),
                        data_type: data_type.clone(),
                    });

                    let mut expr = expression
                        .as_ref()
                        .map(|expr| self.type_expr(scope, expr, None))
                        .flatten();

                    if let Some(ref mut expr) = expr
                        && !expr.coerce_to(data_type)
                    {
                        self.errors.push(Error {
                            span: ident.span,
                            msg: "Wrong type".to_owned(),
                        });
                    }

                    let kind = TypedStmtKind::VariableDefinition {
                        ident: ident.ident.clone(),
                        expr,
                        data_type: data_type.clone(),
                    };
                    typed_statements.push(TypedStatement {
                        kind,
                        return_actuality: BlockReturnActuality::NeverReturns,
                    });
                }
            }
        }

        TypedBlock {
            statements: typed_statements,
            return_actuality: if always_returns {
                BlockReturnActuality::AlwaysReturns
            } else if sometime_returns {
                BlockReturnActuality::SometimesReturns
            } else {
                BlockReturnActuality::NeverReturns
            },
        }
    }

    fn type_expr(
        &mut self,
        scope: &mut Scope,
        expr: &ExpressionSpanned,
        memberof: Option<&StructDeclaration>,
    ) -> Option<TypedExpr> {
        Some(match &expr.expression {
            Expression::IntLiteral(n) => TypedExpr {
                inferred_type: DataType::UnsizedInt,
                kind: TypedExprKind::NumerLiteral(*n),
            },
            Expression::CharacterLiteral(c) => TypedExpr {
                inferred_type: DataType::Char,
                kind: TypedExprKind::CharLiteral(*c),
            },
            Expression::BoolLiteral(b) => TypedExpr {
                inferred_type: DataType::Boolean,
                kind: TypedExprKind::BoolLiteral(*b),
            },
            Expression::FloatLiteral(f) => TypedExpr {
                inferred_type: DataType::UnsizedFloat,
                kind: TypedExprKind::FloatLiteral(*f),
            },
            Expression::Identifier(ident) => self.type_identifier(ident, scope, memberof)?,
            Expression::StringLiteral(string) => TypedExpr {
                inferred_type: DataType::Pointer(Box::new(DataType::Char)),
                kind: TypedExprKind::StringLiteral(string.clone()),
            },
            Expression::StructLiteral { ident, members } => {
                self.type_struct_literal(ident, members, scope)?
            }
            Expression::FunctionCall(call) => self.type_call(call, scope, memberof)?,
            Expression::ArraySubscript {
                ident: _,
                element: _,
            } => todo!(),
            Expression::Binary { lhs, rhs, operator } => {
                self.type_binary(lhs, rhs, *operator, scope)?
            }
            Expression::Unary { expr, operator } => self.type_unary(expr, *operator, scope)?,
        })
    }

    fn type_identifier(
        &mut self,
        ident: &IdentifierSpanned,
        scope: &mut Scope,
        memberof: Option<&StructDeclaration>,
    ) -> Option<TypedExpr> {
        if let Some(struct_dec) = memberof {
            let Some(member) = struct_dec
                .members
                .iter()
                .find(|m| m.ident.ident == ident.ident)
            else {
                self.errors.push(Error {
                    span: ident.span,
                    msg: format!(
                        "No member {} in struct {}",
                        ident.ident, struct_dec.ident.ident
                    ),
                });
                return None;
            };
            Some(TypedExpr {
                inferred_type: member.data_type.clone(),
                kind: TypedExprKind::Identifier(ident.ident.clone()),
            })
        } else {
            let var = scope.find(&ident.ident, ident.span, &mut self.errors)?;
            Some(TypedExpr {
                inferred_type: var.data_type.clone(),
                kind: TypedExprKind::Identifier(ident.ident.clone()), //TODO Identifier -> Variable?
            })
        }
    }

    fn type_call(
        &mut self,
        call: &FunctionCall,
        scope: &mut Scope,
        memberof: Option<&StructDeclaration>,
    ) -> Option<TypedExpr> {
        if memberof.is_some() {
            todo!("function pointers");
        }
        let Some((func, _)) = self.parser.get_function(&call.ident.ident) else {
            self.errors.push(Error {
                span: call.ident.span,
                msg: format!("Cannot find a function `{}`", call.ident.ident),
            });
            return None;
        };

        let mut typed_args = Vec::new();
        for (i, expr) in call.arguments.iter().enumerate() {
            let arg = func.arguments.get(i)?;
            let span = expr.span;
            let mut expr = self.type_expr(scope, expr, None)?;
            if !expr.coerce_to(&arg.1) {
                self.errors.push(Error {
                    span,
                    msg: format!(
                        "Mismatched types: expected {:?}, got {:?}",
                        &arg.1, &expr.inferred_type
                    ),
                });
                return None;
            }
            typed_args.push(expr);
        }

        if func.arguments.len() != call.arguments.len() {
            self.errors.push(Error {
                span: call.span,
                msg: format!(
                    "Expected {} arguments, got {}.",
                    func.arguments.len(),
                    call.arguments.len()
                ),
            });
            return None;
        }

        Some(TypedExpr {
            inferred_type: func.return_type.clone(),
            kind: TypedExprKind::Call(func.ident.ident.clone(), typed_args),
        })
    }

    fn type_struct_literal(
        &mut self,
        ident: &IdentifierSpanned,
        members: &Vec<(IdentifierSpanned, Option<ExpressionSpanned>)>,
        scope: &mut Scope,
    ) -> Option<TypedExpr> {
        let Some(struct_dec) = self
            .parser
            .struct_declarations
            .iter()
            .find(|s| s.ident.ident == ident.ident)
        else {
            self.errors.push(Error {
                span: ident.span,
                msg: format!("No struct {}.", ident.ident),
            });
            return None;
        };

        let mut initialized_fields = Vec::new();
        let len = members.len();
        for (i, (ident, expr)) in members.into_iter().enumerate() {
            let m = struct_dec.members.get(i);
            if let Some(m) = m {
                if m.ident.ident != ident.ident {
                    self.errors.push(Error {
                        span: ident.span,
                        msg: format!("`{}` expected.", m.ident.ident),
                    });
                    return None;
                }
                if let Some(expr) = expr {
                    let mut expr = self.type_expr(scope, expr, None)?;
                    if !expr.coerce_to(&m.data_type) {
                        self.errors.push(Error {
                            span: ident.span,
                            msg: format!(
                                "Mismatched types: expected {:?}, got {:?}",
                                &m.data_type, &expr.inferred_type
                            ),
                        });
                        return None;
                    }
                    initialized_fields.push(expr);
                }
            } else {
                self.errors.push(Error {
                    span: ident.span,
                    msg: format!("No such field `{}`.", ident.ident),
                });
                return None;
            }
        }

        if struct_dec.members.len() > len {
            self.errors.push(Error {
                span: ident.span,
                msg: format!(
                    "Missing struct fields: `{}`",
                    List(&struct_dec.members[len..])
                ),
            });

            return None;
        }

        Some(TypedExpr {
            inferred_type: DataType::Struct(struct_dec.ident.clone()),
            kind: TypedExprKind::StructLiteral(initialized_fields),
        })
    }

    fn type_binary(
        &mut self,
        lhs: &ExpressionSpanned,
        rhs: &ExpressionSpanned,
        op: BinaryOp,
        scope: &mut Scope,
    ) -> Option<TypedExpr> {
        Some(match op {
            BinaryOp::Arithmetic(op) => self.type_arithmetic(lhs, rhs, op, scope)?,
            BinaryOp::Comparison(op) => self.type_comparison(lhs, rhs, op, scope)?,
            BinaryOp::Assign => self.type_assignment(lhs, rhs, scope)?,
            BinaryOp::MemberAccess => self.type_member_access(lhs, rhs, scope)?,
        })
    }

    fn type_member_access(
        &mut self,
        lhs: &ExpressionSpanned,
        rhs: &ExpressionSpanned,
        scope: &mut Scope,
    ) -> Option<TypedExpr> {
        let lhs_span = lhs.span;
        let lhs = self.type_expr(scope, lhs, None)?;

        let DataType::Struct(ident) = lhs.inferred_type.clone() else {
            self.errors.push(Error {
                span: lhs_span,
                msg: "Not a struct.".to_owned(),
            });
            return None;
        };

        let struct_dec = self
            .parser
            .struct_declarations
            .iter()
            .find(|s| s.ident.ident == ident.ident)?;

        let rhs = self.type_expr(scope, rhs, Some(struct_dec))?;

        let TypedExprKind::Identifier(ident) = rhs.kind else {
            todo!()
        };

        let place = self.expr_to_place(lhs_span, lhs)?;

        Some(TypedExpr {
            inferred_type: rhs.inferred_type,
            kind: TypedExprKind::MemberAccess(Box::new(place), ident),
        })
    }

    fn type_assignment(
        &mut self,
        lhs: &ExpressionSpanned,
        rhs: &ExpressionSpanned,
        scope: &mut Scope,
    ) -> Option<TypedExpr> {
        let lhs_span = lhs.span;
        let rhs_span = rhs.span;
        let lhs = self.type_expr(scope, lhs, None)?;
        let rhs = self.type_expr(scope, rhs, None)?;
        let place = self.expr_to_place(lhs_span, lhs)?;

        if place.inferred_type != rhs.inferred_type {
            self.errors.push(Error {
                span: Span::between(&lhs_span, &rhs_span),
                msg: "Can't operate on different types.".to_owned(),
            });
            return None;
        }

        Some(TypedExpr {
            inferred_type: place.inferred_type.clone(),
            kind: TypedExprKind::Assignment(Box::new(place), Box::new(rhs)),
        })
    }

    fn type_comparison(
        &mut self,
        lhs: &ExpressionSpanned,
        rhs: &ExpressionSpanned,
        op: ComparisonOp,
        scope: &mut Scope,
    ) -> Option<TypedExpr> {
        let lhs_span = lhs.span;
        let rhs_span = rhs.span;
        let mut lhs = self.type_expr(scope, lhs, None)?;
        let mut rhs = self.type_expr(scope, rhs, None)?;

        if lhs.inferred_type.is_int() && rhs.inferred_type.is_int() {
            self.coerce_binary(lhs_span, rhs_span, &mut lhs, &mut rhs, UnsizedInt, I32)?;
        } else if lhs.inferred_type.is_float() && rhs.inferred_type.is_float() {
            self.coerce_binary(lhs_span, rhs_span, &mut lhs, &mut rhs, UnsizedFloat, F32)?;
        } else {
            self.errors.push(Error {
                span: lhs_span,
                msg: "Expected numeric types for comparison op.".to_owned(),
            });
            return None;
        }

        Some(TypedExpr {
            inferred_type: DataType::Boolean,
            kind: TypedExprKind::Comparison(op, Box::new(lhs), Box::new(rhs)),
        })
    }

    fn type_arithmetic(
        &mut self,
        lhs: &ExpressionSpanned,
        rhs: &ExpressionSpanned,
        op: ArithmeticOp,
        scope: &mut Scope,
    ) -> Option<TypedExpr> {
        let lhs_span = lhs.span;
        let rhs_span = rhs.span;
        let mut lhs = self.type_expr(scope, lhs, None)?;
        let mut rhs = self.type_expr(scope, rhs, None)?;

        if lhs.inferred_type.is_int() && rhs.inferred_type.is_int() {
            self.coerce_binary(lhs_span, rhs_span, &mut lhs, &mut rhs, UnsizedInt, I32)?;
        } else if lhs.inferred_type.is_float() && rhs.inferred_type.is_float() {
            self.coerce_binary(lhs_span, rhs_span, &mut lhs, &mut rhs, UnsizedFloat, F32)?;
        } else {
            self.errors.push(Error {
                span: lhs_span,
                msg: "Expected numeric types for arithmetic op.".to_owned(),
            });
            return None;
        }

        Some(TypedExpr {
            inferred_type: lhs.inferred_type.clone(),
            kind: TypedExprKind::Arithmetic(op, Box::new(lhs), Box::new(rhs)),
        })
    }

    fn coerce_binary(
        &mut self,
        lhs_span: Span,
        rhs_span: Span,
        lhs: &mut TypedExpr,
        rhs: &mut TypedExpr,
        unsized_ty: DataType,
        default_ty: DataType,
    ) -> Option<()> {
        if lhs.inferred_type == rhs.inferred_type {
            if lhs.inferred_type == unsized_ty {
                //TODO some back propagating inference or constant propagation?
                //It's probably where I'd do bounds checking as well
                lhs.inferred_type = default_ty.clone();
                rhs.inferred_type = default_ty;
            }
        } else if lhs.coerce_to(&rhs.inferred_type) {
        } else if rhs.coerce_to(&lhs.inferred_type) {
        } else if (lhs.inferred_type == UnsizedFloat && rhs.inferred_type == UnsizedInt)
            || (rhs.inferred_type == UnsizedFloat && lhs.inferred_type == UnsizedInt)
        {
            //TODO: I'm not too sure if this check should be here
            lhs.inferred_type = F32;
            rhs.inferred_type = F32;
        } else {
            self.errors.push(Error {
                span: Span::between(&lhs_span, &rhs_span),
                msg: "Can't operate on different types.".to_owned(),
            });
            return None;
        }
        Some(())
    }

    fn type_unary(
        &mut self,
        expr: &ExpressionSpanned,
        op: UnaryOperator,
        scope: &mut Scope,
    ) -> Option<TypedExpr> {
        let span = expr.span;
        let Some(expr) = self.type_expr(scope, expr, None) else {
            return None;
        };

        match op {
            UnaryOperator::Dereference => {
                let DataType::Pointer(ty) = expr.inferred_type.clone() else {
                    self.errors.push(Error {
                        span,
                        msg: "Expected bool".to_owned(),
                    });
                    return None;
                };

                Some(TypedExpr {
                    inferred_type: *ty,
                    kind: TypedExprKind::Dereference(Box::new(expr)),
                })
            }
            UnaryOperator::AddressOf => Some(TypedExpr {
                inferred_type: DataType::Pointer(Box::new(expr.inferred_type.clone())),
                kind: TypedExprKind::UnaryAddressOf(Box::new(self.expr_to_place(span, expr)?)),
            }),
            UnaryOperator::LogicalNot => {
                if expr.inferred_type != DataType::Boolean {
                    self.errors.push(Error {
                        span,
                        msg: "Expected bool".to_owned(),
                    });
                    return None;
                }

                Some(TypedExpr {
                    inferred_type: DataType::Boolean,
                    kind: TypedExprKind::Not(Box::new(expr)),
                })
            }
            UnaryOperator::Negation => {
                if !expr.inferred_type.is_float() && !expr.inferred_type.is_int() {
                    self.errors.push(Error {
                        span,
                        msg: "Expected a numeric type".to_owned(),
                    });
                    return None;
                }

                Some(TypedExpr {
                    inferred_type: expr.inferred_type.clone(),
                    kind: TypedExprKind::Negation(Box::new(expr)),
                })
            }
        }
    }

    fn expr_to_place(&mut self, span: Span, expr: TypedExpr) -> Option<Place> {
        Some(match expr.kind {
            TypedExprKind::Dereference(expr) => Place {
                inferred_type: expr.inferred_type.clone(),
                kind: PlaceKind::Deref(expr),
            },
            TypedExprKind::Identifier(ident) => Place {
                inferred_type: expr.inferred_type,
                kind: PlaceKind::Variable(ident),
            },
            TypedExprKind::MemberAccess(place, member) => Place {
                inferred_type: expr.inferred_type,
                kind: PlaceKind::Member(place, member),
            },
            _ => {
                self.errors.push(Error {
                    span,
                    msg: "Not an lvalue.".to_owned(),
                });
                return None;
            }
        })
    }
}
