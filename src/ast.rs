#![allow(dead_code)]
use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    LetStatement { ident: Token, expr: Expression },
    ReturnStatement(Expression),
    ExpressionStatement(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Ident(Token),
    IntLiteral(i64),
    BoolLiteral(bool),
    PrefixExpression {
        operator: Token,
        right: Box<Expression>,
    },
    InfixExpression {
        operator: Token,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    IfExpression {
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    FunctionLiteral {
        parameters: Vec<Expression>,
        body: BlockStatement,
    },
}

pub type Program = BlockStatement;

pub type BlockStatement = Vec<Statement>;
