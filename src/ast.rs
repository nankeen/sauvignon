#![allow(dead_code)]
use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    LetStatement { ident: Token, expr: Expression },
    ReturnStatement(Expression),
    ExpressionStatement(Expression),
    Empty,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Ident(Token),
    IntLiteral(i64),
    PrefixOperator {
        prefix: Token,
        expr: Box<Expression>,
    },
    Empty,
}

pub type Program = BlockStatement;

pub type BlockStatement = Vec<Statement>;