#![allow(dead_code)]
use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    LetStatement { ident: Token, expr: Expression },
    Empty,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Empty,
}

pub type Program = BlockStatement;

pub type BlockStatement = Vec<Statement>;
