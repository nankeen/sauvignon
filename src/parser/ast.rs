use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Statement {
    LetStatement { ident: Token, expr: Expression },
    ReturnStatement(Expression),
    ExpressionStatement(Expression),
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Expression {
    Ident(String),
    IntLiteral(i64),
    BoolLiteral(bool),
    StringLiteral(String),
    ArrayLiteral(Vec<Expression>),
    HashLiteral(Vec<(Expression, Expression)>),
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
        alternative: BlockStatement,
    },
    FunctionLiteral {
        parameters: Vec<Token>,
        body: BlockStatement,
    },
    Call {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
    IndexExpression {
        left: Box<Expression>,
        index: Box<Expression>,
    },
}

pub type Program = BlockStatement;

pub type BlockStatement = Vec<Statement>;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Precedence {
    Lowest,
    Equality,
    Comparison,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}
