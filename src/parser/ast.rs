use crate::lexer::Token;

/// `Statement` enum describes different types of statements.
/// Statements do not necessarily yield results
#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Statement {
    LetStatement { ident: Token, expr: Expression },
    ReturnStatement(Expression),
    ExpressionStatement(Expression),
}

/// `Expression` enum describes different types of expressions.
/// Expressions will result in an object.
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

/// `Program` contains all of the statements
pub type Program = BlockStatement;

/// `BlockStatement` contains parts of the program, e.g. in an if-block
pub type BlockStatement = Vec<Statement>;

/// Precedence describes the order which expressions must be parsed
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
