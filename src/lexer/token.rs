/// `Token` enum represents the tokens returned by the lexer
#[derive(PartialEq, Clone, Debug, Eq)]
pub enum Token {
    Illegal,
    EOF,

    // Identifiers + literals
    Ident(String),
    IntLiteral(i64),
    BoolLiteral(bool),
    StringLiteral(String),

    // Statements
    Assign,
    If,
    Else,

    // Operators
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,

    // Delimiters
    Comma,
    Semicolon,
    Colon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // Keywords
    Function,
    Let,
    Return,
}
