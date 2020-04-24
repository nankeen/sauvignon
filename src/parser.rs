#![allow(dead_code)]
use crate::ast::*;
use crate::lexer::{Lexer, Token};
use std::iter::Peekable;
use std::mem::discriminant;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    cursor: Token,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Parser<'a> {
        let tok = lexer.next().unwrap();
        Parser {
            lexer: lexer.peekable(),
            cursor: tok,
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, String> {
        let mut program: Program = Vec::new();
        while self.cursor != Token::EOF {
            let statement = self.parse_statement()?;
            program.push(statement);
            self.next_token();
        }
        Ok(program)
    }

    fn next_token(&mut self) {
        self.cursor = self.lexer.next().unwrap();
    }

    fn expect_token(&mut self, expect: Token) -> Result<(), String> {
        match self.lexer.peek() {
            Some(tok) if discriminant(tok) == discriminant(&expect) => {
                self.next_token();
                Ok(())
            }
            _ => Err(format!(
                "unexpected token: expected {:?} got {:?} instead",
                expect, self.cursor
            )),
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.cursor {
            Token::Let => self.parse_let_statement(),
            _ => Err("Invalid token".to_string()),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        self.expect_token(Token::Ident("".to_string()))?;
        let ident = self.cursor.clone();
        self.expect_token(Token::Assign)?;

        // TODO: skipping expressions
        while self.cursor != Token::Semicolon {
            self.next_token();
        }
        Ok(Statement::LetStatement {
            ident,
            expr: Expression::Empty,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, Parser, Token};
    use crate::ast::{Expression, Statement};

    #[test]
    fn test_let_statements() -> Result<(), String> {
        let input = "let x = ;
        let y = 10;
        let foobar = 838383;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;
        println!("Program: {:?}", program);
        let mut statements = program.iter();

        let tests = [
            Statement::LetStatement {
                ident: Token::Ident("x".to_string()),
                expr: Expression::Empty,
            },
            Statement::LetStatement {
                ident: Token::Ident("y".to_string()),
                expr: Expression::Empty,
            },
        ];

        for test in &tests {
            match statements.next() {
                Some(tok) => assert_eq!(*test, *tok),
                None => panic!("unexpected parser error: returned None"),
            };
        }
        Ok(())
    }
}
