#![allow(dead_code)]
use crate::ast::*;
use crate::lexer::{Lexer, Token};
use std::iter::Peekable;
use std::mem::discriminant;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Equality,
    Comparison,
    Sum,
    Product,
    Prefix,
    Call,
}

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
                "expected {:?} got {:?} instead",
                expect, self.cursor
            )),
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.cursor {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        self.next_token();

        // TODO: skipping expressions
        while self.cursor != Token::Semicolon {
            self.next_token();
        }
        Ok(Statement::ReturnStatement(Expression::Empty))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        if let Some(Token::Semicolon) = self.lexer.peek() {
            self.next_token();
        }
        Ok(Statement::ExpressionStatement(expr))
    }

    fn parse_expression(&mut self, _precedence: Precedence) -> Result<Expression, String> {
        match self.cursor {
            Token::Ident(_) => Ok(Expression::Ident(self.cursor.clone())),
            Token::IntLiteral(i) => Ok(Expression::IntLiteral(i)),
            _ => Err(format!("unexpected token {:?} in expression", self.cursor)),
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
        let mut statements = program.iter();

        // TODO: Update test cases with proper expressions
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

    #[test]
    fn test_return_statements() -> Result<(), String> {
        let input = "return 5;
        return 10;
        return 993322;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;
        let mut statements = program.iter();

        // TODO: Update test cases with proper expressions
        let tests = [
            Statement::ReturnStatement(Expression::Empty),
            Statement::ReturnStatement(Expression::Empty),
        ];

        for test in &tests {
            match statements.next() {
                Some(tok) => assert_eq!(*test, *tok),
                None => panic!("unexpected parser error: returned None"),
            };
        }
        Ok(())
    }

    #[test]
    fn test_identifier_expr() -> Result<(), String> {
        let input = "foobar";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;
        let mut statements = program.iter();

        // TODO: Update test cases with proper expressions
        let tests = [Statement::ExpressionStatement(Expression::Ident(
            Token::Ident("foobar".to_string()),
        ))];

        for test in &tests {
            match statements.next() {
                Some(tok) => assert_eq!(*test, *tok),
                None => panic!("unexpected parser error: returned None"),
            };
        }
        Ok(())
    }

    #[test]
    fn test_integer_expr() -> Result<(), String> {
        let input = "5; 1024";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;
        let mut statements = program.iter();

        // TODO: Update test cases with proper expressions
        let tests = [
            Statement::ExpressionStatement(Expression::IntLiteral(5)),
            Statement::ExpressionStatement(Expression::IntLiteral(1024)),
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
