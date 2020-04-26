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

        let expr = self.parse_expression(Precedence::Lowest)?;
        if let Some(Token::Semicolon) = self.lexer.peek() {
            self.next_token();
        }
        Ok(Statement::ReturnStatement(expr))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        if let Some(Token::Semicolon) = self.lexer.peek() {
            self.next_token();
        }
        Ok(Statement::ExpressionStatement(expr))
    }

    fn parse_prefix_expr(&mut self) -> Result<Expression, String> {
        let op = self.cursor.clone();
        self.next_token();
        let expr = self.parse_expression(Precedence::Prefix)?;
        Ok(Expression::PrefixExpression {
            operator: op,
            right: Box::new(expr),
        })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, String> {
        let mut left = match self.cursor {
            Token::Ident(_) => Ok(Expression::Ident(self.cursor.clone())),
            Token::IntLiteral(i) => Ok(Expression::IntLiteral(i)),
            Token::BoolLiteral(b) => Ok(Expression::BoolLiteral(b)),
            Token::Bang => self.parse_prefix_expr(),
            Token::Minus => self.parse_prefix_expr(),
            _ => Err(format!("unexpected token {:?} in expression", self.cursor)),
        }?;

        while precedence < Parser::map_precedence(self.lexer.peek().unwrap()) {
            left = match self.lexer.peek() {
                Some(Token::Semicolon) => break,
                Some(Token::Plus) => self.parse_infix_expr(left)?,
                Some(Token::Minus) => self.parse_infix_expr(left)?,
                Some(Token::Slash) => self.parse_infix_expr(left)?,
                Some(Token::Asterisk) => self.parse_infix_expr(left)?,
                Some(Token::Equal) => self.parse_infix_expr(left)?,
                Some(Token::NotEqual) => self.parse_infix_expr(left)?,
                Some(Token::LessThan) => self.parse_infix_expr(left)?,
                Some(Token::GreaterThan) => self.parse_infix_expr(left)?,
                _ => break,
            };
        }
        Ok(left)
    }

    fn parse_infix_expr(&mut self, left: Expression) -> Result<Expression, String> {
        self.next_token();
        let token = self.cursor.clone();
        let precedence = Parser::map_precedence(&self.cursor);
        self.next_token();
        Ok(Expression::InfixExpression {
            left: Box::new(left),
            operator: token,
            right: Box::new(self.parse_expression(precedence)?),
        })
    }

    fn map_precedence(token: &Token) -> Precedence {
        match token {
            Token::Equal => Precedence::Equality,
            Token::NotEqual => Precedence::Equality,
            Token::LessThan => Precedence::Comparison,
            Token::GreaterThan => Precedence::Comparison,
            Token::Plus => Precedence::Sum,
            Token::Minus => Precedence::Sum,
            Token::Slash => Precedence::Product,
            Token::Asterisk => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        self.expect_token(Token::Ident("".to_string()))?;
        let ident = self.cursor.clone();
        self.expect_token(Token::Assign)?;
        self.next_token();
        let expr = self.parse_expression(Precedence::Lowest)?;
        if let Some(Token::Semicolon) = self.lexer.peek() {
            self.next_token();
        }
        Ok(Statement::LetStatement { ident, expr })
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, Parser, Token};
    use crate::ast::{Expression, Statement};

    #[test]
    fn test_let_statements() -> Result<(), String> {
        let input = "let x = -1;
        let y = 10;
        let foobar = 838383 + 1;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;
        let mut statements = program.iter();

        let tests = [
            Statement::LetStatement {
                ident: Token::Ident("x".to_string()),
                expr: Expression::PrefixExpression {
                    operator: Token::Minus,
                    right: Box::new(Expression::IntLiteral(1)),
                },
            },
            Statement::LetStatement {
                ident: Token::Ident("y".to_string()),
                expr: Expression::IntLiteral(10),
            },
            Statement::LetStatement {
                ident: Token::Ident("foobar".to_string()),
                expr: Expression::InfixExpression {
                    left: Box::new(Expression::IntLiteral(838383)),
                    operator: Token::Plus,
                    right: Box::new(Expression::IntLiteral(1)),
                },
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
        let input = "return -5;
        return 10;
        return 993322;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;
        let mut statements = program.iter();

        // Update test cases with proper expressions
        let tests = [
            Statement::ReturnStatement(Expression::PrefixExpression {
                operator: Token::Minus,
                right: Box::new(Expression::IntLiteral(5)),
            }),
            Statement::ReturnStatement(Expression::IntLiteral(10)),
            Statement::ReturnStatement(Expression::IntLiteral(993322)),
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
    fn test_boolean_expr() -> Result<(), String> {
        let input = "true; false";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;
        let mut statements = program.iter();

        let tests = [
            Statement::ExpressionStatement(Expression::BoolLiteral(true)),
            Statement::ExpressionStatement(Expression::BoolLiteral(false)),
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
    fn test_integer_expr() -> Result<(), String> {
        let input = "5; 1024";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;
        let mut statements = program.iter();

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

    #[test]
    fn test_prefix_expr() -> Result<(), String> {
        let input = "!5; -15;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;
        let mut statements = program.iter();

        let tests = [
            Statement::ExpressionStatement(Expression::PrefixExpression {
                operator: Token::Bang,
                right: Box::new(Expression::IntLiteral(5)),
            }),
            Statement::ExpressionStatement(Expression::PrefixExpression {
                operator: Token::Minus,
                right: Box::new(Expression::IntLiteral(15)),
            }),
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
    fn test_infix_expr() -> Result<(), String> {
        let input = "5+5; 5-5;
        5*5; 5 / 5;
        5 > 5; 5 < 5;
        5 == 5; 5 != 5";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;
        let mut statements = program.iter();

        let tests = [
            Statement::ExpressionStatement(Expression::InfixExpression {
                operator: Token::Plus,
                left: Box::new(Expression::IntLiteral(5)),
                right: Box::new(Expression::IntLiteral(5)),
            }),
            Statement::ExpressionStatement(Expression::InfixExpression {
                operator: Token::Minus,
                left: Box::new(Expression::IntLiteral(5)),
                right: Box::new(Expression::IntLiteral(5)),
            }),
            Statement::ExpressionStatement(Expression::InfixExpression {
                operator: Token::Asterisk,
                left: Box::new(Expression::IntLiteral(5)),
                right: Box::new(Expression::IntLiteral(5)),
            }),
            Statement::ExpressionStatement(Expression::InfixExpression {
                operator: Token::Slash,
                left: Box::new(Expression::IntLiteral(5)),
                right: Box::new(Expression::IntLiteral(5)),
            }),
            Statement::ExpressionStatement(Expression::InfixExpression {
                operator: Token::GreaterThan,
                left: Box::new(Expression::IntLiteral(5)),
                right: Box::new(Expression::IntLiteral(5)),
            }),
            Statement::ExpressionStatement(Expression::InfixExpression {
                operator: Token::LessThan,
                left: Box::new(Expression::IntLiteral(5)),
                right: Box::new(Expression::IntLiteral(5)),
            }),
            Statement::ExpressionStatement(Expression::InfixExpression {
                operator: Token::Equal,
                left: Box::new(Expression::IntLiteral(5)),
                right: Box::new(Expression::IntLiteral(5)),
            }),
            Statement::ExpressionStatement(Expression::InfixExpression {
                operator: Token::NotEqual,
                left: Box::new(Expression::IntLiteral(5)),
                right: Box::new(Expression::IntLiteral(5)),
            }),
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
