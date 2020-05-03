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
            Some(tok) => Err(format!("expected {:?} got {:?} instead", expect, tok)),
            _ => Err("unexpected parsing error".to_string()),
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

    fn parse_grouped_expr(&mut self) -> Result<Expression, String> {
        self.next_token();
        let expr = self.parse_expression(Precedence::Lowest);
        self.expect_token(Token::RParen)?;
        expr
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, String> {
        let mut left = match &self.cursor {
            Token::Ident(s) => Ok(Expression::Ident(s.to_string())),
            Token::IntLiteral(i) => Ok(Expression::IntLiteral(*i)),
            Token::BoolLiteral(b) => Ok(Expression::BoolLiteral(*b)),
            Token::StringLiteral(s) => Ok(Expression::StringLiteral(s.clone())),
            Token::Bang => self.parse_prefix_expr(),
            Token::Minus => self.parse_prefix_expr(),
            Token::LParen => self.parse_grouped_expr(),
            Token::If => self.parse_if_expr(),
            Token::Function => self.parse_fn_expr(),
            Token::LBracket => self.parse_array_expr(),
            Token::LBrace => self.parse_hash_expr(),
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
                Some(Token::LParen) => self.parse_call_expr(left)?,
                Some(Token::LBracket) => self.parse_index_expr(left)?,
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
            Token::LParen => Precedence::Call,
            Token::LBracket => Precedence::Index,
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

    fn parse_if_expr(&mut self) -> Result<Expression, String> {
        self.expect_token(Token::LParen)?;
        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect_token(Token::RParen)?;
        self.expect_token(Token::LBrace)?;
        let consequence = self.parse_block_statement()?;
        let alternative = match self.lexer.peek() {
            Some(Token::Else) => {
                self.next_token();
                self.expect_token(Token::LBrace)?;
                self.parse_block_statement()?
            }
            _ => Vec::new(),
        };
        Ok(Expression::IfExpression {
            condition: Box::new(condition),
            consequence,
            alternative,
        })
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, String> {
        self.next_token();
        let mut block = Vec::new();
        while self.cursor != Token::RBrace && self.cursor != Token::EOF {
            let statement = self.parse_statement()?;
            block.push(statement);
            self.next_token();
        }
        Ok(block)
    }

    fn parse_fn_expr(&mut self) -> Result<Expression, String> {
        self.expect_token(Token::LParen)?;
        let parameters = self.parse_fn_params()?;
        self.expect_token(Token::LBrace)?;
        let body = self.parse_block_statement()?;
        Ok(Expression::FunctionLiteral { parameters, body })
    }

    fn parse_fn_params(&mut self) -> Result<Vec<Token>, String> {
        let mut params = Vec::new();
        if let Some(Token::RParen) = self.lexer.peek() {
            self.next_token();
            return Ok(params);
        };

        self.expect_token(Token::Ident("".to_string()))?;

        params.push(self.cursor.clone());

        while let Some(Token::Comma) = self.lexer.peek() {
            self.next_token();
            self.expect_token(Token::Ident("".to_string()))?;
            params.push(self.cursor.clone());
        }
        self.expect_token(Token::RParen)?;

        Ok(params)
    }

    fn parse_array_expr(&mut self) -> Result<Expression, String> {
        Ok(Expression::ArrayLiteral(
            self.parse_expr_list(Token::RBracket)?,
        ))
    }

    fn parse_hash_expr(&mut self) -> Result<Expression, String> {
        let mut hash = Vec::new();
        if Some(&Token::RBrace) == self.lexer.peek() {
            self.next_token();
            return Ok(Expression::HashLiteral(hash));
        }

        self.next_token();
        let key = self.parse_expression(Precedence::Lowest)?;
        self.expect_token(Token::Colon)?;
        self.next_token();
        let val = self.parse_expression(Precedence::Lowest)?;
        hash.push((key, val));

        while let Some(Token::Comma) = self.lexer.peek() {
            self.next_token();
            self.next_token();
            let key = self.parse_expression(Precedence::Lowest)?;
            self.expect_token(Token::Colon)?;
            self.next_token();
            let val = self.parse_expression(Precedence::Lowest)?;
            hash.push((key, val));
        }
        self.expect_token(Token::RBrace)?;

        Ok(Expression::HashLiteral(hash))
    }

    fn parse_expr_list(&mut self, end: Token) -> Result<Vec<Expression>, String> {
        let mut list = Vec::new();
        if Some(&end) == self.lexer.peek() {
            self.next_token();
            return Ok(list);
        };

        self.next_token();
        list.push(self.parse_expression(Precedence::Lowest)?);

        while let Some(Token::Comma) = self.lexer.peek() {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(Precedence::Lowest)?);
        }
        self.expect_token(end)?;

        Ok(list)
    }

    fn parse_call_expr(&mut self, func: Expression) -> Result<Expression, String> {
        self.expect_token(Token::LParen)?;
        let arguments = self.parse_expr_list(Token::RParen)?;
        Ok(Expression::Call {
            function: Box::new(func),
            arguments,
        })
    }

    fn parse_index_expr(&mut self, left: Expression) -> Result<Expression, String> {
        self.expect_token(Token::LBracket)?;
        self.next_token();
        let index = self.parse_expression(Precedence::Lowest)?;
        self.expect_token(Token::RBracket)?;
        Ok(Expression::IndexExpression {
            left: Box::new(left),
            index: Box::new(index),
        })
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
            "foobar".to_string(),
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
    fn test_string_expr() -> Result<(), String> {
        let input = "\"cheesecake\"";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;
        let mut statements = program.iter();

        let tests = [Statement::ExpressionStatement(Expression::StringLiteral(
            "cheesecake".to_string(),
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
    fn test_array_expr() -> Result<(), String> {
        let input = "[123, \"helo\"]";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;
        let mut statements = program.iter();

        let tests = [Statement::ExpressionStatement(Expression::ArrayLiteral(
            vec![
                Expression::IntLiteral(123),
                Expression::StringLiteral("helo".to_string()),
            ],
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

    #[test]
    fn test_precedence_expr() -> Result<(), String> {
        let input = "1 * (2 + 3);
        1 + (2 + 3) + 4;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;
        let mut statements = program.iter();

        let tests = [
            Statement::ExpressionStatement(Expression::InfixExpression {
                operator: Token::Asterisk,
                left: Box::new(Expression::IntLiteral(1)),
                right: Box::new(Expression::InfixExpression {
                    operator: Token::Plus,
                    left: Box::new(Expression::IntLiteral(2)),
                    right: Box::new(Expression::IntLiteral(3)),
                }),
            }),
            Statement::ExpressionStatement(Expression::InfixExpression {
                operator: Token::Plus,
                left: Box::new(Expression::InfixExpression {
                    operator: Token::Plus,
                    left: Box::new(Expression::IntLiteral(1)),
                    right: Box::new(Expression::InfixExpression {
                        operator: Token::Plus,
                        left: Box::new(Expression::IntLiteral(2)),
                        right: Box::new(Expression::IntLiteral(3)),
                    }),
                }),
                right: Box::new(Expression::IntLiteral(4)),
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
    fn test_if_expr() -> Result<(), String> {
        let input = "if (a > b) { c }";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;
        let mut statements = program.iter();

        let tests = [Statement::ExpressionStatement(Expression::IfExpression {
            condition: Box::new(Expression::InfixExpression {
                operator: Token::GreaterThan,
                left: Box::new(Expression::Ident("a".to_string())),
                right: Box::new(Expression::Ident("b".to_string())),
            }),
            consequence: vec![Statement::ExpressionStatement(Expression::Ident(
                "c".to_string(),
            ))],
            alternative: vec![],
        })];

        for test in &tests {
            match statements.next() {
                Some(tok) => assert_eq!(*test, *tok),
                None => panic!("unexpected parser error: returned None"),
            };
        }
        Ok(())
    }

    #[test]
    fn test_if_else_expr() -> Result<(), String> {
        let input = "if (a > b) { c } else { d }";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;
        let mut statements = program.iter();

        let tests = [Statement::ExpressionStatement(Expression::IfExpression {
            condition: Box::new(Expression::InfixExpression {
                operator: Token::GreaterThan,
                left: Box::new(Expression::Ident("a".to_string())),
                right: Box::new(Expression::Ident("b".to_string())),
            }),
            consequence: vec![Statement::ExpressionStatement(Expression::Ident(
                "c".to_string(),
            ))],
            alternative: vec![Statement::ExpressionStatement(Expression::Ident(
                "d".to_string(),
            ))],
        })];

        for test in &tests {
            match statements.next() {
                Some(tok) => assert_eq!(*test, *tok),
                None => panic!("unexpected parser error: returned None"),
            };
        }
        Ok(())
    }

    #[test]
    fn test_fn_literal_expr() -> Result<(), String> {
        let input = "fn(a, b) { 1 == 2; }";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;
        let mut statements = program.iter();

        let tests = [Statement::ExpressionStatement(
            Expression::FunctionLiteral {
                body: vec![Statement::ExpressionStatement(
                    Expression::InfixExpression {
                        operator: Token::Equal,
                        left: Box::new(Expression::IntLiteral(1)),
                        right: Box::new(Expression::IntLiteral(2)),
                    },
                )],
                parameters: vec![Token::Ident("a".to_string()), Token::Ident("b".to_string())],
            },
        )];

        for test in &tests {
            match statements.next() {
                Some(tok) => assert_eq!(*test, *tok),
                None => panic!("unexpected parser error: returned None"),
            };
        }
        Ok(())
    }

    #[test]
    fn test_call_expr() -> Result<(), String> {
        let input = "add(a, 1+2, 5 == 6);";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;
        let mut statements = program.iter();

        let tests = [Statement::ExpressionStatement(Expression::Call {
            function: Box::new(Expression::Ident("add".to_string())),
            arguments: vec![
                Expression::Ident("a".to_string()),
                Expression::InfixExpression {
                    operator: Token::Plus,
                    left: Box::new(Expression::IntLiteral(1)),
                    right: Box::new(Expression::IntLiteral(2)),
                },
                Expression::InfixExpression {
                    operator: Token::Equal,
                    left: Box::new(Expression::IntLiteral(5)),
                    right: Box::new(Expression::IntLiteral(6)),
                },
            ],
        })];

        for test in &tests {
            match statements.next() {
                Some(tok) => assert_eq!(*test, *tok),
                None => panic!("unexpected parser error: returned None"),
            };
        }
        Ok(())
    }

    #[test]
    fn test_index_expr() -> Result<(), String> {
        let input = "add[5+3]; 6";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;
        let mut statements = program.iter();

        let tests = [
            Statement::ExpressionStatement(Expression::IndexExpression {
                left: Box::new(Expression::Ident("add".to_string())),
                index: Box::new(Expression::InfixExpression {
                    left: Box::new(Expression::IntLiteral(5)),
                    operator: Token::Plus,
                    right: Box::new(Expression::IntLiteral(3)),
                }),
            }),
            Statement::ExpressionStatement(Expression::IntLiteral(6)),
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
    fn test_hash_expr() -> Result<(), String> {
        let input = "{\"alfred\": 2, \"cheese\": 4}; {}";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;
        let mut statements = program.iter();

        let tests = [
            Statement::ExpressionStatement(Expression::HashLiteral(vec![
                (
                    Expression::StringLiteral("alfred".to_string()),
                    Expression::IntLiteral(2),
                ),
                (
                    Expression::StringLiteral("cheese".to_string()),
                    Expression::IntLiteral(4),
                ),
            ])),
            Statement::ExpressionStatement(Expression::HashLiteral(vec![])),
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
