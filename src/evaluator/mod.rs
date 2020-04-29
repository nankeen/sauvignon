#![allow(dead_code)]
mod object;
use crate::ast::{Expression, Statement};
use crate::lexer::Token;
use object::*;

pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {}
    }

    pub fn eval_program(&self, program: &[Statement]) -> Result<Object, &str> {
        self.eval_blockstatement(program)
    }

    fn eval_blockstatement(&self, blockstmt: &[Statement]) -> Result<Object, &str> {
        let mut result = Ok(Object::Null);
        for stmt in blockstmt {
            result = self.eval_statement(stmt);
        }
        result
    }

    fn eval_statement(&self, stmt: &Statement) -> Result<Object, &str> {
        match stmt {
            Statement::ExpressionStatement(expr) => self.eval_expression(expr),
            _ => Err("wot"),
        }
    }

    fn eval_expression(&self, expr: &Expression) -> Result<Object, &str> {
        match expr {
            Expression::IntLiteral(i) => Ok(Object::Integer(*i)),
            Expression::BoolLiteral(b) => Ok(Object::Boolean(*b)),
            Expression::PrefixExpression { right, operator } => {
                let eval_r = self.eval_expression(right)?;
                self.eval_prefix_expr(operator, &eval_r)
            }
            Expression::InfixExpression {
                left,
                right,
                operator,
            } => {
                let eval_l = self.eval_expression(left)?;
                let eval_r = self.eval_expression(right)?;
                self.eval_infix_expr(operator, &eval_l, &eval_r)
            }
            _ => Err("not implemented"),
        }
    }

    fn eval_infix_expr(&self, op: &Token, left: &Object, right: &Object) -> Result<Object, &str> {
        match op {
            Token::Plus => match (left, right) {
                (Object::Integer(l), Object::Integer(r)) => Ok(Object::Integer(l + r)),
                _ => Err("expected integer for infix plus"),
            },
            Token::Minus => match (left, right) {
                (Object::Integer(l), Object::Integer(r)) => Ok(Object::Integer(l - r)),
                _ => Err("expected integer for infix minus"),
            },
            Token::Asterisk => match (left, right) {
                (Object::Integer(l), Object::Integer(r)) => Ok(Object::Integer(l * r)),
                _ => Err("expected integer for infix times"),
            },
            Token::Slash => match (left, right) {
                (Object::Integer(l), Object::Integer(r)) => Ok(Object::Integer(l / r)),
                _ => Err("expected integer for infix divide"),
            },
            Token::Equal => Ok(Object::Boolean(left == right)),
            Token::NotEqual => Ok(Object::Boolean(left != right)),
            Token::LessThan => Ok(Object::Boolean(left < right)),
            Token::GreaterThan => Ok(Object::Boolean(left > right)),
            _ => Err("unknown infix operator"),
        }
    }

    fn eval_prefix_expr(&self, op: &Token, right: &Object) -> Result<Object, &str> {
        match op {
            Token::Bang => self.eval_bang_prefix(right),
            Token::Minus => self.eval_minus_prefix(right),
            _ => Err("not implemented"),
        }
    }

    fn eval_minus_prefix(&self, right: &Object) -> Result<Object, &str> {
        match right {
            Object::Integer(i) => Ok(Object::Integer(-i)),
            _ => Ok(Object::Null),
        }
    }

    fn eval_bang_prefix(&self, right: &Object) -> Result<Object, &str> {
        match right {
            Object::Boolean(true) => Ok(Object::Boolean(false)),
            Object::Boolean(false) => Ok(Object::Boolean(true)),
            Object::Null => Ok(Object::Boolean(true)),
            _ => Ok(Object::Boolean(false)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Evaluator, Object};
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn eval_compare(input: &str, object: Object) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        let evaluator = Evaluator::new();
        let eval = evaluator.eval_program(&program).unwrap();
        assert_eq!(eval, object);
    }

    #[test]
    fn test_eval_simple_expression() {
        eval_compare("32", Object::Integer(32));
        eval_compare("1024", Object::Integer(1024));
        eval_compare("true", Object::Boolean(true));
        eval_compare("false", Object::Boolean(false));
    }

    #[test]
    fn test_eval_prefix() {
        eval_compare("!true", Object::Boolean(false));
        eval_compare("!false", Object::Boolean(true));
        eval_compare("!!true", Object::Boolean(true));
        eval_compare("!!false", Object::Boolean(false));
        eval_compare("!5", Object::Boolean(false));
        eval_compare("!!5", Object::Boolean(true));

        eval_compare("-32", Object::Integer(-32));
        eval_compare("-1024", Object::Integer(-1024));
    }

    #[test]
    fn test_eval_infix_int() {
        eval_compare("32+32", Object::Integer(64));
        eval_compare("8+16", Object::Integer(24));
        eval_compare("8-16", Object::Integer(-8));
        eval_compare("2*16", Object::Integer(32));
        eval_compare("16/2", Object::Integer(8));
        eval_compare("3 + 3 * 8", Object::Integer(27));
        eval_compare("(3 + 3) * 8", Object::Integer(48));
        eval_compare("(13 + 3) / 8", Object::Integer(2));
        eval_compare("13 + 24 / 8", Object::Integer(16));
    }

    #[test]
    fn test_eval_infix_bool() {
        eval_compare("true == true", Object::Boolean(true));
        eval_compare("true != false", Object::Boolean(true));
        eval_compare("true != false == false", Object::Boolean(false));

        eval_compare("32 == 32", Object::Boolean(true));
        eval_compare("32 != 32", Object::Boolean(false));
        eval_compare("8 < 16", Object::Boolean(true));
        eval_compare("8 > 16", Object::Boolean(false));
        eval_compare("2 > 16 == false", Object::Boolean(true));
    }
}
