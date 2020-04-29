#![allow(dead_code)]
mod environment;
mod object;
use crate::ast::{Expression, Statement};
use crate::lexer::Token;
use environment::*;
use object::*;

pub struct Evaluator {
    env: Environment,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            env: Environment::new(),
        }
    }

    pub fn eval_program(&mut self, program: &[Statement]) -> Result<Object, String> {
        let mut result = Object::Null;
        for stmt in program {
            result = self.eval_statement(stmt)?;
            if let Object::ReturnValue(val) = result {
                return Ok(*val);
            }
        }
        Ok(result)
    }

    fn eval_blockstatement(&mut self, blockstmt: &[Statement]) -> Result<Object, String> {
        let mut result = Object::Null;
        for stmt in blockstmt {
            result = self.eval_statement(stmt)?;
            if let Object::ReturnValue(_) = result {
                break;
            }
        }
        Ok(result)
    }

    fn eval_statement(&mut self, stmt: &Statement) -> Result<Object, String> {
        match stmt {
            Statement::ExpressionStatement(expr) => self.eval_expression(expr),
            Statement::ReturnStatement(expr) => {
                let ret = self.eval_expression(expr)?;
                Ok(Object::ReturnValue(Box::new(ret)))
            }
            Statement::LetStatement {
                ident: Token::Ident(id),
                expr,
            } => {
                let val = self.eval_expression(expr)?;
                self.env.set(id, &val);
                Ok(Object::Null)
            }
            _ => Err("wot".to_string()),
        }
    }

    fn eval_expression(&mut self, expr: &Expression) -> Result<Object, String> {
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
            Expression::IfExpression {
                condition,
                consequence,
                alternative,
            } => self.eval_if_expr(condition, consequence, alternative),
            Expression::Ident(id) => self.eval_identifier(id),
            _ => Err("not implemented".to_string()),
        }
    }

    fn eval_identifier(&self, id: &str) -> Result<Object, String> {
        match self.env.get(id) {
            Some(obj) => Ok(obj),
            None => Err(format!("identifier {:?} not found", id)),
        }
    }

    fn is_truthy(&self, obj: &Object) -> bool {
        match obj {
            Object::Boolean(b) => *b,
            Object::Null => false,
            _ => true,
        }
    }

    fn eval_if_expr(
        &mut self,
        condition: &Expression,
        consequence: &[Statement],
        alternative: &[Statement],
    ) -> Result<Object, String> {
        let cond_eval = self.eval_expression(condition)?;
        if self.is_truthy(&cond_eval) {
            self.eval_blockstatement(consequence)
        } else {
            self.eval_blockstatement(alternative)
        }
    }

    fn eval_infix_expr(&self, op: &Token, left: &Object, right: &Object) -> Result<Object, String> {
        match op {
            Token::Plus => match (left, right) {
                (Object::Integer(l), Object::Integer(r)) => Ok(Object::Integer(l + r)),
                _ => Err(format!("type mismatch {:?} {:?} {:?}", left, right, op)),
            },
            Token::Minus => match (left, right) {
                (Object::Integer(l), Object::Integer(r)) => Ok(Object::Integer(l - r)),
                _ => Err(format!("type mismatch {:?} {:?} {:?}", left, right, op)),
            },
            Token::Asterisk => match (left, right) {
                (Object::Integer(l), Object::Integer(r)) => Ok(Object::Integer(l * r)),
                _ => Err(format!("type mismatch {:?} {:?} {:?}", left, right, op)),
            },
            Token::Slash => match (left, right) {
                (Object::Integer(l), Object::Integer(r)) => Ok(Object::Integer(l / r)),
                _ => Err(format!("type mismatch {:?} {:?} {:?}", left, right, op)),
            },
            Token::Equal => Ok(Object::Boolean(left == right)),
            Token::NotEqual => Ok(Object::Boolean(left != right)),
            Token::LessThan => Ok(Object::Boolean(left < right)),
            Token::GreaterThan => Ok(Object::Boolean(left > right)),
            _ => Err(format!("unknown infix operator {:?}", op)),
        }
    }

    fn eval_prefix_expr(&self, op: &Token, right: &Object) -> Result<Object, String> {
        match op {
            Token::Bang => self.eval_bang_prefix(right),
            Token::Minus => self.eval_minus_prefix(right),
            _ => Err(format!("unknown infix operator {:?}", op)),
        }
    }

    fn eval_minus_prefix(&self, right: &Object) -> Result<Object, String> {
        match right {
            Object::Integer(i) => Ok(Object::Integer(-i)),
            _ => Ok(Object::Null),
        }
    }

    fn eval_bang_prefix(&self, right: &Object) -> Result<Object, String> {
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
        let mut evaluator = Evaluator::new();
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

    #[test]
    fn test_if_else() {
        eval_compare("if (true) { 32 }", Object::Integer(32));
        eval_compare("if (false) { 32 }", Object::Null);
        eval_compare("if (1) { 32 }", Object::Integer(32));
        eval_compare("if (32 < 1024) { 32 }", Object::Integer(32));
        eval_compare("if (32 > 1024) { 32 }", Object::Null);
        eval_compare("if (32 < 1024) { 32 } else { 64 }", Object::Integer(32));
        eval_compare("if (32 > 1024) { 32 } else { 64 }", Object::Integer(64));
    }

    #[test]
    fn test_return_statements() {
        eval_compare("return 32;", Object::Integer(32));
        eval_compare("return 32; 16", Object::Integer(32));
        eval_compare("return 2 + 2 * 4;", Object::Integer(10));
        eval_compare("8; return 2 + 2 * 4;", Object::Integer(10));
        eval_compare(
            "if (true) { if (true) { return 32; } return 64; }",
            Object::Integer(32),
        );
    }

    #[test]
    fn test_let_statements() {
        eval_compare("let a = 32; a", Object::Integer(32));
        eval_compare("let a = 2 * 8; a", Object::Integer(16));
        eval_compare("let a = 8; let b = a * 2; b", Object::Integer(16));
    }
}
