#![allow(dead_code)]
mod object;
use crate::ast::{Program, BlockStatement, Statement, Expression};
use object::*;

pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {}
    }

    pub fn eval_program(&self, program: &Program) -> Result<Object, &str> {
        self.eval_blockstatement(program)
    }

    fn eval_blockstatement(&self, blockstmt: &BlockStatement) -> Result<Object, &str> {
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
            _ => Err("not implemented"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use super::{Evaluator, Object};

    fn eval_compare(input: &str, object: Object) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        let evaluator = Evaluator::new();
        let eval = evaluator.eval_program(&program).unwrap();
        assert_eq!(eval, object);
    }

    #[test]
    fn test_eval_integer_expression() {
        eval_compare("32", Object::Integer(32));
        eval_compare("1024", Object::Integer(1024));
    }
}
