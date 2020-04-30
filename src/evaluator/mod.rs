mod builtins;
mod environment;
mod object;
use crate::lexer::Token;
use crate::parser::{Expression, Statement};
use environment::*;
use object::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Evaluator {
    env: Rc<RefCell<Environment>>,
}

impl Evaluator {
    /// Creates a new evaluator with a fresh environment
    pub fn new() -> Evaluator {
        Evaluator {
            env: Rc::new(RefCell::new(Environment::new())),
        }
    }

    /// Evaluates a `Program`, returns the result of the last statement.
    /// This functions unwraps `Object::ReturnValue`.
    ///
    /// # Arguments
    ///
    /// * `program` - Program to be evaluated
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

    /// Evaluates a `BlockStatement`, returns the result of the last statement.
    /// This functions does not unwrap `Object::ReturnValue`.
    ///
    /// # Arguments
    ///
    /// * `blockstmt` - BlockStatement to be evaluated
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

    /// Evaluates a single `Statement`
    ///
    /// # Arguments
    ///
    /// * `stmt` - Statement to be evaluated
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
                self.env.borrow_mut().set(id, &val);
                Ok(Object::Null)
            }
            _ => Err("wot".to_string()),
        }
    }

    /// Evaluates an `Expression`
    ///
    /// # Arguments
    ///
    /// * `expr` - Expression to be evaluated
    fn eval_expression(&mut self, expr: &Expression) -> Result<Object, String> {
        match expr {
            Expression::IntLiteral(i) => Ok(Object::Integer(*i)),
            Expression::BoolLiteral(b) => Ok(Object::Boolean(*b)),
            Expression::StringLiteral(s) => Ok(Object::String(s.clone())),
            Expression::ArrayLiteral(elems) => self.eval_array_expr(elems),
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
            Expression::FunctionLiteral { parameters, body } => Ok(Object::Function(
                parameters.clone(),
                body.clone(),
                Rc::clone(&self.env),
            )),
            Expression::Call {
                function,
                arguments,
            } => self.eval_call(function, arguments),
            Expression::Ident(id) => self.eval_identifier(id),
            Expression::IndexExpression { left, index } => self.eval_index_expr(left, index),
            Expression::HashLiteral(pairs) => self.eval_hash_expr(pairs),
        }
    }

    /// Evaluates hash expressions
    ///
    /// # Arguments
    ///
    /// * `pairs` - Vector of key value pairs (Expression, Expression):w
    fn eval_hash_expr(&mut self, pairs: &[(Expression, Expression)]) -> Result<Object, String> {
        let mut hashmap = HashMap::new();
        for (k_expr, v_expr) in pairs {
            let k = self.eval_expression(k_expr)?;
            let v = self.eval_expression(v_expr)?;
            hashmap.insert(k, v);
        }
        Ok(Object::Hash(hashmap))
    }

    /// Evaluates an index expression used by arrays and hashes
    ///
    /// # Arguments
    ///
    /// * `left_expr` - Expression to the left of `[...`
    /// * `index_expr` - Expression within the `[...]` brackets
    fn eval_index_expr(
        &mut self,
        left_expr: &Expression,
        index_expr: &Expression,
    ) -> Result<Object, String> {
        let left = self.eval_expression(left_expr)?;
        let index = self.eval_expression(index_expr)?;
        match (&left, &index) {
            (Object::Array(elems), Object::Integer(idx)) => {
                if *idx < 0 || *idx as usize > elems.len() - 1 {
                    return Ok(Object::Null);
                }
                Ok(elems[*idx as usize].clone())
            }
            (Object::Hash(hash), idx) => Ok(hash[idx].clone()),
            (_, _) => Err(format!("{:?} cannot be indexed by {:?}", left, index)),
        }
    }

    fn eval_array_expr(&mut self, elems_expr: &[Expression]) -> Result<Object, String> {
        let elems = elems_expr
            .iter()
            .map(|expr| self.eval_expression(expr).unwrap())
            .collect::<Vec<_>>();
        Ok(Object::Array(elems))
    }

    fn eval_call(&mut self, func: &Expression, args: &[Expression]) -> Result<Object, String> {
        let func_obj = self.eval_expression(func)?;
        match func_obj {
            Object::Function(parameters, body, f_env) => {
                self.eval_fn_call(args, &parameters, &body, f_env)
            }
            Object::Builtin(func) => self.eval_builtincall(args, func),
            _ => Err(format!(
                "expected function type for call, got {:?} instead",
                func_obj
            )),
        }
    }

    fn eval_builtincall(
        &mut self,
        args: &[Expression],
        func: BuiltinFunction,
    ) -> Result<Object, String> {
        let args_objs = args
            .iter()
            .map(|expr| self.eval_expression(expr).unwrap())
            .collect::<Vec<_>>();
        func(args_objs)
    }

    fn eval_fn_call(
        &mut self,
        args: &[Expression],
        params: &[Token],
        body: &[Statement],
        f_env: Rc<RefCell<Environment>>,
    ) -> Result<Object, String> {
        if args.len() != params.len() {
            Err(format!(
                "expected {:?} arguments, got {:?} instead",
                args.len(),
                params.len()
            ))
        } else {
            let args_objs = args
                .iter()
                .map(|expr| self.eval_expression(expr).unwrap())
                .collect::<Vec<_>>();
            let saved_env = Rc::clone(&self.env);
            let mut new_env = Environment::new_enclosed(Rc::clone(&f_env));
            for (ident, obj) in params.iter().zip(args_objs.iter()) {
                match ident {
                    Token::Ident(name) => new_env.set(name, obj),
                    _ => {
                        return Err(format!(
                            "expected identifier for parameter got type {:?}",
                            ident
                        ))
                    }
                }
            }
            self.env = Rc::new(RefCell::new(new_env));
            let result = self.eval_blockstatement(body)?;
            self.env = saved_env;
            if let Object::ReturnValue(val) = result {
                return Ok(*val);
            }
            Ok(result)
        }
    }

    fn eval_identifier(&self, id: &str) -> Result<Object, String> {
        match self.env.borrow().get(id) {
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
                (Object::String(l), Object::String(r)) => Ok(Object::String(l.clone() + r)),
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
            Token::LessThan => match (left, right) {
                (Object::Integer(l), Object::Integer(r)) => Ok(Object::Boolean(l < r)),
                _ => Err(format!("type mismatch {:?} {:?} {:?}", left, right, op)),
            },
            Token::GreaterThan => match (left, right) {
                (Object::Integer(l), Object::Integer(r)) => Ok(Object::Boolean(l > r)),
                _ => Err(format!("type mismatch {:?} {:?} {:?}", left, right, op)),
            },
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
    use super::{Evaluator, Expression, Object, Statement, Token};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use std::collections::HashMap;

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
        eval_compare("正", Object::Boolean(true));
        eval_compare("負", Object::Boolean(false));
    }

    #[test]
    fn test_eval_prefix() {
        eval_compare("不正", Object::Boolean(false));
        eval_compare("不負", Object::Boolean(true));
        eval_compare("不不正", Object::Boolean(true));
        eval_compare("不不負", Object::Boolean(false));
        eval_compare("不5", Object::Boolean(false));
        eval_compare("不不5", Object::Boolean(true));

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
        eval_compare("正 當同 正", Object::Boolean(true));
        eval_compare("正 不同 負", Object::Boolean(true));
        eval_compare("正 不同 負 當同 負", Object::Boolean(false));

        eval_compare("32 當同 32", Object::Boolean(true));
        eval_compare("32 不同 32", Object::Boolean(false));
        eval_compare("8 少於 16", Object::Boolean(true));
        eval_compare("8 大於 16", Object::Boolean(false));
        eval_compare("2 大於 16 當同 負", Object::Boolean(true));
    }

    #[test]
    fn test_if_else() {
        eval_compare("如 (正) 始 32 終", Object::Integer(32));
        eval_compare("如 (負) 始 32 終", Object::Null);
        eval_compare("如 (1) 始 32 終", Object::Integer(32));
        eval_compare("如 (32 少於 1024) 始 32 終", Object::Integer(32));
        eval_compare("如 (32 大於 1024) 始 32 終", Object::Null);
        eval_compare(
            "如 (32 少於 1024) 始 32 終 否則 始 64 終",
            Object::Integer(32),
        );
        eval_compare(
            "如 (32 大於 1024) 始 32 終 否則 始 64 終",
            Object::Integer(64),
        );
    }

    #[test]
    fn test_return_statements() {
        eval_compare("歸 32;", Object::Integer(32));
        eval_compare("歸 32; 16", Object::Integer(32));
        eval_compare("歸 2 + 2 * 4;", Object::Integer(10));
        eval_compare("8; 歸 2 + 2 * 4;", Object::Integer(10));
        eval_compare(
            "如 (正) 始 如 (正) 始 歸 32; 終 歸 64; 終",
            Object::Integer(32),
        );
    }

    #[test]
    fn test_let_statements() {
        eval_compare("讓 a 當 32; a", Object::Integer(32));
        eval_compare("讓 a 當 2 * 8; a", Object::Integer(16));
        eval_compare("讓 a 當 8; 讓 b 當 a * 2; b", Object::Integer(16));
    }

    #[test]
    fn test_function() {
        let input = "功能(x) 始 x + 2; 終;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        let mut evaluator = Evaluator::new();
        let eval = evaluator.eval_program(&program).unwrap();
        let test_case = Object::Function(
            vec![Token::Ident("x".to_string())],
            vec![Statement::ExpressionStatement(
                Expression::InfixExpression {
                    left: Box::new(Expression::Ident("x".to_string())),
                    operator: Token::Plus,
                    right: Box::new(Expression::IntLiteral(2)),
                },
            )],
            evaluator.env,
        );
        assert_eq!(eval, test_case);
        eval_compare("讓 a 當 功能(x) 始 x; 終; a(32);", Object::Integer(32));
        eval_compare("讓 a 當 功能(x) 始 歸 x; 終; a(32);", Object::Integer(32));
        eval_compare("讓 a 當 功能(x) 始 x * 2; 終; a(32);", Object::Integer(64));
        eval_compare(
            "讓 a 當 功能(x, y) 始 x + y; 終; a(8, 16);",
            Object::Integer(24),
        );
        eval_compare(
            "讓 a 當 功能(x, y) 始 x + y; 終; a(4 * 2, a(14, 2));",
            Object::Integer(24),
        );

        eval_compare("功能(x) 始 x; 終(32);", Object::Integer(32));
    }

    #[test]
    fn test_eval_string() {
        eval_compare("\"cheesecake\"", Object::String("cheesecake".to_string()));
    }

    #[test]
    fn test_eval_string_concat() {
        eval_compare(
            "\"cheesecake\" + \" tastes good\"",
            Object::String("cheesecake tastes good".to_string()),
        );
    }

    #[test]
    fn test_eval_recursion() {
        eval_compare(
            "let fib = fn(n) {
            if (n < 3) {
                return 1;
            }
            return fib(n-1) + fib(n-2);
        };
        fib(4);
        ",
            Object::Integer(3),
        );
    }

    #[test]
    fn test_eval_builtinfunctions() {
        eval_compare("len(\"test\")", Object::Integer(4));
        eval_compare("len(\"\")", Object::Integer(0));
    }

    #[test]
    fn test_eval_array() {
        eval_compare(
            "[1, 2 * 2, 3 + 3]",
            Object::Array(vec![
                Object::Integer(1),
                Object::Integer(4),
                Object::Integer(6),
            ]),
        )
    }

    #[test]
    fn test_eval_index() {
        eval_compare("[0, 1, 2, 3, 4][1]", Object::Integer(1));
        eval_compare("[0, 1, 2, 3, 4][2+1]", Object::Integer(3));
    }

    #[test]
    fn test_eval_hash() {
        let mut kv_pairs = HashMap::new();
        kv_pairs.insert(Object::String("hello".to_string()), Object::Integer(2));
        kv_pairs.insert(
            Object::String("cheese".to_string()),
            Object::String("cake".to_string()),
        );
        eval_compare(
            "{\"hello\": 2, \"cheese\": \"cake\"}",
            Object::Hash(kv_pairs),
        );
        // Test hash lookup
        eval_compare(
            "{\"hello\": 2, \"cheese\": 4}[\"hello\"]",
            Object::Integer(2),
        );
    }
}
