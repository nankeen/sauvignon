use super::{Environment, Token};
use crate::ast::BlockStatement;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    ReturnValue(Box<Object>),
    Function(Vec<Token>, BlockStatement, Rc<RefCell<Environment>>),
    Builtin(BuiltinFunction),
    String(String),
    Null,
}

pub type BuiltinFunction = fn(Vec<Object>) -> Result<Object, String>;
