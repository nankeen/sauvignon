#![allow(clippy::derive_hash_xor_eq)]
use super::{Environment, Token};
use crate::parser::BlockStatement;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

/// `Object` enum represents an object instance with various types
#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Array(Vec<Object>),
    ReturnValue(Box<Object>),
    Function(Vec<Token>, BlockStatement, Rc<RefCell<Environment>>),
    Builtin(BuiltinFunction),
    String(String),
    Hash(HashMap<Object, Object>),
    Null,
}

/// `BuiltinFunction` type describes the signature all built in functions have
pub type BuiltinFunction = fn(Vec<Object>) -> Result<Object, String>;

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Object::Integer(ref i) => i.hash(state),
            Object::Boolean(ref b) => b.hash(state),
            Object::String(ref s) => s.hash(state),
            _ => "".hash(state),
        }
    }
}
