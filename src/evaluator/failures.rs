use super::Object;
use crate::lexer::Token;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum EvalError {
    StatementNotImplemented,
    IOFailed(std::io::Error),
    ModuleParseFailure(String),
    BadExpectedType(Object, Object),
    BadCallType(Object),
    BadArgumentLength(usize, usize),
    BadToken(Token, Token),
    NotFound(String),
    UnknownOperator(Token),
    RuntimeError(String),
    BadOperator(Token, Object),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalError::StatementNotImplemented => f.write_str("StatementNotImplemented"),
            EvalError::IOFailed(err) => write!(f, "IOFailed: ({})", err),
            EvalError::ModuleParseFailure(err) => write!(f, "ModuleParseFailure: ({})", err),
            EvalError::BadExpectedType(expected, got) => write!(
                f,
                "BadExpected: expected type {:?}, got {:?} instead",
                expected, got
            ),
            EvalError::BadCallType(func_obj) => write!(
                f,
                "BadCallType: expected function type for call, got {:?} instead",
                func_obj
            ),
            EvalError::BadArgumentLength(expected, got) => write!(
                f,
                "BadArgumentLength: expected {} arguments, got {} instead",
                expected, got
            ),
            EvalError::BadToken(expected, got) => write!(
                f,
                "BadToken: expected {:?}, got {:?} instead",
                expected, got
            ),
            EvalError::NotFound(id) => {
                write!(f, "NotFound: {:?} is not found in the environment", id)
            }
            EvalError::UnknownOperator(op) => {
                write!(f, "UnknownOperator: {:?} is not a known operator", op)
            }
            EvalError::RuntimeError(err) => write!(f, "RuntimeError: ({:?})", err),
            EvalError::BadOperator(op, obj) => {
                write!(f, "BadOperator: cannot apply {:?} on {:?}", op, obj)
            }
        }
    }
}

impl Error for EvalError {}

impl From<std::io::Error> for EvalError {
    fn from(err: std::io::Error) -> Self {
        Self::IOFailed(err)
    }
}