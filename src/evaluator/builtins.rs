use super::{BuiltinFunction, EvalError, Object};

/// Returns an array of `(String, Object)` pairs indication function name and function object
pub fn get_builtins() -> Vec<(String, Object)> {
    vec![new_builtin("len", funcs::len)]
}

/// Helper function to create tuple pair of `(String, Object)`
fn new_builtin(name: &str, func: BuiltinFunction) -> (String, Object) {
    (name.to_string(), Object::Builtin(func))
}

/// Module containing built in functions
mod funcs {
    use super::{EvalError, Object};

    pub fn len(args: Vec<Object>) -> Result<Object, EvalError> {
        if args.len() != 1 {
            return Err(EvalError::BadArgumentLength(1, args.len()));
        }
        match &args[0] {
            Object::String(s) => Ok(Object::Integer(s.len() as i64)),
            _ => Err(EvalError::RuntimeError(format!(
                "length of type {:?} cannot be determined",
                args[0]
            ))),
        }
    }
}
