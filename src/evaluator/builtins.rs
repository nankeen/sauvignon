use super::{BuiltinFunction, Object};

pub fn get_builtins() -> Vec<(String, Object)> {
    vec![new_builtin("len", funcs::len)]
}

fn new_builtin(name: &str, func: BuiltinFunction) -> (String, Object) {
    (name.to_string(), Object::Builtin(func))
}

mod funcs {
    use super::Object;

    pub fn len(args: Vec<Object>) -> Result<Object, String> {
        if args.len() != 1 {
            return Err(format!(
                "expected 1 argument for len(), got {:?} instead",
                args.len()
            ));
        }
        match &args[0] {
            Object::String(s) => Ok(Object::Integer(s.len() as i64)),
            _ => Err(format!("length of type {:?} can't be determined", args[0])),
        }
    }
}