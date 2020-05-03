use super::{builtins, Object};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(PartialEq, Debug, Clone, Eq)]
pub struct Environment {
    store: HashMap<String, Object>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Environment {
        let mut store = HashMap::new();
        Self::populate_with_builtins(&mut store);

        Environment {
            store,
            parent: None,
        }
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Environment {
        let mut env = Environment::new();
        env.parent = Some(outer);
        env
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.store.get(name) {
            Some(obj) => Some(obj.clone()),
            None => match self.parent {
                Some(ref parent_env) => parent_env.borrow().get(name),
                None => None,
            },
        }
    }

    pub fn set(&mut self, name: &str, obj: &Object) {
        self.store.insert(name.to_string(), obj.clone());
    }

    fn populate_with_builtins(store: &mut HashMap<String, Object>) {
        for (name, func) in builtins::get_builtins() {
            store.insert(name, func);
        }
    }
}
