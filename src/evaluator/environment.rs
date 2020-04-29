#![allow(dead_code)]
use super::Object;
use std::collections::HashMap;

pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.store.get(name) {
            Some(obj) => Some(obj.clone()),
            None => None,
        }
    }

    pub fn set(&mut self, name: &str, obj: &Object) {
        self.store.insert(name.to_string(), obj.clone());
    }
}
