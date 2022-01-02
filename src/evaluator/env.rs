use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use crate::evaluator::object::Object;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Environment {
    pub store: HashMap<String, Rc<Object>>, 
    pub outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment{store: HashMap::new(), outer: None}
    }

    pub fn new_enclosed(env: Rc<RefCell<Environment>>) -> Environment {
        Environment{store: HashMap::new(), outer: Some(Rc::clone(&env))}
    }

    pub fn get(&self, name: &str) -> Option<Rc<Object>> {
        match self.store.get(name) {
            Some(obj) => Some(Rc::clone(obj)),
            None => {
                match &self.outer {
                    Some(outer_env) => outer_env.borrow_mut().get(name),
                    None => None,
                }
            }
        }
    }

    pub fn set(&mut self, name: String, obj: Rc<Object>) {
        self.store.insert(name, obj);
    }
}