use std::rc::Rc;

use crate::evaluator::object::{Object, Array};
use crate::evaluator::error::EvalError;

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum Builtin {
    Len,
    First,
    Last,
    Rest,
    Push,
    Puts,
}

impl Builtin {
    pub fn inspect(&self) -> String {
        match self {
            Builtin::Len => "len".to_string(),
            Builtin::First => "first".to_string(),
            Builtin::Last => "last".to_string(),
            Builtin::Rest => "rest".to_string(),
            Builtin::Push => "push".to_string(),
            Builtin::Puts => "puts".to_string(),
        }
    }

    pub fn lookup(name: &str) -> Option<Object> {
        match name {
            "len" => Some(Object::Builtin(Builtin::Len)),
            "first" => Some(Object::Builtin(Builtin::First)),
            "last" => Some(Object::Builtin(Builtin::Last)),
            "rest" => Some(Object::Builtin(Builtin::Rest)),
            "push" => Some(Object::Builtin(Builtin::Push)),
            "puts" => Some(Object::Builtin(Builtin::Puts)),
            _ => None,
        }
    }

    pub fn apply(&self, args: &[Rc<Object>]) -> Result<Rc<Object>, EvalError> {
        match self {
            Builtin::Len => {
                if args.len() != 1 {
                    return Err(EvalError::new(format!("Expected 1 argument, but got #args: {}", args.len())));
                }
                let arg = Rc::clone(args.first().unwrap());
                match &*arg {
                    Object::String(s) => Ok(Rc::new(Object::Integer(s.len() as i64))),
                    Object::Array(arr) => Ok(Rc::new(Object::Integer(arr.elements.len() as i64))),
                    obj => Err(EvalError::new(format!("Object {:?} not supported as an argument for len()", obj))),
                }
            },
            Builtin::First => {
                if args.len() != 1 {
                    return Err(EvalError::new(format!("Expected 1 argument, but got #args: {}", args.len())));
                }
                let arg = Rc::clone(args.first().unwrap());
                match &*arg {
                    Object::Array(arr) => {
                        match arr.elements.first() {
                            Some(el) => Ok(Rc::clone(el)),
                            None => Ok(Rc::new(Object::Null)),
                        }
                    }
                    obj => Err(EvalError::new(format!("Object {:?} not supported as an argument for first()", obj))),
                }
            },
            Builtin::Last => {
                if args.len() != 1 {
                    return Err(EvalError::new(format!("Expected 1 argument, but got #args: {}", args.len())));
                }
                let arg = Rc::clone(args.last().unwrap());
                match &*arg {
                    Object::Array(arr) => {
                        match arr.elements.last() {
                            Some(el) => Ok(Rc::clone(el)),
                            None => Ok(Rc::new(Object::Null)),
                        }
                    }
                    obj => Err(EvalError::new(format!("Object {:?} not supported as an argument for last()", obj))),
                }
            },
            Builtin::Rest => {
                if args.len() != 1 {
                    return Err(EvalError::new(format!("Expected 1 argument, but got #args: {}", args.len())));
                }
                let arg = Rc::clone(args.last().unwrap());
                match &*arg {
                    Object::Array(arr) => {
                        if arr.elements.len() <= 1 {
                            Ok(Rc::new(Object::Array(Rc::new(Array{elements: vec![]}))))
                        } else {
                            let mut elements = arr.elements.clone();
                            elements.remove(0);
                            Ok(Rc::new(Object::Array(Rc::new(Array{elements}))))
                        }
                    }
                    obj => Err(EvalError::new(format!("Object {:?} not supported as an argument for rest()", obj))),
                }
            },
            Builtin::Push => {
                if args.len() != 2 {
                    return Err(EvalError::new(format!("push() takes an array and an object")));
                }
                let array = Rc::clone(args.first().unwrap());
                let object = Rc::clone(args.last().unwrap());
                match &*array {
                    Object::Array(arr) => {
                        let mut elements = arr.elements.clone();
                        elements.push(object);
                        Ok(Rc::new(Object::Array(Rc::new(Array{elements}))))
                    },
                    obj => Err(EvalError::new(format!("Object {:?} not supported as an argument for push()", obj))),
                }
            },
            Builtin::Puts => {
                for arg in args {
                    println!("{}", arg.inspect())
                }
                Ok(Rc::new(Object::Null))
            },
        }
    }
}