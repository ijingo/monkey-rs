use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use crate::ast;
use crate::evaluator::env::Environment;
use crate::evaluator::builtin::Builtin;

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    String(String),
    ReturnValue(Rc<Return>),
    Function(Rc<Function>),
    Array(Rc<Array>),
    Hash(Rc<MonkeyHash>),
    Builtin(Builtin),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(i) => i.to_string(),
            Object::Boolean(b) => b.to_string(),
            Object::Null => String::from("null"),
            Object::String(s) => s.to_owned(),
            Object::ReturnValue(r) => r.value.inspect(),
            Object::Function(f) => f.inspect(),
            Object::Array(a) => a.inspect(),
            Object::Hash(h) => h.inspect(),
            Object::Builtin(b) => b.inspect(),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.inspect())
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Return {
    pub value: Rc<Object>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Function {
    pub parameters: Vec<ast::IdentifierExpression>,
    pub body: ast::BlockStatement,
    pub env: Rc<RefCell<Environment>>,
}

impl Hash for Function {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("Hash for Function not implemeneted.");
    }
}

impl Function {
    fn inspect(&self) -> String {
        format!(
            "fn({}) {{\n{}\n}}",
            self.parameters.iter().map(|p| p.to_string()).collect::<Vec<String>>().join(", "), 
            self.body.to_string()
        )
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Array {
    pub elements: Vec<Rc<Object>>,
}

impl Hash for Array {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("Hash for Array not implemeneted.");
    }
}

impl Array {
    fn inspect(&self) -> String {
        format!("[{}]",
            self.elements.iter().map(|e| e.to_string()).collect::<Vec<String>>().join(", ")
        )
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MonkeyHash {
    pub pairs: HashMap<Rc<Object>, Rc<Object>>,
}

impl Hash for MonkeyHash {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("Hash for MonkeyHash not implimeneted.");
    }
}

impl MonkeyHash {
    fn inspect(&self) -> String {
        format!("{{{}}}",
            self.pairs
                .iter()
                .map(|(k, v)| format!("{}: {}", k.inspect(), v.inspect()))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}