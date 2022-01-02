use crate::evaluator::object::Object;

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum Builtin {
    Len,
    Puts,
    First,
    Last,
    Rest,
    Push,
}

impl Builtin {
    pub fn inspect(&self) -> String {
        String::from("")
    }

    pub fn lookup(name: &str) -> Option<Object> {
        None
    }
}