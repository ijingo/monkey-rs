use std::fmt;

#[derive(Debug, Clone)]
pub struct EvalError(String);

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for EvalError {
    fn description(&self) -> &str {
        &self.0
    }
}

impl EvalError {
    pub fn new(msg: String) -> Self {
        EvalError(msg)
    }
}
