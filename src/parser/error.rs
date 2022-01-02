use std::fmt;

pub type ParseErrors = Vec<ParseError>;

#[derive(Debug, Clone)]
pub struct ParseError(String);

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl ParseError {
    pub fn new(msg: String) -> Self {
        ParseError(msg)
    }
}
