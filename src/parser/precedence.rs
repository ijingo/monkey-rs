use crate::lexer::token::Token;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,     //  == or !=
    LessGreater, // > or <
    Sum,         // + or -
    Product,     // * or /
    Prefix,      // -X or !X
    Call,        // Function call
}

impl Precedence {
    pub fn for_token(token: &Token) -> Precedence {
        match token {
            Token::Multiply | Token::Divide => Precedence::Product,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::LessThan | Token::GreaterThan => Precedence::LessGreater,
            Token::Equal | Token::NotEqual => Precedence::Equals,
            Token::LParen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }
}

