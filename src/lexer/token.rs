use std::fmt;

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum Token {
    Illegal,
    EOF,

    // identifier & literals
    Ident(String),
    StringLiteral(String),
    IntLiteral(i64),
    BoolLiteral(bool),

    // Operators
    Plus,              // +
    Minus,             // -
    Multiply,          // *
    Divide,            // /
    Equal,             // ==
    NotEqual,          // !=
    GreaterThan,       // >
    LessThan,          // <
    Bang,              // !

    // Delimiters
    Comma,             // ,
    Colon,             // :
    SemiColon,         // ;
    LParen,            // (
    RParen,            // )
    LBrace,            // {
    RBrace,            // }
    LBracket,          // [
    RBracket,          // ]

    // Statements
    Assign,            // =

    // keywords
    Function,          // "fn"
    Let,               // "let"
    Return,            // "return"
    If,                // "if"
    Else,              // "else"
}

impl Token {
    pub fn lookup_ident(ident: String) -> Self {
        match ident.as_str() {
            "fn" => Token::Function,
            "let" => Token::Let,
            "return" => Token::Return,
            "if" => Token::If,
            "else" => Token::Else,
            "true" => Token::BoolLiteral(true),
            "false" => Token::BoolLiteral(false),
            _ => Token::Ident(ident),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Ident(name) => write!(f, "Ident: `{}`", name),
            Token::StringLiteral(val) => write!(f, "StringLiteral: `{}`", val),
            Token::IntLiteral(val) => write!(f, "IntLiteral: `{}`", val),
            Token::BoolLiteral(val) => write!(f, "BoolLiteral: `{}`", val),
            Token::Plus => write!(f, "Plus: `+`"),
            Token::Minus => write!(f, "Minus: `-`"),
            Token::Multiply => write!(f, "Multiply: `*`"),
            Token::Divide => write!(f, "Divide: `/`"),
            Token::Equal => write!(f, "Equal: `==`"),
            Token::NotEqual => write!(f, "NotEqual: `!=`"),
            Token::GreaterThan => write!(f, "GreaterThan: `>`"),
            Token::LessThan => write!(f, "LessThan: `<`"),
            Token::Bang => write!(f, "Bang: `!`"),
            Token::Comma => write!(f, "Comma: `,`"),
            Token::Colon => write!(f, "Colon: `:`"),
            Token::SemiColon => write!(f, "SemiColon: `;`"),
            Token::LParen => write!(f, "LParen: `(`"),
            Token::RParen => write!(f, "RParen: `)`"),
            Token::LBrace => write!(f, "LBrace: `{{`"),
            Token::RBrace => write!(f, "RBrace: `}}`"),
            Token::LBracket => write!(f, "LBracket: `[`"),
            Token::RBracket => write!(f, "RBracket: `]`"),
            Token::Assign => write!(f, "Assign: `=`"),
            Token::Function => write!(f, "Function `fn`"),
            Token::Let => write!(f, "Let `let`"),
            Token::Return => write!(f, "Return `return`"),
            Token::If => write!(f, "If `if`"),
            Token::Else => write!(f, "Else `else`"),
            Token::EOF => write!(f, "EOF"),
            Token::Illegal => write!(f, "Illegal"),
        }
    }

}