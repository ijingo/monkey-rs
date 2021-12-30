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
            Token::Ident(name) => write!(f, "{}", name),
            Token::StringLiteral(val) => write!(f, "{}", val),
            Token::IntLiteral(val) => write!(f, "{}", val),
            Token::BoolLiteral(val) => write!(f, "{}", val),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Multiply => write!(f, "*"),
            Token::Divide => write!(f, "/"),
            Token::Equal => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::GreaterThan => write!(f, ">"),
            Token::LessThan => write!(f, "<"),
            Token::Bang => write!(f, "!"),
            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::SemiColon => write!(f, ";"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::Assign => write!(f, "="),
            Token::Function => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::Return => write!(f, "return"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::EOF => write!(f, "EOF"),
            Token::Illegal => write!(f, "Illegal"),
        }
    }

}