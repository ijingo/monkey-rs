pub mod token;

use token::Token;
use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl <'a> Lexer<'a> {

    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input.chars().peekable(),
        }
    }

    fn read_char(&mut self) -> Option<char> {
        self.input.next()
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn skip_whitespaces(&mut self) {
        while let Some(ch) = self.input.peek() {
            if ch.is_whitespace() {
                self.read_char();
            } else {
                break;
            }
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespaces();
        match self.read_char() {
            Some('+') => Token::Plus,
            Some('-') => Token::Minus,
            Some('*') => Token::Multiply,
            Some('/') => Token::Divide,
            Some('=') => {
                if let Some('=') = self.peek_char() {
                    self.read_char();
                    Token::Equal
                } else {
                    Token::Assign
                }
            },
            Some('!') => {
                if let Some('=') = self.peek_char() {
                    self.read_char();
                    Token::NotEqual
                } else {
                    Token::Bang
                }
            },
            Some('>') => Token::GreaterThan,
            Some('<') => Token::LessThan,
            Some(',') => Token::Comma,
            Some(':') => Token::Colon,
            Some(';') => Token::SemiColon,
            Some('(') => Token::LParen,
            Some(')') => Token::RParen,
            Some('{') => Token::LBrace,
            Some('}') => Token::RBrace,
            Some('[') => Token::LBracket,
            Some(']') => Token::RBracket,
            Some(ch) => {
                if Self::is_letter(ch) {
                    let ident = self.read_identifier(ch);
                    Token::lookup_ident(ident)
                } else if ch.is_digit(10) {
                    Token::IntLiteral(self.read_int(ch))
                } else if ch == '"' {
                    Token::StringLiteral(self.read_string())
                } else {
                    Token::Illegal
                }
            }
            None => Token::EOF,
        }
    }

    fn read_int(&mut self, ch: char) -> i64 {
        let mut str = String::new();
        str.push(ch);
        while let Some(&ch) = self.peek_char() {
            if ch.is_digit(10) {
                str.push(ch);
                self.read_char().unwrap();
            } else {
                break;
            }
        }
        str.parse().unwrap()
    }

    fn read_string(&mut self)  -> String {
        let mut str = String::new();
        while let Some(ch) = self.read_char() {
            if ch == '"' {
                return str
            }
            str.push(ch)
        }
        str
    }

    fn read_identifier(&mut self, ch: char) -> String {
        let mut ident = String::new();
        ident.push(ch);
        while let Some(&ch) = self.peek_char() {
            if Self::is_letter(ch) {
                ident.push(ch);
                self.read_char().unwrap();
            } else {
                break;
            }
        }
        ident
    }


    fn is_letter(ch: char) -> bool {
        ch.is_alphabetic() || ch == '_'
    }
}

#[cfg(test)]
mod test {
    use token::Token;
    use super::*;

    #[test]
    fn next_token() {
        let input = r#"let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);
!-/*5;
5 < 10 > 5;
if (5 < 10) {
    return true;
} else {
    return false;
}
10 == 10;
10 != 9;
"#;
        let tests = vec![
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::IntLiteral(5),
            Token::SemiColon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::IntLiteral(10),
            Token::SemiColon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::SemiColon,
            Token::RBrace,
            Token::SemiColon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::LParen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::RParen,
            Token::SemiColon,
            Token::Bang,
            Token::Minus,
            Token::Divide,
            Token::Multiply,
            Token::IntLiteral(5),
            Token::SemiColon,
            Token::IntLiteral(5),
            Token::LessThan,
            Token::IntLiteral(10),
            Token::GreaterThan,
            Token::IntLiteral(5),
            Token::SemiColon,
            Token::If,
            Token::LParen,
            Token::IntLiteral(5),
            Token::LessThan,
            Token::IntLiteral(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::BoolLiteral(true),
            Token::SemiColon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::BoolLiteral(false),
            Token::SemiColon,
            Token::RBrace,
            Token::IntLiteral(10),
            Token::Equal,
            Token::IntLiteral(10),
            Token::SemiColon,
            Token::IntLiteral(10),
            Token::NotEqual,
            Token::IntLiteral(9),
            Token::SemiColon,
            Token::EOF,
        ];

        let mut lexer = Lexer::new(input);
        for expect in tests {
            let tok = lexer.next_token();
            assert_eq!(expect, tok, "{} != {}", expect, tok);
        }
    }
}
