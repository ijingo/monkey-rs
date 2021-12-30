pub mod precedence;
pub mod error;

use crate::lexer::{Lexer, token::*};
use crate::ast::*;
use crate::parser::error::*;
use crate::parser::precedence::*;

type PrefixFn = fn(parser: &mut Parser<'_>) -> Result<Expression, ParseError>;
type InfixFn = fn(parser: &mut Parser<'_>, left: Expression) -> Result<Expression, ParseError>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    peek_token: Token,
}

pub fn parse(input: &str) -> Result<Node, ParseErrors> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program()?;

    Ok(Node::Program(Box::new(program)))
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'_>) -> Parser<'_> {
        let mut lexer = lexer;
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Parser {
            lexer,
            current_token,
            peek_token,
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseErrors> {
        let mut program = Program::new();
        let mut errors = ParseErrors::new();
        let mut token = self.current_token.clone();

        while token != Token::EOF {
            match self.parse_statement() {
                Ok(stmt) => program.statements.push(stmt),
                Err(err) => errors.push(err),
            }
            self.next_token();
            token = self.current_token.clone();
        }

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(program)
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match &self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        let name = self.expect_ident()?;
        self.expect_peek_token(Token::Assign)?;
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;
        if self.check_peek_token_is(&Token::SemiColon) {
            self.next_token();
        }

        Ok(Statement::Let(Box::new(LetStatement{name, value})))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;
        if self.check_peek_token_is(&Token::SemiColon) {
            self.next_token();
        }
        Ok(Statement::Return(Box::new(ReturnStatement{value})))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expression = self.parse_expression(Precedence::Lowest)?;
        if self.check_peek_token_is(&Token::SemiColon) {
            self.next_token();
        }
        Ok(Statement::Expression(Box::new(ExpressionStatement{expression})))
    }

    fn check_current_token_is(&mut self, token: &Token) -> bool {
        match (token, &self.current_token) {
            (Token::Ident(_), Token::Ident(_)) => true,
            (Token::IntLiteral(_), Token::IntLiteral(_)) => true,
            _ => token == &self.current_token,
        }
    }

    fn check_peek_token_is(&mut self, token: &Token) -> bool {
        match (token, &self.peek_token) {
            (Token::Ident(_), Token::Ident(_)) => true,
            (Token::IntLiteral(_), Token::IntLiteral(_)) => true,
            _ => token == &self.peek_token,
        }
    }

    fn expect_peek_token(&mut self, token: Token) -> Result<(), ParseError> {
        if self.check_peek_token_is(&token) {
            self.next_token();
            Ok(())
        } else {
            Err(ParseError::new(format!("Unexpected next token: {}. Expect: {}", self.peek_token, token)))
        }
    }

    fn expect_ident(&mut self) -> Result<Identifier, ParseError> {
        match self.peek_token.clone() {
            Token::Ident(ident) => {
                self.next_token();
                Ok(ident)
            },
            _ => Err(ParseError::new(format!("Invalid identifier: {}", self.peek_token))),
        }
    }

    fn parse_expression(&mut self, left_binding_power: Precedence) -> Result<Expression, ParseError> {
        let mut left_expr;
        if let Some(prefix_fn) = Parser::prefix_fn(&self.current_token) {
            left_expr = prefix_fn(self)?;
        } else {
            return Err(ParseError::new(format!("No prefix parser function for {} registered.", self.current_token)));
        }

        while !self.check_peek_token_is(&Token::SemiColon) && left_binding_power < self.peek_right_binding_power() {
            if let Some(infix_fn) = Parser::infix_fn(&self.peek_token) {
                self.next_token();
                left_expr = infix_fn(self, left_expr)?;
            } else {
                return Ok(left_expr);
            }
        }

        Ok(left_expr)
    } 

    fn parse_expression_list(&mut self, stop_token: Token) -> Result<Vec<Expression>, ParseError> {
        let mut res: Vec<Expression> = Vec::new();
        if self.check_peek_token_is(&stop_token) {
            self.next_token();
            return Ok(res);
        }
        self.next_token();
        res.push(self.parse_expression(Precedence::Lowest)?);
        while self.check_peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            res.push(self.parse_expression(Precedence::Lowest)?);
        }
        self.expect_peek_token(stop_token)?;
        Ok(res)
    }

    fn current_right_binding_power(&mut self) -> Precedence {
        Precedence::for_token(&self.current_token)
    }

    fn peek_right_binding_power(&mut self) -> Precedence {
        Precedence::for_token(&self.peek_token)
    }
}

// expresion parser: prefix functions
impl<'a> Parser<'a> {
  fn prefix_fn(token: &Token) -> Option<PrefixFn> {
        match token {
            Token::Ident(_) => Some(Parser::parse_identifier),
            Token::IntLiteral(_) => Some(Parser::parse_integer_literal),
            Token::StringLiteral(_) => Some(Parser::parse_string_literal),
            Token::BoolLiteral(_) => Some(Parser::parse_boolean_literal),
            Token::Bang | Token::Minus => Some(Parser::parse_prefix_expression),
            Token::LParen => Some(Parser::parse_grouped_expression),
            Token::If => Some(Parser::parse_if_expression),
            Token::Function => Some(Parser::parse_function_literal),
            Token::LBracket => Some(Parser::parse_array_literal),
            Token::LBrace => Some(Parser::parse_hash_literal),
            _ => None,
        }
    }

    fn parse_identifier(parser: &mut Parser<'_>) -> Result<Expression, ParseError> {
        if let Token::Ident(name) = &parser.current_token {
            Ok(Expression::Identifier(name.to_owned()))
        } else {
            Err(ParseError::new(format!("Error on parsing identifier {}", parser.current_token)))
        }
    }

    fn parse_integer_literal(parser: &mut Parser<'_>) -> Result<Expression, ParseError> {
        if let Token::IntLiteral(value) = parser.current_token {
            Ok(Expression::Integer(value))
        } else {
            Err(ParseError::new(format!("Error on parsing integer {}", parser.current_token)))
        }
    }

    fn parse_string_literal(parser: &mut Parser<'_>) -> Result<Expression, ParseError> {
        if let Token::StringLiteral(value) = &parser.current_token {
            Ok(Expression::String(value.to_owned()))
        } else {
            Err(ParseError::new(format!("Error on parsing string {}", parser.current_token)))
        }
    }

    fn parse_boolean_literal(parser: &mut Parser<'_>) -> Result<Expression, ParseError> {
        if let Token::BoolLiteral(value) = parser.current_token {
            Ok(Expression::Boolean(value))
        } else {
            Err(ParseError::new(format!("Error on parsing boolean {}", parser.current_token)))
        }
    }

    fn parse_prefix_expression(parser: &mut Parser<'_>) -> Result<Expression, ParseError> {
        let operator = parser.current_token.clone();
        parser.next_token();
        let right = parser.parse_expression(Precedence::Prefix)?;
        Ok(Expression::Prefix(Box::new(PrefixExpression{operator, right})))
    }

    fn parse_grouped_expression(parser: &mut Parser<'_>) -> Result<Expression, ParseError> {
        parser.next_token();
        let expr = parser.parse_expression(Precedence::Lowest)?;
        parser.expect_peek_token(Token::RParen)?;
        Ok(expr)
    }

    fn parse_if_expression(parser: &mut Parser<'_>) -> Result<Expression, ParseError> {
        parser.expect_peek_token(Token::LParen)?;
        parser.next_token();
        let condition = parser.parse_expression(Precedence::Lowest)?;
        parser.expect_peek_token(Token::RParen)?;
        parser.expect_peek_token(Token::LBrace)?;
        let consequence = Parser::parse_block_statement(parser)?;
        let alternative = if parser.check_peek_token_is(&Token::Else) {
            parser.next_token();
            parser.expect_peek_token(Token::LBrace)?;
            Some(Parser::parse_block_statement(parser)?)
        } else {
            None
        };
        Ok(Expression::If(Box::new(IfExpression{condition, consequence, alternative})))
    }

    fn parse_block_statement(parser: &mut Parser<'_>) -> Result<BlockStatement, ParseError> {
        let mut statements = Vec::new();
        parser.next_token();
        while !parser.check_current_token_is(&Token::RBrace) && !parser.check_current_token_is(&Token::EOF) {
            let stmt = parser.parse_statement()?;
            statements.push(stmt);
            parser.next_token();
        }
        Ok(BlockStatement{statements})
    }

    fn parse_function_literal(parser: &mut Parser<'_>) -> Result<Expression, ParseError> {
        parser.expect_peek_token(Token::LParen)?;
        let parameters = Parser::parse_function_parameters(parser)?;
        parser.expect_peek_token(Token::LBrace)?;
        let body = Parser::parse_block_statement(parser)?;
        Ok(Expression::Function(Box::new(FunctionLiteral{parameters,body})))
    }

    fn parse_function_parameters(parser: &mut Parser<'_>) -> Result<Vec<IdentifierExpression>, ParseError> {
        let mut identifiers: Vec<IdentifierExpression> = Vec::new();
        if parser.check_peek_token_is(&Token::RParen) {
            parser.next_token();
            return Ok(identifiers);
        }
        parser.next_token();
        identifiers.push(Parser::parse_identifier_expression(parser)?);
        while parser.check_peek_token_is(&Token::Comma) {
            parser.next_token();
            parser.next_token();
            identifiers.push(Parser::parse_identifier_expression(parser)?);
        }
        parser.expect_peek_token(Token::RParen)?;
        Ok(identifiers)
    }

    fn parse_identifier_expression(parser: &mut Parser<'_>) -> Result<IdentifierExpression, ParseError> {
        if let Token::Ident(name) = &parser.current_token {
            return Ok(IdentifierExpression{name: name.to_owned()});
        }
        Err(ParseError::new(format!("Error on parsing identifier expression with {}", parser.current_token)))
    }

    fn parse_array_literal(parser: &mut Parser<'_>) -> Result<Expression, ParseError> {
        let items = parser.parse_expression_list(Token::RBracket)?;
        Ok(Expression::Array(items))
    }

    fn parse_hash_literal(parser: &mut Parser<'_>) -> Result<Expression, ParseError> {
        let mut pairs: Vec<(Expression, Expression)> = Vec::new();

        while !parser.check_peek_token_is(&Token::RBrace) {
            parser.next_token();
            let key = parser.parse_expression(Precedence::Lowest)?;

            parser.expect_peek_token(Token::Colon)?;
            parser.next_token();
            let value = parser.parse_expression(Precedence::Lowest)?;

            pairs.push((key, value));

            if !parser.check_peek_token_is(&Token::RBrace) {
                parser.expect_peek_token(Token::Comma)?;
            }
        }

        parser.expect_peek_token(Token::RBrace)?;

        Ok(Expression::Hash(pairs))
    }
}

// expresion parser: infix functions
impl<'a> Parser<'a> {
    fn infix_fn(token: &Token) -> Option<InfixFn> {
        match token {
            Token::Plus
             | Token::Minus
             | Token::Multiply
             | Token::Divide
             | Token::Equal
             | Token::NotEqual
             | Token::LessThan
             | Token::GreaterThan => Some(Parser::parse_infix_expression),
            Token::LParen => Some(Parser::parse_call_expression),
            Token::LBracket => Some(Parser::parse_index_expression),
            _ => None,
        }
    }

    fn parse_infix_expression(parser: &mut Parser<'_>, left: Expression) -> Result<Expression, ParseError> {
        let operator = parser.current_token.clone();
        let binding_power = parser.current_right_binding_power();
        parser.next_token();
        let right = parser.parse_expression(binding_power)?;
        Ok(Expression::Infix(Box::new(InfixExpression{operator, left, right})))
    }

    fn parse_call_expression(parser: &mut Parser<'_>, function: Expression) -> Result<Expression, ParseError> {
        let arguments = parser.parse_expression_list(Token::RParen)?;
        Ok(Expression::Call(Box::new(CallExpression{function, arguments})))
    }

    fn parse_index_expression(parser: &mut Parser<'_>, left: Expression) -> Result<Expression, ParseError> {
        parser.next_token();
        let expr = Expression::Index(Box::new(left), Box::new(parser.parse_expression(Precedence::Lowest)?));
        parser.expect_peek_token(Token::RBracket)?;
        Ok(expr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn apply_test(test_case: &[(&str, &str)]) {
        for (input, expected) in test_case {
            match parse(input) {
                Ok(node) => assert_eq!(expected, &format!("{}", node)),
                Err(e) => panic!("Parsing Error: {:#?}", e),
            }
        }
    }

    #[test]
    fn test_let_statement() {
        let test_case = [
            ("let x = 5;", "let x = 5;"),
            ("let y = true;", "let y = true;"),
            ("let foobar = y;", "let foobar = y;"),
        ];

        apply_test(&test_case);
    }

    #[test]
    fn test_return_statement() {
        let test_case = [
            ("return 5;", "return 5;"),
            ("return true;", "return true;"),
            ("return foobar;", "return foobar;"),
        ];

        apply_test(&test_case);
    }

    #[test]
    fn test_identifier_expression() {
        let test_case = [("foobar;", "foobar")];

        apply_test(&test_case);
    }

    #[test]
    fn test_integer_literal_expression() {
        let test_case = [("5;", "5")];

        apply_test(&test_case);
    }

    #[test]
    fn test_parse_prefix_expression() {
        let test_case = [
            ("!5;", "(!5)"),
            ("-15;", "(-15)"),
            ("!foobar;", "(!foobar)"),
            ("-foobar;", "(-foobar)"),
            ("!true;", "(!true)"),
            ("!false;", "(!false)"),
        ];

        apply_test(&test_case);
    }

    #[test]
    fn test_parse_infix_expression() {
        let test_case = [
            ("5 + 5;", "(5 + 5)"),
            ("5 - 5;", "(5 - 5)"),
            ("5 * 5;", "(5 * 5)"),
            ("5 / 5;", "(5 / 5)"),
            ("5 > 5;", "(5 > 5)"),
            ("5 < 5;", "(5 < 5)"),
            ("5 == 5;", "(5 == 5)"),
            ("5 != 5;", "(5 != 5)"),
            ("foobar + barfoo;", "(foobar + barfoo)"),
            ("foobar - barfoo;", "(foobar - barfoo)"),
            ("foobar * barfoo;", "(foobar * barfoo)"),
            ("foobar / barfoo;", "(foobar / barfoo)"),
            ("foobar > barfoo;", "(foobar > barfoo)"),
            ("foobar < barfoo;", "(foobar < barfoo)"),
            ("foobar == barfoo;", "(foobar == barfoo)"),
            ("foobar != barfoo;", "(foobar != barfoo)"),
            ("true == true", "(true == true)"),
            ("true != false", "(true != false)"),
            ("false == false", "(false == false)"),
        ];

        apply_test(&test_case);
    }

    #[test]
    fn test_operator_precedence() {
        let test_case = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("(5 + 5) * 2 * (5 + 5)", "(((5 + 5) * 2) * (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"),
            ("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))"),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        apply_test(&test_case);
    }

    #[test]
    fn test_boolean_literal_expression() {
        let test_case = [
            ("true;", "true"),
            ("false;", "false")
        ];
        apply_test(&test_case);
    }

    #[test]
    fn test_if_expression() {
        let test_case = [
            ("if (x < y) { x }", "if (x < y) { x }")
        ];
        apply_test(&test_case);
    }

    #[test]
    fn test_if_else_expression() {
        let test_case = [
            ("if (x < y) { x } else { y }", "if (x < y) { x } else { y }")
        ];
        apply_test(&test_case);
    }

    #[test]
    fn test_function_expression() {
        let test_case = [
            ("fn() {};", "fn() {  }"),
            ("fn(x) {};", "fn(x) {  }"),
            ("fn(x, y, z) {};", "fn(x, y, z) {  }"),
        ];
        apply_test(&test_case);
    }

    #[test]
    fn test_fn_call_expression() {
        let test_case = [
            ("add(1, 2 * 3, 4 + 5);", "add(1, (2 * 3), (4 + 5))")
        ];
        apply_test(&test_case);
    }

    #[test]
    fn test_string_literal_expression() {
        let test_case = [
            (r#""hello world";"#, r#""hello world""#)
        ];
        apply_test(&test_case);
    }

    #[test]
    fn test_index_expression() {
        let test_case = [
            ("myArray[1 + 1]", "(myArray[(1 + 1)])"),
            (r#"myMap["key"]"#, r#"(myMap["key"])"#),
        ];
        apply_test(&test_case);
    }

    #[test]
    fn test_hash_literal_expression() {
        let test_case = [
            (
                r#"{"one": 1, "two": 2, "three": 3}"#,
                r#"{"one": 1, "two": 2, "three": 3}"#,
            ),
            (r#"{}"#, r#"{}"#),
            (
                r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#,
                r#"{"one": (0 + 1), "two": (10 - 8), "three": (15 / 5)}"#,
            ),
        ];
        apply_test(&test_case);
    }

}