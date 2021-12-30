use crate::lexer::token::Token;

use std::fmt;

pub type Identifier = String;

#[derive(Debug)]
pub enum Node {
    Program(Box<Program>),
    Statement(Box<Statement>),
    Expression(Box<Expression>),
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Node::Program(program) => format!("{}", program),
            Node::Statement(stmt) => format!("{}", stmt),
            Node::Expression(expr) => format!("{}", expr),
        };
        write!(f, "{}", s)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum Statement {
    Let(Box<LetStatement>),
    Return(Box<ReturnStatement>),
    Expression(Box<ExpressionStatement>),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Statement::Let(stmt) => format!("{}", stmt),
            Statement::Return(ret) => format!("{}", ret),
            Statement::Expression(expr) => format!("{}", expr),
        };
        write!(f, "{}", s)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum Expression {
    Identifier(String),
    Integer(i64),
    Prefix(Box<PrefixExpression>),
    Infix(Box<InfixExpression>),
    Boolean(bool),
    String(String),
    If(Box<IfExpression>),
    Function(Box<FunctionLiteral>),
    Call(Box<CallExpression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Expression::Identifier(s) => s.clone(),
            Expression::Integer(value) => format!("{}", value),
            Expression::Prefix(prefix) => prefix.to_string(),
            Expression::Infix(infix) => infix.to_string(),
            Expression::Boolean(b) => b.to_string(),
            Expression::String(s) => format!("\"{}\"", s),
            Expression::If(expr) => expr.to_string(),
            Expression::Function(func) => func.to_string(),
            Expression::Call(call) => call.to_string(),
        };
        write!(f, "{}", s)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct IfExpression {
    pub condition: Expression,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl fmt::Display for IfExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "if {} {{ {} }}", self.condition, self.consequence)?;
        if let Some(stmt) = &self.alternative {
            write!(f, " else {{ {} }}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct FunctionLiteral {
    pub parameters: Vec<IdentifierExpression>,
    pub body: BlockStatement,
}

impl fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = self.parameters.iter().map(|p| p.to_string()).collect::<Vec<String>>().join(", ");
        write!(f, "fn({}) {{ {} }}", s, self.body)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct CallExpression {
    pub function: Expression,
    pub arguments: Vec<Expression>,
}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = self.arguments.iter().map(|arg| arg.to_string()).collect::<Vec<String>>().join(", ");
        write!(f, "{}({})", self.function, s)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct IdentifierExpression {
    pub name: String,
}

impl fmt::Display for IdentifierExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct PrefixExpression {
    pub operator: Token,
    pub right: Expression,
}

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct InfixExpression {
    pub operator: Token,
    pub left: Expression,
    pub right: Expression,
}

impl fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct IntegerLiteral {
    pub value: i64,
}

impl fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct LetStatement {
    pub name: String,
    pub value: Expression,
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct ReturnStatement {
    pub value: Expression,
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "return {};", self.value)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct ExpressionStatement {
    pub expression: Expression
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expression)
    }
}

#[derive(Debug, Default)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            statements: Vec::new(),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = self.statements.iter().map(|stmt| stmt.to_string()).collect::<Vec<String>>().join("");
        write!(f, "{}", s)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn display() {

        let p = Program{
            statements: vec![
                Statement::Let(Box::new(
                    LetStatement{
                            name: "some_name".to_string(),
                            value: Expression::Identifier("some_value".to_string())}))],
        };

        let expected = "let some_name = some_value;";
        let res = p.to_string();
        assert_eq!(expected, res, "{} != {}", expected, res);
    }
}