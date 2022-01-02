pub mod builtin;
pub mod env;
pub mod error;
pub mod object;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::*;
use crate::evaluator::{builtin::*, env::Environment, error::EvalError, object::*};
use crate::lexer::token::Token;

pub fn eval(node: &Node, env: Rc<RefCell<Environment>>) -> Result<Rc<Object>, EvalError> {
    match node {
        Node::Program(program) => eval_program(&program, env),
        Node::Statement(stmt) => eval_statement(&stmt, env),
        Node::Expression(expr) => eval_expression(&expr, env),
    }
}

fn eval_program(program: &Program, env: Rc<RefCell<Environment>>) -> Result<Rc<Object>, EvalError> {
    let mut result = Rc::new(Object::Null);

    for stmt in &program.statements {
        let res = eval_statement(stmt, Rc::clone(&env))?;
        match res.as_ref() {
            Object::ReturnValue(v) => return Ok(Rc::clone(&v.value)),
            _ => result = res,
        }
    }
    Ok(result)
}

fn eval_statement(
    stmt: &Statement,
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    match stmt {
        Statement::Let(let_stmt) => {
            let expr = eval_expression(&let_stmt.value, Rc::clone(&env))?;
            env.borrow_mut()
                .set(let_stmt.name.clone(), Rc::clone(&expr));
            Ok(expr)
        }
        Statement::Return(ret) => {
            let value = eval_expression(&ret.value, env)?;
            Ok(Rc::new(Object::ReturnValue(Rc::new(Return { value }))))
        }
        Statement::Expression(expr) => eval_expression(&expr.expression, env),
    }
}

fn eval_expression(
    expr: &Expression,
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    match expr {
        Expression::Identifier(ident) => eval_identifer(ident, env),
        Expression::Integer(i) => Ok(Rc::new(Object::Integer(*i))),
        Expression::Prefix(expr) => eval_prefix_expression(expr, env),
        Expression::Infix(expr) => eval_infix_expression(expr, env),
        Expression::Boolean(b) => Ok(Rc::new(Object::Boolean(*b))),
        Expression::String(s) => Ok(Rc::new(Object::String(s.to_owned()))),
        Expression::Array(exprs) => {
            let elements = eval_expressions(exprs, env)?;
            Ok(Rc::new(Object::Array(Rc::new(Array { elements }))))
        }
        Expression::Hash(h) => eval_hash(h, env),
        Expression::If(expr) => eval_if_expression(expr, env),
        Expression::Function(func) => eval_func_literal(func, env),
        Expression::Call(call) => eval_func_call(call, env),
        Expression::Index(left, index) => eval_index_expression(left, index, env),
    }
}

fn eval_identifer(
    ident: &Identifier,
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    match env.borrow().get(ident) {
        Some(obj) => Ok(obj.clone()),
        None => match Builtin::lookup(ident) {
            Some(obj) => Ok(Rc::new(obj)),
            None => Err(EvalError::new(format!("identifier not found: {}", ident))),
        },
    }
}

fn eval_prefix_expression(
    expr: &PrefixExpression,
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    let right = eval_expression(&expr.right, env)?;
    match expr.operator {
        Token::Minus => eval_minus_prefix_expression(right),
        Token::Bang => eval_bang_prefix_expression(right),
        _ => Err(EvalError::new(format!(
            "unknown prefix operator: {}",
            expr.operator
        ))),
    }
}

fn eval_minus_prefix_expression(right: Rc<Object>) -> Result<Rc<Object>, EvalError> {
    match *right {
        Object::Integer(val) => Ok(Rc::new(Object::Integer(-val))),
        _ => Err(EvalError::new(format!("unknown operator: -{:?}", right))),
    }
}

fn eval_bang_prefix_expression(right: Rc<Object>) -> Result<Rc<Object>, EvalError> {
    match *right {
        Object::Boolean(b) => Ok(Rc::new(Object::Boolean(!b))),
        Object::Integer(i) => Ok(Rc::new(Object::Boolean(i == 0))),
        _ => Err(EvalError::new(format!("unknown operator: !{:?}", right))),
    }
}

fn eval_infix_expression(
    expr: &InfixExpression,
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    let left = eval_expression(&expr.left, Rc::clone(&env))?;
    let right = eval_expression(&expr.right, Rc::clone(&env))?;
    match (&*left, &*right) {
        (Object::Integer(l), Object::Integer(r)) => {
            eval_integer_infix_expression(&expr.operator, *l, *r)
        }
        (Object::Boolean(l), Object::Boolean(r)) => {
            eval_boolean_infix_expression(&expr.operator, *l, *r)
        }
        (Object::String(l), Object::String(r)) => {
            eval_string_infix_expression(&expr.operator, l, r)
        }
        _ => Err(EvalError::new(format!(
            "type mismatch: {:?} {} {:?}",
            left, expr.operator, right
        ))),
    }
}

fn eval_integer_infix_expression(
    op: &Token,
    left: i64,
    right: i64,
) -> Result<Rc<Object>, EvalError> {
    match op {
        Token::Plus => Ok(Rc::new(Object::Integer(left + right))),
        Token::Minus => Ok(Rc::new(Object::Integer(left - right))),
        Token::Multiply => Ok(Rc::new(Object::Integer(left * right))),
        Token::Divide => Ok(Rc::new(Object::Integer(left / right))),
        Token::LessThan => Ok(Rc::new(Object::Boolean(left < right))),
        Token::GreaterThan => Ok(Rc::new(Object::Boolean(left > right))),
        Token::Equal => Ok(Rc::new(Object::Boolean(left == right))),
        Token::NotEqual => Ok(Rc::new(Object::Boolean(left != right))),
        _ => Err(EvalError::new(format!(
            "unknown operator: {} {} {}",
            left, op, right
        ))),
    }
}

fn eval_boolean_infix_expression(
    op: &Token,
    left: bool,
    right: bool,
) -> Result<Rc<Object>, EvalError> {
    match op {
        Token::Equal => Ok(Rc::new(Object::Boolean(left == right))),
        Token::NotEqual => Ok(Rc::new(Object::Boolean(left != right))),
        _ => Err(EvalError::new(format!(
            "unknown operator: {} {} {}",
            left, op, right
        ))),
    }
}

fn eval_string_infix_expression(
    op: &Token,
    left: &str,
    right: &str,
) -> Result<Rc<Object>, EvalError> {
    match op {
        Token::Plus => Ok(Rc::new(Object::String(format!("{}{}", left, right)))),
        _ => Err(EvalError::new(format!(
            "unknown operator: {} {} {}",
            left, op, right
        ))),
    }
}

fn eval_expressions(
    exprs: &[Expression],
    env: Rc<RefCell<Environment>>,
) -> Result<Vec<Rc<Object>>, EvalError> {
    let mut elements: Vec<Rc<Object>> = Vec::with_capacity(exprs.len());
    for expr in exprs {
        elements.push(eval_expression(expr, Rc::clone(&env))?);
    }
    Ok(elements)
}

fn eval_hash(
    h: &[(Expression, Expression)],
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    let mut pairs: HashMap<Rc<Object>, Rc<Object>> = HashMap::with_capacity(h.len());
    for pair in h {
        let key = eval_expression(&pair.0, Rc::clone(&env))?;
        let value = eval_expression(&pair.1, Rc::clone(&env))?;
        pairs.insert(key, value);
    }
    Ok(Rc::new(Object::Hash(Rc::new(MonkeyHash { pairs }))))
}

fn eval_if_expression(
    expr: &IfExpression,
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    let condition = eval_expression(&expr.condition, Rc::clone(&env))?;
    if truthy(condition) {
        eval_block(&expr.consequence, env)
    } else {
        if let Some(block) = &expr.alternative {
            eval_block(&block, env)
        } else {
            Ok(Rc::new(Object::Null))
        }
    }
}

fn truthy(value: Rc<Object>) -> bool {
    match &*value {
        Object::Boolean(b) => *b,
        Object::Null => false,
        Object::Integer(i) => *i != 0,
        Object::String(s) => s.len() > 0,
        _ => true,
    }
}

fn eval_block(
    block: &BlockStatement,
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    let mut res = Rc::new(Object::Null);
    for stmt in &block.statements {
        let value = eval_statement(stmt, Rc::clone(&env))?;
        match &*value {
            Object::ReturnValue(_) => return Ok(value),
            _ => res = value,
        }
    }
    Ok(res)
}

fn eval_func_literal(
    func: &FunctionLiteral,
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    Ok(Rc::new(Object::Function(Rc::new(Function {
        parameters: func.parameters.clone(),
        body: func.body.clone(),
        env: Rc::clone(&env),
    }))))
}

fn eval_func_call(
    call: &CallExpression,
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    let func = eval_expression(&call.function, Rc::clone(&env))?;
    let args = eval_expressions(&call.arguments, Rc::clone(&env))?;
    match &*func {
        Object::Function(f) => {
            let extended_env = extend_func_env(&f, args);
            let evaluated = eval_block(&f.body, extended_env)?;
            Ok(unwrap_return_value(evaluated))
        }
        Object::Builtin(builtin) => Ok(builtin.apply(&args)?),
        _ => Err(EvalError::new(format!("{:?} is not a function", func))),
    }
}

fn extend_func_env(f: &Function, args: Vec<Rc<Object>>) -> Rc<RefCell<Environment>> {
    let env = Rc::new(RefCell::new(Environment::new_enclosed(Rc::clone(&f.env))));
    f.parameters
        .iter()
        .zip(&args)
        .for_each(|(param, arg)| env.borrow_mut().set(param.name.to_owned(), Rc::clone(&arg)));
    Rc::clone(&env)
}

fn unwrap_return_value(obj: Rc<Object>) -> Rc<Object> {
    if let Object::ReturnValue(ret) = &*obj {
        Rc::clone(&ret.value)
    } else {
        obj
    }
}

fn eval_index_expression(
    left: &Expression,
    index: &Expression,
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    let left_obj = eval_expression(&left, Rc::clone(&env))?;
    let index_obj = eval_expression(&index, Rc::clone(&env))?;
    match (&*left_obj, &*index_obj) {
        (Object::Array(arr), Object::Integer(idx)) => match arr.elements.get(*idx as usize) {
            Some(ele) => Ok(Rc::clone(ele)),
            None => Ok(Rc::new(Object::Null)),
        },
        (Object::Hash(h), _obj) => match &*index_obj {
            Object::String(_) | Object::Integer(_) | Object::Boolean(_) => {
                match h.pairs.get(&*index_obj) {
                    Some(obj) => Ok(Rc::clone(&obj)),
                    None => Ok(Rc::new(Object::Null)),
                }
            }
            _ => Err(EvalError::new(format!("cannot use as hash key: {}", index))),
        },
        _ => Err(EvalError::new(format!(
            "index operator not supported: {}",
            index
        ))),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser;

    #[test]
    fn eval_integer_expression() {
        struct Test<'a> {
            input: &'a str,
            expected: i64,
        }
        let tests = vec![
            Test {
                input: "5",
                expected: 5,
            },
            Test {
                input: "10",
                expected: 10,
            },
            Test {
                input: "-5",
                expected: -5,
            },
            Test {
                input: "-10",
                expected: -10,
            },
            Test {
                input: "5 + 5 + 5 + 5 - 10",
                expected: 10,
            },
            Test {
                input: "2 * 2 * 2 * 2 * 2",
                expected: 32,
            },
            Test {
                input: "-50 + 100 + -50",
                expected: 0,
            },
            Test {
                input: "5 * 2 + 10",
                expected: 20,
            },
            Test {
                input: "5 + 2 * 10",
                expected: 25,
            },
            Test {
                input: "20 + 2 * -10",
                expected: 0,
            },
            Test {
                input: "50 / 2 * 2 + 10",
                expected: 60,
            },
            Test {
                input: "2 * (5 + 10)",
                expected: 30,
            },
            Test {
                input: "3 * 3 * 3 + 10",
                expected: 37,
            },
            Test {
                input: "3 * (3 * 3) + 10",
                expected: 37,
            },
            Test {
                input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
                expected: 50,
            },
        ];

        for t in tests {
            let evaluated = test_eval(t.input);
            test_integer_object(&evaluated, t.expected);
        }
    }

    #[test]
    fn eval_boolean_expression() {
        struct Test<'a> {
            input: &'a str,
            expected: bool,
        }
        let tests = vec![
            Test {
                input: "true",
                expected: true,
            },
            Test {
                input: "false",
                expected: false,
            },
            Test {
                input: "1 < 2",
                expected: true,
            },
            Test {
                input: "1 > 2",
                expected: false,
            },
            Test {
                input: "1 < 1",
                expected: false,
            },
            Test {
                input: "1 > 1",
                expected: false,
            },
            Test {
                input: "1 == 1",
                expected: true,
            },
            Test {
                input: "1 != 1",
                expected: false,
            },
            Test {
                input: "1 == 2",
                expected: false,
            },
            Test {
                input: "1 != 2",
                expected: true,
            },
            Test {
                input: "true == true",
                expected: true,
            },
            Test {
                input: "false == false",
                expected: true,
            },
            Test {
                input: "true == false",
                expected: false,
            },
            Test {
                input: "true != false",
                expected: true,
            },
            Test {
                input: "false != true",
                expected: true,
            },
            Test {
                input: "(1 < 2) == true",
                expected: true,
            },
            Test {
                input: "(1 < 2) == false",
                expected: false,
            },
            Test {
                input: "(1 > 2) == true",
                expected: false,
            },
            Test {
                input: "(1 > 2) == false",
                expected: true,
            },
        ];

        for t in tests {
            let evaluated = test_eval(t.input);

            test_boolean_object(&evaluated, t.expected);
        }
    }

    #[test]
    fn bang_operator() {
        struct Test<'a> {
            input: &'a str,
            expected: bool,
        }
        let tests = vec![
            Test {
                input: "!true",
                expected: false,
            },
            Test {
                input: "!false",
                expected: true,
            },
            Test {
                input: "!5",
                expected: false,
            },
            Test {
                input: "!!true",
                expected: true,
            },
            Test {
                input: "!!false",
                expected: false,
            },
            Test {
                input: "!!5",
                expected: true,
            },
        ];

        for t in tests {
            let evaluated = test_eval(t.input);
            test_boolean_object(&evaluated, t.expected);
        }
    }

    #[test]
    fn if_else_expressions() {
        struct Test<'a> {
            input: &'a str,
            expected: Object,
        }
        let tests = vec![
            Test {
                input: "if (true) { 10 }",
                expected: Object::Integer(10),
            },
            Test {
                input: "if (false) { 10 }",
                expected: Object::Null,
            },
            Test {
                input: "if (1) { 10 }",
                expected: Object::Integer(10),
            },
            Test {
                input: "if (1 < 2) { 10 }",
                expected: Object::Integer(10),
            },
            Test {
                input: "if (1 > 2) { 10 }",
                expected: Object::Null,
            },
            Test {
                input: "if (1 > 2) { 10 } else { 20 }",
                expected: Object::Integer(20),
            },
            Test {
                input: "if (1 < 2) { 10 } else { 20 }",
                expected: Object::Integer(10),
            },
        ];

        for t in tests {
            let evaluated = &*test_eval(t.input);

            match t.expected {
                Object::Integer(i) => test_integer_object(&evaluated, i),
                _ => test_null_object(&evaluated),
            }
        }
    }

    #[test]
    fn return_statements() {
        struct Test<'a> {
            input: &'a str,
            expected: i64,
        }
        let tests = vec![
            Test {
                input: "return 10;",
                expected: 10,
            },
            Test {
                input: "return 10; 9;",
                expected: 10,
            },
            Test {
                input: "return 2 * 5; 9;",
                expected: 10,
            },
            Test {
                input: "9; return 2 * 5; 9;",
                expected: 10,
            },
            Test {
                input: "if (10 > 1) {
                           if (10 > 1) {
                             return 10;
                           }
                           return 1;
                         }",
                expected: 10,
            },
        ];

        for t in tests {
            let evaluated = test_eval(t.input);
            test_integer_object(&evaluated, t.expected)
        }
    }

    #[test]
    fn error_handling() {
        struct Test<'a> {
            input: &'a str,
            expected: &'a str,
        }
        let tests = vec![
            Test {
                input: "5 + true;",
                expected: "type mismatch: Integer(5) + Boolean(true)",
            },
            Test {
                input: "5 + true; 5;",
                expected: "type mismatch: Integer(5) + Boolean(true)",
            },
            Test {
                input: "-true",
                expected: "unknown operator: -Boolean(true)",
            },
            Test {
                input: "true + false",
                expected: "unknown operator: true + false",
            },
            Test {
                input: "5; true + false; 5",
                expected: "unknown operator: true + false",
            },
            Test {
                input: "if (10 > 1) { true + false; }",
                expected: "unknown operator: true + false",
            },
            Test {
                input: "if (10 > 1) {
                             if (10 > 1) {
                                return true + false;
                             }
                             return 1;
                          }",
                expected: "unknown operator: true + false",
            },
            Test {
                input: "foobar",
                expected: "identifier not found: foobar",
            },
            Test {
                input: r#" {"name": "Monkey"}[fn(x) { x }]; "#,
                expected: "cannot use as hash key: fn(x) { x }",
            },
        ];

        for t in tests {
            let env = Rc::new(RefCell::new(Environment::new()));
            match parser::parse(t.input) {
                Ok(node) => match eval(&node, env) {
                    Err(e) => assert_eq!(&format!("{}", e), t.expected),
                    n => panic!("expected error {} but got {:?}", t.expected, n),
                },
                Err(e) => panic!("error {:?} on input {}", e, t.input),
            }
        }
    }

    #[test]
    fn let_statements() {
        struct Test<'a> {
            input: &'a str,
            expected: i64,
        }
        let tests = vec![
            Test {
                input: "let a = 5; a;",
                expected: 5,
            },
            Test {
                input: "let a = 5 * 5; a;",
                expected: 25,
            },
            Test {
                input: "let a = 5; let b = a; b;",
                expected: 5,
            },
            Test {
                input: "let a = 5; let b = a; let c = a + b + 5; c;",
                expected: 15,
            },
        ];

        for t in tests {
            let evaluated = test_eval(t.input);
            test_integer_object(&evaluated, t.expected)
        }
    }

    #[test]
    fn function_object() {
        let input = "fn(x) { x + 2; };";
        let evaluated = &*test_eval(input);

        match evaluated {
            Object::Function(f) => {
                assert_eq!(f.parameters.len(), 1);
                assert_eq!(f.parameters.first().unwrap().name, "x");
                assert_eq!(f.body.to_string(), "(x + 2)");
            }
            _ => panic!("expected function object but got {:?}", evaluated),
        }
    }

    #[test]
    fn function_application() {
        struct Test<'a> {
            input: &'a str,
            expected: i64,
        }
        let tests = vec![
            Test {
                input: "let identity = fn(x) { x; }; identity(5);",
                expected: 5,
            },
            Test {
                input: "let identity = fn(x) { return x; }; identity(5);",
                expected: 5,
            },
            Test {
                input: "let double = fn(x) { x * 2; }; double(5);",
                expected: 10,
            },
            Test {
                input: "let add = fn(x, y) { x + y; }; add(5, 5);",
                expected: 10,
            },
            Test {
                input: "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                expected: 20,
            },
            Test {
                input: "fn(x) { x; }(5)",
                expected: 5,
            },
        ];

        for t in tests {
            test_integer_object(&test_eval(t.input), t.expected)
        }
    }

    #[test]
    fn closures() {
        let input = "let newAdder = fn(x) {
  fn(y) { x + y };
};
let addTwo = newAdder(2);
addTwo(2);";
        test_integer_object(&test_eval(input), 4)
    }

    #[test]
    fn string_literal() {
        let input = r#""Hello World!"#;

        match &*test_eval(input) {
            Object::String(s) => assert_eq!(s, "Hello World!"),
            obj => panic!("{}", format!("expected string but got {:?}", obj)),
        }
    }

    #[test]
    fn string_concatenation() {
        let input = r#""Hello" + " " + "World!""#;

        match &*test_eval(input) {
            Object::String(s) => assert_eq!(s, "Hello World!"),
            obj => panic!("{}", format!("expected string but got {:?}", obj)),
        }
    }

    #[test]
    fn builtin_functions() {
        struct Test<'a> {
            input: &'a str,
            expected: Object,
        }
        let tests = vec![
            Test {
                input: r#"len("")"#,
                expected: Object::Integer(0),
            },
            Test {
                input: r#"len("four")"#,
                expected: Object::Integer(4),
            },
            Test {
                input: r#"len("hello world")"#,
                expected: Object::Integer(11),
            },
            Test {
                input: "len([1, 2, 3])",
                expected: Object::Integer(3),
            },
            Test {
                input: "len([])",
                expected: Object::Integer(0),
            },
            Test {
                input: "first([1, 2, 3])",
                expected: Object::Integer(1),
            },
            Test {
                input: "first([])",
                expected: Object::Null,
            },
            Test {
                input: "last([1, 2, 3])",
                expected: Object::Integer(3),
            },
            Test {
                input: "last([])",
                expected: Object::Null,
            },
            Test {
                input: "rest([1, 2, 3])",
                expected: Object::Array(Rc::new(Array {
                    elements: vec![Rc::new(Object::Integer(2)), Rc::new(Object::Integer(3))],
                })),
            },
            Test {
                input: "rest([])",
                expected: Object::Array(Rc::new(Array { elements: vec![] })),
            },
            Test {
                input: "push([], 1)",
                expected: Object::Array(Rc::new(Array {
                    elements: vec![Rc::new(Object::Integer(1))],
                })),
            },
        ];

        for t in tests {
            let obj = test_eval(t.input);

            match (&t.expected, &*obj) {
                (Object::Integer(exp), Object::Integer(got)) => assert_eq!(
                    *exp, *got,
                    "on input {} expected {} but got {}",
                    t.input, exp, got
                ),
                (Object::Null, Object::Null) => {}
                (Object::Array(ex), Object::Array(got)) => {
                    assert_eq!(ex.elements.len(), got.elements.len());
                    let mut got_iter = (&got.elements).into_iter();
                    for obj in &ex.elements {
                        let got_obj = Rc::clone(got_iter.next().unwrap());
                        match (&*Rc::clone(obj), &*got_obj) {
                            (Object::Integer(exi), Object::Integer(goti)) => {
                                assert_eq!(*exi, *goti)
                            }
                            _ => panic!("{:?} not same type as {:?}", got_obj, obj),
                        }
                    }
                }
                _ => panic!(
                    "on input {} expected {:?} but got {:?}",
                    t.input, t.expected, obj
                ),
            }
        }
    }

    #[test]
    fn builtin_errors() {
        struct Test<'a> {
            input: &'a str,
            expected: &'a str,
        }
        let tests = vec![
            Test {
                input: r#"len(1)"#,
                expected: "Object Integer(1) not supported as an argument for len()",
            },
            Test {
                input: r#"len("one", "two")"#,
                expected: "Expected 1 argument, but got #args: 2",
            },
            Test {
                input: r#"first(1)"#,
                expected: "Object Integer(1) not supported as an argument for first()",
            },
        ];

        for t in tests {
            let env = Rc::new(RefCell::new(Environment::new()));
            match parser::parse(t.input) {
                Ok(node) => match eval(&node, env) {
                    Ok(obj) => panic!("expected error on input {} but got {:?}", t.input, obj),
                    Err(err) => assert_eq!(
                        t.expected,
                        format!("{}", err),
                        "on input {} expected error {} but got {}",
                        t.input,
                        t.expected,
                        format!("{}", err)
                    ),
                },
                Err(e) => panic!("error {:?} on input {}", e, t.input),
            }
        }
    }

    #[test]
    fn array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let obj = test_eval(input);
        match &*obj {
            Object::Array(a) => {
                test_integer_object(a.elements.get(0).unwrap(), 1);
                test_integer_object(a.elements.get(1).unwrap(), 4);
                test_integer_object(a.elements.get(2).unwrap(), 6);
            }
            _ => panic!("expected array but got {:?}", obj),
        }
    }

    #[test]
    fn array_index_expressions() {
        struct Test<'a> {
            input: &'a str,
            expected: i64,
        }
        let tests = vec![
            Test {
                input: "[1, 2, 3][0]",
                expected: 1,
            },
            Test {
                input: "[1, 2, 3][1]",
                expected: 2,
            },
            Test {
                input: "[1, 2, 3][2]",
                expected: 3,
            },
            Test {
                input: "let i = 0; [1][i];",
                expected: 1,
            },
            Test {
                input: "[1, 2, 3][1 + 1];",
                expected: 3,
            },
            Test {
                input: "let myArray = [1, 2, 3]; myArray[2];",
                expected: 3,
            },
            Test {
                input: "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                expected: 6,
            },
            Test {
                input: "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                expected: 2,
            },
        ];

        for t in tests {
            let obj = test_eval(t.input);
            match &*obj {
                Object::Integer(i) => assert_eq!(*i, t.expected),
                _ => panic!("expected int obj but got {:?}", obj),
            }
        }
    }

    #[test]
    fn invalid_array_index() {
        let inputs = vec!["[1, 2, 3][3]", "[1, 2, 3][-1]"];

        for input in inputs {
            let obj = test_eval(input);
            match &*obj {
                Object::Null => {}
                _ => panic!("expected null object, but got {:?}", obj),
            }
        }
    }

    #[test]
    fn hash_index_expressions() {
        struct Test<'a> {
            input: &'a str,
            expected: Object,
        }
        let tests = vec![
            Test {
                input: r#" {"foo":5}["foo"] "#,
                expected: Object::Integer(5),
            },
            Test {
                input: r#" {"foo":5}["bar"] "#,
                expected: Object::Null,
            },
            Test {
                input: r#" let key = "foo"; {"foo":5}[key] "#,
                expected: Object::Integer(5),
            },
            Test {
                input: r#" {}["foo"] "#,
                expected: Object::Null,
            },
            Test {
                input: r#" {5: 5}[5] "#,
                expected: Object::Integer(5),
            },
            Test {
                input: r#" {true: 5}[true] "#,
                expected: Object::Integer(5),
            },
            Test {
                input: r#" {false: 5}[false] "#,
                expected: Object::Integer(5),
            },
        ];

        for t in tests {
            let obj = test_eval(t.input);

            match (&t.expected, &*obj) {
                (Object::Integer(exp), Object::Integer(got)) => assert_eq!(
                    *exp, *got,
                    "on input {} expected {} but got {}",
                    t.input, exp, got
                ),
                (Object::Null, Object::Null) => {}
                _ => panic!(
                    "on input {} expected {:?} but got {:?}",
                    t.input, t.expected, obj
                ),
            }
        }
    }

    fn test_eval(input: &str) -> Rc<Object> {
        let env = Rc::new(RefCell::new(Environment::new()));
        match parser::parse(input) {
            Ok(node) => eval(&node, env).expect(input),
            Err(e) => panic!("error {:?} on input {}", e, input),
        }
    }

    fn test_integer_object(obj: &Object, expected: i64) {
        match obj {
            Object::Integer(i) => assert_eq!(i, &expected),
            _ => panic!("expected integer object, but got {:?}", obj),
        }
    }

    fn test_boolean_object(obj: &Object, expected: bool) {
        match obj {
            Object::Boolean(b) => assert_eq!(b, &expected),
            _ => panic!("expected boolean object, but got {:?}", obj),
        }
    }

    fn test_null_object(obj: &Object) {
        match obj {
            Object::Null => {}
            _ => panic!("expected null but got {:?}", obj),
        }
    }
}
