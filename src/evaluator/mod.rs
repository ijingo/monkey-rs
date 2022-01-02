pub mod object;
pub mod builtin;
pub mod env;
pub mod error;

use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::*;
use crate::evaluator::{
    error::EvalError,
    env::Environment,
    object::*,
    builtin::*,
};
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

fn eval_statement(stmt: &Statement, env: Rc<RefCell<Environment>>) -> Result<Rc<Object>, EvalError> {
    match stmt {
        Statement::Let(let_stmt) => {
            let expr = eval_expression(&let_stmt.value, Rc::clone(&env))?;
            env.borrow_mut().set(let_stmt.name.clone(), Rc::clone(&expr));
            Ok(expr)
        },
        Statement::Return(ret) => {
            let value = eval_expression(&ret.value, env)?;
            Ok(Rc::new(Object::ReturnValue(Rc::new(Return{value}))))
        },
        Statement::Expression(expr) => eval_expression(&expr.expression, env),
    }
}

fn eval_expression(expr: &Expression, env: Rc<RefCell<Environment>>) -> Result<Rc<Object>, EvalError> {
    match expr {
        Expression::Identifier(ident) => eval_identifer(ident, env),
        Expression::Integer(i) => Ok(Rc::new(Object::Integer(*i))),
        Expression::Prefix(expr) => eval_prefix_expression(expr, env),
        Expression::Infix(expr) => eval_infix_expression(expr, env),
        Expression::Boolean(b) => Ok(Rc::new(Object::Boolean(*b))),
        Expression::String(s) => Ok(Rc::new(Object::String(s.to_owned()))),
        Expression::Array(exprs) => {
            let elements = eval_expressions(exprs, env)?;
            Ok(Rc::new(Object::Array(Rc::new(Array{elements}))))
        },
        Expression::Hash(h) => eval_hash(h, env),
        Expression::If(expr) => eval_if_expression(expr, env),
        Expression::Function(func) => eval_func_literal(func, env),
        Expression::Call(call) => eval_func_call(call, env),
        Expression::Index(left, index) => eval_index_expression(left, index, env),
    }
}

fn eval_identifer(ident: &Identifier, env: Rc<RefCell<Environment>>) -> Result<Rc<Object>, EvalError> {
    match env.borrow().get(ident) {
        Some(obj) => Ok(obj.clone()),
        None => {
            match Builtin::lookup(ident) {
                Some(obj) => Ok(Rc::new(obj)),
                None => Err(EvalError::new(format!("identifier not found: {}", ident)))
            }
        }
    }
}

fn eval_prefix_expression(expr: &PrefixExpression, env: Rc<RefCell<Environment>>) -> Result<Rc<Object>, EvalError> {
    let right = eval_expression(&expr.right, env)?;
    match expr.operator {
        Token::Minus => eval_minus_prefix_expression(right),
        Token::Bang => eval_bang_prefix_expression(right),
        _ => Err(EvalError::new(format!("unknown prefix operator: {}", expr.operator))),
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
        _ => Err(EvalError::new(format!("unknown operator: !{:?}", right))),
    }
}

fn eval_infix_expression(expr: &InfixExpression, env: Rc<RefCell<Environment>>) -> Result<Rc<Object>, EvalError> {
    let left = eval_expression(&expr.left, Rc::clone(&env))?;
    let right = eval_expression(&expr.right, Rc::clone(&env))?;
    match (&*left, &*right) {
        (Object::Integer(l), Object::Integer(r)) => eval_integer_infix_expression(&expr.operator, *l, *r),
        (Object::Boolean(l), Object::Boolean(r)) => eval_boolean_infix_expression(&expr.operator, *l, *r),
        (Object::String(l), Object::String(r)) => eval_string_infix_expression(&expr.operator, l, r),
        _ =>  Err(EvalError::new(format!("type mismatch: {:?} {} {:?}", left, expr.operator, right))),
    }
}

fn eval_integer_infix_expression(op: &Token, left: i64, right: i64)-> Result<Rc<Object>, EvalError>  {
    match op {
        Token::Plus => Ok(Rc::new(Object::Integer(left + right))),
        Token::Minus => Ok(Rc::new(Object::Integer(left - right))),
        Token::Multiply => Ok(Rc::new(Object::Integer(left * right))),
        Token::Divide => Ok(Rc::new(Object::Integer(left / right))),
        Token::LessThan => Ok(Rc::new(Object::Boolean(left < right))),
        Token::GreaterThan => Ok(Rc::new(Object::Boolean(left > right))),
        Token::Equal => Ok(Rc::new(Object::Boolean(left == right))),
        Token::NotEqual => Ok(Rc::new(Object::Boolean(left != right))),
        _ => Err(EvalError::new(format!("unknown operator: {}", op))),
    }
}

fn eval_boolean_infix_expression(op: &Token, left: bool, right: bool)-> Result<Rc<Object>, EvalError>  {
    match op {
        Token::Equal => Ok(Rc::new(Object::Boolean(left == right))),
        Token::NotEqual => Ok(Rc::new(Object::Boolean(left != right))),
        _ => Err(EvalError::new(format!("unknown operator: {}", op))),
    }
}

fn eval_string_infix_expression(op: &Token, left: &str, right: &str)-> Result<Rc<Object>, EvalError>  {
    match op {
        Token::Plus => Ok(Rc::new(Object::String(format!("{}{}", left, right)))),
        _ => Err(EvalError::new(format!("unknown operator: {}", op))),
    }
}

fn eval_expressions(exprs: &[Expression], env: Rc<RefCell<Environment>>) -> Result<Vec<Rc<Object>>, EvalError> {
    let mut elements: Vec<Rc<Object>> = Vec::with_capacity(exprs.len());
    for expr in exprs {
        elements.push(eval_expression(expr, Rc::clone(&env))?);
    }
    Ok(elements)
}

fn eval_hash(h: &[(Expression, Expression)], env: Rc<RefCell<Environment>>) -> Result<Rc<Object>, EvalError> {
    let mut pairs: HashMap<Rc<Object>, Rc<Object>> = HashMap::with_capacity(h.len());
    for pair in h {
        let key = eval_expression(&pair.0, Rc::clone(&env))?;
        let value = eval_expression(&pair.1, Rc::clone(&env))?;
        pairs.insert(key, value);
    }
    Ok(Rc::new(Object::Hash(Rc::new(MonkeyHash{pairs}))))
}

fn eval_if_expression(expr: &IfExpression, env: Rc<RefCell<Environment>>) -> Result<Rc<Object>, EvalError> {
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

fn truthy(value: Rc<Object>) -> bool  {
    match &*value {
        Object::Boolean(b) => *b,
        Object::Null => false,
        Object::Integer(i) => *i != 0,
        Object::String(s) => s.len() > 0,
        _ => true,
    }
}

fn eval_block(block: &BlockStatement, env: Rc<RefCell<Environment>>) -> Result<Rc<Object>, EvalError> {
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

fn eval_func_literal(func: &FunctionLiteral, env: Rc<RefCell<Environment>>) -> Result<Rc<Object>, EvalError> {
    Ok(Rc::new(Object::Function(Rc::new(
        Function{
            parameters: func.parameters.clone(),
            body: func.body.clone(),
            env: Rc::clone(&env),
        }
    ))))
}

fn eval_func_call(call: &CallExpression, env: Rc<RefCell<Environment>>) -> Result<Rc<Object>, EvalError> {
    let func = eval_expression(&call.function, Rc::clone(&env))?;
    let args = eval_expressions(&call.arguments, Rc::clone(&env))?;
    match &*func {
        Object::Function(f) => {
            let extended_env = extend_func_env(&f, args);
            let evaluated = eval_block(&f.body, extended_env)?;
            Ok(unwrap_return_value(evaluated))
        },
        Object::Builtin(builtin) => {
            // TODO: finish builtin applys 
            Err(EvalError::new(format!("{:?} is not a function", func)))
        },
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

fn eval_index_expression(left: &Expression, index: &Expression, env: Rc<RefCell<Environment>>) -> Result<Rc<Object>, EvalError> {
    let left_obj = eval_expression(&left, Rc::clone(&env))?;
    let index_obj = eval_expression(&index, Rc::clone(&env))?;
    match (&*left_obj, &*index_obj) {
        (Object::Array(arr), Object::Integer(idx)) => {
            match arr.elements.get(*idx as usize) {
                Some(ele) => Ok(Rc::clone(ele)),
                None => Ok(Rc::new(Object::Null)),
            }
        },
        (Object::Hash(h), _obj) => {
            match &*index_obj {
                Object::String(_) | Object::Integer(_) | Object::Boolean(_) => {
                    match h.pairs.get(&*index_obj) {
                        Some(obj) => Ok(Rc::clone(&obj)),
                        None => Ok(Rc::new(Object::Null)),
                    }
                },
                _ =>  Err(EvalError::new(format!("cannot use as hash key: {}", index))),
            }
        }
        _ =>  Err(EvalError::new(format!("index operator not supported: {}", index))),
    }
}