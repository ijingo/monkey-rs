use rustyline::error::ReadlineError;
use rustyline::Editor;

use std::cell::RefCell;
use std::rc::Rc;

use crate::evaluator::env::Environment;
use crate::evaluator::eval;
use crate::parser;

pub fn start() {
    let mut rl = Editor::<()>::new();
    let env: Rc<RefCell<Environment>> = Rc::new(RefCell::new(Environment::new()));

    loop {
        match rl.readline(">>> ") {
            Ok(line) => {
                rl.add_history_entry(<String as AsRef<str>>::as_ref(&line));

                if line.trim() == "exit" {
                    break;
                }

                match parser::parse(&line) {
                    Ok(node) => match eval(&node, Rc::clone(&env)) {
                        Ok(evaluated) => {
                            println!("{}", evaluated)
                        }
                        Err(err) => eprintln!("{}", err),
                    },
                    Err(errors) => {
                        for e in errors {
                            eprintln!("{}", e);
                        }
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}
