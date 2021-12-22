use std::io;
use monkey::repl;

fn main() {
    println!("Welcome to the Monkey REPL!");
    let input = io::stdin();
    let output = io::stdout();

    repl::start(input.lock(), output.lock());
}