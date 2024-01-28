use std::env;
use compiler::*;

fn main() {
    let args: Vec<String> = env::args().collect();

    let result = interpret_file(&args[1]);

    match result {
        interpreter::Value::Integer(i) => println!("{}", i),
        interpreter::Value::Boolean(b) => println!("{}", b),
        interpreter::Value::Unit => println!("Unit"),
    }
}
