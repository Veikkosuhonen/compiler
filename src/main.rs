use std::env;
use std::fs;
use compiler::*;

fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    let tokens = tokenizer::tokenize(&contents);
    let expression = parser::parse(tokens);
    let result = interpreter::interpret(expression);

    match result {
        interpreter::Value::Integer(i) => println!("{}", i),
        interpreter::Value::Boolean(b) => println!("{}", b),
        interpreter::Value::Unit => println!("Unit"),
    }
}
