use std::fs;
pub mod tokenizer;
pub mod parser;
pub mod interpreter;
pub mod ir_generator;
pub mod sym_table;

pub fn interpret_file(path: &String) -> interpreter::Value {
    let contents = fs::read_to_string(path)
        .expect("Should have been able to read the file");

    let tokens = tokenizer::tokenize(&contents);
    let expression = parser::parse(tokens);
    let result = interpreter::interpret_program(expression);

    result
}
