use std::fs;

use parser::Expression;
pub mod tokenizer;
pub mod parser;
pub mod interpreter;
pub mod ir_generator;
pub mod sym_table;
pub mod type_checker;
pub mod builtin_functions;

fn read_file(path: &String) -> String {
    let contents = fs::read_to_string(path)
        .expect("Should have been able to read the file");
    contents
}

fn parse_source(contents: String) -> Expression {
    let tokens = tokenizer::tokenize(&contents);
    let expression = parser::parse(tokens);
    expression
}

pub fn interpret_file(path: &String) -> interpreter::Value {
    let contents = read_file(path);
    let expression = parse_source(contents);
    let result = interpreter::interpret_program(expression);

    result
}

pub fn typecheck_file(path: &String) -> type_checker::Type {
    let contents = read_file(path);
    let expression = parse_source(contents);
    let result = type_checker::typecheck_program(expression);

    result
}
