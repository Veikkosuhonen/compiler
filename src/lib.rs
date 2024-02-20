use std::fs;

use parser::Module;
pub mod tokenizer;
pub mod parser;
pub mod interpreter;
pub mod ir_generator;
pub mod sym_table;
pub mod type_checker;
pub mod builtin_functions;
pub mod asm_generator;
pub mod e2e;

fn read_file(path: &String) -> String {
    let contents = fs::read_to_string(path)
        .expect("Should have been able to read the file");
    contents
}

fn parse_source(contents: String) -> Module {
    let tokens = tokenizer::tokenize(&contents);
    let module = parser::parse(tokens);
    module
}

pub fn interpret_file(path: &String) -> interpreter::Value {
    let contents = read_file(path);
    let module = parse_source(contents);
    let result = interpreter::interpret_program(&module);

    result
}

pub fn typecheck_file(path: &String) -> type_checker::TypedASTNode {
    let contents = read_file(path);
    let module = parse_source(contents);
    let result = type_checker::typecheck_program(module);

    result
}
