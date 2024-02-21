use std::fs;

use interpreter::UserDefinedFunction;
use parser::{ASTNode, Module};
use report_error::{report_syntax_error, report_tokenisation_error};
use type_checker::{TypedASTNode, TypedUserDefinedFunction};
pub mod tokenizer;
pub mod parser;
pub mod interpreter;
pub mod ir_generator;
pub mod sym_table;
pub mod report_error;
pub mod type_checker;
pub mod builtin_functions;
pub mod asm_generator;
pub mod e2e;

fn read_file(path: &String) -> String {
    let contents = fs::read_to_string(path)
        .expect("Should have been able to read the file");
    contents
}

fn parse_source(contents: String) -> Module<UserDefinedFunction, ASTNode> {
    let tokens = tokenizer::tokenize(&contents)
        .or_else(|err| { report_tokenisation_error(&contents, &err); Err(err) })
        .expect("Should've been able to tokenize the source");

    let module = parser::parse(tokens)
        .or_else(|err| { report_syntax_error(&contents, &err); Err(err) })
        .expect("Should've been able to parse the source");
    
    module
}

pub fn interpret_file(path: &String) -> interpreter::Value {
    let contents = read_file(path);
    let module = parse_source(contents);
    let result = interpreter::interpret_program(&module);

    result
}

pub fn typecheck_file(path: &String) -> Module<TypedUserDefinedFunction, TypedASTNode> {
    let contents = read_file(path);
    let module = parse_source(contents);
    let result = type_checker::typecheck_program(module);

    result
}
