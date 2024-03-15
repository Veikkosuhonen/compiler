use std::{fs, time::Instant};

use parser::{Module, Struct, UserDefinedFunction};
use report_error::{report_syntax_error, report_tokenisation_error};
use type_checker::{TypedStruct, TypedUserDefinedFunction};
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
pub mod analyzer;

fn read_file(path: &String) -> String {
    let contents = fs::read_to_string(path)
        .expect("Should have been able to read the file");
    contents
}

fn parse_source(contents: String) -> Module<UserDefinedFunction, Struct> {
    let tokens = tokenizer::tokenize(&contents)
        .or_else(|err| { report_tokenisation_error(&contents, &err); Err(err) })
        .expect("Should've been able to tokenize the source");

    let module = parser::parse(tokens)
        .or_else(|err| { report_syntax_error(&contents, &err); Err(err) })
        .expect("Should've been able to parse the source");

    module
}

pub fn parse_file(path: &String) -> Module<UserDefinedFunction, Struct> {
    let contents = read_file(path);
    let _start = Instant::now();
    let module = parse_source(contents);
    // eprintln!("Tokenize & parse in {} ms", _start.elapsed().as_millis());
    module
}

pub fn typecheck_file(path: &String) -> Module<TypedUserDefinedFunction, TypedStruct> {
    let module = parse_file(path);
    let result = type_checker::typecheck_program(module);

    result
}

pub fn interpret_file(path: &String) -> interpreter::Value {
    let module = typecheck_file(path);
    let _start = Instant::now();
    let result = interpreter::interpret_program(&module);
    // eprintln!("Interpret in {} ms", _start.elapsed().as_millis());

    result
}
