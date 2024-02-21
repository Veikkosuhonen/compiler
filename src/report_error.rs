use crate::{parser::SyntaxError, tokenizer::TokenisationError};

pub fn report_tokenisation_error(source: &String, error: &TokenisationError) {
    let line = error.location.line;
    let col = error.location.column;
    let source_line = source.split("\n").nth(line - 1).unwrap();
    let mut caret = String::new();
    for _ in 0..col {
        caret.push(' ');
    }
    caret.push('^');
    println!("Tokenisation error at line {}:\n{}\n{}\n{}", line, source_line, caret, error.message);
}

pub fn report_syntax_error(source: &String, error: &SyntaxError) {
    let start_line = error.start.line;
    let start_col = error.start.column;
    let end_col = error.end.column;
    let line = source.split("\n").nth(start_line - 1).unwrap();
    let mut caret = String::new();
    for _ in 0..start_col {
        caret.push(' ');
    }
    for _ in start_col..end_col {
        caret.push('^');
    }
    println!("Syntax error at line {}:\n{}\n{}\n{}", start_line, line, caret, error.message);
}

