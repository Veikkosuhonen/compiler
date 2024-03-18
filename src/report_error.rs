use crate::{analyzer::Warning, parser::SyntaxError, tokenizer::TokenisationError};

pub fn report_tokenisation_error(source: &String, error: &TokenisationError) {
    let line = error.location.line;
    let line_idx = line.checked_sub(1).unwrap_or(0);
    let col = error.location.column;
    let source_line = source.split("\n").nth(line_idx).unwrap_or("");
    let mut caret = String::new();
    for _ in 0..col {
        caret.push(' ');
    }
    caret.push('^');
    eprintln!("Tokenisation error at line {}:\n{}\n{}\n{}", line, source_line, caret, error.message);
}

pub fn report_syntax_error(source: &String, error: &SyntaxError) {
    let start_line = error.start.line;
    let line_idx = start_line.checked_sub(1).unwrap_or(0);
    let start_col = error.start.column;
    let end_col = error.end.column;
    let line = source.split("\n").nth(line_idx).unwrap_or("");
    let mut caret = String::new();
    for _ in 0..start_col - 1 {
        caret.push(' ');
    }
    for _ in start_col - 1..end_col - 1 {
        caret.push('^');
    }
    eprintln!("Syntax error at line {}:\n{}\n{}\n{}", start_line, line, caret, error.message);
}

pub fn report_warning(source: &String, warning: &Warning) {
    let start_line = warning.span.start.line;
    let line_idx = start_line.checked_sub(1).unwrap_or(0);
    let start_col = warning.span.start.column;
    let end_col = warning.span.end.column;
    let line = source.split("\n").nth(line_idx).unwrap_or("");
    let mut caret = String::new();
    for _ in 0..start_col - 1 {
        caret.push(' ');
    }
    for _ in start_col - 1..end_col - 1 {
        caret.push('^');
    }
    eprintln!("Warning, line {}: \n{}\n{}\n{}\n", start_line, line, caret, warning.message);
}

