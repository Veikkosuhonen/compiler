pub mod tokenizer;
pub mod parser;
use std::env;
use std::fs;


fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];

    println!("Searching for {}", file_path);

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    let result = tokenizer::tokenize(&contents);

    // Print tokens
    println!("Tokens:");
    for token in result {
        println!("{:?}: {}", token.token_type, token.value);
    }

    println!("With text:\n{contents}");
}
