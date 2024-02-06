use std::env;
use compiler::{ir_generator::{generate_ir, generate_ir_code}, *};

fn main() {
    let args: Vec<String> = env::args().collect();

    let cmd = &args[1];
    let path = &args[2];

    match cmd.as_str() {
        "i" => {
            let result = interpret_file(path);

            match result {
                interpreter::Value::Integer(i) => println!("{}", i),
                interpreter::Value::Boolean(b) => println!("{}", b),
                interpreter::Value::Function(_) => println!("Function"),
                interpreter::Value::Unit => println!("Unit"),
            }
        },
        "ir" => {
            let typed_ast = typecheck_file(path);
            let ir = generate_ir(typed_ast);
            let code = generate_ir_code(ir);
            println!("{}", code);
        },
        _ => panic!("Unknown command {}", cmd)
    }
}
