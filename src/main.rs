use std::env;
use compiler::{asm_generator::generate_asm, e2e::run_tests, ir_generator::{generate_ir, generate_ir_code}, *};

fn main() {
    let args: Vec<String> = env::args().collect();

    let cmd = &args[1];
    let path = &args.get(2);

    if let Some(path) = path {
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
            "asm" => {
                let typed_ast = typecheck_file(path);
                let ir = generate_ir(typed_ast);
                let asm = generate_asm(ir);
                println!("{}", asm);
            },
            _ => panic!("Unknown command {}", cmd)
        }
    } else {
        match cmd.as_str() {
            "e2e" => {
                run_tests()
            },
            _ => panic!("Unknown command {}", cmd)
        }
    }
}
