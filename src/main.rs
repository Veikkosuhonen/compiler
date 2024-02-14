use clap::{Parser, Subcommand};

use compiler::{asm_generator::generate_asm, e2e::run_tests, ir_generator::{generate_ir, generate_ir_code}, *};


/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    I { path: String },
    Ir { path: String },
    Asm { path: String },
    E2e,
}

fn main() {
    let args = Args::parse();

    match &args.command {
        Commands::I { path } => {
            let result = interpret_file(&path);

            match result {
                interpreter::Value::Integer(i) => println!("{}", i),
                interpreter::Value::Boolean(b) => println!("{}", b),
                interpreter::Value::Function(_) => println!("Function"),
                interpreter::Value::Unit => println!("Unit"),
            }
        },
        Commands::Ir { path } => {
            let typed_ast = typecheck_file(&path);
            let ir = generate_ir(typed_ast);
            let code = generate_ir_code(ir);
            println!("{}", code);
        },
        Commands::Asm { path } => {
            let typed_ast = typecheck_file(&path);
            let ir = generate_ir(typed_ast);
            let asm = generate_asm(ir);
            println!("{}", asm);
        },
        Commands::E2e => {
            run_tests()
        }
    };
}
