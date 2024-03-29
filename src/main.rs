use clap::{Parser, Subcommand, Args};

use compiler::{asm_generator::generate_asm, e2e::run_tests, ir_generator::generate_ir_code, *};


/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct CliArgs {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    P { path: String },
    I { path: String },
    T { path: String },
    Ir { path: String },
    Asm { path: String },
    Dot { path: String },
    Rd { path: String },
    Lv { path: String },
    E2e(E2EArgs),
}

#[derive(Args, Debug)]
struct E2EArgs {
    #[arg(short, long)]
    compiled_only: bool,

    #[arg(short, long)]
    benchmark: bool
}

fn main() {
    let args = CliArgs::parse();

    match &args.command {
        Commands::I { path } => {
            let result = interpret_file(&path);
            match result {
                interpreter::Value::Integer(i) => println!("{}", i),
                interpreter::Value::Boolean(b) => println!("{}", b),
                interpreter::Value::Function(_) => println!("Function"),
                _ => println!("Unit"),
            }
        },
        Commands::P { path } => {
            let module = parse_file(&path);
            println!("{:#?}", module);
        },
        Commands::T { path } => {
            let module = typecheck_file(&path);
            println!("{:#?}", module);
        },
        Commands::Ir { path } => {
            let ir = compile_to_ir(path, true);
            let code = generate_ir_code(ir);
            println!("{}", code);
        },
        Commands::Asm { path } => {
            let ir = compile_to_ir(path, true);
            let asm = generate_asm(ir);
            println!("{}", asm);
        },
        Commands::Dot { path } => {
            let ir = compile_to_ir(path, true);
            let dot = analyzer::ir_to_flowgraph(ir);
            println!("{}", dot);
        },
        Commands::Rd { path } => {
            let ir = compile_to_ir(path, true);
            analyzer::print_reaching_definitions(ir);
        },
        Commands::Lv { path } => {
            let ir = compile_to_ir(path, true);
            analyzer::print_live_vars(ir);
        },
        Commands::E2e(args) => {
            run_tests(args.compiled_only, args.benchmark)
        }
    };
}
