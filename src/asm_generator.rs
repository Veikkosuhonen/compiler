use std::collections::HashMap;

use crate::{ir_generator::{IREntry, IRVar, Instruction}, sym_table::Symbol, tokenizer::Op};

pub fn generate_asm(ir: HashMap<String, Vec<IREntry>>) -> String {
    let functions_code = ir.iter().map(|(fun_name, ir)| {
        generate_function_asm(fun_name, ir)
    }).collect::<Vec<String>>();

    let source = functions_code.join("\n\n        ");

    add_stdlib_code(source)
}

pub fn generate_function_asm(_: &str, fun_ir: &Vec<IREntry>) -> String {
    let mut locals_addresses: HashMap<String, String> = HashMap::new();
    let mut params_backups: Vec<(String, String)> = vec![];
    let mut next_stack_loc = -8;

    let mut add_var = |name: &String| {
        if !locals_addresses.contains_key(name) {
            let address = format!("{next_stack_loc}(%rbp)");
            locals_addresses.insert(name.clone(), address.clone());

            if name.starts_with("p") {
                let param_idx = name.get(1..).map(|n| n.parse::<usize>().unwrap()).unwrap();
                let source_reg = get_argument_register(param_idx);
                params_backups.push((source_reg, address));
            }
            
            next_stack_loc -= 8;
        }
    };

    for entry in fun_ir {
        match &entry.instruction {
            Instruction::LoadIntConst { dest, .. } => add_var(&dest.name),
            Instruction::LoadBoolConst { dest,.. } => add_var(&dest.name),
            Instruction::Copy { source, dest } => {
                add_var(&source.name);
                add_var(&dest.name);
            },
            Instruction::Call { args, dest,.. } => {
                for ir_var in args {
                    add_var(&ir_var.name);
                }
                add_var(&dest.name);
            },
            _ => { /* No vars needed */ }
        }
    }

    // println!("{fun_name}: {:?}", locals_addresses);

    let locals_size = 8 * locals_addresses.len();

    let function_body = fun_ir.iter().map(|entry| {

        let asm = match &entry.instruction {
            Instruction::LoadIntConst { value, dest } => {
                let dest_loc = get_var_address(&dest.name, &locals_addresses);
                vec![
                    format!("movq ${}, {}", value, dest_loc)
                ]
            },
            Instruction::LoadBoolConst { value, dest } => {
                let dest_loc = get_var_address(&dest.name, &locals_addresses);
                let byte_val = if *value { 1 } else { 0 };
                vec![
                    format!("movq ${}, {}", byte_val, dest_loc)
                ]
            },
            Instruction::Call { fun, args, dest } => {
                vec![
                    generate_call(fun, args, dest, &locals_addresses)
                ]
            },
            Instruction::Copy { source, dest } => {
                let src_loc = get_var_address(&source.name, &locals_addresses);
                let src_reg = "%rax";
                let dest_loc = get_var_address(&dest.name, &locals_addresses);

                vec![
                    format!("movq {}, {}", src_loc, src_reg), 
                    format!("movq {}, {}", src_reg, dest_loc)
                ]
            },
            Instruction::Label(name) => {
                vec![
                    format!("{name}:")
                ]
            },
            Instruction::FunctionLabel(name) => {
                let mut lines = vec![
                    format!(".global {name}"),
                    format!(".type {name}, @function"),
                    format!("{name}:"),
                    format!("pushq %rbp"),
                    format!("movq %rsp, %rbp"),
                    format!("subq ${locals_size}, %rsp"),
                    format!("# param backups ({})", params_backups.len()),
                ];
                for (source_reg, dest_address) in &params_backups {
                    lines.push(format!("movq {source_reg}, {dest_address}"));
                }
                lines
            },
            Instruction::Jump(label) => {
                vec![
                    format!("jmp {}", label)
                ]
            },
            Instruction::CondJump { cond, then_label, else_label } => {
                let cond_loc = get_var_address(&cond.name, &locals_addresses);
                vec![
                    format!("cmpq $0, {}", cond_loc),
                    format!("jne {}", then_label), 
                    format!("jmp {}", else_label)
                ]
            },
            Instruction::Return { source } => {
                let source_loc = get_var_address(&source.name, &locals_addresses);
                vec![
                    format!("movq {}, %rax", source_loc)
                ]
            },
        }.join("\n        ");

        format!("# {}\n        {}", entry.to_string(), asm)


    }).collect::<Vec<String>>().join("\n\n        ");

    format!("
        {function_body}

        # Restore stack pointer
        movq %rbp, %rsp
        popq %rbp
        ret
")
}

fn generate_call(fun: &Box<IRVar>, args: &Vec<Box<IRVar>>, dest: &Box<IRVar>, addresses: &HashMap<String, String>) -> String {
    let dest_loc = get_var_address(&dest.name, addresses);
    let callee = match args.len() {
        1 => Op::unary_from_str(&fun.name).map(Symbol::Operator),
        2 => Op::binary_from_str(&fun.name).map(Symbol::Operator),
        _ => Err("")
    }.unwrap_or(Symbol::Identifier(fun.name.clone()));

    match callee {
        Symbol::Operator(op) => {
            let arg_1_loc = get_var_address(&args[0].name, addresses);
            let arg_2_opt = &args.get(1).map(|ir_var| get_var_address(&ir_var.name, addresses));

            if let Some(arg_2_loc) = arg_2_opt {
                match op {
                    Op::Add => {
                        vec![
                            bin_op(&arg_1_loc, arg_2_loc, &dest_loc, "addq")
                        ]
                    },
                    Op::Sub => {
                        vec![
                            bin_op(&arg_1_loc, arg_2_loc, &dest_loc, "subq")
                        ]
                    },
                    Op::Mul => {
                        vec![
                            bin_op(&arg_1_loc, arg_2_loc, &dest_loc, "imulq")
                        ]
                    },
                    Op::Div => {
                        vec![
                            format!("movq {}, %rax", arg_1_loc),
                            format!("cqto"), // wottefok
                            format!("idivq {}", arg_2_loc),
                            format!("movq %rax, {}", dest_loc),    
                        ]
                    },
                    Op::Mod => {
                        vec![
                            format!("movq {}, %rax", arg_1_loc),
                            format!("cqto"), // wottefok
                            format!("idivq {}", arg_2_loc),
                            format!("movq %rdx, {}", dest_loc), // %rdx contains remainder    
                        ]
                    },
                    Op::Equals => {
                        vec![
                            comparison(&arg_1_loc, arg_2_loc, &dest_loc, "sete"),
                        ]
                    },
                    Op::NotEquals => {
                        vec![
                            comparison(&arg_1_loc, arg_2_loc, &dest_loc, "setne"),
                        ]
                    },
                    Op::GT => {
                        vec![
                            comparison(&arg_1_loc, arg_2_loc, &dest_loc, "setg"),
                        ]
                    },
                    Op::GTE => {
                        vec![
                            comparison(&arg_1_loc, arg_2_loc, &dest_loc, "setge"),
                        ]
                    },
                    Op::LT => {
                        vec![
                            comparison(&arg_1_loc, arg_2_loc, &dest_loc, "setl"),
                        ]
                    },
                    Op::LTE => {
                        vec![
                            comparison(&arg_1_loc, arg_2_loc, &dest_loc, "setle"),
                        ]
                    },
                    Op::And => {
                        vec![
                            format!("movq {}, %rax", arg_1_loc),
                            format!("andq {}, %rax", arg_2_loc),
                            format!("movq %rax, {}", dest_loc)
                        ]
                    },
                    Op::Or => {
                        vec![
                            format!("movq {}, %rax", arg_1_loc),
                            format!("orq {}, %rax", arg_2_loc),
                            format!("movq %rax, {}", dest_loc)
                        ]
                    },
                    _ => todo!("{:?}", op)
                }
            } else {
                match op {
                    Op::Not => {
                        vec![
                            format!("movq {}, %rax", arg_1_loc),
                            format!("xorq $0x1, %rax"),
                            format!("movq %rax, {}", dest_loc)
                        ]
                    },
                    Op::UnarySub => {
                        vec![
                            format!("movq {}, %rax", arg_1_loc),
                            format!("negq %rax"),
                            format!("movq %rax, {}", dest_loc)
                        ]
                    },
                    _ => todo!("{:?}", op)
                }
            }
        },
        Symbol::Identifier(name) => {
            match name.as_str() {
                "print_int" => {
                    let arg_loc = get_var_address(&args[0].name, addresses);

                    vec![
                        format!("movq {}, %rsi", arg_loc),
                        format!("movq $print_format, %rdi"),
                        format!("call printf"),
                    ]
                },
                "print_bool" => {
                    let arg_loc = get_var_address(&args[0].name, addresses);

                    vec![
                        format!("movq {}, %rsi", arg_loc),
                        format!("andq $0x1, %rsi"),
                        format!("movq $print_format, %rdi"),
                        format!("call printf"),
                    ]
                },
                "read_int" => {
                    vec![
                        format!("movq $scan_format, %rdi"),
                        format!("leaq {}, %rsi", dest_loc),
                        format!("call scanf"),
                        format!("cmpq $1, %rax"),
                        format!("jne .Lend"),
                    ]
                },
                _ => generate_function_call(&name, args, dest, addresses)
            }
        }
    }.join("\n        ")
}

fn generate_function_call(fun_name: &String, args: &Vec<Box<IRVar>>, dest: &Box<IRVar>, addresses: &HashMap<String, String>) -> Vec<String> {
    let dest = get_var_address(&dest.name, addresses);
    let mut asm: Vec<String> = vec![];
    for (idx, arg) in args.iter().enumerate() {
        let register = get_argument_register(idx);
        let arg_loc = get_var_address(&arg.name, addresses);
        asm.push(format!("movq {}, {}", arg_loc, register));
    }
    asm.push(format!("call {}", fun_name));
    asm.push(format!("movq %rax, {}", dest));
    asm
}

fn get_var_address(name: &String, addresses: &HashMap<String, String>) -> String {
    if name == "U" {
        String::from("$0")
    } else {
        addresses.get(name).expect(format!("Address of var {name} to be defined").as_str()).clone()
    }
}

fn get_argument_register(idx: usize) -> String {
    vec![
        "%rdi",
        "%rsi",
        "%rdx",
        "%rcx",
        "%r8",
        "%r9",
    ].get(idx).expect("Argument idx to be < 6").to_string()
}

fn bin_op(arg_1: &String, arg_2: &String, dest: &String, op: &str) -> String { // todo fix this formatting
    format!("movq {}, %rax \n        {} {}, %rax \n        movq %rax, {}", arg_2, op, arg_1, dest)
}

fn comparison(arg1: &String, arg2: &String, dest: &String, op: &str) -> String {
    vec![
        format!("xor %rax, %rax"),
        format!("movq {}, %rdx", arg1),
        format!("cmpq {}, %rdx", arg2),
        format!("{} %al", op),
        format!("movq %rax, {}", dest),
    ].join("\n        ")
}

fn add_stdlib_code(source: String) -> String {
    format!("
# Metadata for debuggers and other tools
.extern printf
.extern scanf

.section .text  # Begins code and data

{}

# Actual end
.Lend:
        # Return from main with status code 0
        movq $0, %rax
        movq %rbp, %rsp
        popq %rbp
        ret

# String data that we pass to functions 'scanf' and 'printf'
scan_format:
.asciz \"%ld\"

print_format:
.asciz \"%ld\\n\"

print_bool_format:
.asciz \"%s\\n\"

true_str:
    .ascii \"true\\n\"

false_str:
    .ascii \"false\\n\"

", source)
}