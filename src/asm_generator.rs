use std::collections::HashMap;

use crate::{ir_generator::{IREntry, IRVar, Instr}, sym_table::Symbol, tokenizer::Op, type_checker::Type};

pub fn generate_asm(ir: HashMap<String, Vec<IREntry>>) -> String {
    let functions_code = ir.iter().map(|(fun_name, ir)| {
        generate_function_asm(fun_name, ir)
    }).collect::<Vec<String>>();

    let source = functions_code.join("\n\n        ");

    add_stdlib_code(source)
}

enum Address {
    Register(String),
    Memory(i32),
}

impl Address {
    fn to_string(&self) -> String {
        match self {
            Address::Register(reg) => reg.clone(),
            Address::Memory(addr) => format!("{}(%rbp)", addr)
        }
    }
}

pub fn generate_function_asm(fun_name: &str, fun_ir: &Vec<IREntry>) -> String {
    let mut locals: HashMap<String, (Address, Type)> = HashMap::new();
    let mut stack_size: usize = 0;

    let mut add_var = |var: &IRVar| {
        // println!("IRVar = {:?}", var);
        if locals.contains_key(&var.name) {
        //if let Some((Address::Memory(base_addr), var_type)) = locals.get(&var.name) {
            // This may be a member reference
            // if let Some(member_name) = &var.field {
            //     if let Type::Struct(struct_type) = var_type {
            //         let (member_offset, member_type) = struct_type.get_member(&member_name);
            //         let address = *base_addr + member_offset as i32 * 8;
            //         locals.insert(format!("{}.{}", var.name, member_name), (Address::Memory(address), member_type));
            //     } else if let Type::Pointer(inner_type) = var_type {
            //         if let Type::Struct(struct_type) = inner_type.as_ref() {
            //             let (member_offset, member_type) = struct_type.get_member(&member_name);
            //             let address = *base_addr + member_offset as i32 * 8;
            //             println!("{}.{member_name} is at {address}", var.name);
            //             locals.insert(format!("{}.{}", var.name, member_name), (Address::Memory(address), member_type));
            //         } else {
            //             panic!("IR has a member access to non-struct")
            //         }
            //     } else {
            //         panic!("IR has a member access to non-struct")
            //     }
            // }
        } else if var.name == "_return" {
            locals.insert(var.name.clone(), (Address::Register("%rax".to_string()), var.var_type.clone()));
        } else {
            stack_size += var.size();
            let address = -(8 * (stack_size - 1) as i32);
            locals.insert(var.name.clone(), (Address::Memory(address), var.var_type.clone()));
            // println!("{} is at {address} with size {}", var.name, var.size())
        }
    };

    for entry in fun_ir {
        match &entry.instruction {
            Instr::LoadIntConst { dest, .. } => add_var(&dest),
            Instr::LoadBoolConst { dest,.. } => add_var(&dest),
            Instr::Copy { source, dest, .. } => {
                add_var(&source);
                add_var(&dest);
            },
            Instr::Call { args, dest,.. } => {
                for ir_var in args {
                    add_var(&ir_var);
                }
                add_var(&dest);
            },
            Instr::Declare { var } => add_var(&var),
            _ => { /* no vars */}
        }
    }

    // align to 16 bytes
    let locals_size = 8 * (stack_size + stack_size % 2);

    let function_body = fun_ir.iter().map(|entry| {

        let asm = match &entry.instruction {
            Instr::FunctionLabel { name, params } => {
                let mut lines = vec![
                    format!(".global {name}"),
                    format!(".type {name}, @function"),
                    format!("{name}:"),
                    format!("pushq %rbp"),
                    format!("movq %rsp, %rbp"),
                    format!("subq ${locals_size}, %rsp"),
                    format!("# param backups ({})", params.len()),
                ];
                for (idx, param) in params.iter().enumerate() {
                    let param_reg = get_argument_register(idx);
                    let (local_address,_) = locals.get(&param.name).expect(format!("In {fun_name}, param {} is not defined", param.name).as_str());
                    lines.push(format!("movq {param_reg}, {}", local_address.to_string()));
                }
                lines
            },
            Instr::LoadIntConst { value, dest } => {
                let (dest_loc, mut lines) = get_var_address(&dest, &locals);
                lines.push(format!("movq ${}, {}", value, dest_loc));
                lines
            },
            Instr::LoadBoolConst { value, dest } => {
                let (dest_loc, mut lines) = get_var_address(&dest, &locals);
                let byte_val = if *value { 1 } else { 0 };
                lines.push(format!("movq ${}, {}", byte_val, dest_loc));
                lines
            },
            Instr::Call { fun, args, dest } => {
                vec![
                    generate_call(fun, args, dest, &locals)
                ]
            },
            Instr::Copy { source, dest } => {
                let (src_loc, mut lines) = get_var_address(&source, &locals);
                let (dest_loc, arg_lines) = get_var_address(&dest, &locals);
                lines.extend(arg_lines);

                lines.extend(copy(&src_loc, &dest_loc));
                lines
            },
            Instr::Label(name) => {
                vec![
                    format!("{name}:")
                ]
            },
            Instr::Jump(label) => {
                vec![
                    format!("jmp {}", label)
                ]
            },
            Instr::CondJump { cond, then_label, else_label } => {
                let (cond_loc, mut lines) = get_var_address(&cond, &locals);
                
                lines.push(format!("cmpq $0, {}", cond_loc));
                lines.push(format!("jne {}", then_label));
                lines.push(format!("jmp {}", else_label));
                lines
            },
            Instr::Declare { .. } => { vec![] },
        }.join("\n        ");

        format!("# {}\n        {}", entry.to_string(), asm)


    }).collect::<Vec<String>>().join("\n\n        ");

    format!("
        {function_body}
        # Restore stack pointer
        movq %rbp, %rsp
        popq %rbp
        ret
    ", )
}

fn generate_call(fun: &Box<IRVar>, args: &Vec<Box<IRVar>>, dest: &Box<IRVar>, addresses: &HashMap<String, (Address, Type)>) -> String {
    let (dest_loc, mut lines) = get_var_address(&dest, addresses);
    let callee = match args.len() {
        1 => Op::unary_from_str(&fun.name).map(Symbol::Operator),
        2 => Op::binary_from_str(&fun.name).map(Symbol::Operator),
        _ => Err("")
    }.unwrap_or(Symbol::Identifier(fun.name.clone()));

    match callee {
        Symbol::Operator(op) => {
            let arg_1 = &args[0];
            let (arg_1_loc, arg_lines) = get_var_address(&arg_1, addresses);
            lines.extend(arg_lines);
            let arg_2_opt = args.get(1).map(|ir_var| get_var_address(&ir_var, addresses));

            if let Some((arg_2_loc, arg_lines)) = arg_2_opt {
                lines.extend(arg_lines);
    
                lines.extend(match op {
                    Op::Add => {
                        vec![
                            bin_op(&arg_1_loc, &arg_2_loc, &dest_loc, "addq")
                        ]
                    },
                    Op::Sub => {
                        vec![
                            bin_op(&arg_1_loc, &arg_2_loc, &dest_loc, "subq")
                        ]
                    },
                    Op::Mul => {
                        vec![
                            bin_op(&arg_1_loc, &arg_2_loc, &dest_loc, "imulq")
                        ]
                    },
                    Op::Div => {
                        vec![
                            mov(&arg_1_loc, "%rax"),
                            format!("cqto"), // wottefok
                            format!("idivq {}", arg_2_loc),
                            mov("%rax", &dest_loc),   
                        ]
                    },
                    Op::Mod => {
                        vec![
                            mov(&arg_1_loc, "%rax"),
                            format!("cqto"), // wottefok
                            format!("idivq {}", arg_2_loc),
                            mov("%rdx", &dest_loc),   // %rdx contains remainder    
                        ]
                    },
                    Op::Equals => {
                        vec![
                            comparison(&arg_1_loc, &arg_2_loc, &dest_loc, "sete"),
                        ]
                    },
                    Op::NotEquals => {
                        vec![
                            comparison(&arg_1_loc, &arg_2_loc, &dest_loc, "setne"),
                        ]
                    },
                    Op::GT => {
                        vec![
                            comparison(&arg_1_loc, &arg_2_loc, &dest_loc, "setg"),
                        ]
                    },
                    Op::GTE => {
                        vec![
                            comparison(&arg_1_loc, &arg_2_loc, &dest_loc, "setge"),
                        ]
                    },
                    Op::LT => {
                        vec![
                            comparison(&arg_1_loc, &arg_2_loc, &dest_loc, "setl"),
                        ]
                    },
                    Op::LTE => {
                        vec![
                            comparison(&arg_1_loc, &arg_2_loc, &dest_loc, "setle"),
                        ]
                    },
                    _ => panic!("{:?} does not have an intrinsic definition", op)
                });
            } else {
                lines.extend(match op {
                    Op::Not => {
                        vec![
                            mov(&arg_1_loc, "%rax"),
                            format!("xorq $0x1, %rax"),
                            mov("%rax", &dest_loc),
                        ]
                    },
                    Op::UnarySub => {
                        vec![
                            mov(&arg_1_loc, "%rax"),
                            format!("negq %rax"),
                            mov("%rax", &dest_loc),
                        ]
                    },
                    Op::AddressOf => {
                        vec![
                            format!("leaq {arg_1_loc}, %rax"),
                            mov("%rax", &dest_loc),
                        ]
                    },
                    Op::Deref => {
                        vec![
                            mov(&arg_1_loc, "%rax"),
                            mov("(%rax)", "%rax"),
                            mov("%rax", &dest_loc),
                        ]
                    },
                    Op::New => {
                        let base_addr = match addresses.get(&arg_1.name).expect(format!("Address of var {} to be defined", arg_1.name).as_str()).0 {
                            Address::Memory(addr) => addr,
                            Address::Register(_) => panic!("Called new on a value in register")
                        };
                        let mut mm = vec![
                            mov(format!("${}", arg_1.size() * 8).as_str(), "%rdi"),
                            format!("call malloc"),
                            format!("test %rax, %rax"),
                            format!("jz .Lend"),
                        ];
                        for offset in 0..arg_1.size() {
                            mm.append(&mut copy(
                                &format!("{}(%rbp)", base_addr + offset as i32 * 8),
                                "%rax",
                            ));
                        }
                        mm.push(format!("{} # Move address of {} to {}", mov("%rax", &dest_loc), arg_1.name, dest.name));
                        mm
                    },
                    Op::Delete => {
                        vec![
                            mov(&arg_1_loc, "%rdi"),
                            format!("call free"),
                        ]
                    },
                    _ => todo!("{:?}", op)
                });
            }
        },
        Symbol::Identifier(name) => {
            lines.extend(match name.as_str() {
                "print_int" => {
                    let (arg_loc, mut lines) = get_var_address(&args[0], addresses);

                    lines.push(mov(&arg_loc, "%rsi"));
                    lines.push(format!("movq $print_format, %rdi"));
                    lines.push(format!("call printf"));
                    lines                    
                },
                "print_bool" => {
                    let (arg_loc, mut lines) = get_var_address(&args[0], addresses);

                    lines.push(mov(&arg_loc, "%rsi"));
                    lines.push(format!("andq $0x1, %rsi"));
                    lines.push(format!("movq $print_format, %rdi"));
                    lines.push(format!("call printf"));
                    lines
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
            })
        }
    };
    
    lines.join("\n        ")
}

fn generate_function_call(fun_name: &String, args: &Vec<Box<IRVar>>, dest: &Box<IRVar>, addresses: &HashMap<String, (Address, Type)>) -> Vec<String> {
    let (dest, mut lines) = get_var_address(&dest, addresses);
    for (idx, arg) in args.iter().enumerate() {
        let register = get_argument_register(idx);
        let (arg_loc, arg_lines) = get_var_address(&arg, addresses);
        lines.extend(arg_lines);
        lines.push(mov(&arg_loc,&register));
    }
    lines.push(format!("call {}", fun_name));
    lines.push(mov("%rax", &dest));
    lines
}

fn get_var_address(var: &IRVar, addresses: &HashMap<String, (Address, Type)>) -> (String, Vec<String>) {
    if var.name == "U" {
        (String::from("$0"), vec![])
    } else {
        let (address, var_type) = addresses.get(&var.name).expect(format!("Address of var {} to be defined", var.name).as_str());
        match address {
            Address::Register(reg) => (reg.clone(), vec![]),
            Address::Memory(addr) => {
                // Special case if were referring to a field of a struct
                match var_type {
                    // Pointer to struct
                    Type::Pointer(pointer_type) => {
                        if let Type::Struct(struct_type) = pointer_type.as_ref() {
                            if let Some(field) = &var.field {
                                let mut lines = vec![];
                                let address_reg = String::from("%rdx");
                                // Put struct base pointer in %rdx
                                lines.push(mov(&format!("{addr}(%rbp)"), &address_reg));
                                // Get offset (multiplied by 8) and add it to the base pointer
                                let (offset,_) = struct_type.get_member(&field);
                                if offset != 0 {
                                    lines.push(format!("addq ${}, {address_reg}", offset * 8));
                                }

                                return (format!("({address_reg})"), lines)
                            }
                        }
                    },
                    // Direct struct
                    Type::Struct(struct_type) => {
                        if let Some(field) = &var.field {
                            let (offset,_) = struct_type.get_member(&field);
                            let addr = *addr + offset as i32 * 8;
                            return (format!("{addr}(%rbp)"), vec![])
                        }
                    },
                    _ => {},
                }
                (format!("{}(%rbp)", addr), vec![])
            }
        }
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
    format!("movq {}, %rax \n        {} {}, %rax \n        movq %rax, {}", arg_1, op, arg_2, dest)
}

fn copy(arg1: &str, arg2: &str) -> Vec<String> {
    let mut lines = vec![];
    let tmp = String::from("%rax");
    lines.push(mov(arg1, &tmp));
    lines.push(mov(&tmp, arg2));
    lines
}

fn mov(arg1: &str, arg2: &str) -> String {
    if arg1 != arg2 {
        return format!("movq {arg1}, {arg2}")
    }
    format!("# skip movq {arg1}, {arg2}")
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
.extern malloc
.extern free

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

#[cfg(test)]
mod tests {
    use crate::{ir_generator::generate_ir, parse_source, type_checker::typecheck_program};

    use super::generate_asm;

    #[test]
    fn regression_1() {
        let ir = generate_ir(
            typecheck_program(
                parse_source(String::from("
                fun identity(x: Int): Int {
                    x
                }
                
                fun tuplaa(x: Int): Int {
                    identity(x) + identity(x)
                }
                
                print_int(tuplaa(69));
                "))
            )
        );

        generate_asm(ir);
    }

    #[test]
    fn structs() {
        let ir = generate_ir(
            typecheck_program(
                parse_source(String::from("
                struct Dog { size: Int, isHungry: Bool }
                var doggo = new Dog { isHungry: false, size: 100 };
                doggo.isHungry = true;
                "))
            )
        );

        println!("{}", ir.get("main").unwrap().iter().map(|i| i.to_string()).collect::<Vec<String>>().join("\n"));

        println!("{}", generate_asm(ir));
    }
}
