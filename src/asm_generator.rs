use std::collections::HashMap;

use crate::{ir_generator::{IREntry, Instruction}, sym_table::Symbol, tokenizer::Op};

fn bin_op(arg_1: &String, arg_2: &String, dest: &String, op: &str) -> String {
    format!("movq {}, %rax \n{} {}, %rax \nmovq %rax, {}", arg_2, op, arg_1, dest)
}

fn comparison(arg1: &String, arg2: &String, dest: &String, op: &str) -> String {
    vec![
        format!("xor %rax, %rax"),
        format!("movq {}, %rdx", arg1),
        format!("cmpq {}, %rdx", arg2),
        format!("{} %al", op),
        format!("movq %rax, {}", dest),
    ].join("\n")
}

pub fn generate_asm(ir: Vec<IREntry>) -> String {

    let mut locations = HashMap::new();
    let mut next_stack_loc = -8;

    let mut add_var = |name: &Symbol| {
        if !locations.contains_key(name) {
            locations.insert(name.clone(), format!("{next_stack_loc}(%rbp)"));
            next_stack_loc -= 8;
        }
    };

    for entry in &ir {
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

    let locals_size = 8 * locations.len();

    let function_code = ir.iter().map(|entry| {

        let asm = match &entry.instruction {
            Instruction::LoadIntConst { value, dest } => {
                let dest_loc = locations.get(&dest.name).unwrap();
                vec![
                    format!("movq ${}, {}", value, dest_loc)
                ]
            },
            Instruction::LoadBoolConst { value, dest } => {
                let dest_loc = locations.get(&dest.name).unwrap();
                let byte_val = if *value { 1 } else { 0 };
                vec![
                    format!("movq ${}, {}", byte_val, dest_loc)
                ]
            },
            Instruction::Call { fun, args, dest } => {
                let dest_loc = locations.get(&dest.name).unwrap();

                match &fun.name {
                    Symbol::Operator(op) => {
                        let arg_1_loc = locations.get(&args[0].name).unwrap();
                        let arg_2_opt = &args.get(1).and_then(|irvar| locations.get(&irvar.name));

                        if let Some(arg_2_loc) = arg_2_opt {
                            match op {
                                Op::Add => {
                                    vec![
                                        bin_op(arg_1_loc, arg_2_loc, dest_loc, "addq")
                                    ]
                                },
                                Op::Sub => {
                                    vec![
                                        bin_op(arg_1_loc, arg_2_loc, dest_loc, "subq")
                                    ]
                                },
                                Op::Mul => {
                                    vec![
                                        bin_op(arg_1_loc, arg_2_loc, dest_loc, "imulq")
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
                                        comparison(arg_1_loc, arg_2_loc, dest_loc, "sete"),
                                    ]
                                },
                                Op::NotEquals => {
                                    vec![
                                        comparison(arg_1_loc, arg_2_loc, dest_loc, "setne"),
                                    ]
                                },
                                Op::GT => {
                                    vec![
                                        comparison(arg_1_loc, arg_2_loc, dest_loc, "setg"),
                                    ]
                                },
                                Op::GTE => {
                                    vec![
                                        comparison(arg_1_loc, arg_2_loc, dest_loc, "setge"),
                                    ]
                                },
                                Op::LT => {
                                    vec![
                                        comparison(arg_1_loc, arg_2_loc, dest_loc, "setl"),
                                    ]
                                },
                                Op::LTE => {
                                    vec![
                                        comparison(arg_1_loc, arg_2_loc, dest_loc, "setle"),
                                    ]
                                },
                                _ => todo!("{:?}", op)
                            }
                        } else {
                            let arg_loc = locations.get(&args[0].name).unwrap();

                            match op {
                                Op::Not => {
                                    vec![
                                        format!("movq {}, %rax", arg_loc),
                                        format!("xorg 0x1, %rax"),
                                        format!("movq %rax, {}", dest_loc)
                                    ]
                                },
                                Op::Sub => {
                                    vec![
                                        format!("movq {}, %rax", arg_loc),
                                        format!("negq %rax"),
                                        format!("movq %rax, {}", dest_loc)
                                    ]
                                },
                                _ => todo!("{:?}", op)
                            }
                        }
                    },
                    Symbol::Identifier(name) => {
                        todo!("{}", name)
                    }
                }
            },
            Instruction::Copy { source, dest } => {
                let src_loc = locations.get(&source.name).unwrap();
                let src_reg = "%rax";
                let dest_loc = locations.get(&dest.name).unwrap();

                vec![
                    format!("movq {}, {}", src_loc, src_reg), 
                    format!("movq {}, {}", src_reg, dest_loc)
                ]
            },
            Instruction::Label(name) => {
                vec![
                    format!(".{}:", name)
                ]
            },
            Instruction::Jump(label) => {
                vec![
                    format!("jmp .{}", label)
                ]
            },
            Instruction::CondJump { cond, then_label, else_label } => {
                let cond_loc = locations.get(&cond.name).unwrap();
                vec![
                    format!("cmpq $0, {}", cond_loc),
                    format!("jne .{}", then_label), 
                    format!("jmp .{}", else_label)
                ]
            },
            Instruction::Return => {
                vec![
                    format!("# return")
                ]
            },
        }.join("\n");

        format!("# {:?}\n{}", entry.instruction, asm)


    }).collect::<Vec<String>>().join("\n\n");

    format!("
# Metadata for debuggers and other tools
.global main
.type main, @function
.extern printf
.extern scanf

.section .text  # Begins code and data

# Label that marks beginning of main function
main:
# Function stack setup
pushq %rbp
movq %rsp, %rbp
subq ${}, %rsp

{}

movq -24(%rbp), %rsi

# Call function 'printf(\"%ld\\n\", %rsi)'
# to print the number in %rsi.
movq $print_format, %rdi
call printf

# Labels starting with \".L\" are local to this function,
# i.e. another function than \"main\" could have its own \".Lend\".
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
", locals_size, function_code)

}
