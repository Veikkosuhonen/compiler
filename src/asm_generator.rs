use std::collections::HashMap;

use crate::ir_generator::{IREntry, Instruction};

fn bin_op(arg_1: &String, arg_2: &String, dest: &String, op: &str) -> String {
    format!("movq {}, %rax \n{} {}, %rax \nmovq %rax, {}", arg_2, op, arg_1, dest)
}

pub fn generate_asm(ir: Vec<IREntry>) -> String {

    let mut locations = HashMap::new();
    let mut next_stack_loc = -8;

    let mut add_var = |name: &String| {
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
                format!("movq ${}, {}", value, dest_loc)
            },
            Instruction::LoadBoolConst { value, dest } => {
                let dest_loc = locations.get(&dest.name).unwrap();
                let byte_val = if *value { 1 } else { 0 };
                format!("movq ${}, {}", byte_val, dest_loc)
            },
            Instruction::Call { fun, args, dest } => {
                match fun.name.as_str() {
                    "+" => {
                        let dest_loc = locations.get(&dest.name).unwrap();
                        let arg_1_loc = locations.get(&args[0].name).unwrap();
                        let arg_2_loc = locations.get(&args[1].name).unwrap();

                        bin_op(arg_1_loc, arg_2_loc, dest_loc, "addq")
                    },
                    _ => todo!("{}", fun.name)
                }
            },
            Instruction::Copy { source, dest } => {
                let src_loc = locations.get(&source.name).unwrap();
                let src_reg = "%rax";
                let dest_loc = locations.get(&dest.name).unwrap();

                format!("movq {}, {} \nmovq {}, {}", src_loc, src_reg, src_reg, dest_loc)
            },
            Instruction::Label(name) => {
                format!(".{}:", name)
            },
                Instruction::Jump(label) => {
                format!("jmp .{}", label)
            },
            Instruction::CondJump { cond, then_label, else_label } => {
                let cond_loc = locations.get(&cond.name).unwrap();
                format!("cmpq $0, {}\njne .{}\njmp .{}", cond_loc, then_label, else_label)
            },
            Instruction::Return => {
                format!("# return")
            },
        };

        format!("# {:?}\n{}", entry.instruction, asm)


    }).collect::<Vec<String>>().join("\n");

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
