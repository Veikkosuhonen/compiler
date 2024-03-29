use std::collections::HashMap;

use crate::{ir_generator::{IREntry, IRVar, Instr}, lang_type::Type, sym_table::Symbol, tokenizer::Op};

pub fn generate_asm(ir: HashMap<String, Vec<IREntry>>) -> String {
    let functions_code = ir.iter().map(|(fun_name, ir)| {
        generate_function_asm(fun_name, ir)
    }).collect::<Vec<String>>();

    let source = functions_code.join("\n\n        ");

    add_stdlib_code(source)
}

#[derive(Clone)]
enum Address {
    Register(String),
    RegisterPointer((String, i64)),
    Memory(i64),
}

impl Address {
    fn to_string(&self) -> String {
        match self {
            Address::Register(reg) => reg.clone(),
            Address::RegisterPointer((reg, offset)) => format!("{}({})", offset, reg),
            Address::Memory(addr) => format!("{}(%rbp)", addr),
        }
    }
}

fn add_var(var: &IRVar, locals: &mut HashMap<String, (Address, Type)>, stack_size: &mut usize, lines: &mut Vec<String>) {
    // eprintln!("Defining {}", var.to_string());
    if let Some(parent) = &var.parent {
        add_var(parent.as_ref(), locals, stack_size, lines);
        return;
    }

    if locals.contains_key(&var.name) {
        return
    }
    
    if var.name == "_return" {
        locals.insert(var.name.clone(), (Address::Register("%rax".to_string()), var.var_type.clone()));
    } else {
        *stack_size += var.size();
        let address = Address::Memory(-(8 * *stack_size as i64));

        // Is this a named function? If so, we need to now put its address into the stack so we can use it as a value.
        // (in contrast, if it is an anon function, its address is already on the stack or in arg register)
        if let Type::Function { id: Some(id),.. } = &var.var_type {
            lines.push(format!("movq ${id}, {} # Save address of module function '{id}' to var '{}'", address.to_string(), var.name))
        }

        locals.insert(var.name.clone(), (address, var.var_type.clone()));
    }
}

fn get_address_of_var<'a>(irvar: &'a IRVar, locals: &'a mut HashMap<String, (Address, Type)>) -> Option<&'a (Address, Type)> {
    if let Some(parent) = &irvar.parent {
        get_address_of_var(&parent, locals)
    } else {
        // eprintln!("Getting {} from {:?}", irvar.name, locals.keys());
        locals.get(&irvar.name)
    }
}

fn generate_function_asm(fun_name: &str, fun_ir: &Vec<IREntry>) -> String {
    let mut locals: HashMap<String, (Address, Type)> = HashMap::new();
    let mut stack_size: usize = 0;
    let mut init_lines: Vec<String> = vec![];

    for entry in fun_ir {
        for var in entry.variables() {
            add_var(&var, &mut locals, &mut stack_size, &mut init_lines);
        }
    }

    // align to 16 bytes
    let locals_size = 8 * (stack_size + stack_size % 2);

    let function_body = fun_ir.iter().map(|entry| {

        let asm = match &entry.instruction {
            Instr::FunctionLabel { name, params } => {
                // Function prelude and whatnot
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
                    let (local_address,_) = get_address_of_var(param, &mut locals).expect(format!("In {fun_name}, param {} is not defined", param.to_string()).as_str());
                    lines.push(mov(&param_reg, &local_address.to_string()));
                }
                // Add some variable inits
                lines.extend(init_lines.iter().map(|l| l.to_string()));
                lines
            },
            Instr::LoadIntConst { value, dest } => {
                let (dest_loc, mut lines) = get_var_address(&dest, &locals);
                if i32::MIN as i64 <= *value && *value <= i32::MAX as i64 {
                    lines.push(format!("movq ${}, {}", value, dest_loc.to_string()));
                } else {
                    lines.push(format!("movabsq ${value}, %rax"));
                    lines.push(mov("%rax", &dest_loc.to_string()));
                }
                lines
            },
            Instr::LoadBoolConst { value, dest } => {
                let (dest_loc, mut lines) = get_var_address(&dest, &locals);
                let byte_val = if *value { 1 } else { 0 };
                lines.push(format!("movq ${}, {}", byte_val, dest_loc.to_string()));
                lines
            },
            Instr::Call { fun, args, dest } => {
                let mut lines = generate_call(fun, args, &locals);
                let (dest_loc, dest_lines) = get_var_address(&dest, &locals);
                lines.extend(dest_lines);
                lines.push(mov("%rax", &dest_loc.to_string()));
                lines
            },
            Instr::Copy { source, dest } => {
                let (src_loc, mut lines) = get_var_address_using_argument_reg(&source, &locals, 0);
                let (dest_loc, arg_lines) = get_var_address_using_argument_reg(&dest, &locals, 1);
                lines.extend(arg_lines);

                lines.extend(copy(&src_loc, &dest_loc));
                lines
            },
            Instr::Label(name) => vec![format!("{name}:")],
            Instr::Jump(label) => vec![format!("jmp {}", label)],
            Instr::CondJump { cond, then_label, else_label } => {
                let (cond_loc, mut lines) = get_var_address(&cond, &locals);
                
                lines.push(format!("cmpq $0, {}", cond_loc.to_string()));
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

fn generate_call(fun: &Box<IRVar>, args: &Vec<Box<IRVar>>, addresses: &HashMap<String, (Address, Type)>) -> Vec<String> {
    let callee = match args.len() {
        1 => Op::unary_from_str(&fun.name).map(Symbol::Operator),
        2 => Op::binary_from_str(&fun.name).map(Symbol::Operator),
        _ => Err("")
    }.unwrap_or(Symbol::Identifier(fun.name.clone()));

    // Next generate the actual function call, which uses the argument registers and returns the result in %rax
    match callee {
        Symbol::Operator(op) => {
            let arg_1 = &args[0];
            let (arg_1_loc, mut lines) = get_var_address(&arg_1, addresses);
            let arg_2_opt = args.get(1).map(|ir_var| get_var_address_using_argument_reg(&ir_var, addresses, 1));

            if let Some((arg_2_loc, arg_lines)) = arg_2_opt {
                lines.extend(arg_lines);
    
                lines.extend(match op {
                    Op::Add => bin_op(&arg_1_loc, &arg_2_loc, "addq"),
                    Op::Sub => bin_op(&arg_1_loc, &arg_2_loc, "subq"),
                    Op::Mul => bin_op(&arg_1_loc, &arg_2_loc, "imulq"),
                    Op::Div => {
                        vec![
                            mov(&arg_1_loc.to_string(), "%rax"),
                            format!("cqto"), // wottefok
                            format!("idivq {}", arg_2_loc.to_string()),
                        ]
                    },
                    Op::Mod => {
                        vec![
                            mov(&arg_1_loc.to_string(), "%rax"),
                            format!("cqto"), // wottefok
                            format!("idivq {}", arg_2_loc.to_string()),
                            mov("%rdx", "%rax"),   // %rdx contains remainder    
                        ]
                    },
                    Op::Equals => comparison(&arg_1_loc, &arg_2_loc, "sete"),
                    Op::NotEquals => comparison(&arg_1_loc, &arg_2_loc, "setne"),
                    Op::GT => comparison(&arg_1_loc, &arg_2_loc, "setg"),
                    Op::GTE => comparison(&arg_1_loc, &arg_2_loc, "setge"),
                    Op::LT => comparison(&arg_1_loc, &arg_2_loc, "setl"),
                    Op::LTE => comparison(&arg_1_loc, &arg_2_loc, "setle"),
                    _ => panic!("{:?} does not have an intrinsic definition", op)
                });
            } else {
                lines.extend(match op {
                    Op::Not => {
                        vec![
                            mov(&arg_1_loc.to_string(), "%rax"),
                            format!("xorq $0x1, %rax"),
                        ]
                    },
                    Op::UnarySub => {
                        vec![
                            mov(&arg_1_loc.to_string(), "%rax"),
                            format!("negq %rax"),
                        ]
                    },
                    Op::AddressOf => {
                        vec![
                            format!("leaq {}, %rax", arg_1_loc.to_string()),
                        ]
                    },
                    Op::Deref => {
                        vec![
                            mov(&arg_1_loc.to_string(), "%rax"),
                            mov("(%rax)", "%rax"),
                        ]
                    },
                    Op::New => {
                        // Allocate memory with malloc, storing the block base address at %rdi
                        let mut mm = vec![
                            mov(format!("${}", arg_1.size() * 8).as_str(), "%rdi"),
                            format!("call malloc"),
                            format!("test %rax, %rax # Check that malloc succeeded"),
                            format!("jz .Lerr"),
                            format!("movq %rax, %rdi # Store block base address at %rdi"),
                        ];
                        // Iterate over the fields of arg_1
                        for (offset, param) in arg_1.var_type.get_fields().iter().enumerate() {
                            // arg_var refers to the field of arg_1
                            let mut arg_var = IRVar::new(param.name.clone(), param.param_type.clone());
                            arg_var.parent = Some(arg_1.clone());
                            // Compute the address of arg_var and store it in source_addr
                            let (source_addr, arg_lines) = get_var_address(&arg_var, addresses);
                            mm.extend(arg_lines);
                            // Copy from source_addr to the block base + offset
                            mm.append(&mut copy(
                                &source_addr,
                                &Address::RegisterPointer((String::from("%rdi"), offset as i64 * 8)),
                            ));
                        }
                        mm.push(format!("{} # Move the pointer to variable", mov("%rdi", "%rax")));
                        mm
                    },
                    Op::Delete => {
                        vec![
                            mov(&arg_1_loc.to_string(), "%rdi"),
                            format!("call free"),
                        ]
                    },
                    _ => todo!("{:?}", op)
                });
            };
            lines
        },
        Symbol::Identifier(name) => {
            match name.as_str() {
                _ => generate_function_call(fun.clone(), args, addresses)
            }
        }
    }
}

fn generate_function_call(fun: Box<IRVar>, args: &Vec<Box<IRVar>>, addresses: &HashMap<String, (Address, Type)>) -> Vec<String> {
    let mut lines = vec![];
    for (idx, arg) in args.iter().enumerate() {
        let (arg_loc, arg_lines) = get_var_address_using_argument_reg(&arg, addresses, idx);
        lines.extend(arg_lines);
        lines.push(mov(&arg_loc.to_string(),&get_argument_register(idx)));
    }
    // Slightly different calling whether the function has an id or not.
    if let Type::Function { id: Some(id),.. } = &fun.var_type {
        // Call it by name
        lines.push(format!("call {id}"));
    } else {
        // Call it by address
        let (func_addr, func_addr_lines) = get_var_address(&fun, addresses);
        lines.extend(func_addr_lines);
        lines.extend(copy(&func_addr, &Address::Register(String::from("%rax"))));
        lines.push(format!("call *%rax"));
    }
    lines
}

fn get_var_address_using_register(var: &IRVar, addresses: &HashMap<String, (Address, Type)>, address_reg: String) -> (Address, Vec<String>) {
    // eprintln!("Getting address of {}", var.to_string());
    if let Some(parent) = &var.parent {
        // Get base address
        // eprintln!("Getting address of parent {}", parent.to_string());
        let (base_address, mut lines) = get_var_address_using_register(parent, addresses, address_reg.clone());
        // Special case if were referring to a field of a struct
        match &parent.var_type {
            // Value of pointer?
            Type::Pointer(pointer_type) => {
                // Member value of pointer?
                if let Type::Struct(struct_type) = pointer_type.as_ref() {
                    // Put struct base pointer in %rdx
                    lines.push(format!("{} # Put struct '{}' base pointer in {}", mov(&base_address.to_string(), &address_reg), parent.name, address_reg));
                    // Get offset (multiplied by 8) and add it to the base pointer
                    // eprintln!("{:?} -> {:?}", struct_type, var.to_short_string());
                    let (offset,_) = struct_type.get_member(&var.name);

                    return (Address::RegisterPointer((address_reg, offset as i64 * 8)), lines)
                } else if var.name == "value" { // Just the base address
                    lines.push(format!("{} # Put pointer address '{}' in {}", mov(&base_address.to_string(), &address_reg), parent.name, address_reg));
                    return (Address::RegisterPointer((address_reg, 0)), lines)
                } else {
                    panic!("Cannot refer to non-struct pointer by {:?}", var.name)
                }
            },
            // Direct struct
            Type::Struct(struct_type) => {
                let addr = match base_address {
                    Address::Memory(addr) => addr,
                    // Address::RegisterPointer(_) => todo!(),
                    _ => panic!("Direct struct should only be in memory")
                };

                let (offset,_) = struct_type.get_member(&var.name);
                let addr = addr + offset as i64 * 8;
                return (Address::Memory(addr), vec![])
            },
            _ => {
                // Whatever we're getting, the field name should be "value" as this is a primitive. (Abstract this and the above branch to single case?)
                if var.name != "value" {
                    panic!("Primitive type {:?} has no field {}", parent.var_type, var.name)
                }
                return (base_address, vec![])
            },
        }
        
    } else {
        let (address,_) = addresses.get(&var.name).expect(format!("Address of var {} to be defined", var.name).as_str());
        (address.clone(), vec![])
    }
}

fn get_var_address(var: &IRVar, addresses: &HashMap<String, (Address, Type)>) -> (Address, Vec<String>) {
    let (addr, lines) = get_var_address_using_register(var, addresses, String::from("%rax"));
    (addr, lines)
}

fn get_var_address_using_argument_reg(var: &IRVar, addresses: &HashMap<String, (Address, Type)>, idx: usize) -> (Address, Vec<String>) {
    get_var_address_using_register(var, addresses, get_argument_register(idx))
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

fn bin_op(arg_1: &Address, arg_2: &Address, op: &str) -> Vec<String> { // todo fix this formatting
    vec![
        mov(&arg_1.to_string(), "%rax"),
        format!("{} {}, %rax", op, arg_2.to_string()),
    ]
}

fn copy(arg1: &Address, arg2: &Address) -> Vec<String> {
    let mut lines = vec![];
    let tmp = String::from("%rax");
    lines.push(mov(&arg1.to_string(), &tmp));
    lines.push(mov(&tmp, &arg2.to_string()));
    lines
}

fn mov(arg1: &str, arg2: &str) -> String {
    if arg1 != arg2 {
        return format!("movq {arg1}, {arg2}")
    }
    format!("# skip movq {arg1}, {arg2}")
}

fn comparison(arg1: &Address, arg2: &Address, op: &str) -> Vec<String> {
    vec![
        format!("xor %rax, %rax"),
        mov(&arg1.to_string(), "%rdx"),
        format!("cmpq {}, %rdx", arg2.to_string()),
        format!("{} %al", op),
    ]
}

fn add_stdlib_code(source: String) -> String {
    format!("
# Metadata for debuggers and other tools
.extern printf
.extern scanf
.extern malloc
.extern free

.section .rodata
scan_format:
.asciz \"%ld\"

print_format:
.asciz \"%ld\\n\"

print_bool_format:
.asciz \"%s\\n\"

true_str:  .string \"true\\n\"
false_str: .string \"false\\n\"

.text

{}

# Error
.Lerr:
        # Return from main with status code 1
        movq $1, %rax
        movq %rbp, %rsp
        popq %rbp
        ret

.global print_int
.type print_int, @function
print_int:
    pushq %rbp
    movq %rsp, %rbp
    movq %rdi, %rsi
    movq $print_format, %rdi
    call printf
    movq %rbp, %rsp
    popq %rbp
    ret

.global print_bool
.type print_bool, @function
print_bool:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp

    testq %rdi, %rdi
    jz .Lprint_false
.Lprint_true:
    movq $true_str, %rdi
    jmp .Lprint

.Lprint_false:
    movq $false_str, %rdi

.Lprint:
    xorl %eax, %eax
    call printf

    movq %rbp, %rsp
    popq %rbp
    ret

.global read_int
.type read_int, @function
read_int:
    pushq %rbp
    movq %rsp, %rbp
        movq $scan_format, %rdi
        call scanf
        cmpq $1, %rax
        jne .Lerr
        movq %rsi, %rax
    movq %rbp, %rsp
    popq %rbp
    ret

# String data that we pass to functions 'scanf' and 'printf'


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

        println!("{}", ir.get("identity").unwrap().iter().map(|i| i.to_string()).collect::<Vec<String>>().join("\n"));

        generate_asm(ir);
    }

    #[test]
    fn regression_2() {
        let ir = generate_ir(
            typecheck_program(
                parse_source(String::from("
                var x: Int* = &3;
                var y: Int* = &4;
                            
                *x = *x * *y;
                print_int(*x); // Prints 12
                "))
            )
        );

        println!("{}", ir.get("main").unwrap().iter().map(|i| i.to_string()).collect::<Vec<String>>().join("\n"));

        println!("{}", generate_asm(ir));
    }

    #[test]
    fn structs() {
        let _ir = generate_ir(
            typecheck_program(
                parse_source(String::from("
                struct Tail { length: Int }
                struct Dog { size: Int, tail: Tail* }
                var doggo = new Dog { size: 100, tail: new Tail { length: 5 } };
                doggo.tail.length = 6;
                "))
            )
        );

        println!("{}", _ir.get("main").unwrap().iter().map(|i| i.to_string()).collect::<Vec<String>>().join("\n"));

        println!("{}", generate_asm(_ir));
    }

    #[test]
    fn struct_arg() {
        let ir = generate_ir(
            typecheck_program(
                parse_source(String::from("
                struct Tail { length: Int }
                struct Dog { size: Int, isHungry: Bool, tail: Tail* }
                fun create_dog(size: Int, isHungry: Bool, tail_length: Int): Dog* {
                    new Dog { size: size, isHungry: isHungry, tail: new Tail { length: tail_length } }
                }

                fun print_dog(dog: Dog*) {
                    print_int(dog.size);
                    print_bool(dog.isHungry);
                    print_int(dog.tail.length);
                }

                var doggo = create_dog(10, true, 5);
                "))
            )
        );

        println!("{}", ir.get("main").unwrap().iter().map(|i| i.to_string()).collect::<Vec<String>>().join("\n"));

        println!("{}", generate_asm(ir));
    }

    #[test]
    fn primitive_heap_alloc() {
        let ir = generate_ir(
            typecheck_program(
                parse_source(String::from(" 
                  var x: Int* = new Int(123);
                "))
            )
        );

        println!("{}", ir.get("main").unwrap().iter().map(|i| i.to_string()).collect::<Vec<String>>().join("\n"));

        println!("{}", generate_asm(ir));
    }
}
