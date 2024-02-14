
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
subq $432, %rsp

# Label("start")
.start:

# LoadIntConst { value: 2, dest: IRVar { name: Identifier("x0"), var_type: Integer } }
movq $2, -8(%rbp)

# LoadIntConst { value: 1, dest: IRVar { name: Identifier("x1"), var_type: Integer } }
movq $1, -16(%rbp)

# Call { fun: IRVar { name: Operator(GT), var_type: Function(FunctionType { param_types: [Integer, Integer], return_type: Boolean }) }, args: [IRVar { name: Identifier("x0"), var_type: Integer }, IRVar { name: Identifier("x1"), var_type: Integer }], dest: IRVar { name: Identifier("x2"), var_type: Boolean } }
xor %rax, %rax
movq -8(%rbp), %rdx
cmpq -16(%rbp), %rdx
setg %al
movq %rax, -24(%rbp)

# Call { fun: IRVar { name: Identifier("print_bool"), var_type: Function(FunctionType { param_types: [Boolean], return_type: Unit }) }, args: [IRVar { name: Identifier("x2"), var_type: Boolean }], dest: IRVar { name: Identifier("x3"), var_type: Unit } }
movq -24(%rbp), %rsi
andq $0x1, %rsi
movq $print_format, %rdi
call printf

# LoadIntConst { value: 1, dest: IRVar { name: Identifier("x4"), var_type: Integer } }
movq $1, -40(%rbp)

# LoadIntConst { value: 2, dest: IRVar { name: Identifier("x5"), var_type: Integer } }
movq $2, -48(%rbp)

# Call { fun: IRVar { name: Operator(GT), var_type: Function(FunctionType { param_types: [Integer, Integer], return_type: Boolean }) }, args: [IRVar { name: Identifier("x4"), var_type: Integer }, IRVar { name: Identifier("x5"), var_type: Integer }], dest: IRVar { name: Identifier("x6"), var_type: Boolean } }
xor %rax, %rax
movq -40(%rbp), %rdx
cmpq -48(%rbp), %rdx
setg %al
movq %rax, -56(%rbp)

# Call { fun: IRVar { name: Identifier("print_bool"), var_type: Function(FunctionType { param_types: [Boolean], return_type: Unit }) }, args: [IRVar { name: Identifier("x6"), var_type: Boolean }], dest: IRVar { name: Identifier("x7"), var_type: Unit } }
movq -56(%rbp), %rsi
andq $0x1, %rsi
movq $print_format, %rdi
call printf

# LoadIntConst { value: 1, dest: IRVar { name: Identifier("x8"), var_type: Integer } }
movq $1, -72(%rbp)

# LoadIntConst { value: 1, dest: IRVar { name: Identifier("x9"), var_type: Integer } }
movq $1, -80(%rbp)

# Call { fun: IRVar { name: Operator(GTE), var_type: Function(FunctionType { param_types: [Integer, Integer], return_type: Boolean }) }, args: [IRVar { name: Identifier("x8"), var_type: Integer }, IRVar { name: Identifier("x9"), var_type: Integer }], dest: IRVar { name: Identifier("x10"), var_type: Boolean } }
xor %rax, %rax
movq -72(%rbp), %rdx
cmpq -80(%rbp), %rdx
setge %al
movq %rax, -88(%rbp)

# Call { fun: IRVar { name: Identifier("print_bool"), var_type: Function(FunctionType { param_types: [Boolean], return_type: Unit }) }, args: [IRVar { name: Identifier("x10"), var_type: Boolean }], dest: IRVar { name: Identifier("x11"), var_type: Unit } }
movq -88(%rbp), %rsi
andq $0x1, %rsi
movq $print_format, %rdi
call printf

# LoadIntConst { value: 1, dest: IRVar { name: Identifier("x12"), var_type: Integer } }
movq $1, -104(%rbp)

# LoadIntConst { value: 2, dest: IRVar { name: Identifier("x13"), var_type: Integer } }
movq $2, -112(%rbp)

# Call { fun: IRVar { name: Operator(GTE), var_type: Function(FunctionType { param_types: [Integer, Integer], return_type: Boolean }) }, args: [IRVar { name: Identifier("x12"), var_type: Integer }, IRVar { name: Identifier("x13"), var_type: Integer }], dest: IRVar { name: Identifier("x14"), var_type: Boolean } }
xor %rax, %rax
movq -104(%rbp), %rdx
cmpq -112(%rbp), %rdx
setge %al
movq %rax, -120(%rbp)

# Call { fun: IRVar { name: Identifier("print_bool"), var_type: Function(FunctionType { param_types: [Boolean], return_type: Unit }) }, args: [IRVar { name: Identifier("x14"), var_type: Boolean }], dest: IRVar { name: Identifier("x15"), var_type: Unit } }
movq -120(%rbp), %rsi
andq $0x1, %rsi
movq $print_format, %rdi
call printf

# LoadIntConst { value: 1, dest: IRVar { name: Identifier("x16"), var_type: Integer } }
movq $1, -136(%rbp)

# LoadIntConst { value: 2, dest: IRVar { name: Identifier("x17"), var_type: Integer } }
movq $2, -144(%rbp)

# Call { fun: IRVar { name: Operator(LT), var_type: Function(FunctionType { param_types: [Integer, Integer], return_type: Boolean }) }, args: [IRVar { name: Identifier("x16"), var_type: Integer }, IRVar { name: Identifier("x17"), var_type: Integer }], dest: IRVar { name: Identifier("x18"), var_type: Boolean } }
xor %rax, %rax
movq -136(%rbp), %rdx
cmpq -144(%rbp), %rdx
setl %al
movq %rax, -152(%rbp)

# Call { fun: IRVar { name: Identifier("print_bool"), var_type: Function(FunctionType { param_types: [Boolean], return_type: Unit }) }, args: [IRVar { name: Identifier("x18"), var_type: Boolean }], dest: IRVar { name: Identifier("x19"), var_type: Unit } }
movq -152(%rbp), %rsi
andq $0x1, %rsi
movq $print_format, %rdi
call printf

# LoadIntConst { value: 2, dest: IRVar { name: Identifier("x20"), var_type: Integer } }
movq $2, -168(%rbp)

# LoadIntConst { value: 1, dest: IRVar { name: Identifier("x21"), var_type: Integer } }
movq $1, -176(%rbp)

# Call { fun: IRVar { name: Operator(LT), var_type: Function(FunctionType { param_types: [Integer, Integer], return_type: Boolean }) }, args: [IRVar { name: Identifier("x20"), var_type: Integer }, IRVar { name: Identifier("x21"), var_type: Integer }], dest: IRVar { name: Identifier("x22"), var_type: Boolean } }
xor %rax, %rax
movq -168(%rbp), %rdx
cmpq -176(%rbp), %rdx
setl %al
movq %rax, -184(%rbp)

# Call { fun: IRVar { name: Identifier("print_bool"), var_type: Function(FunctionType { param_types: [Boolean], return_type: Unit }) }, args: [IRVar { name: Identifier("x22"), var_type: Boolean }], dest: IRVar { name: Identifier("x23"), var_type: Unit } }
movq -184(%rbp), %rsi
andq $0x1, %rsi
movq $print_format, %rdi
call printf

# LoadIntConst { value: 1, dest: IRVar { name: Identifier("x24"), var_type: Integer } }
movq $1, -200(%rbp)

# LoadIntConst { value: 1, dest: IRVar { name: Identifier("x25"), var_type: Integer } }
movq $1, -208(%rbp)

# Call { fun: IRVar { name: Operator(LTE), var_type: Function(FunctionType { param_types: [Integer, Integer], return_type: Boolean }) }, args: [IRVar { name: Identifier("x24"), var_type: Integer }, IRVar { name: Identifier("x25"), var_type: Integer }], dest: IRVar { name: Identifier("x26"), var_type: Boolean } }
xor %rax, %rax
movq -200(%rbp), %rdx
cmpq -208(%rbp), %rdx
setle %al
movq %rax, -216(%rbp)

# Call { fun: IRVar { name: Identifier("print_bool"), var_type: Function(FunctionType { param_types: [Boolean], return_type: Unit }) }, args: [IRVar { name: Identifier("x26"), var_type: Boolean }], dest: IRVar { name: Identifier("x27"), var_type: Unit } }
movq -216(%rbp), %rsi
andq $0x1, %rsi
movq $print_format, %rdi
call printf

# LoadIntConst { value: 2, dest: IRVar { name: Identifier("x28"), var_type: Integer } }
movq $2, -232(%rbp)

# LoadIntConst { value: 1, dest: IRVar { name: Identifier("x29"), var_type: Integer } }
movq $1, -240(%rbp)

# Call { fun: IRVar { name: Operator(LTE), var_type: Function(FunctionType { param_types: [Integer, Integer], return_type: Boolean }) }, args: [IRVar { name: Identifier("x28"), var_type: Integer }, IRVar { name: Identifier("x29"), var_type: Integer }], dest: IRVar { name: Identifier("x30"), var_type: Boolean } }
xor %rax, %rax
movq -232(%rbp), %rdx
cmpq -240(%rbp), %rdx
setle %al
movq %rax, -248(%rbp)

# Call { fun: IRVar { name: Identifier("print_bool"), var_type: Function(FunctionType { param_types: [Boolean], return_type: Unit }) }, args: [IRVar { name: Identifier("x30"), var_type: Boolean }], dest: IRVar { name: Identifier("x31"), var_type: Unit } }
movq -248(%rbp), %rsi
andq $0x1, %rsi
movq $print_format, %rdi
call printf

# LoadIntConst { value: 123, dest: IRVar { name: Identifier("x32"), var_type: Integer } }
movq $123, -264(%rbp)

# LoadIntConst { value: 123, dest: IRVar { name: Identifier("x33"), var_type: Integer } }
movq $123, -272(%rbp)

# Call { fun: IRVar { name: Operator(Equals), var_type: Function(FunctionType { param_types: [Integer, Integer], return_type: Boolean }) }, args: [IRVar { name: Identifier("x32"), var_type: Integer }, IRVar { name: Identifier("x33"), var_type: Integer }], dest: IRVar { name: Identifier("x34"), var_type: Boolean } }
xor %rax, %rax
movq -264(%rbp), %rdx
cmpq -272(%rbp), %rdx
sete %al
movq %rax, -280(%rbp)

# Call { fun: IRVar { name: Identifier("print_bool"), var_type: Function(FunctionType { param_types: [Boolean], return_type: Unit }) }, args: [IRVar { name: Identifier("x34"), var_type: Boolean }], dest: IRVar { name: Identifier("x35"), var_type: Unit } }
movq -280(%rbp), %rsi
andq $0x1, %rsi
movq $print_format, %rdi
call printf

# LoadIntConst { value: 122, dest: IRVar { name: Identifier("x36"), var_type: Integer } }
movq $122, -296(%rbp)

# LoadIntConst { value: 123, dest: IRVar { name: Identifier("x37"), var_type: Integer } }
movq $123, -304(%rbp)

# Call { fun: IRVar { name: Operator(Equals), var_type: Function(FunctionType { param_types: [Integer, Integer], return_type: Boolean }) }, args: [IRVar { name: Identifier("x36"), var_type: Integer }, IRVar { name: Identifier("x37"), var_type: Integer }], dest: IRVar { name: Identifier("x38"), var_type: Boolean } }
xor %rax, %rax
movq -296(%rbp), %rdx
cmpq -304(%rbp), %rdx
sete %al
movq %rax, -312(%rbp)

# Call { fun: IRVar { name: Identifier("print_bool"), var_type: Function(FunctionType { param_types: [Boolean], return_type: Unit }) }, args: [IRVar { name: Identifier("x38"), var_type: Boolean }], dest: IRVar { name: Identifier("x39"), var_type: Unit } }
movq -312(%rbp), %rsi
andq $0x1, %rsi
movq $print_format, %rdi
call printf

# LoadIntConst { value: 122, dest: IRVar { name: Identifier("x40"), var_type: Integer } }
movq $122, -328(%rbp)

# LoadIntConst { value: 123, dest: IRVar { name: Identifier("x41"), var_type: Integer } }
movq $123, -336(%rbp)

# Call { fun: IRVar { name: Operator(NotEquals), var_type: Function(FunctionType { param_types: [Integer, Integer], return_type: Boolean }) }, args: [IRVar { name: Identifier("x40"), var_type: Integer }, IRVar { name: Identifier("x41"), var_type: Integer }], dest: IRVar { name: Identifier("x42"), var_type: Boolean } }
xor %rax, %rax
movq -328(%rbp), %rdx
cmpq -336(%rbp), %rdx
setne %al
movq %rax, -344(%rbp)

# Call { fun: IRVar { name: Identifier("print_bool"), var_type: Function(FunctionType { param_types: [Boolean], return_type: Unit }) }, args: [IRVar { name: Identifier("x42"), var_type: Boolean }], dest: IRVar { name: Identifier("x43"), var_type: Unit } }
movq -344(%rbp), %rsi
andq $0x1, %rsi
movq $print_format, %rdi
call printf

# LoadIntConst { value: 123, dest: IRVar { name: Identifier("x44"), var_type: Integer } }
movq $123, -360(%rbp)

# LoadIntConst { value: 123, dest: IRVar { name: Identifier("x45"), var_type: Integer } }
movq $123, -368(%rbp)

# Call { fun: IRVar { name: Operator(NotEquals), var_type: Function(FunctionType { param_types: [Integer, Integer], return_type: Boolean }) }, args: [IRVar { name: Identifier("x44"), var_type: Integer }, IRVar { name: Identifier("x45"), var_type: Integer }], dest: IRVar { name: Identifier("x46"), var_type: Boolean } }
xor %rax, %rax
movq -360(%rbp), %rdx
cmpq -368(%rbp), %rdx
setne %al
movq %rax, -376(%rbp)

# Call { fun: IRVar { name: Identifier("print_bool"), var_type: Function(FunctionType { param_types: [Boolean], return_type: Unit }) }, args: [IRVar { name: Identifier("x46"), var_type: Boolean }], dest: IRVar { name: Identifier("x47"), var_type: Unit } }
movq -376(%rbp), %rsi
andq $0x1, %rsi
movq $print_format, %rdi
call printf

# LoadBoolConst { value: false, dest: IRVar { name: Identifier("x48"), var_type: Boolean } }
movq $0, -392(%rbp)

# Call { fun: IRVar { name: Operator(Not), var_type: Function(FunctionType { param_types: [Boolean], return_type: Boolean }) }, args: [IRVar { name: Identifier("x48"), var_type: Boolean }], dest: IRVar { name: Identifier("x49"), var_type: Boolean } }
movq -392(%rbp), %rax
xorq $0x1, %rax
movq %rax, -400(%rbp)

# Call { fun: IRVar { name: Identifier("print_bool"), var_type: Function(FunctionType { param_types: [Boolean], return_type: Unit }) }, args: [IRVar { name: Identifier("x49"), var_type: Boolean }], dest: IRVar { name: Identifier("x50"), var_type: Unit } }
movq -400(%rbp), %rsi
andq $0x1, %rsi
movq $print_format, %rdi
call printf

# LoadBoolConst { value: true, dest: IRVar { name: Identifier("x51"), var_type: Boolean } }
movq $1, -416(%rbp)

# Call { fun: IRVar { name: Operator(Not), var_type: Function(FunctionType { param_types: [Boolean], return_type: Boolean }) }, args: [IRVar { name: Identifier("x51"), var_type: Boolean }], dest: IRVar { name: Identifier("x52"), var_type: Boolean } }
movq -416(%rbp), %rax
xorq $0x1, %rax
movq %rax, -424(%rbp)

# Call { fun: IRVar { name: Identifier("print_bool"), var_type: Function(FunctionType { param_types: [Boolean], return_type: Unit }) }, args: [IRVar { name: Identifier("x52"), var_type: Boolean }], dest: IRVar { name: Identifier("x53"), var_type: Unit } }
movq -424(%rbp), %rsi
andq $0x1, %rsi
movq $print_format, %rdi
call printf

# Return
# return

# Labels starting with ".L" are local to this function,
# i.e. another function than "main" could have its own ".Lend".
.Lend:
# Return from main with status code 0
movq $0, %rax
movq %rbp, %rsp
popq %rbp
ret

# String data that we pass to functions 'scanf' and 'printf'
scan_format:
.asciz "%ld"

print_format:
.asciz "%ld\n"

print_bool_format:
.asciz "%s\n"

true_str:
    .ascii "true\n"

false_str:
    .ascii "false\n"

