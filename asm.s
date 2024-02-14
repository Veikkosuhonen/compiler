
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
subq $112, %rsp

# Label("start")
.start:

# Call { fun: IRVar { name: Identifier("read_int"), var_type: Function(FunctionType { param_types: [], return_type: Integer }) }, args: [], dest: IRVar { name: Identifier("x0"), var_type: Integer } }
movq $scan_format, %rdi
leaq -8(%rbp), %rsi
call scanf
cmpq $1, %rax
jne .Lend

# LoadIntConst { value: 0, dest: IRVar { name: Identifier("x1"), var_type: Integer } }
movq $0, -16(%rbp)

# Label("L0")
.L0:

# LoadIntConst { value: 10, dest: IRVar { name: Identifier("x3"), var_type: Integer } }
movq $10, -24(%rbp)

# Call { fun: IRVar { name: Operator(LT), var_type: Function(FunctionType { param_types: [Integer, Integer], return_type: Boolean }) }, args: [IRVar { name: Identifier("x1"), var_type: Integer }, IRVar { name: Identifier("x3"), var_type: Integer }], dest: IRVar { name: Identifier("x4"), var_type: Boolean } }
xor %rax, %rax
movq -16(%rbp), %rdx
cmpq -24(%rbp), %rdx
setl %al
movq %rax, -32(%rbp)

# CondJump { cond: IRVar { name: Identifier("x4"), var_type: Boolean }, then_label: "L1", else_label: "L2" }
cmpq $0, -32(%rbp)
jne .L1
jmp .L2

# Label("L1")
.L1:

# LoadIntConst { value: 2, dest: IRVar { name: Identifier("x5"), var_type: Integer } }
movq $2, -40(%rbp)

# Call { fun: IRVar { name: Operator(Mul), var_type: Function(FunctionType { param_types: [Integer, Integer], return_type: Integer }) }, args: [IRVar { name: Identifier("x5"), var_type: Integer }, IRVar { name: Identifier("x0"), var_type: Integer }], dest: IRVar { name: Identifier("x6"), var_type: Integer } }
movq -8(%rbp), %rax 
imulq -40(%rbp), %rax 
movq %rax, -48(%rbp)

# Copy { source: IRVar { name: Identifier("x6"), var_type: Integer }, dest: IRVar { name: Identifier("x0"), var_type: Integer } }
movq -48(%rbp), %rax
movq %rax, -8(%rbp)

# LoadIntConst { value: 1, dest: IRVar { name: Identifier("x7"), var_type: Integer } }
movq $1, -56(%rbp)

# Call { fun: IRVar { name: Operator(Add), var_type: Function(FunctionType { param_types: [Integer, Integer], return_type: Integer }) }, args: [IRVar { name: Identifier("x1"), var_type: Integer }, IRVar { name: Identifier("x7"), var_type: Integer }], dest: IRVar { name: Identifier("x8"), var_type: Integer } }
movq -56(%rbp), %rax 
addq -16(%rbp), %rax 
movq %rax, -64(%rbp)

# Copy { source: IRVar { name: Identifier("x8"), var_type: Integer }, dest: IRVar { name: Identifier("x1"), var_type: Integer } }
movq -64(%rbp), %rax
movq %rax, -16(%rbp)

# Call { fun: IRVar { name: Identifier("print_int"), var_type: Function(FunctionType { param_types: [Integer], return_type: Unit }) }, args: [IRVar { name: Identifier("x0"), var_type: Integer }], dest: IRVar { name: Identifier("x9"), var_type: Unit } }
movq -8(%rbp), %rsi
movq $print_format, %rdi
call printf

# Copy { source: IRVar { name: Identifier("U"), var_type: Unit }, dest: IRVar { name: Identifier("x2"), var_type: Unit } }
movq -80(%rbp), %rax
movq %rax, -88(%rbp)

# Jump("L0")
jmp .L0

# Label("L2")
.L2:

# LoadIntConst { value: 5000, dest: IRVar { name: Identifier("x10"), var_type: Integer } }
movq $5000, -96(%rbp)

# Call { fun: IRVar { name: Operator(GT), var_type: Function(FunctionType { param_types: [Integer, Integer], return_type: Boolean }) }, args: [IRVar { name: Identifier("x0"), var_type: Integer }, IRVar { name: Identifier("x10"), var_type: Integer }], dest: IRVar { name: Identifier("x11"), var_type: Boolean } }
xor %rax, %rax
movq -8(%rbp), %rdx
cmpq -96(%rbp), %rdx
setg %al
movq %rax, -104(%rbp)

# Call { fun: IRVar { name: Identifier("print_bool"), var_type: Function(FunctionType { param_types: [Boolean], return_type: Unit }) }, args: [IRVar { name: Identifier("x11"), var_type: Boolean }], dest: IRVar { name: Identifier("x12"), var_type: Unit } }
movq -104(%rbp), %rsi
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

