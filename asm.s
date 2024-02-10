
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
subq $32, %rsp

# Label("start")
.start:
# LoadBoolConst { value: false, dest: IRVar { name: "x0", var_type: Boolean } }
movq $0, -8(%rbp)
# CondJump { cond: IRVar { name: "x0", var_type: Boolean }, then_label: "L0", else_label: "L2" }
cmpq $0, -8(%rbp)
jne .L0
jmp .L2
# Label("L0")
.L0:
# LoadIntConst { value: 1, dest: IRVar { name: "x2", var_type: Integer } }
movq $1, -16(%rbp)
# Copy { source: IRVar { name: "x2", var_type: Integer }, dest: IRVar { name: "x1", var_type: Integer } }
movq -16(%rbp), %rax 
movq %rax, -24(%rbp)
# Jump("L1")
jmp .L1
# Label("L2")
.L2:
# LoadIntConst { value: 2, dest: IRVar { name: "x3", var_type: Integer } }
movq $2, -32(%rbp)
# Copy { source: IRVar { name: "x3", var_type: Integer }, dest: IRVar { name: "x1", var_type: Integer } }
movq -32(%rbp), %rax 
movq %rax, -24(%rbp)
# Label("L1")
.L1:
# Return
# return

movq -24(%rbp), %rsi

# Call function 'printf("%ld\n", %rsi)'
# to print the number in %rsi.
movq $print_format, %rdi
call printf

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

