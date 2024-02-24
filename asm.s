
# Metadata for debuggers and other tools
.extern printf
.extern scanf

.section .text  # Begins code and data


        # Function(main)
        .global main
        .type main, @function
        main:
        pushq %rbp
        movq %rsp, %rbp
        subq $24, %rsp
        # param backups (0)

        # LoadIntConst(19, x0)
        movq $19, -8(%rbp)

        # Call(fibonacci, [x0], x1)
        movq -8(%rbp), %rdi
        call fibonacci
        movq %rax, -16(%rbp)

        # Call(print_int, [x1], x2)
        movq -16(%rbp), %rsi
        movq $print_format, %rdi
        call printf

        # Return(U)
        movq $0, %rax

        # Restore stack pointer
        movq %rbp, %rsp
        popq %rbp
        ret


        
        # Function(fibonacci)
        .global fibonacci
        .type fibonacci, @function
        fibonacci:
        pushq %rbp
        movq %rsp, %rbp
        subq $128, %rsp
        # param backups (1)
        movq %rdi, -16(%rbp)

        # LoadIntConst(0, x1)
        movq $0, -8(%rbp)

        # Call(<=, [p0,x1], x2)
        xor %rax, %rax
        movq -16(%rbp), %rdx
        cmpq -8(%rbp), %rdx
        setle %al
        movq %rax, -24(%rbp)

        # CondJump(x2, .Lfibonacci_0, .Lfibonacci_2)
        cmpq $0, -24(%rbp)
        jne .Lfibonacci_0
        jmp .Lfibonacci_2

        # Label(.Lfibonacci_0)
        .Lfibonacci_0:

        # LoadIntConst(0, x3)
        movq $0, -32(%rbp)

        # Copy(x3, x0)
        movq -32(%rbp), %rax
        movq %rax, -40(%rbp)

        # Jump(.Lfibonacci_1)
        jmp .Lfibonacci_1

        # Label(.Lfibonacci_2)
        .Lfibonacci_2:

        # LoadIntConst(1, x5)
        movq $1, -48(%rbp)

        # Call(==, [p0,x5], x6)
        xor %rax, %rax
        movq -16(%rbp), %rdx
        cmpq -48(%rbp), %rdx
        sete %al
        movq %rax, -56(%rbp)

        # CondJump(x6, .Lfibonacci_3, .Lfibonacci_5)
        cmpq $0, -56(%rbp)
        jne .Lfibonacci_3
        jmp .Lfibonacci_5

        # Label(.Lfibonacci_3)
        .Lfibonacci_3:

        # LoadIntConst(1, x7)
        movq $1, -64(%rbp)

        # Copy(x7, x4)
        movq -64(%rbp), %rax
        movq %rax, -72(%rbp)

        # Jump(.Lfibonacci_4)
        jmp .Lfibonacci_4

        # Label(.Lfibonacci_5)
        .Lfibonacci_5:

        # LoadIntConst(1, x8)
        movq $1, -80(%rbp)

        # Call(-, [p0,x8], x9)
        movq -80(%rbp), %rax 
        subq -16(%rbp), %rax 
        movq %rax, -88(%rbp)

        # Call(fibonacci, [x9], x10)
        movq -88(%rbp), %rdi
        call fibonacci
        movq %rax, -96(%rbp)

        # LoadIntConst(2, x11)
        movq $2, -104(%rbp)

        # Call(-, [p0,x11], x12)
        movq -104(%rbp), %rax 
        subq -16(%rbp), %rax 
        movq %rax, -112(%rbp)

        # Call(fibonacci, [x12], x13)
        movq -112(%rbp), %rdi
        call fibonacci
        movq %rax, -120(%rbp)

        # Call(+, [x10,x13], x14)
        movq -120(%rbp), %rax 
        addq -96(%rbp), %rax 
        movq %rax, -128(%rbp)

        # Copy(x14, x4)
        movq -128(%rbp), %rax
        movq %rax, -72(%rbp)

        # Label(.Lfibonacci_4)
        .Lfibonacci_4:

        # Copy(x4, x0)
        movq -72(%rbp), %rax
        movq %rax, -40(%rbp)

        # Label(.Lfibonacci_1)
        .Lfibonacci_1:

        # Return(x0)
        movq -40(%rbp), %rax

        # Restore stack pointer
        movq %rbp, %rsp
        popq %rbp
        ret


# Actual end
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


